# ==============================================================================
# Tests for fetch_core.R - Core fetch calculation functions
# ==============================================================================

test_that("fetch from center of circular lake equals radius", {
  # Create circular lake with known radius

radius <- 1000
  lake <- create_circular_lake(radius = radius, n_points = 720)
  site <- create_site(500000, 4800000, "Center")

  result <- calc_test_fetch(site, lake, buffer_m = 0)

  # Mean fetch should equal radius (within 2% tolerance for discretization)
  expect_equal(result$mean, radius, tolerance = 0.02)

  # All directions should be approximately equal
  expect_lt(sd(result$fetch), radius * 0.02)
})

test_that("fetch from edge of circular lake has correct min/max", {
  radius <- 1000
  lake <- create_circular_lake(radius = radius, n_points = 720)

  # Point at 80% of radius from center
  offset <- 0.8 * radius
  site <- create_site(500000 + offset, 4800000, "Edge")

  result <- calc_test_fetch(site, lake, buffer_m = 0)

  expected_min <- radius - offset  # 200m
  expected_max <- radius + offset  # 1800m

  # Min fetch should be approximately radius - offset
  expect_equal(result$min, expected_min, tolerance = 0.15)

  # Max fetch should be approximately radius + offset
  expect_equal(result$max, expected_max, tolerance = 0.05)
})

test_that("fetch from center of rectangular lake matches geometry", {
  width <- 2000   # E-W
  height <- 1000  # N-S

  lake <- create_rectangular_lake(width = width, height = height)
  site <- create_site(500000, 4800000, "Center")

  result <- calc_test_fetch(site, lake, buffer_m = 0)

  # Find cardinal direction indices
  n_idx <- which(result$angles == 0)
  e_idx <- which(result$angles == 90)
  s_idx <- which(result$angles == 180)
  w_idx <- which(result$angles == 270)

  # N/S fetch should be height/2
  expect_equal(result$fetch[n_idx], height / 2, tolerance = 0.05)
  expect_equal(result$fetch[s_idx], height / 2, tolerance = 0.05)

  # E/W fetch should be width/2
  expect_equal(result$fetch[e_idx], width / 2, tolerance = 0.05)
  expect_equal(result$fetch[w_idx], width / 2, tolerance = 0.05)
})

test_that("effective fetch calculated correctly", {
  radius <- 1000
  lake <- create_circular_lake(radius = radius, n_points = 720)
  site <- create_site(500000, 4800000, "Center")

  result <- calc_test_fetch(site, lake, buffer_m = 0)

  # Effective fetch = mean of top 3
  top3 <- sort(result$fetch, decreasing = TRUE)[1:3]
  effective <- mean(top3)

  # For circular lake at center, effective should equal radius
  expect_equal(effective, radius, tolerance = 0.02)
})

test_that("fetch respects maximum distance setting", {
  # Large lake where fetch would exceed max
  lake <- create_circular_lake(radius = 10000, n_points = 720)
  site <- create_site(500000, 4800000, "Center")

  # Set max fetch to 5000m
  old_max <- lakefetch_options()$max_fetch_m
  lakefetch_options(max_fetch_m = 5000)

  result <- calc_test_fetch(site, lake, buffer_m = 0)

  lakefetch_options(max_fetch_m = old_max)

  # All fetch values should be capped at 5000
  expect_true(all(result$fetch <= 5000))
})

test_that("angle resolution affects number of directions", {
  lake <- create_circular_lake(radius = 1000)
  site <- create_site(500000, 4800000, "Center")

  # Test with 5 degree resolution
  old_res <- lakefetch_options()$angle_resolution_deg
  lakefetch_options(angle_resolution_deg = 5)
  result5 <- calc_test_fetch(site, lake, buffer_m = 0)

  # Test with 10 degree resolution
  lakefetch_options(angle_resolution_deg = 10)
  result10 <- calc_test_fetch(site, lake, buffer_m = 0)

  lakefetch_options(angle_resolution_deg = old_res)

  # 5-degree should have 72 directions, 10-degree should have 36
expect_equal(length(result5$angles), 72)
  expect_equal(length(result10$angles), 36)
})

test_that("orbital velocity calculation is reasonable", {
  # Test calc_orbital function
  # For 1000m fetch, orbital velocity should be positive and reasonable

  orbital <- lakefetch:::calc_orbital(1000)

  expect_true(orbital > 0)
  expect_true(orbital < 1)  # Should be less than 1 m/s for 1km fetch
})

test_that("exposure categories are assigned correctly", {
  # Sheltered: < 2500m
  # Moderate: 2500-5000m
  # Exposed: > 5000m

  # Create lakes of different sizes
  small_lake <- create_circular_lake(radius = 1000)  # 2km diameter
  medium_lake <- create_circular_lake(radius = 2000)  # 4km diameter
  large_lake <- create_circular_lake(radius = 4000)  # 8km diameter

  site <- create_site(500000, 4800000, "Center")

  # Small lake should be sheltered (effective fetch ~1000m)
  result_small <- calc_test_fetch(site, small_lake, buffer_m = 0)
  expect_lt(result_small$mean, 2500)

  # Medium lake should be moderate (effective fetch ~2000m)
  result_medium <- calc_test_fetch(site, medium_lake, buffer_m = 0)
  expect_gt(result_medium$mean, 1500)
  expect_lt(result_medium$mean, 5000)

  # Large lake should be exposed (effective fetch ~4000m)
  result_large <- calc_test_fetch(site, large_lake, buffer_m = 0)
  expect_gt(result_large$mean, 3500)
})
