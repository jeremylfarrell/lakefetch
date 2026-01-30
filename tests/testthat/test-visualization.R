# ==============================================================================
# Tests for visualization.R - Plotting functions
# ==============================================================================

# Create test fetch data for visualization tests (no OSM calls)
create_test_fetch_data <- function() {
  # Use helper from helper-geometries.R
  lake <- create_circular_lake(radius = 1000)
  site <- create_site(500000, 4800000, "TestSite")

  # Add required columns
  site$lake_osm_id <- "test"
  site$lake_name <- "Test Lake"
  site$lake_area_km2 <- lake$area_km2

  # Get boundary using local helper (NOT get_lake_boundary which calls OSM)
  lake_boundary <- tryCatch({
    sf::st_cast(lake, "MULTILINESTRING")
  }, error = function(e) {
    sf::st_boundary(lake)
  })

  angles <- seq(0, 355, by = 5)
  fetch_vals <- lakefetch:::get_highres_fetch(site, lake_boundary, lake, angles)

  # Add fetch columns
  for (i in seq_along(angles)) {
    site[[paste0("fetch_", angles[i])]] <- fetch_vals[i]
  }

  site$fetch_mean <- mean(fetch_vals)
  site$fetch_max <- max(fetch_vals)
  site$fetch_effective <- mean(sort(fetch_vals, decreasing = TRUE)[1:3])
  site$orbital_effective <- lakefetch:::calc_orbital(site$fetch_effective)
  site$exposure_category <- "Sheltered"

  list(
    results = site,
    lakes = lake,
    angles = angles
  )
}

test_that("plot_fetch_map returns ggplot object", {
  skip_if_not_installed("ggplot2")

  fetch_data <- create_test_fetch_data()
  p <- plot_fetch_map(fetch_data)

  expect_s3_class(p, "ggplot")
})

test_that("plot_fetch_bars returns ggplot object", {
  skip_if_not_installed("ggplot2")

  fetch_data <- create_test_fetch_data()
  p <- plot_fetch_bars(fetch_data)

  expect_s3_class(p, "ggplot")
})

test_that("plot_fetch_rose returns base plot without error", {
  fetch_data <- create_test_fetch_data()

  # Should complete without error
  expect_no_error(
    plot_fetch_rose(fetch_data, "TestSite")
  )
})

test_that("create_ray_geometries returns sf with correct structure", {
  fetch_data <- create_test_fetch_data()

  rays <- create_ray_geometries(fetch_data)

  expect_s3_class(rays, "sf")
  expect_true("Site" %in% names(rays))
  expect_true("Angle" %in% names(rays))
  expect_true("Distance" %in% names(rays))

  # Should have one ray per angle per site
  n_angles <- length(fetch_data$angles)
  n_sites <- nrow(fetch_data$results)
  expect_equal(nrow(rays), n_angles * n_sites)
})

test_that("create_ray_geometries produces valid linestrings", {
  fetch_data <- create_test_fetch_data()

  rays <- create_ray_geometries(fetch_data)

  # All geometries should be linestrings
  geom_types <- sf::st_geometry_type(rays)
  expect_true(all(geom_types == "LINESTRING"))

  # All should be valid
  expect_true(all(sf::st_is_valid(rays)))
})

test_that("make_rose_plot_base64 returns base64 string", {
  fetch_data <- create_test_fetch_data()

  b64 <- lakefetch:::make_rose_plot_base64(
    fetch_data$results[1, ],
    "TestSite"
  )

  expect_type(b64, "character")
  expect_true(grepl("^data:image/png;base64,", b64))
})
