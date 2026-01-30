# ==============================================================================
# lakefetch Validation Tests
# ==============================================================================
# This script validates fetch calculations against known analytical solutions.
#
# Test 1: Circular lake - fetch from center should equal radius in all directions
# Test 2: Circular lake - fetch from edge point should range from 0 to diameter
# Test 3: Rectangular lake - fetch should match geometric expectations
#
# Run with: source("inst/validation/validate_fetch.R")
# ==============================================================================

library(sf)

# Load package (use devtools::load_all() if not installed)
if (!requireNamespace("lakefetch", quietly = TRUE)) {
  devtools::load_all(".")
} else {
  library(lakefetch)
}

cat("\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("LAKEFETCH VALIDATION TESTS\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n\n")

# ==============================================================================
# Helper Functions
# ==============================================================================

#' Create a circular lake polygon
#' @param center_x X coordinate of center (in meters, UTM)
#' @param center_y Y coordinate of center (in meters, UTM)
#' @param radius Radius in meters
#' @param n_points Number of points to approximate circle
#' @param epsg EPSG code for CRS
create_circular_lake <- function(center_x, center_y, radius, n_points = 360, epsg = 32618) {
  angles <- seq(0, 2 * pi, length.out = n_points + 1)
  x <- center_x + radius * cos(angles)
  y <- center_y + radius * sin(angles)

  coords <- cbind(x, y)
  poly <- st_polygon(list(coords))
  lake_sf <- st_sf(
    osm_id = "test_circle",
    name = "Test Circular Lake",
    geometry = st_sfc(poly, crs = epsg)
  )
  lake_sf$area_km2 <- as.numeric(st_area(lake_sf)) / 1e6
  return(lake_sf)
}

#' Create a rectangular lake polygon
create_rectangular_lake <- function(center_x, center_y, width, height, epsg = 32618) {
  half_w <- width / 2
  half_h <- height / 2

  coords <- rbind(
    c(center_x - half_w, center_y - half_h),
    c(center_x + half_w, center_y - half_h),
    c(center_x + half_w, center_y + half_h),
    c(center_x - half_w, center_y + half_h),
    c(center_x - half_w, center_y - half_h)  # close polygon
  )

  poly <- st_polygon(list(coords))
  lake_sf <- st_sf(
    osm_id = "test_rectangle",
    name = "Test Rectangular Lake",
    geometry = st_sfc(poly, crs = epsg)
  )
  lake_sf$area_km2 <- as.numeric(st_area(lake_sf)) / 1e6
  return(lake_sf)
}

#' Create a site point
create_site <- function(x, y, name = "Test Site", epsg = 32618) {
  site_sf <- st_sf(
    Site = name,
    site_name = name,
    lake_osm_id = "test",
    lake_name = "Test Lake",
    geometry = st_sfc(st_point(c(x, y)), crs = epsg)
  )
  return(site_sf)
}

#' Run fetch calculation on a test lake/site
run_test_fetch <- function(site_sf, lake_sf, buffer_m = 0) {
  # Temporarily set buffer to 0 for precise testing
  old_buffer <- lakefetch_options()$buffer_distance_m
  lakefetch_options(buffer_distance_m = buffer_m)

  # Get lake boundary
  lake_boundary <- tryCatch({
    st_cast(lake_sf, "MULTILINESTRING")
  }, error = function(e) {
    st_boundary(lake_sf)
  })

  # Get angles
  angle_res <- lakefetch_options()$angle_resolution_deg
  angles <- seq(0, 360 - angle_res, by = angle_res)

  # Calculate fetch using internal function
  fetch_dists <- lakefetch:::get_highres_fetch(site_sf, lake_boundary, lake_sf, angles)

  # Restore buffer setting
  lakefetch_options(buffer_distance_m = old_buffer)

  return(list(
    angles = angles,
    fetch = fetch_dists,
    mean = mean(fetch_dists),
    max = max(fetch_dists),
    min = min(fetch_dists)
  ))
}

# ==============================================================================
# TEST 1: Circular Lake - Center Point
# ==============================================================================
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("TEST 1: Circular Lake - Point at Center\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n\n")

# Create a circular lake with 1000m radius
radius <- 1000  # meters
center_x <- 500000
center_y <- 4800000
epsg <- 32618  # UTM 18N

circular_lake <- create_circular_lake(center_x, center_y, radius, n_points = 720, epsg = epsg)
center_site <- create_site(center_x, center_y, "Center", epsg)

cat("Lake: Circular, radius =", radius, "m\n")
cat("Site: Center of lake\n")
cat("Expected fetch: ", radius, "m in all directions\n\n")

# Run fetch calculation
result1 <- run_test_fetch(center_site, circular_lake, buffer_m = 0)

cat("Results:\n")
cat("  Mean fetch:  ", round(result1$mean, 1), "m (expected:", radius, "m)\n")
cat("  Max fetch:   ", round(result1$max, 1), "m\n")
cat("  Min fetch:   ", round(result1$min, 1), "m\n")
cat("  Std dev:     ", round(sd(result1$fetch), 1), "m\n")

# Calculate error
error_pct <- abs(result1$mean - radius) / radius * 100
cat("\n  Error: ", round(error_pct, 2), "%\n")

test1_pass <- error_pct < 2  # Allow 2% error due to polygon discretization
cat("  Status: ", ifelse(test1_pass, "PASS", "FAIL"), "\n\n")

# ==============================================================================
# TEST 2: Circular Lake - Edge Point
# ==============================================================================
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("TEST 2: Circular Lake - Point Near Edge\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n\n")

# Point at 80% of radius from center (toward east)
edge_offset <- 0.8 * radius
edge_site <- create_site(center_x + edge_offset, center_y, "Edge", epsg)

cat("Lake: Circular, radius =", radius, "m\n")
cat("Site: 80% of radius from center (", edge_offset, "m east)\n")
cat("Expected: Min fetch ~", radius - edge_offset, "m (east), Max fetch ~", radius + edge_offset, "m (west)\n\n")

result2 <- run_test_fetch(edge_site, circular_lake, buffer_m = 0)

expected_min <- radius - edge_offset
expected_max <- radius + edge_offset

cat("Results:\n")
cat("  Min fetch:   ", round(result2$min, 1), "m (expected: ~", expected_min, "m)\n")
cat("  Max fetch:   ", round(result2$max, 1), "m (expected: ~", expected_max, "m)\n")
cat("  Mean fetch:  ", round(result2$mean, 1), "m\n")

# Check min and max are in expected range
min_error <- abs(result2$min - expected_min) / expected_min * 100
max_error <- abs(result2$max - expected_max) / expected_max * 100

cat("\n  Min fetch error: ", round(min_error, 1), "%\n")
cat("  Max fetch error: ", round(max_error, 1), "%\n")

test2_pass <- min_error < 15 && max_error < 5  # More tolerance for min (edge effects)
cat("  Status: ", ifelse(test2_pass, "PASS", "FAIL"), "\n\n")

# ==============================================================================
# TEST 3: Rectangular Lake - Center Point
# ==============================================================================
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("TEST 3: Rectangular Lake - Point at Center\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n\n")

# Create a 2000m x 1000m rectangular lake
width <- 2000   # E-W dimension
height <- 1000  # N-S dimension

rect_lake <- create_rectangular_lake(center_x, center_y, width, height, epsg)
rect_center_site <- create_site(center_x, center_y, "RectCenter", epsg)

cat("Lake: Rectangle, ", width, "m (E-W) x ", height, "m (N-S)\n")
cat("Site: Center of lake\n")
cat("Expected: N/S fetch = ", height/2, "m, E/W fetch = ", width/2, "m\n\n")

result3 <- run_test_fetch(rect_center_site, rect_lake, buffer_m = 0)

# Find fetch in cardinal directions
angle_res <- lakefetch_options()$angle_resolution_deg
n_idx <- which(result3$angles == 0)      # North
e_idx <- which(result3$angles == 90)     # East
s_idx <- which(result3$angles == 180)    # South
w_idx <- which(result3$angles == 270)    # West

fetch_n <- result3$fetch[n_idx]
fetch_e <- result3$fetch[e_idx]
fetch_s <- result3$fetch[s_idx]
fetch_w <- result3$fetch[w_idx]

cat("Results (cardinal directions):\n")
cat("  North (0°):   ", round(fetch_n, 1), "m (expected:", height/2, "m)\n")
cat("  East (90°):   ", round(fetch_e, 1), "m (expected:", width/2, "m)\n")
cat("  South (180°): ", round(fetch_s, 1), "m (expected:", height/2, "m)\n")
cat("  West (270°):  ", round(fetch_w, 1), "m (expected:", width/2, "m)\n")

# Check errors
n_error <- abs(fetch_n - height/2) / (height/2) * 100
e_error <- abs(fetch_e - width/2) / (width/2) * 100
s_error <- abs(fetch_s - height/2) / (height/2) * 100
w_error <- abs(fetch_w - width/2) / (width/2) * 100

cat("\n  N/S error: ", round(max(n_error, s_error), 1), "%\n")
cat("  E/W error: ", round(max(e_error, w_error), 1), "%\n")

test3_pass <- max(n_error, e_error, s_error, w_error) < 5
cat("  Status: ", ifelse(test3_pass, "PASS", "FAIL"), "\n\n")

# ==============================================================================
# TEST 4: Effective Fetch Calculation
# ==============================================================================
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("TEST 4: Effective Fetch Calculation\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n\n")

# Using circular lake center, effective fetch should be close to radius
# (mean of top 3 directions, which should all be ~radius)

top3 <- sort(result1$fetch, decreasing = TRUE)[1:3]
effective_fetch <- mean(top3)

cat("Circular lake center point:\n")
cat("  Top 3 fetch values: ", paste(round(top3, 1), collapse = ", "), "m\n")
cat("  Effective fetch:    ", round(effective_fetch, 1), "m\n")
cat("  Expected:           ", radius, "m\n")

eff_error <- abs(effective_fetch - radius) / radius * 100
cat("  Error:              ", round(eff_error, 2), "%\n")

test4_pass <- eff_error < 2
cat("  Status: ", ifelse(test4_pass, "PASS", "FAIL"), "\n\n")

# ==============================================================================
# Summary
# ==============================================================================
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("VALIDATION SUMMARY\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n\n")

tests <- c(
  "Test 1: Circular lake center" = test1_pass,
  "Test 2: Circular lake edge" = test2_pass,
  "Test 3: Rectangular lake center" = test3_pass,
  "Test 4: Effective fetch" = test4_pass
)

for (name in names(tests)) {
  status <- ifelse(tests[name], "PASS", "FAIL")
  cat(sprintf("  %-35s %s\n", name, status))
}

cat("\n")
passed <- sum(tests)
total <- length(tests)
cat(sprintf("Results: %d/%d tests passed\n", passed, total))

if (all(tests)) {
  cat("\nVALIDATION SUCCESSFUL: Fetch calculations match analytical expectations.\n")
} else {
  cat("\nVALIDATION FAILED: Some tests did not pass. Review results above.\n")
}
cat("\n")
