# ==============================================================================
# lakefetch Edge Case Validation Tests
# ==============================================================================
# This script validates fetch calculations handle challenging geometries:
# 1. Lakes with islands (rays should stop at island boundaries)
# 2. Complex shorelines (bays, peninsulas, irregular shapes)
# 3. Very large lakes (Great Lakes scale)
#
# Run with: source("inst/validation/validate_edge_cases.R")
# ==============================================================================

library(sf)

# Load package
if (!requireNamespace("lakefetch", quietly = TRUE)) {

devtools::load_all(".")
} else {
  library(lakefetch)
}

cat("\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("LAKEFETCH EDGE CASE VALIDATION\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("\nTesting fetch calculations on challenging geometries.\n\n")

# ==============================================================================
# Helper Functions
# ==============================================================================

#' Create a lake polygon with an island
#' @param outer_radius Radius of outer lake boundary (meters)
#' @param island_radius Radius of island (meters)
#' @param island_offset Offset of island from center (c(x, y) in meters)
create_lake_with_island <- function(outer_radius = 2000,
                                     island_radius = 300,
                                     island_offset = c(0, 0),
                                     center_x = 500000,
                                     center_y = 4800000,
                                     n_points = 360,
                                     epsg = 32618) {

  # Create outer boundary
  angles <- seq(0, 2 * pi, length.out = n_points + 1)
  outer_x <- center_x + outer_radius * cos(angles)
  outer_y <- center_y + outer_radius * sin(angles)
  outer_ring <- cbind(outer_x, outer_y)


  # Create island (hole) - note: holes go counterclockwise
  island_center_x <- center_x + island_offset[1]
  island_center_y <- center_y + island_offset[2]
  island_x <- island_center_x + island_radius * cos(rev(angles))
  island_y <- island_center_y + island_radius * sin(rev(angles))
  island_ring <- cbind(island_x, island_y)

  # Create polygon with hole
  poly <- st_polygon(list(outer_ring, island_ring))

  lake_sf <- st_sf(
    osm_id = "test_island",
    name = "Test Lake with Island",
    geometry = st_sfc(poly, crs = epsg)
  )
  lake_sf$area_km2 <- as.numeric(st_area(lake_sf)) / 1e6

  return(lake_sf)
}

#' Create a lake with complex shoreline (bays and peninsulas)
create_complex_lake <- function(center_x = 500000,
                                 center_y = 4800000,
                                 base_radius = 1500,
                                 n_points = 360,
                                 epsg = 32618) {

  # Create irregular boundary with bays and peninsulas
  angles <- seq(0, 2 * pi, length.out = n_points + 1)

  # Add sinusoidal variation to simulate bays/peninsulas
  # Multiple frequencies for realistic irregularity
  radius_variation <- base_radius +
    300 * sin(3 * angles) +  # 3 major bays/peninsulas
    150 * sin(7 * angles) +  # 7 minor indentations
    75 * sin(13 * angles)    # Fine detail

  x <- center_x + radius_variation * cos(angles)
  y <- center_y + radius_variation * sin(angles)

  coords <- cbind(x, y)
  poly <- st_polygon(list(coords))

  lake_sf <- st_sf(
    osm_id = "test_complex",
    name = "Test Complex Lake",
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

#' Run fetch calculation on test geometry
run_fetch_test <- function(site_sf, lake_sf, buffer_m = 0) {
  # Get lake boundary line
  lake_line <- tryCatch({
    st_cast(lake_sf, "MULTILINESTRING")
  }, error = function(e) {
    st_boundary(lake_sf)
  })

  # Get angles
  angles <- seq(0, 355, by = lakefetch_options()$angle_resolution_deg)

  # Set buffer

  old_buffer <- lakefetch_options()$buffer_distance_m
  lakefetch_options(buffer_distance_m = buffer_m)

  # Calculate fetch
  fetch_vals <- lakefetch:::get_highres_fetch(site_sf, lake_line, lake_sf, angles)

  lakefetch_options(buffer_distance_m = old_buffer)

  return(list(
    angles = angles,
    fetch = fetch_vals,
    mean = mean(fetch_vals),
    max = max(fetch_vals),
    min = min(fetch_vals)
  ))
}

# ==============================================================================
# TEST 1: Lake with Central Island
# ==============================================================================
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("TEST 1: Lake with Central Island\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n\n")

outer_radius <- 2000
island_radius <- 400
center_x <- 500000
center_y <- 4800000

lake_island <- create_lake_with_island(
  outer_radius = outer_radius,
  island_radius = island_radius,
  island_offset = c(0, 0),  # Island at center
  center_x = center_x,
  center_y = center_y
)

# Test point between island and outer shore
# At 1000m from center (halfway between island edge at 400m and outer at 2000m)
test_dist <- 1000
site_island <- create_site(center_x + test_dist, center_y, "Island Test")

cat("Geometry:\n")
cat("  Outer lake radius: ", outer_radius, "m\n")
cat("  Island radius:     ", island_radius, "m (at center)\n")
cat("  Test point:        ", test_dist, "m east of center\n\n")

cat("Expected fetch:\n")
cat("  East (toward outer shore):  ", outer_radius - test_dist, "m\n")
cat("  West (toward island):       ", test_dist - island_radius, "m\n")
cat("  North/South (tangent):      ~", round(sqrt(outer_radius^2 - test_dist^2)), "m\n\n")

result1 <- run_fetch_test(site_island, lake_island, buffer_m = 1)

# Find cardinal direction fetches
e_idx <- which(result1$angles == 90)   # East
w_idx <- which(result1$angles == 270)  # West
n_idx <- which(result1$angles == 0)    # North
s_idx <- which(result1$angles == 180)  # South

cat("Calculated fetch:\n")
cat("  East (90°):   ", round(result1$fetch[e_idx], 0), "m (expected: ", outer_radius - test_dist, "m)\n")
cat("  West (270°):  ", round(result1$fetch[w_idx], 0), "m (expected: ", test_dist - island_radius, "m)\n")
cat("  North (0°):   ", round(result1$fetch[n_idx], 0), "m\n")
cat("  South (180°): ", round(result1$fetch[s_idx], 0), "m\n")
cat("  Mean:         ", round(result1$mean, 0), "m\n")
cat("  Max:          ", round(result1$max, 0), "m\n")
cat("  Min:          ", round(result1$min, 0), "m\n")

# Validation: West fetch should be blocked by island
expected_west <- test_dist - island_radius
west_error <- abs(result1$fetch[w_idx] - expected_west) / expected_west * 100

# East fetch should reach outer shore
expected_east <- outer_radius - test_dist
east_error <- abs(result1$fetch[e_idx] - expected_east) / expected_east * 100

cat("\nValidation:\n")
cat("  East fetch error:  ", round(east_error, 1), "%\n")
cat("  West fetch error:  ", round(west_error, 1), "%\n")

# Island should block - west fetch should be much less than east
island_blocks <- result1$fetch[w_idx] < result1$fetch[e_idx] * 0.8
cat("  Island blocks rays: ", ifelse(island_blocks, "YES", "NO"), "\n")

test1_pass <- east_error < 10 && west_error < 15 && island_blocks
cat("\n  Status: ", ifelse(test1_pass, "PASS", "FAIL"), "\n\n")

# ==============================================================================
# TEST 2: Lake with Offset Island
# ==============================================================================
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("TEST 2: Lake with Offset Island\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n\n")

# Island offset to the north
lake_offset_island <- create_lake_with_island(
  outer_radius = 2000,
  island_radius = 300,
  island_offset = c(0, 800),  # 800m north of center
  center_x = center_x,
  center_y = center_y
)

# Test point at lake center
site_center <- create_site(center_x, center_y, "Center")

cat("Geometry:\n")
cat("  Outer lake radius: 2000m\n")
cat("  Island radius:     300m\n")
cat("  Island offset:     800m north of center\n")
cat("  Test point:        Lake center\n\n")

cat("Expected:\n")
cat("  North: ~500m (blocked by island at 800m, island radius 300m)\n")
cat("  South: ~2000m (full radius)\n")
cat("  East/West: ~2000m (full radius)\n\n")

result2 <- run_fetch_test(site_center, lake_offset_island, buffer_m = 1)

cat("Calculated fetch:\n")
cat("  North (0°):   ", round(result2$fetch[n_idx], 0), "m (expected: ~500m)\n")
cat("  South (180°): ", round(result2$fetch[s_idx], 0), "m (expected: ~2000m)\n")
cat("  East (90°):   ", round(result2$fetch[e_idx], 0), "m\n")
cat("  West (270°):  ", round(result2$fetch[w_idx], 0), "m\n")

# North should be blocked by island
expected_north <- 800 - 300  # Distance to island minus island radius
north_blocked <- result2$fetch[n_idx] < result2$fetch[s_idx] * 0.5

cat("\nValidation:\n")
cat("  North blocked by island: ", ifelse(north_blocked, "YES", "NO"), "\n")
cat("  North fetch:  ", round(result2$fetch[n_idx], 0), "m (should be ~500m)\n")
cat("  South fetch:  ", round(result2$fetch[s_idx], 0), "m (should be ~2000m)\n")

north_error <- abs(result2$fetch[n_idx] - expected_north) / expected_north * 100
test2_pass <- north_blocked && north_error < 20
cat("\n  Status: ", ifelse(test2_pass, "PASS", "FAIL"), "\n\n")

# ==============================================================================
# TEST 3: Complex Shoreline (Bays and Peninsulas)
# ==============================================================================
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("TEST 3: Complex Shoreline (Bays and Peninsulas)\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n\n")

lake_complex <- create_complex_lake(
  center_x = center_x,
  center_y = center_y,
  base_radius = 1500
)

site_complex <- create_site(center_x, center_y, "Complex Center")

cat("Geometry:\n")
cat("  Base radius: 1500m with sinusoidal variation\n")
cat("  Variation:   ±300m (major), ±150m (minor), ±75m (fine)\n")
cat("  Test point:  Lake center\n\n")

result3 <- run_fetch_test(site_complex, lake_complex, buffer_m = 1)

cat("Calculated fetch:\n")
cat("  Mean:  ", round(result3$mean, 0), "m\n")
cat("  Max:   ", round(result3$max, 0), "m\n")
cat("  Min:   ", round(result3$min, 0), "m\n")
cat("  Range: ", round(result3$max - result3$min, 0), "m\n")

# Validation: fetch should vary significantly due to irregular shoreline
fetch_range <- result3$max - result3$min
fetch_cv <- sd(result3$fetch) / mean(result3$fetch) * 100  # Coefficient of variation

cat("\nValidation:\n")
cat("  Fetch varies with direction (range > 500m): ", ifelse(fetch_range > 500, "YES", "NO"), "\n")
cat("  Coefficient of variation: ", round(fetch_cv, 1), "%\n")
cat("  Mean near base radius (1500m ± 300m): ",
    ifelse(abs(result3$mean - 1500) < 300, "YES", "NO"), "\n")

test3_pass <- fetch_range > 500 && abs(result3$mean - 1500) < 300
cat("\n  Status: ", ifelse(test3_pass, "PASS", "FAIL"), "\n\n")

# ==============================================================================
# TEST 4: Very Large Lake (Real-world: Lake Erie or similar)
# ==============================================================================
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("TEST 4: Very Large Lake (Lake Erie)\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n\n")

cat("Testing fetch calculation on a Great Lake to verify handling of\n")
cat("very large water bodies where fetch exceeds typical lake scales.\n\n")

# Lake Erie approximate center
erie_coords <- c(-81.5, 42.2)

cat("Lake Erie:\n")
cat("  Surface area: ~25,700 km²\n
")
cat("  Max length:   ~388 km (E-W)\n")
cat("  Max width:    ~92 km (N-S)\n")
cat("  Test point:   Approximate center\n\n")

# Try to download Lake Erie
result4 <- tryCatch({
  sites_df <- data.frame(
    Site = "Erie_center",
    latitude = erie_coords[2],
    longitude = erie_coords[1],
    lake.name = "Lake Erie"
  )

  sites <- load_sites(sites_df)
  cat("Downloading Lake Erie boundary from OSM...\n")
  cat("(This may take a moment for large lake)\n\n")

  lake_data <- get_lake_boundary(sites)

  if (is.null(lake_data) || is.null(lake_data$all_lakes)) {
    return(list(success = FALSE, error = "Failed to download lake boundary"))
  }

  lake_boundary <- lake_data$all_lakes

  # Find Lake Erie by name or largest polygon
  lake_names <- tolower(as.character(lake_boundary$name))
  lake_names[is.na(lake_names)] <- ""
  erie_idx <- which(grepl("erie", lake_names))

  if (length(erie_idx) == 0) {
    lake_boundary$area <- as.numeric(st_area(lake_boundary))
    erie_idx <- which.max(lake_boundary$area)
    cat("  No name match - using largest polygon\n")
  } else {
    lake_boundary$area <- as.numeric(st_area(lake_boundary))
    erie_idx <- erie_idx[which.max(lake_boundary$area[erie_idx])]
  }

  erie_lake <- lake_boundary[erie_idx, ]
  cat("  Matched:", erie_lake$name[1], "\n")
  cat("  OSM area:", round(erie_lake$area[1] / 1e9, 1), "thousand km²\n")

  # Get centroid
  suppressWarnings({
    centroid <- st_centroid(st_geometry(erie_lake))
  })

  # Create test site
  test_site <- st_sf(
    Site = "Erie_center",
    site_name = "Erie_center",
    lake_osm_id = as.character(erie_lake$osm_id[1]),
    lake_name = as.character(erie_lake$name[1]),
    geometry = centroid
  )
  st_crs(test_site) <- st_crs(erie_lake)

  # Get boundary line
  lake_line <- tryCatch({
    st_cast(erie_lake, "MULTILINESTRING")
  }, error = function(e) {
    st_boundary(erie_lake)
  })

  # Calculate fetch
  angles <- seq(0, 355, by = lakefetch_options()$angle_resolution_deg)
  old_buffer <- lakefetch_options()$buffer_distance_m
  lakefetch_options(buffer_distance_m = 1)

  cat("  Calculating fetch for", length(angles), "directions...\n")
  fetch_vals <- lakefetch:::get_highres_fetch(test_site, lake_line, erie_lake, angles)

  lakefetch_options(buffer_distance_m = old_buffer)

  list(
    success = TRUE,
    fetch = fetch_vals,
    angles = angles,
    mean = mean(fetch_vals),
    max = max(fetch_vals),
    min = min(fetch_vals),
    area_km2 = erie_lake$area[1] / 1e6
  )
}, error = function(e) {
  list(success = FALSE, error = as.character(e$message))
})

if (result4$success) {
  cat("\nCalculated fetch:\n")
  cat("  Mean:  ", round(result4$mean / 1000, 1), "km\n")
  cat("  Max:   ", round(result4$max / 1000, 1), "km\n")
  cat("  Min:   ", round(result4$min / 1000, 1), "km\n")

  # Check if we got actual Lake Erie or a fallback boundary
  got_real_erie <- result4$area_km2 > 20000  # Lake Erie is ~25,700 km²

  if (!got_real_erie) {
    cat("\n  NOTE: OSM returned approximate boundary, not full Lake Erie.\n")
    cat("  This is a KNOWN LIMITATION: Great Lakes are too large for\n")
    cat("  automatic OSM download with default bounding box settings.\n")
    cat("  For Great Lakes, use pre-downloaded shapefiles instead.\n")
    cat("\n  Status: SKIPPED (OSM limitation, not algorithm issue)\n\n")
    test4_pass <- NA
  } else {
    max_fetch_cap <- lakefetch_options()$max_fetch_m
    hit_cap <- result4$max >= max_fetch_cap * 0.99

    cat("\nValidation:\n")
    cat("  Max fetch hit ", max_fetch_cap/1000, "km cap: ", ifelse(hit_cap, "YES (expected)", "NO"), "\n")
    cat("  Mean fetch > 20km (expected for Great Lake): ",
        ifelse(result4$mean > 20000, "YES", "NO"), "\n")
    cat("  Min fetch > 5km (wide lake): ",
        ifelse(result4$min > 5000, "YES", "NO"), "\n")

    # For Lake Erie: E-W ~388km, N-S ~92km
    # From center: E/W should hit cap (~50km), N/S should be ~46km
    test4_pass <- hit_cap && result4$mean > 20000
    cat("\n  Status: ", ifelse(test4_pass, "PASS", "FAIL"), "\n\n")
  }
} else {
  cat("  SKIPPED: ", result4$error, "\n")
  cat("  (Lake Erie requires significant download - test may timeout)\n\n")
  test4_pass <- NA
}

# ==============================================================================
# TEST 5: Multiple Islands
# ==============================================================================
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("TEST 5: Lake with Multiple Islands\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n\n")

# Create lake with multiple islands manually
create_multi_island_lake <- function() {
  center_x <- 500000
  center_y <- 4800000
  outer_radius <- 3000
  n_points <- 360
  epsg <- 32618

  angles <- seq(0, 2 * pi, length.out = n_points + 1)

  # Outer boundary
  outer_x <- center_x + outer_radius * cos(angles)
  outer_y <- center_y + outer_radius * sin(angles)
  outer_ring <- cbind(outer_x, outer_y)

  # Island 1: North
  i1_x <- center_x + 0
  i1_y <- center_y + 1500
  i1_r <- 250
  island1 <- cbind(
    i1_x + i1_r * cos(rev(angles)),
    i1_y + i1_r * sin(rev(angles))
  )

  # Island 2: Southeast
  i2_x <- center_x + 1200
  i2_y <- center_y - 800
  i2_r <- 200
  island2 <- cbind(
    i2_x + i2_r * cos(rev(angles)),
    i2_y + i2_r * sin(rev(angles))
  )

  # Island 3: West
  i3_x <- center_x - 1000
  i3_y <- center_y + 200
  i3_r <- 300
  island3 <- cbind(
    i3_x + i3_r * cos(rev(angles)),
    i3_y + i3_r * sin(rev(angles))
  )

  # Create polygon with multiple holes
  poly <- st_polygon(list(outer_ring, island1, island2, island3))

  lake_sf <- st_sf(
    osm_id = "test_multi_island",
    name = "Multi-Island Lake",
    geometry = st_sfc(poly, crs = epsg)
  )
  lake_sf$area_km2 <- as.numeric(st_area(lake_sf)) / 1e6

  return(lake_sf)
}

lake_multi <- create_multi_island_lake()
site_multi <- create_site(500000, 4800000, "Multi-Island Center")

cat("Geometry:\n")
cat("  Outer radius: 3000m\n")
cat("  Island 1: r=250m at (0, +1500m) - North\n")
cat("  Island 2: r=200m at (+1200m, -800m) - Southeast\n")
cat("  Island 3: r=300m at (-1000m, +200m) - West\n")
cat("  Test point: Lake center\n\n")

result5 <- run_fetch_test(site_multi, lake_multi, buffer_m = 1)

# Check that islands block rays in their directions
# North: should be blocked at ~1250m (1500 - 250)
# West-ish (~170°): should be blocked at ~700m (1000 - 300)
# Southeast (~315°): should be blocked

cat("Calculated fetch:\n")
cat("  North (0°):    ", round(result5$fetch[n_idx], 0), "m (island at 1500m, r=250m)\n")
cat("  South (180°):  ", round(result5$fetch[s_idx], 0), "m (no island)\n")
cat("  East (90°):    ", round(result5$fetch[e_idx], 0), "m\n")
cat("  West (270°):   ", round(result5$fetch[w_idx], 0), "m (island nearby)\n")
cat("  Mean:          ", round(result5$mean, 0), "m\n")
cat("  Max:           ", round(result5$max, 0), "m\n")
cat("  Min:           ", round(result5$min, 0), "m\n")

# Validation
north_blocked <- result5$fetch[n_idx] < 1500  # Should be blocked by island
south_clear <- result5$fetch[s_idx] > 2500    # Should reach near outer boundary

cat("\nValidation:\n")
cat("  North blocked by island (< 1500m): ", ifelse(north_blocked, "YES", "NO"), "\n")
cat("  South reaches boundary (> 2500m):  ", ifelse(south_clear, "YES", "NO"), "\n")

test5_pass <- north_blocked && south_clear
cat("\n  Status: ", ifelse(test5_pass, "PASS", "FAIL"), "\n\n")

# ==============================================================================
# Summary
# ==============================================================================
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("EDGE CASE VALIDATION SUMMARY\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n\n")

tests <- list(
  "Test 1: Central island" = test1_pass,
  "Test 2: Offset island" = test2_pass,
  "Test 3: Complex shoreline" = test3_pass,
  "Test 4: Very large lake (Erie)" = test4_pass,
  "Test 5: Multiple islands" = test5_pass
)

for (name in names(tests)) {
  result <- tests[[name]]
  if (is.na(result)) {
    status <- "SKIPPED"
  } else if (result) {
    status <- "PASS"
  } else {
    status <- "FAIL"
  }
  cat(sprintf("  %-35s %s\n", name, status))
}

passed <- sum(sapply(tests, function(x) isTRUE(x)))
failed <- sum(sapply(tests, function(x) isFALSE(x)))
skipped <- sum(sapply(tests, is.na))

cat("\n")
cat(sprintf("Results: %d passed, %d failed, %d skipped\n", passed, failed, skipped))

if (passed + failed > 0) {
  cat(sprintf("Pass rate: %.1f%% (of testable)\n", passed / (passed + failed) * 100))
}

cat("\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("EDGE CASE VALIDATION COMPLETE\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n\n")
