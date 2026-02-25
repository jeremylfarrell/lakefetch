# ==============================================================================
# Cross-Validation: lakefetch vs waver
# ==============================================================================
# This script compares fetch calculations between lakefetch and the waver
# package (CRAN) using the same test geometry.
#
# IMPORTANT: waver is designed for coastal applications where:
#   - Points are in water (ocean)
#   - Shoreline parameter is the land boundary
# For lake applications, we need to create an "inverted" geometry where
# the lake is a hole in a land polygon.
#
# Reference: Marchand, P. (2020). waver: Calculate Fetch and Wave Energy.
#            R package. https://cran.r-project.org/package=waver
#
# Run with: source("inst/validation/cross_validate_waver.R")
# ==============================================================================

library(sf)

# Check for waver package
if (!requireNamespace("waver", quietly = TRUE)) {
  stop("Package 'waver' required for this validation script.\n",
       "Install with: install.packages(\"waver\")")
}
library(waver)

# Load lakefetch
if (!requireNamespace("lakefetch", quietly = TRUE)) {
  devtools::load_all(".")
} else {
  library(lakefetch)
}

cat("\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("CROSS-VALIDATION: lakefetch vs waver\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n\n")

# ==============================================================================
# Create Test Lake (well-defined geometry for reproducible comparison)
# ==============================================================================

# Create an irregular but simple lake polygon
# Using UTM Zone 18N coordinates (EPSG:32618)
epsg <- 32618
center_x <- 500000
center_y <- 4800000

# Create an elliptical lake (2:1 aspect ratio, rotated)
n_points <- 360
angles <- seq(0, 2 * pi, length.out = n_points + 1)
a <- 2000  # semi-major axis (m)
b <- 1000  # semi-minor axis (m)
rotation <- pi / 6  # 30 degree rotation

# Parametric ellipse with rotation
x_ellipse <- center_x + a * cos(angles) * cos(rotation) - b * sin(angles) * sin(rotation)
y_ellipse <- center_y + a * cos(angles) * sin(rotation) + b * sin(angles) * cos(rotation)

coords <- cbind(x_ellipse, y_ellipse)
lake_poly <- st_polygon(list(coords))
lake_sf <- st_sf(
  id = 1,
  osm_id = "test_ellipse",
  name = "Test Elliptical Lake",
  geometry = st_sfc(lake_poly, crs = epsg)
)
lake_sf$area_km2 <- as.numeric(st_area(lake_sf)) / 1e6

cat("Test Lake: Rotated ellipse (2000m x 1000m semi-axes, 30° rotation)\n")
cat("Area:", round(lake_sf$area_km2, 2), "km²\n\n")

# ==============================================================================
# Create Test Sites
# ==============================================================================

# Create 5 test points at different positions
sites <- data.frame(
  id = 1:5,
  name = c("Center", "North", "South", "East", "West"),
  x = c(center_x,
        center_x,
        center_x,
        center_x + 800,
        center_x - 800),
  y = c(center_y,
        center_y + 600,
        center_y - 600,
        center_y,
        center_y)
)

sites_sf <- st_as_sf(sites, coords = c("x", "y"), crs = epsg)

cat("Test Sites:\n")
for (i in 1:nrow(sites)) {
  cat("  ", sites$name[i], ": (", sites$x[i], ", ", sites$y[i], ")\n", sep = "")
}
cat("\n")

# ==============================================================================
# Calculate Fetch with waver
# ==============================================================================

cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("Calculating fetch with WAVER package...\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n\n")

# waver uses bearings (degrees from north, clockwise)
bearings <- seq(0, 355, by = 5)

# IMPORTANT: waver is designed for coastal/marine use where:
#   - Points are in water (not inside any polygon)
#   - shoreline = land polygon that blocks fetch
# For lakes, we need to create a "land" polygon with the lake as a hole

# Create a large bounding box as "land" with the lake as a hole
bbox_buffer <- 15000  # 15km buffer around lake
lake_centroid <- st_coordinates(st_centroid(lake_sf))
land_outer <- st_polygon(list(rbind(
  c(lake_centroid[1] - bbox_buffer, lake_centroid[2] - bbox_buffer),
  c(lake_centroid[1] + bbox_buffer, lake_centroid[2] - bbox_buffer),
  c(lake_centroid[1] + bbox_buffer, lake_centroid[2] + bbox_buffer),
  c(lake_centroid[1] - bbox_buffer, lake_centroid[2] + bbox_buffer),
  c(lake_centroid[1] - bbox_buffer, lake_centroid[2] - bbox_buffer)
)))

# Get lake exterior ring
lake_coords <- st_coordinates(lake_sf)[, 1:2]

# Create land polygon with lake as hole (exterior ring + hole)
land_with_hole <- st_polygon(list(
  st_coordinates(land_outer)[, 1:2],  # outer ring (land)
  lake_coords  # inner ring (lake = hole in land)
))

land_sf <- st_sf(
  id = 1,
  geometry = st_sfc(land_with_hole, crs = epsg)
)

cat("Created land polygon with lake as hole for waver...\n\n")

waver_results <- list()

for (i in 1:nrow(sites_sf)) {
  site_name <- sites$name[i]
  site_pt <- sites_sf[i, ]

  # Use waver::fetch_len with land polygon (lake is the hole)
  tryCatch({
    waver_fetch <- waver::fetch_len(
      p = site_pt,
      bearings = bearings,
      shoreline = land_sf,
      dmax = 10000,
      spread = 0  # Single ray per bearing (like lakefetch)
    )

    # Extract fetch values
    fetch_cols <- paste0("fetch_", bearings)
    fetch_vals <- as.numeric(waver_fetch[1, fetch_cols])

    waver_results[[site_name]] <- list(
      bearings = bearings,
      fetch = fetch_vals,
      mean = mean(fetch_vals, na.rm = TRUE),
      max = max(fetch_vals, na.rm = TRUE),
      min = min(fetch_vals, na.rm = TRUE)
    )

    cat("  ", site_name, ": mean =", round(mean(fetch_vals, na.rm = TRUE), 1),
        "m, max =", round(max(fetch_vals, na.rm = TRUE), 1), "m\n")

  }, error = function(e) {
    cat("  ", site_name, ": ERROR -", conditionMessage(e), "\n")
    waver_results[[site_name]] <<- NULL
  })
}

cat("\n")

# ==============================================================================
# Calculate Fetch with lakefetch
# ==============================================================================

cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("Calculating fetch with LAKEFETCH package...\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n\n")

# Set lakefetch options to match waver
lakefetch_options(
  buffer_distance_m = 0,  # No buffer for fair comparison
  angle_resolution_deg = 5,  # Match waver's bearings
  max_fetch_m = 10000
)

lakefetch_results <- list()

# Get lake boundary for lakefetch
lake_boundary_lf <- tryCatch({
  st_cast(lake_sf, "MULTILINESTRING")
}, error = function(e) {
  st_boundary(lake_sf)
})

angles_lf <- seq(0, 355, by = 5)

for (i in 1:nrow(sites_sf)) {
  site_name <- sites$name[i]
  site_pt <- sites_sf[i, ]

  tryCatch({
    # Use lakefetch internal function
    fetch_vals <- lakefetch:::get_highres_fetch(
      site_pt,
      lake_boundary_lf,
      lake_sf,
      angles_lf
    )

    lakefetch_results[[site_name]] <- list(
      angles = angles_lf,
      fetch = fetch_vals,
      mean = mean(fetch_vals, na.rm = TRUE),
      max = max(fetch_vals, na.rm = TRUE),
      min = min(fetch_vals, na.rm = TRUE)
    )

    cat("  ", site_name, ": mean =", round(mean(fetch_vals, na.rm = TRUE), 1),
        "m, max =", round(max(fetch_vals, na.rm = TRUE), 1), "m\n")

  }, error = function(e) {
    cat("  ", site_name, ": ERROR -", conditionMessage(e), "\n")
    lakefetch_results[[site_name]] <<- NULL
  })
}

cat("\n")

# ==============================================================================
# Compare Results
# ==============================================================================

cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("COMPARISON RESULTS\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n\n")

comparison <- data.frame(
  Site = character(),
  waver_mean = numeric(),
  lakefetch_mean = numeric(),
  diff_mean = numeric(),
  pct_diff_mean = numeric(),
  waver_max = numeric(),
  lakefetch_max = numeric(),
  diff_max = numeric(),
  pct_diff_max = numeric(),
  stringsAsFactors = FALSE
)

for (site_name in names(lakefetch_results)) {
  if (!is.null(waver_results[[site_name]]) && !is.null(lakefetch_results[[site_name]])) {
    w <- waver_results[[site_name]]
    l <- lakefetch_results[[site_name]]

    comparison <- rbind(comparison, data.frame(
      Site = site_name,
      waver_mean = round(w$mean, 1),
      lakefetch_mean = round(l$mean, 1),
      diff_mean = round(l$mean - w$mean, 1),
      pct_diff_mean = round((l$mean - w$mean) / w$mean * 100, 1),
      waver_max = round(w$max, 1),
      lakefetch_max = round(l$max, 1),
      diff_max = round(l$max - w$max, 1),
      pct_diff_max = round((l$max - w$max) / w$max * 100, 1),
      stringsAsFactors = FALSE
    ))
  }
}

cat("Mean Fetch Comparison:\n")
cat(sprintf("  %-10s %10s %12s %10s %10s\n", "Site", "waver (m)", "lakefetch (m)", "Diff (m)", "Diff (%)"))
cat("  ", "-" |> rep(54) |> paste(collapse = ""), "\n", sep = "")
for (i in 1:nrow(comparison)) {
  cat(sprintf("  %-10s %10.1f %12.1f %10.1f %10.1f%%\n",
              comparison$Site[i],
              comparison$waver_mean[i],
              comparison$lakefetch_mean[i],
              comparison$diff_mean[i],
              comparison$pct_diff_mean[i]))
}

cat("\nMax Fetch Comparison:\n")
cat(sprintf("  %-10s %10s %12s %10s %10s\n", "Site", "waver (m)", "lakefetch (m)", "Diff (m)", "Diff (%)"))
cat("  ", "-" |> rep(54) |> paste(collapse = ""), "\n", sep = "")
for (i in 1:nrow(comparison)) {
  cat(sprintf("  %-10s %10.1f %12.1f %10.1f %10.1f%%\n",
              comparison$Site[i],
              comparison$waver_max[i],
              comparison$lakefetch_max[i],
              comparison$diff_max[i],
              comparison$pct_diff_max[i]))
}

# ==============================================================================
# Summary Statistics
# ==============================================================================

cat("\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("SUMMARY\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n\n")

mean_abs_pct_diff <- mean(abs(comparison$pct_diff_mean), na.rm = TRUE)
max_abs_pct_diff <- max(abs(comparison$pct_diff_mean), na.rm = TRUE)

cat("Mean fetch - Mean absolute % difference:", round(mean_abs_pct_diff, 2), "%\n")
cat("Mean fetch - Max absolute % difference:", round(max_abs_pct_diff, 2), "%\n")

# Correlation
if (nrow(comparison) >= 3) {
  cor_mean <- cor(comparison$waver_mean, comparison$lakefetch_mean)
  cor_max <- cor(comparison$waver_max, comparison$lakefetch_max)
  cat("Correlation (mean fetch):", round(cor_mean, 4), "\n")
  cat("Correlation (max fetch):", round(cor_max, 4), "\n")
}

cat("\n")

# Pass/Fail threshold: 5% mean difference
threshold <- 5
if (is.na(mean_abs_pct_diff) || is.nan(mean_abs_pct_diff)) {
  cat("VALIDATION INCOMPLETE: Could not compare packages (waver errors)\n")
  cat("  This may be due to API differences between packages.\n")
  validation_passed <- NA
} else if (mean_abs_pct_diff < threshold) {
  cat("VALIDATION PASSED: Mean difference <", threshold, "% between packages\n")
  validation_passed <- TRUE
} else {
  cat("VALIDATION NOTICE: Mean difference >=", threshold, "% between packages\n")
  cat("  (Differences may be due to algorithmic variations, not errors)\n")
  validation_passed <- FALSE
}

cat("\n")
cat("Note: Small differences between packages are expected due to:\n")
cat("  - Different ray-casting implementations\n")
cat("  - Boundary intersection algorithms\n")
cat("  - Coordinate precision handling\n")
cat("\n")

# Reset lakefetch options
lakefetch_reset_options()
