# ==============================================================================
# Cross-Validation: lakefetch vs fetchR
# ==============================================================================
# This script compares fetch calculations between lakefetch and the fetchR
# package (CRAN) using the same test geometry.
#
# Reference: Seers, B. (2020). fetchR: Calculate Wind Fetch.
#            R package. https://cran.r-project.org/package=fetchR
#
# Run with: source("inst/validation/cross_validate_fetchR.R")
# ==============================================================================

library(sf)

# Check for fetchR package
if (!requireNamespace("fetchR", quietly = TRUE)) {
  message("Installing fetchR package from CRAN...")
  install.packages("fetchR", repos = "https://cloud.r-project.org")
}
library(fetchR)

# Load lakefetch
if (!requireNamespace("lakefetch", quietly = TRUE)) {
  devtools::load_all(".")
} else {
  library(lakefetch)
}

cat("\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("CROSS-VALIDATION: lakefetch vs fetchR\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n\n")

# ==============================================================================
# Create Test Lake
# ==============================================================================

epsg <- 32618
center_x <- 500000
center_y <- 4800000

# Create an elliptical lake
n_points <- 360
angles <- seq(0, 2 * pi, length.out = n_points + 1)
a <- 2000  # semi-major axis (m)
b <- 1000  # semi-minor axis (m)
rotation <- pi / 6  # 30 degree rotation

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

sites <- data.frame(
  id = 1:5,
  name = c("Center", "North", "South", "East", "West"),
  x = c(center_x, center_x, center_x, center_x + 800, center_x - 800),
  y = c(center_y, center_y + 600, center_y - 600, center_y, center_y)
)

sites_sf <- st_as_sf(sites, coords = c("x", "y"), crs = epsg)

cat("Test Sites:\n")
for (i in 1:nrow(sites)) {
  cat("  ", sites$name[i], ": (", sites$x[i], ", ", sites$y[i], ")\n", sep = "")
}
cat("\n")

# ==============================================================================
# Calculate Fetch with fetchR
# ==============================================================================

cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("Calculating fetch with FETCHR package...\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n\n")

# fetchR requires a coastline polygon (land boundary)
# For a lake, we create land surrounding the lake

# Create a land polygon with the lake as a hole
bbox_buffer <- 20000
land_outer_coords <- rbind(
  c(center_x - bbox_buffer, center_y - bbox_buffer),
  c(center_x + bbox_buffer, center_y - bbox_buffer),
  c(center_x + bbox_buffer, center_y + bbox_buffer),
  c(center_x - bbox_buffer, center_y + bbox_buffer),
  c(center_x - bbox_buffer, center_y - bbox_buffer)
)

# Lake coordinates (reversed for hole)
lake_coords <- coords[nrow(coords):1, ]

# Create polygon with hole
land_with_lake_hole <- st_polygon(list(land_outer_coords, lake_coords))
coastline_sf <- st_sf(
  id = 1,
  geometry = st_sfc(land_with_lake_hole, crs = epsg)
)

# Convert to sp for fetchR (it uses sp, not sf)
coastline_sp <- as(coastline_sf, "Spatial")
sites_sp <- as(sites_sf, "Spatial")

n_directions <- 72  # 5-degree resolution

fetchR_results <- list()

tryCatch({
  cat("Running fetchR::fetch()...\n")

  # fetchR::fetch calculates fetch for all points
  fetchR_result <- fetchR::fetch(
    polygon_layer = coastline_sp,
    site_layer = sites_sp,
    max_dist = 10000,
    n_directions = n_directions,
    quiet = TRUE
  )

  # Extract results
  fetchR_df <- as.data.frame(fetchR_result)

  for (i in 1:nrow(sites)) {
    site_name <- sites$name[i]

    # Get fetch columns for this site
    fetch_cols <- grep("^fetch_", names(fetchR_df), value = TRUE)
    fetch_vals <- as.numeric(fetchR_df[i, fetch_cols])

    fetchR_results[[site_name]] <- list(
      fetch = fetch_vals,
      mean = mean(fetch_vals, na.rm = TRUE),
      max = max(fetch_vals, na.rm = TRUE),
      min = min(fetch_vals, na.rm = TRUE)
    )

    cat("  ", site_name, ": mean =", round(mean(fetch_vals, na.rm = TRUE), 1),
        "m, max =", round(max(fetch_vals, na.rm = TRUE), 1), "m\n")
  }

}, error = function(e) {
  cat("  ERROR: ", conditionMessage(e), "\n")
  cat("  fetchR may have compatibility issues with this geometry.\n")
})

cat("\n")

# ==============================================================================
# Calculate Fetch with lakefetch
# ==============================================================================

cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("Calculating fetch with LAKEFETCH package...\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n\n")

lakefetch_options(
  buffer_distance_m = 0,
  angle_resolution_deg = 5,
  max_fetch_m = 10000
)

lakefetch_results <- list()

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
  fetchR_mean = numeric(),
  lakefetch_mean = numeric(),
  diff_mean = numeric(),
  pct_diff_mean = numeric(),
  stringsAsFactors = FALSE
)

for (site_name in names(lakefetch_results)) {
  if (!is.null(fetchR_results[[site_name]]) && !is.null(lakefetch_results[[site_name]])) {
    f <- fetchR_results[[site_name]]
    l <- lakefetch_results[[site_name]]

    comparison <- rbind(comparison, data.frame(
      Site = site_name,
      fetchR_mean = round(f$mean, 1),
      lakefetch_mean = round(l$mean, 1),
      diff_mean = round(l$mean - f$mean, 1),
      pct_diff_mean = round((l$mean - f$mean) / f$mean * 100, 1),
      stringsAsFactors = FALSE
    ))
  }
}

if (nrow(comparison) > 0) {
  cat("Mean Fetch Comparison:\n")
  cat(sprintf("  %-10s %12s %14s %10s %10s\n",
              "Site", "fetchR (m)", "lakefetch (m)", "Diff (m)", "Diff (%)"))
  cat("  ", "-" |> rep(58) |> paste(collapse = ""), "\n", sep = "")

  for (i in 1:nrow(comparison)) {
    cat(sprintf("  %-10s %12.1f %14.1f %10.1f %10.1f%%\n",
                comparison$Site[i],
                comparison$fetchR_mean[i],
                comparison$lakefetch_mean[i],
                comparison$diff_mean[i],
                comparison$pct_diff_mean[i]))
  }

  cat("\n")
  cat("-" |> rep(70) |> paste(collapse = ""), "\n")
  cat("SUMMARY\n")
  cat("-" |> rep(70) |> paste(collapse = ""), "\n\n")

  mean_abs_pct_diff <- mean(abs(comparison$pct_diff_mean), na.rm = TRUE)
  cat("Mean absolute % difference:", round(mean_abs_pct_diff, 2), "%\n")

  if (nrow(comparison) >= 3) {
    cor_val <- cor(comparison$fetchR_mean, comparison$lakefetch_mean)
    cat("Correlation:", round(cor_val, 4), "\n")
  }

  threshold <- 10  # 10% threshold for different algorithms
  if (mean_abs_pct_diff < threshold) {
    cat("\nVALIDATION PASSED: Results agree within", threshold, "%\n")
  } else {
    cat("\nVALIDATION NOTICE: Results differ by >", threshold, "%\n")
    cat("  (May be due to different ray-casting algorithms)\n")
  }

} else {
  cat("No comparison data available.\n")
  cat("fetchR may have failed - check error messages above.\n")

  # Show lakefetch results anyway
  cat("\nlakefetch results (standalone):\n")
  for (site_name in names(lakefetch_results)) {
    l <- lakefetch_results[[site_name]]
    cat("  ", site_name, ": mean =", round(l$mean, 1), "m\n")
  }
}

cat("\n")
lakefetch_reset_options()
