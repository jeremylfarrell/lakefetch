# ==============================================================================
# Create Example Datasets for lakefetch Package
# ==============================================================================
# This script creates example datasets included with the package.
# Run this script to regenerate the data files in data/
#
# Usage: source("data-raw/create_example_data.R")
# ==============================================================================

library(sf)

# ==============================================================================
# Dataset 1: adirondack_sites
# ==============================================================================
# Example lake sampling sites from the Adirondack region of New York State.
# These are synthetic but realistic coordinates for demonstration purposes.

adirondack_sites <- data.frame(
  Site = c(
    "Blue_Mountain_1", "Blue_Mountain_2", "Blue_Mountain_3",
    "Raquette_1", "Raquette_2", "Raquette_3", "Raquette_4",
    "Long_Lake_1", "Long_Lake_2",
    "Tupper_1", "Tupper_2", "Tupper_3"
  ),
  lake.name = c(
    rep("Blue Mountain Lake", 3),
    rep("Raquette Lake", 4),
    rep("Long Lake", 2),
    rep("Tupper Lake", 3)
  ),
  latitude = c(
    # Blue Mountain Lake (~43.87°N)
    43.8721, 43.8695, 43.8648,
    # Raquette Lake (~43.82°N)
    43.8234, 43.8189, 43.8267, 43.8156,
    # Long Lake (~43.98°N)
    43.9812, 43.9756,
    # Tupper Lake (~44.23°N)
    44.2298, 44.2245, 44.2187
  ),
  longitude = c(
    # Blue Mountain Lake (~74.44°W)
    -74.4412, -74.4356, -74.4489,
    # Raquette Lake (~74.66°W)
    -74.6623, -74.6578, -74.6701, -74.6534,
    # Long Lake (~74.42°W)
    -74.4234, -74.4189,
    # Tupper Lake (~74.46°W)
    -74.4612, -74.4567, -74.4498
  ),
  datetime = as.POSIXct(c(
    "2024-07-15 09:30:00", "2024-07-15 10:15:00", "2024-07-15 11:00:00",
    "2024-07-16 08:45:00", "2024-07-16 09:30:00", "2024-07-16 10:15:00", "2024-07-16 11:00:00",
    "2024-07-17 09:00:00", "2024-07-17 10:00:00",
    "2024-07-18 08:30:00", "2024-07-18 09:15:00", "2024-07-18 10:00:00"
  ), tz = "America/New_York"),
  stringsAsFactors = FALSE
)

cat("Created adirondack_sites:", nrow(adirondack_sites), "sites across",
    length(unique(adirondack_sites$lake.name)), "lakes\n")

# ==============================================================================
# Dataset 2: example_lake
# ==============================================================================
# A simple circular lake polygon for demonstration and testing.
# This synthetic lake has known geometry for validation.

center_x <- 500000
center_y <- 4800000
radius <- 1000  # 1 km radius
n_points <- 360

angles <- seq(0, 2 * pi, length.out = n_points + 1)
x <- center_x + radius * cos(angles)
y <- center_y + radius * sin(angles)

coords <- cbind(x, y)
poly <- st_polygon(list(coords))

example_lake <- st_sf(
  osm_id = "example_001",
  name = "Example Circular Lake",
  area_km2 = pi * (radius/1000)^2,
  geometry = st_sfc(poly, crs = 32618)  # UTM 18N
)

cat("Created example_lake: circular lake with radius", radius, "m\n")

# ==============================================================================
# Dataset 3: wisconsin_lakes
# ==============================================================================
# A small dataset of well-known Wisconsin lakes for testing.

wisconsin_lakes <- data.frame(
  Site = c(
    "Mendota_N", "Mendota_S", "Mendota_Deep",
    "Monona_1", "Monona_2",
    "Geneva_E", "Geneva_W", "Geneva_Center"
  ),
  lake.name = c(
    rep("Lake Mendota", 3),
    rep("Lake Monona", 2),
    rep("Geneva Lake", 3)
  ),
  latitude = c(
    # Lake Mendota
    43.1125, 43.0756, 43.0995,
    # Lake Monona
    43.0634, 43.0589,
    # Geneva Lake
    42.5912, 42.5834, 42.5878
  ),
  longitude = c(
    # Lake Mendota
    -89.4234, -89.4012, -89.4045,
    # Lake Monona
    -89.3612, -89.3789,
    # Geneva Lake
    -88.4312, -88.5123, -88.4756
  ),
  stringsAsFactors = FALSE
)

cat("Created wisconsin_lakes:", nrow(wisconsin_lakes), "sites across",
    length(unique(wisconsin_lakes$lake.name)), "lakes\n")

# ==============================================================================
# Save datasets
# ==============================================================================

# Create data directory if needed
if (!dir.exists("data")) {
  dir.create("data")
}

# Save as .rda files (R data format)
save(adirondack_sites, file = "data/adirondack_sites.rda", compress = "xz")
save(example_lake, file = "data/example_lake.rda", compress = "xz")
save(wisconsin_lakes, file = "data/wisconsin_lakes.rda", compress = "xz")

cat("\nDatasets saved to data/:\n")
cat("  - adirondack_sites.rda\n")
cat("  - example_lake.rda\n")
cat("  - wisconsin_lakes.rda\n")

cat("\nTo use in package, add documentation in R/data.R\n")
