# ==============================================================================
# Run Lakefetch with Historical Weather Integration
# ==============================================================================
# This script demonstrates how to calculate fetch AND add historical weather
# context including cumulative wave energy based on actual wind conditions.
#
# Your CSV file should have:
#   - latitude/longitude columns
#   - A datetime column (e.g., "datetime", "sample_date", "date")
#
# The script will add:
#   - Wind metrics (mean, max, direction) for 24h, 3-day, 7-day windows
#   - Temperature, precipitation, humidity, pressure, cloud cover, solar radiation
#   - Cumulative wave energy based on actual wind + fetch
#   - Estimated wave heights and orbital velocities
# ==============================================================================

# Load the package
devtools::load_all(".")

# ------------------------------------------------------------------------------
# Configure your input
# ------------------------------------------------------------------------------

# Your CSV file with lat, lon, and datetime columns
INPUT_FILE <- "your_data_with_dates.csv"

# Example: Create sample data if you don't have a file yet
# This creates a test dataset - replace with your actual file
if (!file.exists(INPUT_FILE)) {
  message("Creating example data file...")
  example_data <- data.frame(
    Site = c("Site_A", "Site_B", "Site_C"),
    latitude = c(42.2213, 42.1305, 42.1696),
    longitude = c(-79.4587, -79.3682, -79.4021),
    datetime = c("2024-07-15 10:00:00", "2024-07-15 14:00:00", "2024-07-16 09:00:00")
  )
  write.csv(example_data, "example_weather_data.csv", row.names = FALSE)
  INPUT_FILE <- "example_weather_data.csv"
}

# Water depth for orbital velocity calculation (meters)
WATER_DEPTH_M <- 5

# ------------------------------------------------------------------------------
# Run the analysis
# ------------------------------------------------------------------------------

# Step 1: Load sites (will auto-detect datetime column)
cat("Loading sites...\n")
sites <- load_sites(INPUT_FILE)
print(sites)

# Step 2: Get lake boundaries
cat("\nDownloading lake boundaries...\n")
lake <- get_lake_boundary(sites)

# Step 3: Calculate fetch
cat("\nCalculating fetch...\n")
results <- fetch_calculate(sites, lake, add_context = FALSE)

# Step 4: Add weather context
# This fetches historical weather from Open-Meteo API
cat("\nFetching historical weather data...\n")
results_with_weather <- add_weather_context(
  results$results,
  datetime_col = "datetime",
  windows_hours = c(24, 72, 168),  # 24h, 3-day, 7-day windows
  depth_m = WATER_DEPTH_M
)

# ------------------------------------------------------------------------------
# View results
# ------------------------------------------------------------------------------

cat("\n=== Results with Weather Context ===\n")

# Key columns to display
key_cols <- c(
  "Site", "datetime", "lake_name", "exposure_category",
  # Fetch
  "fetch_effective",
  # Wind at sample time
  "wind_at_sample", "wind_dir_at_sample",
  # Wind history
  "wind_mean_24h", "wind_max_24h",
  "wind_mean_3d", "wind_max_3d",
  # Wave energy
  "wave_energy_24h", "wave_energy_3d",
  "wave_height_max_3d",
  "orbital_velocity_mean_3d",
  # Other weather
  "temp_mean_24h", "precip_total_3d",
  "days_since_strong_wind"
)

# Filter to existing columns
display_cols <- key_cols[key_cols %in% names(results_with_weather)]
print(sf::st_drop_geometry(results_with_weather[, display_cols]))

# ------------------------------------------------------------------------------
# Save results
# ------------------------------------------------------------------------------

output_file <- gsub("\\.csv$", "_with_weather.csv", INPUT_FILE)
cat("\nSaving results to:", output_file, "\n")

# Convert to data frame and add coordinates
output_df <- sf::st_drop_geometry(results_with_weather)
coords <- sf::st_coordinates(sf::st_transform(results_with_weather, 4326))
output_df$longitude <- coords[, 1]
output_df$latitude <- coords[, 2]

# Remove the rose_plot column if present
output_df$rose_plot <- NULL

write.csv(output_df, output_file, row.names = FALSE)

cat("\nDone! Results saved with", ncol(output_df), "columns.\n")

# ------------------------------------------------------------------------------
# Column descriptions
# ------------------------------------------------------------------------------

cat("\n=== Weather Column Guide ===\n")
cat("
Wind Metrics (for each time window: 24h, 3d, 7d):
  wind_mean_*      - Mean wind speed (m/s)
  wind_max_*       - Maximum wind speed (m/s)
  wind_gust_max_*  - Maximum wind gust (m/s)
  wind_dir_dominant_* - Wind-speed-weighted dominant direction (degrees)

Wave Metrics (calculated from wind + directional fetch):
  wave_energy_*    - Cumulative wave energy (proportional to mixing)
  wave_height_mean_* - Mean significant wave height (m)
  wave_height_max_* - Maximum wave height in window (m)
  orbital_velocity_mean_3d - Mean bottom orbital velocity (m/s)

Temperature:
  temp_mean_*, temp_min_*, temp_max_* - Air temperature (C)

Precipitation:
  precip_total_*   - Total precipitation (mm)

Other:
  humidity_mean_*  - Relative humidity (%)
  pressure_mean_*  - Atmospheric pressure (hPa)
  cloud_cover_mean_* - Cloud cover (%)
  solar_rad_mean_* - Solar radiation (W/m2)

Special:
  wind_at_sample   - Wind speed at sample time
  wind_dir_at_sample - Wind direction at sample time
  temp_at_sample   - Temperature at sample time
  days_since_strong_wind - Days since wind > 8 m/s
")
