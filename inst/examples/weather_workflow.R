# ==============================================================================
# lakefetch Weather Integration Example
# ==============================================================================
# Demonstrates how to calculate fetch AND add historical weather context
# including cumulative wave energy based on actual wind conditions.
#
# Your CSV file should have:
#   - latitude/longitude columns
#   - A datetime column (e.g., "datetime", "sample_date", "date")

library(lakefetch)

# ------------------------------------------------------------------------------
# 1. Load Site Data with Datetime
# ------------------------------------------------------------------------------

# Load from CSV (must have datetime column)
sites <- load_sites("your_sites.csv")

# Or create sample data
sites <- data.frame(
  Site = c("Site_A", "Site_B", "Site_C"),
  latitude = c(42.2213, 42.1305, 42.1696),
  longitude = c(-79.4587, -79.3682, -79.4021),
  datetime = as.POSIXct(c("2024-07-15 10:00:00", "2024-07-15 14:00:00", "2024-07-16 09:00:00"))
)
sites <- load_sites(sites)

# ------------------------------------------------------------------------------
# 2. Calculate Fetch
# ------------------------------------------------------------------------------

lake <- get_lake_boundary(sites)
results <- fetch_calculate(sites, lake, add_context = FALSE)

# ------------------------------------------------------------------------------
# 3. Add Lake Depth Estimates
# ------------------------------------------------------------------------------

# Tries HydroLAKES first, then estimates from lake area
results_with_depth <- add_lake_depth(results$results, results$lakes)

# ------------------------------------------------------------------------------
# 4. Add Weather Context
# ------------------------------------------------------------------------------

# Fetches historical weather from Open-Meteo API (free, no registration)
# Uses depth_mean_m column for wave calculations
results_with_weather <- add_weather_context(
  results_with_depth,
  datetime_col = "datetime",
  windows_hours = c(24, 72, 168)  # 24h, 3-day, 7-day windows
)

# ------------------------------------------------------------------------------
# 5. View Results
# ------------------------------------------------------------------------------

# Key weather columns added:
#   Wind: wind_mean_24h, wind_max_3d, wind_dir_dominant_24h, etc.
#   Waves: wave_energy_24h, wave_height_max_3d, orbital_velocity_mean_3d
#   Weather: temp_mean_24h, precip_total_3d, days_since_strong_wind

key_cols <- c("Site", "exposure_category", "depth_mean_m",
              "wind_mean_24h", "wave_energy_3d", "temp_mean_24h")
print(sf::st_drop_geometry(results_with_weather[, key_cols]))

# ------------------------------------------------------------------------------
# 6. Save Results
# ------------------------------------------------------------------------------

output_df <- sf::st_drop_geometry(results_with_weather)
coords <- sf::st_coordinates(sf::st_transform(results_with_weather, 4326))
output_df$longitude <- coords[, 1]
output_df$latitude <- coords[, 2]
write.csv(output_df, "results_with_weather.csv", row.names = FALSE)
