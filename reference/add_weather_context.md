# Add Weather Context to Fetch Results

Adds historical weather metrics and cumulative wave energy to fetch
calculation results. For each site, the function queries the Open-Meteo
historical-weather API for wind speed and direction in the days leading
up to the sample's `datetime`, combines those winds with the site's
directional fetch to estimate wave height (Sverdrup-Munk-Bretschneider
equations), and integrates wave energy across the requested look-back
window(s).

## Usage

``` r
add_weather_context(
  fetch_results,
  datetime_col = "datetime",
  windows_hours = c(24, 72, 168),
  depth_m = NULL
)
```

## Arguments

- fetch_results:

  sf object with fetch results (must have datetime column)

- datetime_col:

  Name of the datetime column

- windows_hours:

  Numeric vector of look-back windows in hours over which to integrate
  cumulative wave energy. The default `c(24, 72, 168)` produces metrics
  for the 1-day, 3-day, and 7-day windows ending at each site's sampling
  `datetime`. For each window the function adds columns named
  `wave_energy_24h`, `wave_energy_72h`, `wave_energy_168h`, etc.

- depth_m:

  Water depth for orbital velocity calculation

## Value

sf object with additional weather columns

## Details

The input data must have a datetime column in POSIXct format or a format
that can be parsed (ISO 8601, or common date-time formats). Network
access to the Open-Meteo API is required; sites are queried sequentially
with a short pause between calls to respect the public API rate limit.

## Examples

``` r
if (FALSE) { # interactive()
csv_path <- system.file("extdata", "sample_sites.csv", package = "lakefetch")
sites <- load_sites(csv_path)
lake <- get_lake_boundary(sites)
results <- fetch_calculate(sites, lake)

# Add datetime to results
results$results$datetime <- as.POSIXct("2024-07-15 14:00:00")

# Add weather context
results_with_weather <- add_weather_context(
  results$results,
  datetime_col = "datetime"
)
}
```
