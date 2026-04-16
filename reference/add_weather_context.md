# Add Weather Context to Fetch Results

Adds historical weather metrics and cumulative wave energy to fetch
calculation results.

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

  Vector of time windows in hours (default c(24, 72, 168))

- depth_m:

  Water depth for orbital velocity calculation

## Value

sf object with additional weather columns

## Details

The input data must have a datetime column in POSIXct format or a format
that can be parsed (ISO 8601, or common date-time formats).

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
