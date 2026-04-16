# Plot Fetch Map

Create a map showing site locations colored by exposure category.

## Usage

``` r
plot_fetch_map(fetch_data, title = "Fetch Analysis - Site Locations")
```

## Arguments

- fetch_data:

  Results from
  [`fetch_calculate`](https://jeremylfarrell.github.io/lakefetch/reference/fetch_calculate.md)

- title:

  Optional plot title

## Value

A ggplot2 object

## Examples

``` r
if (FALSE) { # interactive()
csv_path <- system.file("extdata", "sample_sites.csv", package = "lakefetch")
sites <- load_sites(csv_path)
lake <- get_lake_boundary(sites)
results <- fetch_calculate(sites, lake)
plot_fetch_map(results)
}
```
