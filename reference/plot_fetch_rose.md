# Plot Fetch Rose Diagram

Create a rose diagram showing directional fetch for a single site.

## Usage

``` r
plot_fetch_rose(fetch_data, site, title = NULL)
```

## Arguments

- fetch_data:

  Results from
  [`fetch_calculate`](https://jeremylfarrell.github.io/lakefetch/reference/fetch_calculate.md)

- site:

  Site name (character) or row index (integer) to plot

- title:

  Optional plot title (defaults to site name)

## Value

Invisible NULL (creates base R plot)

## Examples

``` r
if (FALSE) { # interactive()
csv_path <- system.file("extdata", "sample_sites.csv", package = "lakefetch")
sites <- load_sites(csv_path)
lake <- get_lake_boundary(sites)
results <- fetch_calculate(sites, lake)
plot_fetch_rose(results, results$results$Site[1])
}
```
