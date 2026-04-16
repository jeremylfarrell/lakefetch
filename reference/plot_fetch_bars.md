# Plot Fetch Bar Chart

Create a bar chart showing effective fetch by site.

## Usage

``` r
plot_fetch_bars(fetch_data, title = "Effective Fetch by Site")
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
plot_fetch_bars(results)
}
```
