# Launch Interactive Fetch App

Launch a Shiny app for interactive exploration of fetch calculation
results. Click on site markers to view fetch rays and detailed
information. Click anywhere on the map to analyze a new point.

## Usage

``` r
fetch_app(fetch_data, title = NULL)
```

## Arguments

- fetch_data:

  Results from
  [`fetch_calculate`](https://jeremylfarrell.github.io/lakefetch/reference/fetch_calculate.md)

- title:

  Optional app title

## Value

Launches a Shiny app (does not return)

## Details

Requires the shiny, leaflet, and base64enc packages (suggested
dependencies).

The app displays:

- Interactive map with satellite imagery

- Site markers colored by exposure category

- Click markers to see fetch rays

- Popup with rose diagram and metrics

- Click anywhere on the map to analyze a new point

## Examples

``` r
if (interactive()) {
  sites <- load_sites("my_sites.csv")
  lake <- get_lake_boundary(sites)
  results <- fetch_calculate(sites, lake)
  fetch_app(results)
}
```
