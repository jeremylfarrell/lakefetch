# lakefetch: Calculate Fetch and Wave Exposure for Lake Sampling Points

The lakefetch package provides tools for calculating fetch (open water
distance) and wave exposure metrics for lake sampling points. It
downloads lake boundaries from OpenStreetMap, calculates directional
fetch using ray-casting, and optionally integrates with NHD for
hydrological context.

## Main Functions

- [`fetch_calculate`](https://jeremylfarrell.github.io/lakefetch/reference/fetch_calculate.md):

  Main entry point for fetch calculation

- [`load_sites`](https://jeremylfarrell.github.io/lakefetch/reference/load_sites.md):

  Load and validate site data

- [`get_lake_boundary`](https://jeremylfarrell.github.io/lakefetch/reference/get_lake_boundary.md):

  Get lake boundary from OSM or file

- [`add_lake_context`](https://jeremylfarrell.github.io/lakefetch/reference/add_lake_context.md):

  Add NHD hydrological context

- [`fetch_app`](https://jeremylfarrell.github.io/lakefetch/reference/fetch_app.md):

  Launch interactive Shiny app

## Visualization

- [`plot_fetch_map`](https://jeremylfarrell.github.io/lakefetch/reference/plot_fetch_map.md):

  Map of sites colored by exposure

- [`plot_fetch_bars`](https://jeremylfarrell.github.io/lakefetch/reference/plot_fetch_bars.md):

  Bar chart of effective fetch

- [`plot_fetch_rose`](https://jeremylfarrell.github.io/lakefetch/reference/plot_fetch_rose.md):

  Rose diagram for single site

- [`create_ray_geometries`](https://jeremylfarrell.github.io/lakefetch/reference/create_ray_geometries.md):

  Create ray lines for mapping

## Configuration

- [`lakefetch_options`](https://jeremylfarrell.github.io/lakefetch/reference/lakefetch_options.md):

  Get/set package options

- [`lakefetch_reset_options`](https://jeremylfarrell.github.io/lakefetch/reference/lakefetch_reset_options.md):

  Reset options to defaults

## See also

Useful links:

- <https://jeremylfarrell.github.io/lakefetch/>

- <https://github.com/jeremylfarrell/lakefetch>

- Report bugs at <https://github.com/jeremylfarrell/lakefetch/issues>

## Author

**Maintainer**: Jeremy Lynch Farrell <farrej2@rpi.edu>
