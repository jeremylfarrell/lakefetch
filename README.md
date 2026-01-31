# lakefetch

<!-- badges: start -->
[![R-CMD-check](https://github.com/jeremylfarrell/lakefetch/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jeremylfarrell/lakefetch/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Calculate fetch (open water distance) and wave exposure metrics for lake sampling sites.

## Overview

**lakefetch** calculates directional fetch using a ray-casting algorithm, can download lake boundaries from OpenStreetMap, and provides exposure classification for ecological and limnological studies. Unlike marine-focused packages (fetchR, waver), lakefetch is designed specifically for freshwater lakes with features like multi-lake batch processing and NHD integration.

## Installation

Install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("jeremylfarrell/lakefetch")
```

## Quick Start

```r
library(lakefetch)

# Load your sampling sites (CSV with latitude/longitude columns)
sites <- load_sites("my_sites.csv")

# Download lake boundaries from OpenStreetMap
lake <- get_lake_boundary(sites)

# Calculate fetch for all sites
results <- fetch_calculate(sites, lake)

# View results
results$results  # sf object with fetch data and exposure categories
```

## Features

- **Automatic boundary download**: Downloads lake polygons from OpenStreetMap with multi-server fallback
- **Ray-casting fetch calculation**: Measures distance to shore in all directions at configurable resolution
- **Multi-lake support**: Process sites across multiple lakes in a single analysis
- **Exposure classification**: Automatic categorization into Sheltered/Moderate/Exposed
- **NHD integration**: Optional integration with National Hydrography Dataset for US lakes (outlets, inlets, watershed area)
- **Weather integration**: Weather integration is optional and disabled by default during package checks
- **Visualization**: Static plots (maps, bar charts, rose diagrams) and interactive Shiny app

## Example Workflow

```r
library(lakefetch)

# Use built-in example data
data("adirondack_sites")

# Get lake boundaries
lake <- get_lake_boundary(adirondack_sites)

# Calculate fetch
results <- fetch_calculate(sites = adirondack_sites, lake = lake)

# Visualize results
plot_fetch_map(results)
plot_fetch_bars(results)

# Launch interactive app (requires shiny, leaflet)
fetch_app(results)
```

## Key Functions

| Function | Description |
|----------|-------------|
| `load_sites()` | Load sampling sites from CSV |
| `get_lake_boundary()` | Download lake boundaries from OSM or load from file |
| `fetch_calculate()` | Calculate directional fetch and exposure metrics |
| `plot_fetch_map()` | Map of sites colored by exposure category |
| `plot_fetch_bars()` | Bar chart of effective fetch by site |
| `plot_fetch_rose()` | Directional fetch rose diagram for a site |
| `fetch_app()` | Interactive Shiny app for exploration |

## Output Metrics

For each sampling site, lakefetch calculates:

- **Directional fetch**: Distance to shore at each angle (default: 10° resolution)
- **fetch_mean**: Mean fetch across all directions
- **fetch_max**: Maximum fetch (longest open water distance)
- **fetch_effective**: Mean of top 3 fetch values (primary exposure metric)
- **exposure_category**: Sheltered (<2.5 km), Moderate (2.5-5 km), or Exposed (>5 km)

## Using Local Boundary Files

If you have your own lake boundary shapefile or geopackage:

```r
lake <- get_lake_boundary(sites, file = "my_lake_boundary.shp")
results <- fetch_calculate(sites, lake)
```

## Optional Features

### NHD Integration (US lakes only)

```r
# Requires nhdplusTools package
# install.packages("nhdplusTools")

results <- fetch_calculate(sites, lake, add_context = TRUE)
# Adds: outlet location, inlet locations, watershed area, connectivity classification
```
### Weather Integration

```r
# Add historical wind data for wave energy calculations
# Requires a datetime column in your sites data
results <- add_weather_context(results$results, datetime_col = "datetime")
```

## Example Datasets
The package includes example datasets for testing:

- `adirondack_sites`: Sampling sites from Adirondack lakes (New York)
- `wisconsin_lakes`: Lake boundaries from Wisconsin
- `example_lake`: Single lake polygon for testing

## Citation

```r
citation("lakefetch")
```

## License

MIT License

## Contributing

Issues and pull requests are welcome at [GitHub](https://github.com/jeremylfarrell/lakefetch/issues).
