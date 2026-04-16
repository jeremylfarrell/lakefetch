# Getting Started with lakefetch

## Introduction

The **lakefetch** package calculates fetch (open water distance) and
wave exposure metrics for lake sampling points. Fetch is an important
physical parameter that influences wave energy, sediment resuspension,
and habitat characteristics.

## Installation

``` r
install.packages("lakefetch")

# Optional packages for extra features:
# install.packages("nhdplusTools")                      # NHD integration
# install.packages(c("shiny", "leaflet", "base64enc"))  # Interactive app
```

## Quick Start

### 1. Load your site data

``` r
library(lakefetch)

# From CSV file
sites <- load_sites("my_lake_sites.csv")

# Or create manually
sites <- data.frame(
  Site = c("Site_A", "Site_B", "Site_C"),
  latitude = c(43.42, 43.43, 43.41),
  longitude = c(-73.69, -73.68, -73.70)
)
sites <- load_sites(sites)
```

Your CSV should have columns for site names and coordinates. Column
names starting with “lat” and “lon” are automatically detected.

### 2. Get lake boundary

``` r
# Automatically download from OpenStreetMap
lake <- get_lake_boundary(sites)

# Or load from a local shapefile
lake <- get_lake_boundary(sites, file = "lake_boundary.shp")
```

### 3. Calculate fetch

``` r
results <- fetch_calculate(sites, lake)

# View results
head(results$results)
```

### 4. Visualize

``` r
# Map of sites
plot_fetch_map(results)

# Bar chart
plot_fetch_bars(results)

# Interactive app
fetch_app(results)
```

## Understanding the Results

The
[`fetch_calculate()`](https://jeremylfarrell.github.io/lakefetch/reference/fetch_calculate.md)
function returns a list containing:

- `results`: An sf object with one row per site, containing:
  - `fetch_0`, `fetch_5`, …, `fetch_355`: Fetch distance in each
    direction (meters)
  - `fetch_mean`: Average fetch across all directions
  - `fetch_max`: Maximum fetch (longest open water distance)
  - `fetch_effective`: Mean of the three longest fetch values
  - `orbital_effective`: Estimated wave orbital velocity (m/s)
  - `exposure_category`: Classification as “Sheltered”, “Moderate”, or
    “Exposed”
- `lakes`: The lake polygon(s) used for calculation
- `angles`: The angles (in degrees) used for ray-casting

## Exposure Categories

Sites are classified based on effective fetch:

| Category  | Effective Fetch | Typical Conditions                   |
|-----------|-----------------|--------------------------------------|
| Sheltered | \< 2.5 km       | Protected bays, minimal wave action  |
| Moderate  | 2.5 - 5 km      | Some wave exposure, mixed conditions |
| Exposed   | \> 5 km         | Open water, significant wave energy  |

## Configuration Options

Customize the calculation parameters:

``` r
# View current options
lakefetch_options()

# Change options
lakefetch_options(
  buffer_distance_m = 20,      # GPS accuracy buffer
  angle_resolution_deg = 10,   # Direction resolution (degrees)
  max_fetch_m = 30000,         # Maximum fetch distance
  use_parallel = TRUE          # Parallel processing for multiple lakes
)

# Reset to defaults
lakefetch_reset_options()
```

## NHD Integration

If the `nhdplusTools` package is installed, lakefetch can add
hydrological context from the National Hydrography Dataset: - Lake
outlet and inlet locations - Distance and direction to outlet/inlets
from each site - Connectivity classification (Headwater, Drainage,
Terminal, Isolated) - Outlet stream order - Watershed area

``` r
# NHD context is added automatically if available
results <- fetch_calculate(sites, lake, add_context = TRUE)

# Check connectivity
table(results$results$connectivity_class)
```

## Working with Multiple Lakes

If your sites span multiple lakes, lakefetch handles this automatically:

``` r
# Sites are assigned to their containing lake polygon
sites_with_lakes <- assign_sites_to_lakes(lake$sites, lake$all_lakes)

# Check assignment
table(sites_with_lakes$lake_name)
```

## Exporting Results

``` r
# To CSV (without geometry)
write.csv(sf::st_drop_geometry(results$results),
          "fetch_results.csv", row.names = FALSE)

# To GeoPackage (with geometry)
sf::st_write(results$results, "fetch_results.gpkg")

# Export ray geometries for GIS
rays <- create_ray_geometries(results)
sf::st_write(rays, "fetch_rays.gpkg")
```
