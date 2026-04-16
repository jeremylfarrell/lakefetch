# Add Lake Context from NHD

Add hydrological context to fetch results using the National Hydrography
Dataset (NHD). Includes outlet/inlet locations, watershed area,
connectivity classification, and stream order.

## Usage

``` r
add_lake_context(fetch_results, lake_polygons, utm_epsg)
```

## Arguments

- fetch_results:

  sf object with fetch calculation results

- lake_polygons:

  sf object with lake polygons

- utm_epsg:

  EPSG code for UTM projection

## Value

sf object with additional columns for NHD context

## Details

Requires the nhdplusTools package. If not available, returns the input
with NA columns added for consistent output format.

Added columns include:

- nhd_permanent_id: NHD permanent identifier

- nhd_gnis_name: GNIS name from NHD

- nhd_areasqkm: Area in square kilometers from NHD

- outlet_dist_m: Distance to outlet in meters

- outlet_bearing: Compass direction to outlet

- inlet_nearest_dist_m: Distance to nearest inlet

- inlet_nearest_bearing: Compass direction to nearest inlet

- inlet_count: Number of inlets

- connectivity_class: Headwater/Drainage/Terminal/Isolated

- outlet_stream_order: Strahler stream order at outlet

- watershed_area_ha: Watershed area in hectares

- lake_watershed_ratio: Lake area / watershed area

## Examples

``` r
if (FALSE) { # interactive()
csv_path <- system.file("extdata", "sample_sites.csv", package = "lakefetch")
sites <- load_sites(csv_path)
lake <- get_lake_boundary(sites)
results <- fetch_calculate(sites, lake)
results_with_context <- add_lake_context(results$results, results$lakes, lake$utm_epsg)
}
```
