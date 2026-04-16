# Get Lake Boundary

Get lake boundary polygon(s) either from OpenStreetMap or from a local
file.

## Usage

``` r
get_lake_boundary(sites, file = NULL)
```

## Arguments

- sites:

  A data.frame with latitude and longitude columns, or an sf object.

- file:

  Optional file path to a shapefile or geopackage with lake boundaries.

## Value

A list with elements:

- all_lakes:

  sf object with lake polygons in UTM projection

- sites:

  sf object with site points in UTM projection

- utm_epsg:

  EPSG code for the UTM projection used

## Details

If `file` is provided, the lake boundary is loaded from the file.
Otherwise, the function downloads lake boundaries from OpenStreetMap
based on the bounding box of the provided sites.

## Examples

``` r
if (FALSE) { # interactive()
csv_path <- system.file("extdata", "sample_sites.csv", package = "lakefetch")
sites <- load_sites(csv_path)
lake_data <- get_lake_boundary(sites)
}
```
