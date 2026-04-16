# Assign Sites to Lakes

Perform spatial join to assign each site to its containing lake polygon.

## Usage

``` r
assign_sites_to_lakes(sites_sf, water_polygons, tolerance_m = NULL)
```

## Arguments

- sites_sf:

  sf object with site points

- water_polygons:

  sf object with lake polygons

- tolerance_m:

  Buffer distance for matching sites near lake edges

## Value

sf object with sites and added columns for lake_osm_id, lake_name,
lake_area_km2

## Examples

``` r
if (FALSE) { # interactive()
csv_path <- system.file("extdata", "sample_sites.csv", package = "lakefetch")
sites <- load_sites(csv_path)
lake_data <- get_lake_boundary(sites)

# Assign sites to their containing lakes
sites_assigned <- assign_sites_to_lakes(
  lake_data$sites,
  lake_data$all_lakes,
  tolerance_m = 50
)

# Check assignments
table(sites_assigned$lake_name)
}
```
