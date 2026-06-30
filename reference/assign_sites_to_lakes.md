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

  Buffer distance in meters for matching sites that fall just outside
  lake polygons (e.g., due to GPS noise or coarse OSM boundaries).
  Default is the value from
  [`lakefetch_options()`](https://jeremylfarrell.github.io/lakefetch/reference/lakefetch_options.md)
  (50 m). For datasets with appreciable GPS error or for very coarsely
  mapped shorelines, try 100-500 m.

## Value

sf object with sites and added columns for lake_osm_id, lake_name,
lake_area_km2

## Examples

``` r
if (FALSE) { # interactive()
csv_path <- system.file("extdata", "sample_sites.csv", package = "lakefetch")
sites <- load_sites(csv_path)
lake_data <- get_lake_boundary(sites)

# Assign sites to their containing lakes. Default tolerance (50 m) is
# appropriate for sites with accurate coordinates that fall inside the
# lake polygon. If your sites are near the shoreline or your GPS error is
# larger, increase tolerance_m (e.g., 200-500 m).
sites_assigned <- assign_sites_to_lakes(
  lake_data$sites,
  lake_data$all_lakes,
  tolerance_m = 200
)

# Check assignments
table(sites_assigned$lake_name)
}
```
