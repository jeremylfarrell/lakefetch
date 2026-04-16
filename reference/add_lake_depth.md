# Add Depth Information to Fetch Results

Looks up or estimates depth for each lake in the fetch results and adds
depth columns.

## Usage

``` r
add_lake_depth(fetch_results, lakes, user_depths = NULL)
```

## Arguments

- fetch_results:

  sf object with fetch results

- lakes:

  sf object with lake polygons

- user_depths:

  Named vector of user-provided depths (names = lake IDs)

## Value

fetch_results with added depth columns

## Examples

``` r
if (FALSE) { # interactive()
data(adirondack_sites)
sites <- load_sites(adirondack_sites)
lake <- get_lake_boundary(sites)
results <- fetch_calculate(sites, lake)

# Add depth estimates
results$results <- add_lake_depth(results$results, results$lakes)

# Or provide known depths using an actual lake_osm_id from results
lake_id <- results$lakes$osm_id[1]
depths <- setNames(15.5, lake_id)
results$results <- add_lake_depth(results$results, results$lakes, user_depths = depths)
}
```
