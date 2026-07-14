# Get Lake Boundary

Get lake boundary polygon(s) either from OpenStreetMap or from a local
file.

## Usage

``` r
get_lake_boundary(
  sites,
  file = NULL,
  timeout = 90,
  simplify_tolerance_m = 0,
  total_timeout_s = 300
)
```

## Arguments

- sites:

  A data.frame with latitude and longitude columns, or an sf object.

- file:

  Optional file path to a shapefile or geopackage with lake boundaries.

- timeout:

  Integer; Overpass API query timeout in seconds. Default is 90.
  Increase for very large lakes (e.g., `timeout = 300` for Mälaren or
  Great Lakes) or when server load is high.

- simplify_tolerance_m:

  Numeric; if greater than 0, simplify lake polygons with
  `sf::st_simplify(dTolerance = simplify_tolerance_m)` (in meters,
  applied in the UTM projection). Useful for very large or complex lakes
  where an exact coastline is not needed and a coarser polygon greatly
  speeds up fetch ray-casting. Typical values: 50-500 m for large lakes
  (e.g., Mälaren, Vättern). Default is 0 (no simplification).

- total_timeout_s:

  Numeric; soft wall-clock budget in seconds on the total time
  `get_lake_boundary()` will spend downloading from OSM. The budget is
  consulted at natural breakpoints (between Overpass query types,
  between clusters for spread-out sites, and between name-filtered
  queries) and aborts further work when exceeded. It is NOT a hard cap
  on a single
  [`osmdata::osmdata_sf()`](https://docs.ropensci.org/osmdata/reference/osmdata_sf.html)
  call: if Overpass returns HTTP 429, osmdata does its own
  60-second-per-retry backoff loop internally and we cannot safely
  interrupt that from R without risking a segfault on Windows. So on a
  heavily throttled server a single call may still exceed
  `total_timeout_s` by several minutes. For a hard ceiling, wrap the
  call in
  [`R.utils::withTimeout()`](https://henrikbengtsson.github.io/R.utils/reference/withTimeout.html)
  yourself, or supply a local boundary file via the `file` argument.
  Default 300 seconds (5 minutes); set to `Inf` to disable.

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

For very large lakes (\> ~500 km\\^2\\), the default 90-second Overpass
API timeout may be exceeded. Use `timeout = 300` or higher in those
cases. For lakes with very complex shorelines (e.g., Mälaren, Vättern,
Võrtsjärv), additionally pass `simplify_tolerance_m = 100` (or higher)
to coarsen the polygon and speed up downstream fetch calculations.

## Examples

``` r
if (FALSE) { # interactive()
csv_path <- system.file("extdata", "sample_sites.csv", package = "lakefetch")
sites <- load_sites(csv_path)
lake_data <- get_lake_boundary(sites)

# For very large lakes, increase the timeout
lake_data <- get_lake_boundary(sites, timeout = 300)

# For large/complex lakes, also coarsen the shoreline
lake_data <- get_lake_boundary(sites, timeout = 300,
                               simplify_tolerance_m = 100)
}
```
