# Calculate Fetch for Lake Sampling Sites

Main entry point for fetch calculation. Takes sites and lake boundaries,
calculates directional fetch using ray-casting, and returns results with
exposure metrics.

## Usage

``` r
fetch_calculate(
  sites,
  lake,
  depth_m = NULL,
  fetch_method = NULL,
  add_context = TRUE,
  find_max_fetch = FALSE
)
```

## Arguments

- sites:

  Data frame or sf object with site locations

- lake:

  Lake boundary data from
  [`get_lake_boundary`](https://jeremylfarrell.github.io/lakefetch/reference/get_lake_boundary.md)

- depth_m:

  Water depth in meters for orbital velocity calculation. Can be a
  single value (applied to all sites), a vector (one per site), or NULL
  to use depth from sites data or default from options.

- fetch_method:

  Method for calculating effective fetch. Options:

  "top3"

  :   Mean of the 3 highest directional fetch values (default)

  "max"

  :   Maximum directional fetch value

  "cosine"

  :   SPM/CERC cosine-weighted average. Uses 9 radials centered on the
      direction of maximum fetch at 6-degree intervals, weighted by
      cosine of angle from center. Based on Shore Protection Manual
      (1984).

  If NULL, uses the value from
  [`lakefetch_options`](https://jeremylfarrell.github.io/lakefetch/reference/lakefetch_options.md).

- add_context:

  Logical; add NHD context if available (default TRUE)

- find_max_fetch:

  Logical; if TRUE, finds the location in each lake with the maximum
  possible fetch using a longest-internal-chord algorithm. The result is
  returned as a `$max_fetch` element in the output list. Default FALSE.

## Value

A list with elements:

- results:

  sf object with fetch results for each site

- lakes:

  sf object with lake polygons used

- angles:

  Vector of angles used for fetch calculation

- max_fetch:

  (only if `find_max_fetch = TRUE`) sf object with one row per lake
  containing the maximum fetch location, chord length (meters), and
  chord bearing (degrees)

## Details

For each site, the function:

1.  Assigns the site to its containing lake polygon

2.  Buffers the site inward from shore (GPS accuracy adjustment)

3.  Casts rays in all directions at specified angle resolution

4.  Measures distance to shore in each direction

5.  Calculates summary metrics (mean, max, effective fetch)

6.  Calculates orbital velocity using depth

7.  Derives exposure category (Sheltered/Moderate/Exposed)

Exposure thresholds can be configured via
[`lakefetch_options`](https://jeremylfarrell.github.io/lakefetch/reference/lakefetch_options.md).

## References

Shore Protection Manual (1984). U.S. Army Corps of Engineers, Coastal
Engineering Research Center. 4th Edition.

## Examples

``` r
if (FALSE) { # interactive()
csv_path <- system.file("extdata", "sample_sites.csv", package = "lakefetch")
sites <- load_sites(csv_path)
lake <- get_lake_boundary(sites)
results <- fetch_calculate(sites, lake)

# With explicit depth
results <- fetch_calculate(sites, lake, depth_m = 5)

# Using cosine-weighted effective fetch (SPM method)
results <- fetch_calculate(sites, lake, fetch_method = "cosine")

# Access results
results$results  # sf with all fetch data
results$lakes    # lake polygons

# Find the location with maximum fetch in each lake
results <- fetch_calculate(sites, lake, find_max_fetch = TRUE)
results$max_fetch  # sf with max fetch location per lake
}
```
