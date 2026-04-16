# Get or Set lakefetch Package Options

Get or set options that control the behavior of lakefetch functions.

## Usage

``` r
lakefetch_options(...)
```

## Arguments

- ...:

  Named arguments to set options. If empty, returns all current options.

## Value

If no arguments, returns a list of all current options. If arguments
provided, sets those options and returns invisible NULL.

## Details

Available options:

- buffer_distance_m:

  GPS accuracy buffer in meters (default: 10)

- angle_resolution_deg:

  Direction resolution in degrees (default: 5)

- max_fetch_m:

  Maximum fetch distance in meters (default: 50000)

- validation_buffer_m:

  Shore detection validation buffer (default: 10)

- default_wind_speed_ms:

  Default wind speed in m/s (default: 10)

- default_depth_m:

  Default water depth in meters (default: 10)

- gps_tolerance_m:

  Buffer for matching sites to lakes (default: 100)

- fetch_method:

  Effective fetch calculation method: "top3" (mean of 3 highest
  directional fetches, default), "max" (maximum directional fetch), or
  "cosine" (SPM/CERC cosine-weighted average across 9 radials at
  6-degree intervals; see Shore Protection Manual, 1984)

- exposure_sheltered_m:

  Fetch threshold below which sites are classified as "Sheltered"
  (default: 2500 m). This is a practical default; no universal standard
  exists in the literature. Adjust based on your study system.

- exposure_exposed_m:

  Fetch threshold above which sites are classified as "Exposed"
  (default: 5000 m). Sites between thresholds are "Moderate". See Mason
  et al. (2018) for Great Lakes exposure mapping methodology.

- exposure_relative_sheltered:

  Proportion of lake maximum fetch below which sites are classified as
  "Sheltered" in the relative exposure system (default: 0.25). Sites are
  classified relative to the lake's longest internal chord (maximum
  possible fetch).

- exposure_relative_exposed:

  Proportion of lake maximum fetch above which sites are classified as
  "Exposed" in the relative exposure system (default: 0.50). Sites
  between thresholds are "Moderate".

- use_parallel:

  Use parallel processing for multi-lake (default: TRUE)

- use_nhd:

  Use NHD integration if available (default: TRUE)

## References

Shore Protection Manual (1984). U.S. Army Corps of Engineers, Coastal
Engineering Research Center. 4th Edition.

Mason, L. A., Riseng, C. M., Laber, A. L., & Rutherford, E. S. (2018).
Effective fetch and relative exposure index maps for the Laurentian
Great Lakes. Scientific Data, 5, 180295.

## Examples

``` r
# Get all options
lakefetch_options()
#> $buffer_distance_m
#> [1] 10
#> 
#> $angle_resolution_deg
#> [1] 5
#> 
#> $max_fetch_m
#> [1] 50000
#> 
#> $validation_buffer_m
#> [1] 10
#> 
#> $default_wind_speed_ms
#> [1] 10
#> 
#> $default_depth_m
#> [1] 10
#> 
#> $gps_tolerance_m
#> [1] 100
#> 
#> $fetch_method
#> [1] "top3"
#> 
#> $exposure_sheltered_m
#> [1] 2500
#> 
#> $exposure_exposed_m
#> [1] 5000
#> 
#> $exposure_relative_sheltered
#> [1] 0.25
#> 
#> $exposure_relative_exposed
#> [1] 0.5
#> 
#> $use_parallel
#> [1] TRUE
#> 
#> $use_nhd
#> [1] TRUE
#> 

# Get specific option
lakefetch_options()$buffer_distance_m
#> [1] 10

# Set options
lakefetch_options(buffer_distance_m = 20, angle_resolution_deg = 10)
```
