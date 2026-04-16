# Get Lake Depth Estimates

Retrieves or estimates lake depth for wave calculations. Uses
user-provided depth if available, otherwise estimates from lake surface
area using empirical relationships.

## Usage

``` r
get_lake_depth(
  lake_polygon,
  site_coords = NULL,
  user_depth = NULL,
  method = "auto"
)
```

## Arguments

- lake_polygon:

  sf polygon of the lake

- site_coords:

  Coordinates of the sample site (optional, for future bathymetry grid
  support)

- user_depth:

  User-provided depth in meters (highest priority)

- method:

  Method for depth estimation: "auto" or "empirical"

## Value

A list with elements:

- depth_mean:

  Estimated mean depth in meters

- depth_max:

  Estimated maximum depth in meters (if available)

- source:

  Source of the estimate ("user" or "empirical")

- confidence:

  Confidence level ("high", "medium", "low")

## Details

Depth estimation methods:

1.  User-provided: Direct input, highest confidence

2.  Empirical: Estimated from lake surface area using published
    relationships

The empirical method uses the relationship from Cael et al. (2017):
mean_depth ~ 10.3 \* area_km2^0.25

## References

Messager, M.L., Lehner, B., Grill, G., Nedeva, I., Schmitt, O. (2016):
Estimating the volume and age of water stored in global lakes using a
geo-statistical approach. Nature Communications, 7: 13603.

Cael, B.B., Heathcote, A.J., Seekell, D.A. (2017): The volume and mean
depth of Earth's lakes. Geophysical Research Letters, 44: 209-218.

## Examples

``` r
data(example_lake)

# With user-provided depth
depth <- get_lake_depth(example_lake, user_depth = 8.5)

# Estimate from lake area
depth <- get_lake_depth(example_lake)
#>   Lake depth estimated from area (3.14 km2): mean ~ 13.7m, max ~ 34.3m
```
