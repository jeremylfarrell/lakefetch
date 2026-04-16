# Create Ray Geometries for Map Visualization

Create line geometries representing fetch rays from each site. Useful
for detailed visualization of the ray-casting results.

## Usage

``` r
create_ray_geometries(fetch_data)
```

## Arguments

- fetch_data:

  Results from
  [`fetch_calculate`](https://jeremylfarrell.github.io/lakefetch/reference/fetch_calculate.md)

## Value

An sf object with ray line geometries

## Examples

``` r
if (FALSE) { # interactive()
csv_path <- system.file("extdata", "sample_sites.csv", package = "lakefetch")
sites <- load_sites(csv_path)
lake <- get_lake_boundary(sites)
results <- fetch_calculate(sites, lake)
rays <- create_ray_geometries(results)

# Plot rays for a specific site
site_name <- results$results$Site[1]
site_rays <- rays[rays$Site == site_name, ]
ggplot2::ggplot() + ggplot2::geom_sf(data = site_rays, ggplot2::aes(color = Distance))
}
```
