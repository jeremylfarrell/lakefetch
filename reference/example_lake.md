# Example Circular Lake Polygon

A simple circular lake polygon for demonstration and testing purposes.
This synthetic lake has known geometry (1 km radius) which makes it
useful for validating fetch calculations.

## Usage

``` r
example_lake
```

## Format

An sf object with 1 row and 3 variables:

- osm_id:

  Identifier (synthetic)

- name:

  Lake name

- area_km2:

  Surface area in square kilometers (~3.14 km²)

- geometry:

  POLYGON geometry in UTM Zone 18N (EPSG:32618)

## Source

Synthetic data for demonstration and validation

## Details

The lake is centered at UTM coordinates (500000, 4800000) with a radius
of 1000 meters. For a site at the center, fetch should equal 1000 m in
all directions.

## Examples

``` r
# Load the dataset
data(example_lake)

# View structure
print(example_lake)
#> Simple feature collection with 1 feature and 3 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 499000 ymin: 4799000 xmax: 501000 ymax: 4801000
#> Projected CRS: WGS 84 / UTM zone 18N
#>        osm_id                  name area_km2                       geometry
#> 1 example_001 Example Circular Lake 3.141593 POLYGON ((501000 4800000, 5...

# Plot the lake
library(ggplot2)
ggplot(example_lake) + geom_sf()

```
