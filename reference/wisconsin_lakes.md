# Wisconsin Lake Sampling Sites

A dataset containing example sampling sites from well-known Wisconsin
lakes. These coordinates are useful for testing with real lake
boundaries from OpenStreetMap.

## Usage

``` r
wisconsin_lakes
```

## Format

A data frame with 8 rows and 4 variables:

- Site:

  Unique site identifier

- lake.name:

  Name of the lake

- latitude:

  Latitude in decimal degrees (WGS84)

- longitude:

  Longitude in decimal degrees (WGS84)

## Source

Synthetic data based on real lake locations

## Details

The dataset includes sites from three Wisconsin lakes:

- Lake Mendota (3 sites) - Madison's largest lake, well-studied

- Lake Monona (2 sites) - Connected to Mendota via Yahara River

- Geneva Lake (3 sites) - Popular recreational lake in SE Wisconsin

## Examples

``` r
# Load the dataset
data(wisconsin_lakes)

# View the data
head(wisconsin_lakes)
#>           Site    lake.name latitude longitude
#> 1    Mendota_N Lake Mendota  43.1125 -89.42340
#> 2    Mendota_S Lake Mendota  43.0756 -89.40120
#> 3 Mendota_Deep Lake Mendota  43.0995 -89.40450
#> 4     Monona_1  Lake Monona  43.0634 -89.36120
#> 5     Monona_2  Lake Monona  43.0589 -89.37890
#> 6     Geneva_E  Geneva Lake  42.5849 -88.44205

if (FALSE) { # interactive()
# Use with lakefetch (requires internet connection)
sites <- load_sites(wisconsin_lakes)
lake_data <- get_lake_boundary(sites)
results <- fetch_calculate(sites, lake_data)
}
```
