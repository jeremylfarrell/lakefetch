# Adirondack Lake Sampling Sites

A dataset containing example lake sampling sites from the Adirondack
region of New York State. These synthetic but realistic coordinates
demonstrate typical multi-lake sampling scenarios.

## Usage

``` r
adirondack_sites
```

## Format

A data frame with 12 rows and 5 variables:

- Site:

  Unique site identifier

- lake.name:

  Name of the lake

- latitude:

  Latitude in decimal degrees (WGS84)

- longitude:

  Longitude in decimal degrees (WGS84)

- datetime:

  Date and time of sampling (POSIXct)

## Source

Synthetic data for demonstration purposes

## Details

The dataset includes sites from four Adirondack lakes:

- Blue Mountain Lake (3 sites)

- Raquette Lake (4 sites)

- Long Lake (2 sites)

- Tupper Lake (3 sites)

## Examples

``` r
# Load the dataset
data(adirondack_sites)

# View structure
str(adirondack_sites)
#> 'data.frame':    12 obs. of  5 variables:
#>  $ Site     : chr  "Blue_Mountain_1" "Blue_Mountain_2" "Blue_Mountain_3" "Raquette_1" ...
#>  $ lake.name: chr  "Blue Mountain Lake" "Blue Mountain Lake" "Blue Mountain Lake" "Raquette Lake" ...
#>  $ latitude : num  43.9 43.9 43.9 43.8 43.8 ...
#>  $ longitude: num  -74.4 -74.4 -74.4 -74.7 -74.7 ...
#>  $ datetime : POSIXct, format: "2024-07-15 09:30:00" "2024-07-15 10:15:00" ...

if (FALSE) { # interactive()
# Use with lakefetch (requires internet connection)
sites <- load_sites(adirondack_sites)
lake_data <- get_lake_boundary(sites)
results <- fetch_calculate(sites, lake_data)
}
```
