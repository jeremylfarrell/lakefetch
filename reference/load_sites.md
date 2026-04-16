# Load Sites from CSV or Data Frame

Load and validate site data for fetch calculation. Automatically detects
coordinate columns (latitude/longitude) and cleans the data.

## Usage

``` r
load_sites(x, lat_col = NULL, lon_col = NULL, site_col = NULL, lake_col = NULL)
```

## Arguments

- x:

  Either a file path to a CSV file or a data.frame with site data.

- lat_col:

  Optional character string specifying the name of the latitude column.
  If NULL (default), auto-detects columns starting with "lat".

- lon_col:

  Optional character string specifying the name of the longitude column.
  If NULL (default), auto-detects columns starting with "lon".

- site_col:

  Optional character string specifying the name of the site identifier
  column. If NULL (default), auto-detects a column named "site".

- lake_col:

  Optional character string specifying the name of the lake name column.
  If NULL (default), auto-detects common lake name patterns.

## Value

A data.frame with columns Site, latitude, longitude, and any additional
columns from the input. Includes attributes "location_name" and
"location_column" if a location was detected.

## Details

The function:

- Detects latitude/longitude columns (names starting with "lat"/"lon")

- Cleans coordinate values (removes non-numeric characters)

- Creates Site column if not present

- Removes rows with invalid or missing coordinates

- Detects location name from data columns or filename

Column names can be specified explicitly using the `lat_col`, `lon_col`,
`site_col`, and `lake_col` arguments. This is useful when your data uses
non-standard column names that the auto-detection cannot find.

## Examples

``` r
# Load from data frame
df <- data.frame(
  Site = c("A", "B", "C"),
  latitude = c(43.42, 43.43, 43.41),
  longitude = c(-73.69, -73.68, -73.70)
)
sites <- load_sites(df)
#>   Loaded 3 rows with columns: Site, latitude, longitude
#>   Using columns: Latitude = latitude, Longitude = longitude
#>   Final valid samples: 3

# Load with custom column names
df2 <- data.frame(
  sample_id = c("A", "B"),
  y_coord = c(43.42, 43.43),
  x_coord = c(-73.69, -73.68),
  reservoir = c("Lake One", "Lake One")
)
sites <- load_sites(df2, lat_col = "y_coord", lon_col = "x_coord",
                    site_col = "sample_id", lake_col = "reservoir")
#>   Loaded 2 rows with columns: sample_id, y_coord, x_coord, reservoir
#>   Using columns: Latitude = y_coord, Longitude = x_coord
#>   Using specified lake name column: reservoir
#>   Final valid samples: 2
#>   Detected location from column 'reservoir': Lake One
```
