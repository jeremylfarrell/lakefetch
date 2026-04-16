# Launch Interactive Fetch App with File Upload

Launch a standalone Shiny app where users can upload a CSV file with GPS
coordinates, and the app will automatically download lake boundaries,
calculate fetch, and display interactive results.

## Usage

``` r
fetch_app_upload(title = "Lake Fetch Calculator")
```

## Arguments

- title:

  Optional app title (default: "Lake Fetch Calculator")

## Value

Launches a Shiny app (does not return)

## Details

Requires the shiny, leaflet, and base64enc packages (suggested
dependencies).

The app workflow:

1.  Upload a CSV file with latitude/longitude columns

2.  App downloads lake boundaries from OpenStreetMap

3.  Calculates fetch for all uploaded points

4.  Displays interactive map with results

5.  Click anywhere on a lake to analyze additional points

6.  Download results as CSV or GeoPackage

CSV file requirements:

- Must have columns starting with "lat" and "lon" (case-insensitive)

- Optional "Site" column for point names

- Additional columns are preserved in output

## Examples

``` r
if (interactive()) {
  # Launch the upload app
  fetch_app_upload()
}
```
