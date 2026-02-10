# lakefetch 0.1.1

## Improvements

* **Optimized OSM downloads for spread-out sites**: `download_lake_osm()` now uses cluster-based querying when sites span more than 0.5 degrees. Instead of a single bounding box covering all sites (which could span the entire globe for datasets like GLEON), sites are grouped into spatial clusters and queried individually. This prevents Overpass API timeouts and avoids downloading irrelevant water bodies.
* **Name-filtered Overpass queries**: When a `lake.name` column is available, the function first tries a name-filtered query to download only the relevant lake polygon, skipping the broad `natural=water` query when successful. This dramatically reduces data for known-name lakes.
* **Minimum area filter**: Water bodies smaller than 0.0001 km² (100 m²) are automatically filtered out after download, removing garden ponds, fountains, and other tiny features.

* **Custom column names**: `load_sites()` now accepts `lat_col`, `lon_col`, `site_col`, and `lake_col` arguments to explicitly specify column names when auto-detection doesn't match your data format.
* **Progress bars**: Long-running fetch calculations now display progress bars in interactive sessions, so users can see that computation is proceeding. Progress is shown for site buffering, directional fetch calculation, and multi-lake sequential processing.
* **Shiny app performance**: `fetch_app()` and `fetch_app_upload()` now use a hybrid approach for large datasets. For small datasets (<=50 sites), rose diagrams are pre-rendered in popups as before. For large datasets (>50 sites), rose diagrams and rays are generated on demand when a marker is clicked, preventing the app from freezing or crashing at startup.
* **Marker clustering**: Shiny apps automatically cluster markers when there are more than 30 sites or when sites span a wide geographic area (>5 degrees), preventing browser slowdowns and overlapping markers.

## Bug fixes

* **Invalid (0,0) coordinates**: `load_sites()` now detects and removes rows where both latitude and longitude are 0, which typically indicates missing data rather than a real location.

# lakefetch 0.1.0

Initial CRAN release.
## Features

* **Fetch calculation**: Ray-casting algorithm to measure directional fetch (open water distance) from sampling points to shoreline
* **Automatic boundary download**: Downloads lake boundaries from OpenStreetMap with multi-server fallback for reliability
* **Multi-lake support**: Process multiple lakes in a single analysis with automatic site-to-lake assignment
* **Name-based matching**: Falls back to lake name matching when spatial intersection fails
* **NHD integration**: Optional integration with National Hydrography Dataset for US lakes (outlets, inlets, watershed area, connectivity classification)
* **Weather integration**: Historical weather data from Open-Meteo API for wave energy calculations
* **Depth estimation**: Empirical depth estimation from lake surface area
* **Visualization**: Static plots (maps, bar charts, rose diagrams) and interactive Shiny app
* **Exposure classification**: Automatic classification into Sheltered/Moderate/Exposed categories

## Validation

* Analytical validation against synthetic lakes with known geometry (0% error)
* Literature validation against lakes with published morphometry (100% pass rate)
* Edge case validation for islands, complex shorelines, and multiple islands (100% pass rate)
* 79 unit tests via testthat

## Documentation

* Getting started vignette with complete workflow example
* Validation vignette documenting test methodology and results
* Example datasets: `adirondack_sites`, `wisconsin_lakes`, `example_lake`
