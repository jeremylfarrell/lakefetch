# lakefetch 0.1.1

## New features

* **Maximum fetch location**: `fetch_calculate()` gains a `find_max_fetch` parameter that identifies the location in each lake with the highest possible fetch. Uses an efficient longest-internal-chord algorithm (sub-second per lake) rather than brute-force grid search. Returns the point location, chord length, bearing, and optionally full directional fetch profile.
* **Relative exposure classification**: Sites are now classified by both absolute fetch thresholds (`exposure_category`) and lake-relative proportional thresholds (`exposure_relative`). The proportional method classifies sites based on the ratio of effective fetch to the lake's maximum possible fetch (longest internal chord), providing lake-size-aware exposure context. Default thresholds: Sheltered < 25%, Exposed > 50%. New output columns: `fetch_proportion`, `lake_max_chord_m`, `exposure_relative`.

## Improvements

* **Optimized OSM downloads for spread-out sites**: `download_lake_osm()` now handles geographically spread datasets (e.g., GLEON's 429 global sites). When site spread exceeds 0.5 degrees, sites are grouped into spatial clusters (~0.1 degree grid) and each cluster gets a small bounding box query. This replaces the old single-bbox approach that would cover the entire globe and timeout. Tested with 50 globally-spread GLEON sites (48/50 matched, 26 min download).
* **Robust Overpass API handling**: Each cluster query retries up to 3 times across 3 different Overpass servers, with 1-second rate limiting between queries. Failed clusters are reported at the end so users know which lake boundaries may be missing.
* **Minimum area filter**: Water bodies smaller than 0.0001 km² (100 m²) are automatically filtered out after download, removing garden ponds, fountains, and other tiny features.
* **Download progress bar**: OSM cluster downloads now display a progress bar and report elapsed time when complete.
* **Custom column names**: `load_sites()` now accepts `lat_col`, `lon_col`, `site_col`, and `lake_col` arguments to explicitly specify column names when auto-detection doesn't match your data format.
* **Progress bars**: Long-running fetch calculations now display progress bars in interactive sessions, so users can see that computation is proceeding. Progress is shown for site buffering, directional fetch calculation, and multi-lake sequential processing.
* **Shiny app performance**: `fetch_app()` and `fetch_app_upload()` now use a hybrid approach for large datasets. For small datasets (<=50 sites), rose diagrams are pre-rendered in popups as before. For large datasets (>50 sites), rose diagrams and rays are generated on demand when a marker is clicked, preventing the app from freezing or crashing at startup.
* **Marker clustering**: Shiny apps automatically cluster markers when there are more than 30 sites or when sites span a wide geographic area (>5 degrees), preventing browser slowdowns and overlapping markers.

## Bug fixes

* **Invalid (0,0) coordinates**: `load_sites()` now detects and removes rows where both latitude and longitude are 0, which typically indicates missing data rather than a real location.
* **Geometry processing crash**: Fixed `st_is_valid()` crash when processing corrupted or empty geometries from large OSM downloads. Empty geometries are now skipped and invalid ones are repaired automatically.
* **Buffer match count**: Fixed negative match count display in `assign_sites_to_lakes()` when `st_join` produced duplicate rows.

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
* 79 unit tests via testthat (now 112)

## Documentation

* Getting started vignette with complete workflow example
* Validation vignette documenting test methodology and results
* Example datasets: `adirondack_sites`, `wisconsin_lakes`, `example_lake`
