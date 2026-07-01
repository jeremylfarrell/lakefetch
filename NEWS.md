# lakefetch 0.1.7

## Bug fixes

* **`plot_fetch_rose()` overplotting: real fix**: The v0.1.6 fix (setting
  `par(new = FALSE)`) was insufficient. The underlying cause is that
  ggplot2 (used by `plot_fetch_map()` and `plot_fetch_bars()`, and
  internally by `fetch_app()`) leaves an active grid viewport on the
  device. Base R `plot.new()` cannot clear a grid viewport, so
  subsequent rose diagrams were drawn inside the ggplot's coordinate
  system, on top of the previous content. `plot_fetch_rose()` now calls
  `grid::grid.newpage()` at the start to reset the device state, then
  proceeds with base R plotting into a clean region.
* **`wisconsin_lakes` Geneva Lake coordinates**: All three Geneva Lake
  sites in the built-in dataset (`Geneva_E`, `Geneva_W`, `Geneva_Center`)
  had coordinates on land near the shore rather than in the water. The
  OSM polygon for Lake Geneva extends further south (down to ~42.5455 N)
  and further west (out to ~-88.5727 W) than the previous coordinates
  assumed. All three sites now sit inside the polygon, so
  `assign_sites_to_lakes()` no longer skips them.

# lakefetch 0.1.6

Small follow-up fixes after Khondula's second review pass on v0.1.5
(ropensci/software-review#762).

## Bug fixes

* **`plot_fetch_rose()` overplotting after Shiny apps**: Running
  `plot_fetch_rose()` immediately after `fetch_app()` could produce a
  rose that overlaid the previous plot or lacked a background, because
  Shiny left the graphics device with `par("new") = TRUE`. The function
  now explicitly resets `par(new = FALSE)` before drawing.

## Improvements

* **Rose diagram color changed from blue to purple**: The rose polygon
  is now filled in semi-transparent purple with a `#7B3E9E` border,
  matching the common wind-rose convention and avoiding visual confusion
  with the blue lake polygons rendered underneath in the Shiny map.

## Documentation

* **`wisconsin_lakes` description in README**: Corrected from
  "Lake boundary polygons from Wisconsin" (misleading) to
  "Sampling sites on three Wisconsin lakes (Mendota, Monona, Geneva)".

# lakefetch 0.1.5

Changes in response to the second rOpenSci peer review by Khondula
(ropensci/software-review#762).

## Bug fixes

* **Invalid UTM EPSG when sf input is in a projected CRS**: When a user
  passed an sf object in a non-WGS84 CRS (e.g., the bundled `example_lake`
  in UTM zone 18N) to `get_lake_boundary()`, the UTM zone detection was
  performed on the raw projected coordinates and produced nonsensical EPSG
  codes such as `EPSG:32683364`. The UTM zone is now computed from the
  centroid of the WGS84-transformed sites instead of the raw input.
* **Sites unassigned to lakes now emit a warning, not just a message**:
  When `assign_sites_to_lakes()` cannot match one or more sites, it now
  emits a `warning()` in addition to the diagnostic messages. The previous
  message-only behavior could be missed in console output, leading users
  to believe the workflow had fully succeeded when in fact some sites had
  NA fetch values.
* **Getting-started vignette runnable end-to-end**: The Quick Start chunks
  were reorganized so the `sites` object is not overwritten with mismatched
  coordinates, and the network-dependent chunks are explicitly marked
  `eval = FALSE` with a note explaining how to run them interactively.

## Improvements

* **Flexible column-name detection for `get_lake_boundary()`**: The function
  now auto-detects latitude / longitude columns on data.frame input the
  same way `load_sites()` does (accepts "lat" / "latitude" / "y" and
  "lon" / "long" / "longitude" / "lng" / "x", case-insensitive). Previously
  it required the exact column names produced by `load_sites()`.
* **Faster downloads for spread-out named lakes**: When all sites have a
  known lake name and the site spread exceeds 0.5 degrees,
  `get_lake_boundary()` now issues a single name-filtered Overpass query
  covering the union bounding box instead of looping through per-cluster
  broad queries. For datasets like `wisconsin_lakes` (3 lakes spread
  across ~1 degree), this typically reduces download time from many
  minutes to under a minute.
* **`add_weather_context()` docs expanded**: The function documentation
  now describes what the function does (queries Open-Meteo, combines with
  fetch, integrates wave energy), and the `windows_hours` argument is
  documented explicitly (per-window column names, intended use as
  look-back windows ending at each sample's `datetime`).
* **`assign_sites_to_lakes()` `tolerance_m` documented**: The default
  (50 m via `lakefetch_options()`) is now explained, with guidance to
  increase it (200-500 m) for sites with appreciable GPS error or
  near-shore coordinates. The example uses `tolerance_m = 200` so users
  see a working invocation.

## Shiny app improvements

* **Colorblind-friendly palette**: Exposure markers, rays, and legend text
  now use the Okabe-Ito palette (blue / orange / vermillion) instead of
  green / gold / red. The previous palette was hard to distinguish for
  users with red-green color vision deficiency, and the bright yellow
  legend text on the default sidebar background was hard to read.
* **Layer switcher (OSM / Imagery / USGS NHD)**: The map now offers Esri
  World Imagery and OpenStreetMap as switchable base layers, with the
  USGS National Hydrography Dataset hosted overlay available as a toggle.
* **Custom point analysis no longer clobbered by marker clicks**: In
  `fetch_app_upload()`, clicking a pre-loaded site marker previously wrote
  the marker's rose plot into the same sidebar slot as the custom-point
  analysis result, hiding it. The pre-loaded site details now have their
  own sidebar slot and the custom point analysis stays visible.

# lakefetch 0.1.4

Changes in response to the rOpenSci peer review by Jorrit Mesman
(ropensci/software-review#762).

## New features

* **Configurable Overpass timeout**: `get_lake_boundary()` gains a `timeout`
  argument (default 90 seconds). Pass `timeout = 300` (or higher) for very
  large lakes such as Mälaren, Vättern, Võrtsjärv, or the Great Lakes where
  the default Overpass query may time out.
* **Polygon simplification for large/complex lakes**: `get_lake_boundary()`
  gains a `simplify_tolerance_m` argument that applies
  `sf::st_simplify(dTolerance = ...)` in meters to the returned lake
  polygons. Useful for lakes with very complex shorelines where an exact
  coastline is not needed and a coarser polygon greatly speeds up fetch
  ray-casting. Works for both OSM-downloaded and user-supplied boundary
  files.

## Bug fixes

* **Sample sites placed inside lakes**: Both `inst/extdata/sample_sites.csv`
  and the `adirondack_sites` example dataset previously contained points
  located just outside the corresponding lake polygons, which produced `NA`
  exposure values and prevented some output features from being displayed.
  All example site coordinates have been verified to fall inside their lake
  boundaries.
* **Silent fallback fetch values for points outside lakes**: When a site
  was outside the lake polygon, `get_highres_fetch()` previously returned
  the maximum search distance (50 km) for all directions, producing
  silently incorrect fetch results. It now returns `NA` for all directions
  in this case, matching what users expect.
* **Name-targeted OSM queries for huge lakes**: When a single site sat inside
  a very large lake (e.g., Mälaren, Vättern), the narrow query bounding box
  fell entirely inside the lake polygon and Overpass returned no features
  (since Overpass returns features whose nodes fall inside the bbox, and
  the boundary nodes of a huge polygon are far outside a small interior
  bbox). When a lake name is provided, `get_lake_boundary()` now expands
  the bounding box for the name-filtered query to at least 1.5 degrees in
  each dimension, which is safe because the name filter is highly
  selective. This is what allows the Mälaren / Vättern / Võrtsjärv test
  cases from the rOpenSci review to retrieve their boundaries from a
  single coordinate.
* **NHD integration `!anyNA(x)` error on unmatched sites**: When all sites
  failed lake assignment (e.g., coordinates >500 m from any same-named
  OSM polygon), the resulting lake set could be empty or contain only
  empty geometries, and `add_lake_context()` would pass an NA-containing
  bounding box to `nhdplusTools::get_waterbodies()`, which errored with
  "!anyNA(x) is not TRUE". `add_lake_context()` now short-circuits early
  in this case and returns the fetch results with NA NHD columns.
* **`fetch_app()` crash with user-supplied shapefiles**: Fixed
  "missing value where TRUE/FALSE needed" error when launching the Shiny
  app on results derived from a user-supplied lake boundary. The UTM EPSG
  code is now stored on the fetch result so the app no longer has to
  re-derive it (which could return `NA` for non-standard CRS strings), and
  the file-loading CRS check no longer depends on the `$input` string
  representation.
* **`plot_fetch_map()` clipped large lakes**: The map bounding box was
  computed from sites only, so when sites were clustered in one part of a
  large lake the rest of the lake was cropped off. The bounding box now
  unions sites and lake polygons.
* **Rose diagram integer index bug**: Fixed an integer-index issue in
  `plot_fetch_rose()` that affected rose diagrams for custom-clicked
  points in the Shiny app.

## Documentation

* **Quick Start examples use installed sample data**: README and
  `vignettes/getting-started.Rmd` Quick Start blocks no longer reference
  `my_lake_sites.csv` / `my_sites.csv` (which did not exist). They now use
  `system.file("extdata", "sample_sites.csv", package = "lakefetch")`.
* **`depth_m` argument clarified**: Documentation for `fetch_calculate()`
  now explicitly states that `depth_m` should be the mean water depth
  (preferred over maximum depth for representing wave attenuation across
  the water column).
* **Angle-resolution discoverability**: README now points to
  `lakefetch_options(angle_resolution_deg = ...)` as the way to change the
  default 5-degree fetch ray resolution.

# lakefetch 0.1.3

## Bug fixes

* Fixed commented-out code in `@examples` for `add_lake_depth()` and
  `get_lake_boundary()`. Examples now use runnable code throughout.

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
* 446 unit tests via testthat with 75% code coverage

## Documentation

* Getting started vignette with complete workflow example
* Validation vignette documenting test methodology and results
* Example datasets: `adirondack_sites`, `wisconsin_lakes`, `example_lake`
