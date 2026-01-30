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
