## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* Local: Windows 11 x64 (build 26200), R 4.4.1
* R-hub: Ubuntu Linux (R-devel)
* R-hub: macOS ARM64 (R-devel)
* R-hub: Windows (R-devel)

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Notes for CRAN reviewers

This is the first submission of lakefetch to CRAN.

### Package purpose

lakefetch calculates fetch (open water distance) and wave exposure metrics
for freshwater lake sampling sites. It addresses a gap in the R ecosystem —
existing fetch packages (fetchR, waver) focus on marine/coastal applications,
while lakefetch is designed specifically for inland lakes with features like:

- Automatic lake boundary download from OpenStreetMap
- Multi-lake batch processing
- Optional NHD (National Hydrography Dataset) integration for US lakes
- Interactive Shiny app for visualization
- Configurable effective fetch methods (top-3, maximum, SPM cosine-weighted)

### API usage

The package makes HTTP requests to:
- OpenStreetMap Overpass API (for lake boundary download)
- Open-Meteo API (for optional historical weather data)

All API calls are wrapped in tryCatch() with informative error messages,
and users can alternatively provide local boundary files.

### Examples

Most examples use `\dontrun{}` because they require internet connectivity
for API calls or launch interactive Shiny apps. The package includes
built-in example datasets (`adirondack_sites`, `wisconsin_lakes`,
`example_lake`) for offline testing.
