## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Test environments

* Local: Windows 11, R 4.4.1
* GitHub Actions: ubuntu-latest (release), windows-latest (release), macOS-latest (release)

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Notes for CRAN reviewers

This is the initial CRAN submission for lakefetch.

### Package purpose

lakefetch calculates fetch (open water distance) and wave exposure metrics for freshwater lake sampling sites. It fills a gap in the R ecosystem - existing fetch packages (fetchR, waver) focus on marine/coastal applications, while lakefetch is designed specifically for inland lakes with features like:

- Automatic lake boundary download from OpenStreetMap
- Multi-lake batch processing
- Optional NHD (National Hydrography Dataset) integration for US lakes
- Interactive Shiny app for visualization

### API usage

The package makes HTTP requests to:
- OpenStreetMap Overpass API (for lake boundary download)
- Open-Meteo API (for optional historical weather data)

All API calls are wrapped in tryCatch() with informative error messages, and users can alternatively provide local boundary files.

### Examples

Most examples use `\dontrun{}` because they require internet connectivity for API calls or launch interactive Shiny apps. The package includes built-in example datasets (`adirondack_sites`, `wisconsin_lakes`, `example_lake`) for offline testing.
