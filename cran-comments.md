## R CMD check results

0 errors | 0 warnings | 1 note

NOTE: New submission. Possibly misspelled words (Hydrography, NHD,
OpenStreetMap, hydrological) are correct domain-specific terms listed
in inst/WORDLIST.

## Test environments

* Local: Windows 11 x64 (build 26200), R 4.4.1
* Win-builder: Windows Server 2022, R 4.5.2 Patched (2026-02-13)
* Win-builder: Windows Server 2022, R-devel
* R-hub: Ubuntu Linux (R-devel)
* R-hub: Windows (R-devel)
* R-hub: macOS ARM64 (R-devel)
* Mac Builder: macOS 26.2, Apple M1, R-devel

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

### URL note

The USGS National Hydrography Dataset URL (https://www.usgs.gov/national-hydrography)
in DESCRIPTION returns HTTP 403 to automated checkers but is accessible in
browsers. This is a known USGS server configuration.

### Examples

Most examples use `\dontrun{}` because they require internet connectivity
for API calls or launch interactive Shiny apps. The package includes
built-in example datasets (`adirondack_sites`, `wisconsin_lakes`,
`example_lake`) for offline testing.
