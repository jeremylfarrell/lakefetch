## R CMD check results

0 errors | 0 warnings | 2 notes

NOTE 1: New submission with "misspelled" words (Hydrography, NHD,
OpenStreetMap, hydrological) — these are correct technical terms.

NOTE 2: `.github` directory containing GitHub Actions workflows for R-hub
automated checking. This is intentional and follows current best practices
for R package CI/CD.

## Test environments

* Local: Windows 11 x64 (build 26200), R 4.4.1
* Win-builder: Windows Server 2022, R 4.5.2 Patched (2026-02-13)
* Win-builder: Windows Server 2022, R-devel
* R-hub: Ubuntu Linux (R-devel)
* R-hub: Windows (R-devel)
* R-hub: macOS ARM64 (R-devel) — infrastructure failure during dependency
  installation (httpuv compilation failed due to missing m4/perl on runner;
  not a package issue). Package code was not reached.

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
