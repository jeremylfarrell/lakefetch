## Resubmission (v0.1.2)

Addressing feedback from Konstanze Lauseker (second review):

* Removed all `if(FALSE)` usage in `@examples` across all R files
* Wrapped all lengthy examples involving network calls (OpenStreetMap, NHD,
  weather APIs) in `\donttest{}`
* Where possible, replaced with truly runnable examples (e.g., `get_lake_depth()`
  using the bundled `example_lake` dataset, `load_sites()` using `system.file()`)
* Added `inst/extdata/sample_sites.csv` as a small example data file; all
  examples now reference it via
  `system.file("extdata", "sample_sites.csv", package = "lakefetch")`

## R CMD check results

0 errors | 0 warnings | 1 note

NOTE: "Hydrography" and "hydrological" flagged as possibly misspelled —
both are correct technical terms referring to the National Hydrography
Dataset (NHD) from the USGS.

## Test environments

* Local: Windows 11 x64 (build 26200), R 4.4.1
* Win-builder: Windows Server 2022, R-devel (2026-03-04)

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Notes for CRAN reviewers

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
