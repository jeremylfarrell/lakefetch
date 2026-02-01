# lakefetch - Development Status

Last updated: 2026-02-01

## Package Overview

**lakefetch** calculates fetch (open water distance) and wave exposure metrics for freshwater lake sampling sites. It downloads lake boundaries from OpenStreetMap, calculates directional fetch using ray-casting, and provides wave exposure classifications.

**GitHub:** https://github.com/jeremylfarrell/lakefetch

## Current Status: READY FOR CRAN SUBMISSION

All checks pass. Awaiting colleague feedback before submitting.

### Test Results

| Environment | Status |
|-------------|--------|
| Local Windows 11, R 4.4.1 | ✅ 0 errors, 0 warnings, 0 notes |
| R-hub Ubuntu Linux (R-devel) | ✅ Pass |
| R-hub macOS ARM64 (R-devel) | ✅ Pass |
| R-hub Windows (R-devel) | ✅ Pass |

## Installation (for colleagues testing)

```r
# Install from GitHub
install.packages("remotes")
remotes::install_github("jeremylfarrell/lakefetch")

# Or with all optional dependencies
remotes::install_github("jeremylfarrell/lakefetch", dependencies = TRUE)
```

## Quick Start

```r
library(lakefetch)

# Option 1: Use built-in example data
data(wisconsin_lakes)
sites <- load_sites(wisconsin_lakes)
lake <- get_lake_boundary(sites)
results <- fetch_calculate(sites, lake)

# Option 2: Use your own CSV with lat/lon columns
sites <- load_sites("your_sites.csv")
lake <- get_lake_boundary(sites)
results <- fetch_calculate(sites, lake)

# View results
print(results$results)

# Interactive Shiny app
fetch_app(results)
```

## Standalone Shiny App

Users can run the app without any R coding - just upload a CSV:

```r
library(lakefetch)
fetch_app_upload()
```

This launches a browser-based app where users can:
- Upload a CSV with lat/lon columns
- Automatically download lake boundaries
- View interactive map with fetch rays
- Download results as CSV or GeoPackage

## Key Features

- **Automatic lake boundary download** from OpenStreetMap
- **Multi-lake batch processing** - handles CSV with sites on multiple lakes
- **Directional fetch** - ray-casting at configurable angle resolution (default 5°)
- **Effective fetch methods:**
  - `"top3"` - mean of 3 highest directional fetches (default)
  - `"max"` - maximum directional fetch
  - `"cosine"` - SPM/CERC cosine-weighted method
- **Wave metrics** - orbital velocity using SMB equations with depth attenuation
- **Exposure classification** - Sheltered/Moderate/Exposed (configurable thresholds)
- **Optional NHD integration** - outlet/inlet detection for US lakes
- **Optional weather integration** - historical wind data from Open-Meteo API
- **Interactive visualization** - Shiny app with Leaflet maps

## Configuration

```r
# View current options
lakefetch_options()

# Change options
lakefetch_options(
  fetch_method = "cosine",           # SPM method
  exposure_sheltered_m = 2000,       # Custom threshold
  exposure_exposed_m = 6000,         # Custom threshold
  default_depth_m = 5,               # For orbital velocity
  angle_resolution_deg = 10          # Faster but less precise
)

# Reset to defaults
lakefetch_reset_options()
```

## CSV Input Format

The CSV file should have:
- **Required:** Columns starting with `lat` and `lon` (e.g., `latitude`, `longitude`)
- **Optional:** `Site` column for site names
- **Optional:** `depth` or `depth_m` column for site-specific depths
- **Optional:** `datetime` column for weather integration

Example:
```
Site,latitude,longitude,depth_m
Site_A,43.42,-73.69,5.5
Site_B,43.43,-73.68,8.2
Site_C,43.41,-73.70,3.1
```

## CRAN Submission (Next Steps)

When ready to submit:

```r
# Final check
devtools::check()

# Submit to CRAN
devtools::submit_cran()
```

You'll receive:
1. Confirmation email from CRAN (reply to confirm)
2. Review result email (usually 1-5 business days)

## Files for CRAN

- `cran-comments.md` - Notes for CRAN reviewers
- `NEWS.md` - Version changelog
- `README.md` - Package documentation
- `inst/CITATION` - Citation information

## Known Notes

- The `.github` folder (for R-hub GitHub Actions) generates a NOTE in R CMD check - this is expected and acceptable for CRAN
- Most examples use `\dontrun{}` due to API calls - this is standard practice

## Package Structure

```
lakefetch/
├── R/
│   ├── bathymetry.R       # Depth estimation (empirical)
│   ├── data.R             # Built-in datasets documentation
│   ├── data_loading.R     # CSV loading and validation
│   ├── fetch_core.R       # Main fetch calculation
│   ├── globals.R          # Package options
│   ├── lake_sources.R     # OSM boundary download
│   ├── nhd_integration.R  # NHD outlet/inlet detection
│   ├── shiny_app.R        # Interactive Shiny apps
│   ├── visualization.R    # Plotting functions
│   ├── weather_integration.R  # Open-Meteo API
│   └── zzz.R              # Package startup
├── data/                  # Built-in example datasets
├── inst/
│   ├── CITATION           # How to cite the package
│   └── examples/          # Example workflows
├── man/                   # Documentation (auto-generated)
├── tests/                 # Unit tests
├── vignettes/             # Long-form documentation
├── DESCRIPTION            # Package metadata
├── NAMESPACE              # Exports (auto-generated)
├── README.md              # GitHub readme
├── NEWS.md                # Changelog
└── cran-comments.md       # CRAN submission notes
```

## References

- Shore Protection Manual (1984). U.S. Army Corps of Engineers, CERC. 4th Edition.
- Sverdrup & Munk (1947). Wind, sea, and swell. U.S. Navy Hydrographic Office.
- Cael et al. (2017). Volume and mean depth of Earth's lakes. GRL.
- Mason et al. (2018). Effective fetch maps for the Great Lakes. Scientific Data.

## Contact

Jeremy Lynch Farrell
farrej2@rpi.edu
https://github.com/jeremylfarrell/lakefetch
