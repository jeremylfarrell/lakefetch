# lakefetch Development TODO

## High Priority

### Validation & Testing
- [x] Add validation against known fetch values (circular lake analytical test)
      Run with: `source("inst/validation/validate_fetch.R")`
      Tests: circular lake center, circular lake edge, rectangular lake, effective fetch
      Results: 4/4 tests PASSED with 0% error
- [x] Cross-validate against fetchR/waver packages
      Status: BLOCKED - compatibility issues:
      - waver: designed for coastal applications, different input assumptions (points in water, land polygon)
      - fetchR: not available for R 4.4.x
      Note: Analytical validation is more rigorous anyway (known exact solutions)
- [x] Add validation against published fetch data from literature
      Run with: `source("inst/validation/validate_literature.R")`
      Tests: Lake Sunapee (NH), Cayuga Lake (NY), Green Lake (WI)
      Results: 3/3 tests PASSED (100%)
      Reference: World Lake Database (ILEC), state limnology surveys
- [x] Create formal unit test suite with testthat
      Run with: `devtools::test()`
      79 tests across 4 test files:
      - test-fetch_core.R (18 tests) - analytical validation
      - test-data_loading.R (25 tests) - input handling
      - test-options.R (24 tests) - configuration
      - test-visualization.R (12 tests) - plotting functions
- [x] Test edge cases: islands, complex shorelines, very large lakes
      Run with: `source("inst/validation/validate_edge_cases.R")`
      Tests: central island, offset island, complex shoreline, multiple islands
      Results: 4/4 testable cases PASSED (100%)
      Note: Great Lakes require shapefiles (OSM bounding box limitation)

### Documentation
- [ ] Write methods paper (target: JOSS - Journal of Open Source Software)
- [ ] Add more vignettes (troubleshooting, advanced usage, weather integration)
- [x] Document validation results
      Added: vignettes/validation.Rmd with comprehensive validation documentation
- [x] Add example datasets to package
      Added: adirondack_sites, example_lake, wisconsin_lakes
      Documentation: R/data.R with roxygen2 documentation

### CRAN Submission
- [x] Run R CMD check with --as-cran
      Results: 0 errors, 0 warnings, 1 note (system time check - ignorable)
- [x] Fix any NOTEs/WARNINGs
      Fixed: removed hydrolinks dependency, added .Rbuildignore entries,
      added missing imports (ave), added globalVariables (req)
- [x] Write CRAN submission comments
      Added: cran-comments.md with test environments, validation summary, dependencies
- [ ] Submit to CRAN

## Medium Priority

### Methodology Improvements
- [ ] Add wind-weighted effective fetch option (weight by prevailing wind direction)
- [ ] Add uncertainty quantification (confidence intervals on fetch estimates)
- [ ] Improve wave energy model (consider spectral wave models)
- [ ] Better island/archipelago handling

### Data Sources
- [ ] Add NHD as alternative/fallback to OSM for US lakes
- [ ] Add offline mode with cached boundaries
- [ ] Improve depth estimates (bathymetry integration where available)
- [ ] Add support for Great Lakes and other very large lakes

### Performance
- [ ] Optimize ray-casting for large datasets
- [ ] Add progress bars for long-running operations
- [ ] Improve parallel processing efficiency

## Low Priority

### Features
- [ ] Add fetch calculation for coastal/marine sites
- [ ] Export to additional formats (KML, GeoJSON)
- [ ] Add batch processing CLI tool
- [ ] Integration with other lake databases (LAGOSNE, etc.)

### User Experience
- [ ] Improve error messages
- [ ] Add input data validation warnings
- [ ] Shiny app improvements (download buttons, more customization)

## Completed
- [x] Core fetch calculation
- [x] OSM boundary download with multiple server fallback
- [x] Multi-lake support with name-based matching
- [x] NHD integration for outlets/inlets
- [x] Historical weather integration
- [x] Interactive Shiny app
- [x] Exposure classification
- [x] Basic visualization functions
- [x] CLAUDE.md documentation

---

## Notes

### Validation Strategy
1. **Analytical validation**: Circular lake where fetch from center = radius in all directions
2. **Literature validation**: Compare to published fetch values (e.g., from waver paper or limnology studies)
3. **Cross-package validation**: Compare results with fetchR and waver on same lake/sites

### JOSS Paper Outline
1. Summary - what the package does
2. Statement of need - why it's useful for limnologists
3. Key features - auto-download, multi-lake, weather integration
4. Validation - analytical + real-world comparisons
5. Example application - Adirondack lakes dataset
6. Acknowledgments & references

### Known Issues
- OSM Overpass API can be unreliable (mitigated with multi-server fallback)
- Some small lakes not in OSM
- NHD integration US-only
