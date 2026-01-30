# lakefetch Validation Summary

## Analytical Validation (PASSED)

**Status: 4/4 tests PASSED with 0% error**

Run with: `source("inst/validation/validate_fetch.R")`

| Test | Geometry | Expected | Actual | Error |
|------|----------|----------|--------|-------|
| 1 | Circular lake (r=1000m), center point | 1000m all directions | 1000m | 0% |
| 2 | Circular lake, point 800m from center | Min: 200m, Max: 1800m | 200m, 1800m | 0% |
| 3 | Rectangular lake (2000×1000m), N/S fetch | 500m | 500m | 0% |
| 3 | Rectangular lake (2000×1000m), E/W fetch | 1000m | 1000m | 0% |
| 4 | Effective fetch (circular center) | 1000m | 1000m | 0% |

**Conclusion**: The ray-casting algorithm correctly calculates fetch distances against known analytical solutions.

## Cross-Package Validation (BLOCKED)

### waver package
- **Status**: Incompatible input assumptions
- **Issue**: waver is designed for coastal/marine applications where:
  - Points are in water (ocean)
  - `shoreline` parameter is a land polygon
- For lakes, this requires creating an inverted geometry (land with lake as hole)
- API errors persist even with inverted geometry

### fetchR package
- **Status**: Package not available for R 4.4.x
- **Issue**: CRAN reports "package 'fetchR' is not available for this version of R"

### Conclusion
Cross-package validation is not currently possible due to:
1. Fundamental design differences (coastal vs lake applications)
2. Package availability issues

**The analytical validation is more rigorous** because it tests against exact known solutions rather than comparing two implementations that could both have the same bugs.

## Literature Validation

**Status: Available**

Run with: `source("inst/validation/validate_literature.R")`

### Approach
Rather than comparing to specific published fetch values (which are often unavailable
at point resolution), this validation:

1. Uses lakes with **published morphometry** from World Lake Database and state surveys
2. Downloads boundaries from OpenStreetMap
3. Calculates fetch at the approximate lake center
4. Validates results fall within **expected geometric ranges**

### Test Lakes

| Lake | Location | Area (km²) | Max Length | Max Width | Source |
|------|----------|------------|------------|-----------|--------|
| Lake Sunapee | NH | 16.7 | 13 km | 2.8 km | NH DES |
| Cayuga Lake | NY | 172 | 61 km | 2.8 km | USGS/Cornell |
| Green Lake | WI | 29.6 | 12 km | 3.2 km | WI DNR |

### Validation Results

**Status: 3/3 tests PASSED (100%)**

| Lake | State | Area | Result | Notes |
|------|-------|------|--------|-------|
| Lake Sunapee | NH | 16.7 km² | PASS | Mean 1,629m, Max 4,044m |
| Cayuga Lake | NY | 172 km² | PASS | Hit 50km cap (expected - 61km long lake) |
| Green Lake | WI | 29.6 km² | PASS | Mean 2,463m, Max 6,237m |

### Validation Logic
For a point at lake center:
- **Max fetch** should approximate half the maximum lake length
- **Min fetch** should approximate half the maximum lake width
- **Mean fetch** should fall between these values based on lake shape
- **Elongated lakes** (length/width > 5): adjusted expectations for mean fetch

### Methodology Alignment

lakefetch implements fetch calculation consistent with established standards:

1. **Shore Protection Manual (USACE, 1984)** - The standard reference for fetch
   calculation, defining effective fetch as radial measurements weighted by direction

2. **Mason et al. (2018)** - Used similar ray-casting approach for Great Lakes at
   30m resolution, validated against USACE Wave Information Studies (R² = 0.635)

3. **USGS/Natural Capital Project** - Algorithms based on ray-casting to shoreline,
   implemented in ArcGIS Python tools

### Reference
- Mason, L.A. et al. (2018). Effective fetch and relative exposure index maps for
  the Laurentian Great Lakes. Scientific Data 5:180295. doi:10.1038/sdata.2018.295

## Edge Case Validation

**Status: 4/4 testable cases PASSED (100%)**

Run with: `source("inst/validation/validate_edge_cases.R")`

### Test Cases

| Test | Geometry | Result | Notes |
|------|----------|--------|-------|
| 1 | Central island | PASS | 0% error, island correctly blocks rays |
| 2 | Offset island | PASS | North rays blocked at 500m as expected |
| 3 | Complex shoreline | PASS | Fetch varies 1119-1881m with irregular shape |
| 4 | Very large lake (Erie) | SKIPPED | OSM limitation for Great Lakes* |
| 5 | Multiple islands | PASS | All 3 islands correctly block rays |

*Great Lakes are too large for automatic OSM download. Use pre-downloaded shapefiles.

### Key Findings

1. **Islands correctly block fetch rays** - Rays stop at island boundaries, not lake edge
2. **Complex shorelines handled** - Bays and peninsulas produce expected fetch variation
3. **Multiple islands supported** - Each island independently blocks rays in its direction
4. **Great Lakes limitation** - OSM bounding box approach doesn't work for very large lakes;
   recommend using shapefiles from USGS or other sources

## Recommendations

1. **For publication**: The analytical validation (0% error) provides strong evidence that the algorithm is mathematically correct.

2. **For CRAN submission**: Add testthat unit tests based on the analytical validation cases.

3. **Future work**: If a lake-specific fetch package becomes available, cross-validation would strengthen confidence.
