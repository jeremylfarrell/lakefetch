# ==============================================================================
# Create lakefetch Package Presentation
# ==============================================================================
# Run this script in R to generate a PowerPoint presentation
# Requires: install.packages("officer")

if (!require("officer", quietly = TRUE)) {
  install.packages("officer")
  library(officer)
}

# Create presentation
ppt <- read_pptx()

# ------------------------------------------------------------------------------
# Title Slide
# ------------------------------------------------------------------------------
ppt <- add_slide(ppt, layout = "Title Slide", master = "Office Theme")
ppt <- ph_with(ppt, value = "lakefetch", location = ph_location_type(type = "ctrTitle"))
ppt <- ph_with(ppt, value = "An R Package for Calculating Fetch and Wave Exposure in Lakes",
               location = ph_location_type(type = "subTitle"))

# ------------------------------------------------------------------------------
# Why Fetch Matters
# ------------------------------------------------------------------------------
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Why Fetch Matters", location = ph_location_type(type = "title"))
ppt <- ph_with(ppt, value = c(
  "Fetch = unobstructed distance wind can travel across water",
  "",
  "Controls wave energy and mixing:",
  "
    \u2022 Sediment resuspension",
  "    \u2022 Nutrient cycling",
  "    \u2022 Littoral habitat structure",
  "    \u2022 Macrophyte distribution",
  "",
  "Critical for understanding:",
  "    \u2022 Benthic community composition",
  "    \u2022 Water clarity patterns",
  "    \u2022 Shoreline erosion",
  "    \u2022 Site-specific sampling conditions"
), location = ph_location_type(type = "body"))

# ------------------------------------------------------------------------------
# The Problem
# ------------------------------------------------------------------------------
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "The Problem", location = ph_location_type(type = "title"))
ppt <- ph_with(ppt, value = c(
  "Calculating fetch manually is tedious:",
  "",
  "    \u2022 Need accurate lake boundary data",
  "    \u2022 Must measure distances in multiple directions",
  "    \u2022 Complex geometry for irregular shorelines",
  "    \u2022 Time-consuming for multiple sites/lakes",
  "",
  "Existing tools:",
  "    \u2022 Often require expensive GIS software",
  "    \u2022 Limited batch processing",
  "    \u2022 No integration with weather data"
), location = ph_location_type(type = "body"))

# ------------------------------------------------------------------------------
# Introducing lakefetch
# ------------------------------------------------------------------------------
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Introducing lakefetch", location = ph_location_type(type = "title"))
ppt <- ph_with(ppt, value = c(
  "An R package that automates fetch calculation:",
  "",
  "    \u2022 Downloads lake boundaries from OpenStreetMap",
  "    \u2022 Calculates directional fetch via ray-casting",
  "    \u2022 Handles multiple lakes automatically",
  "    \u2022 Classifies sites by wave exposure",
  "",
  "Optional integrations:",
  "    \u2022 NHD for outlet/inlet locations & watershed data",
  "    \u2022 Historical weather for cumulative wave energy",
  "    \u2022 HydroLAKES for depth estimates"
), location = ph_location_type(type = "body"))

# ------------------------------------------------------------------------------
# How It Works
# ------------------------------------------------------------------------------
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "How It Works: Ray-Casting Algorithm", location = ph_location_type(type = "title"))
ppt <- ph_with(ppt, value = c(
"1. Load sampling site coordinates (CSV or data.frame)",
  "",
  "2. Download or load lake boundary polygons",
  "",
  "3. For each site, cast rays in all directions (5\u00b0 resolution)",
  "",
  "4. Measure distance to shoreline in each direction",
  "",
  "5. Calculate summary metrics:",
  "    \u2022 Mean fetch (average across all directions)",
  "    \u2022 Max fetch (longest open water distance)",
  "    \u2022 Effective fetch (mean of top 3 directions)",
  "    \u2022 Estimated orbital velocity"
), location = ph_location_type(type = "body"))

# ------------------------------------------------------------------------------
# Simple Workflow
# ------------------------------------------------------------------------------
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Simple 4-Step Workflow", location = ph_location_type(type = "title"))
ppt <- ph_with(ppt, value = c(
  "library(lakefetch)",
  "",
  "# 1. Load site data",
  "sites <- load_sites(\"my_sites.csv\")",
  "",
  "# 2. Get lake boundaries (auto-download from OSM)",
  "lake <- get_lake_boundary(sites)",
  "",
  "# 3. Calculate fetch",
  "results <- fetch_calculate(sites, lake)",
  "",
  "# 4. Visualize",
  "plot_fetch_map(results)",
  "fetch_app(results)  # Interactive Shiny app"
), location = ph_location_type(type = "body"))

# ------------------------------------------------------------------------------
# Input Requirements
# ------------------------------------------------------------------------------
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Input Requirements", location = ph_location_type(type = "title"))
ppt <- ph_with(ppt, value = c(
  "Minimal input: CSV with coordinates",
  "",
  "Required columns:",
  "    \u2022 Latitude (column starting with 'lat')",
  "    \u2022 Longitude (column starting with 'lon')",
  "",
  "Optional columns:",
  "    \u2022 Site - site identifiers",
  "    \u2022 lake.name - for multi-lake matching",
  "    \u2022 datetime - for weather integration",
  "",
  "Lake boundaries:",
  "    \u2022 Auto-downloaded from OpenStreetMap, OR",
  "    \u2022 Provide local shapefile/geopackage"
), location = ph_location_type(type = "body"))

# ------------------------------------------------------------------------------
# Output Data
# ------------------------------------------------------------------------------
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Output: Rich Spatial Dataset", location = ph_location_type(type = "title"))
ppt <- ph_with(ppt, value = c(
  "Returns sf object with:",
  "",
  "Directional fetch:",
  "    \u2022 fetch_0, fetch_5, ... fetch_355 (meters per direction)",
  "",
  "Summary metrics:",
  "    \u2022 fetch_mean, fetch_max, fetch_effective",
  "    \u2022 orbital_effective (wave orbital velocity)",
  "    \u2022 exposure_category (Sheltered/Moderate/Exposed)",
  "",
  "Lake context:",
  "    \u2022 lake_name, lake_area_km2",
  "    \u2022 outlet/inlet distances (with NHD)",
  "    \u2022 watershed area, connectivity class"
), location = ph_location_type(type = "body"))

# ------------------------------------------------------------------------------
# Exposure Categories
# ------------------------------------------------------------------------------
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Exposure Classification", location = ph_location_type(type = "title"))
ppt <- ph_with(ppt, value = c(
  "Sites classified by effective fetch:",
  "",
  "SHELTERED (< 2.5 km)",
  "    \u2022 Protected bays and coves",
  "    \u2022 Minimal wave action",
  "",
  "MODERATE (2.5 - 5 km)",
  "    \u2022 Some wave exposure",
  "    \u2022 Mixed conditions",
  "",
  "EXPOSED (> 5 km)",
  "    \u2022 Open water locations",
  "    \u2022 Significant wave energy potential"
), location = ph_location_type(type = "body"))

# ------------------------------------------------------------------------------
# Visualization Options
# ------------------------------------------------------------------------------
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Visualization Options", location = ph_location_type(type = "title"))
ppt <- ph_with(ppt, value = c(
  "Static plots (ggplot2):",
  "    \u2022 plot_fetch_map() - map with exposure colors",
  "    \u2022 plot_fetch_bars() - bar chart of effective fetch",
  "    \u2022 plot_fetch_rose() - directional rose diagram",
  "",
  "Interactive Shiny app:",
  "    \u2022 fetch_app() - explore results on satellite imagery",
  "    \u2022 Click markers to view fetch rays",
  "    \u2022 Click anywhere to analyze new points",
  "",
  "    \u2022 fetch_app_upload() - standalone upload interface",
  "",
  "Export ray geometries for GIS:",
  "    \u2022 create_ray_geometries() -> GeoPackage/Shapefile"
), location = ph_location_type(type = "body"))

# ------------------------------------------------------------------------------
# Weather Integration
# ------------------------------------------------------------------------------
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Historical Weather Integration", location = ph_location_type(type = "title"))
ppt <- ph_with(ppt, value = c(
  "Combine fetch with actual wind conditions:",
  "",
  "add_weather_context() fetches from Open-Meteo API:",
  "    \u2022 Wind speed/direction history",
  "    \u2022 Temperature, precipitation",
  "    \u2022 Solar radiation, cloud cover",
  "",
  "Calculates cumulative wave energy:",
  "    \u2022 Directional fetch + wind direction over time",
  "    \u2022 Estimated wave heights",
  "    \u2022 Bottom orbital velocities",
  "",
  "Time windows: 24h, 3-day, 7-day preceding sample"
), location = ph_location_type(type = "body"))

# ------------------------------------------------------------------------------
# Multi-Lake Support
# ------------------------------------------------------------------------------
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Multi-Lake Support", location = ph_location_type(type = "title"))
ppt <- ph_with(ppt, value = c(
  "Handles datasets spanning multiple lakes:",
  "",
  "    \u2022 Sites automatically assigned to containing lake",
  "    \u2022 Spatial intersection + buffer tolerance",
  "    \u2022 Name-based matching as fallback",
  "",
  "    \u2022 Fetch calculated separately per lake",
  "    \u2022 Parallel processing for speed",
  "",
  "Results include lake identifiers:",
  "    \u2022 Filter/group by lake_name",
  "    \u2022 Compare exposure across lakes"
), location = ph_location_type(type = "body"))

# ------------------------------------------------------------------------------
# Dependencies
# ------------------------------------------------------------------------------
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Package Dependencies", location = ph_location_type(type = "title"))
ppt <- ph_with(ppt, value = c(
  "Core (required):",
  "    \u2022 sf - spatial operations",
  "    \u2022 osmdata - OpenStreetMap download",
  "    \u2022 ggplot2 - static visualizations",
  "",
  "Interactive app:",
  "    \u2022 shiny, leaflet - web interface",
  "",
  "Optional enhancements:",
  "    \u2022 nhdplusTools - NHD integration",
  "    \u2022 jsonlite - weather API",
  "    \u2022 parallel - multi-lake speedup"
), location = ph_location_type(type = "body"))

# ------------------------------------------------------------------------------
# Use Cases
# ------------------------------------------------------------------------------
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Use Cases", location = ph_location_type(type = "title"))
ppt <- ph_with(ppt, value = c(
  "Benthic ecology:",
  "    \u2022 Explain community composition patterns",
  "    \u2022 Substrate disturbance assessment",
  "",
  "Water quality:",
  "    \u2022 Sediment resuspension potential",
  "    \u2022 Mixing and stratification context",
  "",
  "Sampling design:",
  "    \u2022 Stratify sites by exposure class",
  "    \u2022 Account for fetch in statistical models",
  "",
  "Long-term monitoring:",
  "    \u2022 Consistent fetch metrics across years",
  "    \u2022 Integrate with weather for event analysis"
), location = ph_location_type(type = "body"))

# ------------------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------------------
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Summary", location = ph_location_type(type = "title"))
ppt <- ph_with(ppt, value = c(
  "lakefetch provides:",
  "",
  "    \u2713 Automated fetch calculation from GPS coordinates",
  "    \u2713 Automatic lake boundary download",
  "    \u2713 Multi-lake and parallel processing",
  "    \u2713 Exposure classification",
  "    \u2713 Interactive visualization",
  "    \u2713 Weather and wave energy integration",
  "    \u2713 NHD hydrological context",
  "",
  "Simple workflow:",
  "    load_sites() -> get_lake_boundary() -> fetch_calculate()",
  "",
  "Questions?"
), location = ph_location_type(type = "body"))

# ------------------------------------------------------------------------------
# Save presentation
# ------------------------------------------------------------------------------
output_file <- "lakefetch_presentation.pptx"
print(ppt, target = output_file)
message("Presentation saved to: ", normalizePath(output_file))
