# ==============================================================================
# Run Lakefetch Shiny App
# ==============================================================================
# This script loads site data, calculates fetch, and launches the interactive
# Shiny app for visualization.
#
# Usage:
#   1. Open this script in RStudio
#   2. Click "Source" or press Ctrl+Shift+S to run
#   3. The app will open in your browser
#
# In the app:
#   - Click any site marker to view its fetch rays
#   - Click anywhere on a lake to analyze a new custom point
#   - Results appear in the sidebar
# ==============================================================================

# Load the package from source
devtools::load_all(".")

# ------------------------------------------------------------------------------
# Configure your input file here
# ------------------------------------------------------------------------------
INPUT_FILE <- "adk.csv"

# ------------------------------------------------------------------------------
# Run the analysis
# ------------------------------------------------------------------------------

# Load sites
cat("Loading sites from:", INPUT_FILE, "\n")
sites <- load_sites(INPUT_FILE)

# Get lake boundaries from OpenStreetMap
cat("Downloading lake boundaries...\n")
lake <- get_lake_boundary(sites)

# Calculate fetch (set add_context = TRUE to include NHD outlet/inlet info)
cat("Calculating fetch...\n")
results <- fetch_calculate(sites, lake, add_context = FALSE)

# Launch the interactive Shiny app
cat("Launching Shiny app...\n")
fetch_app(results)
