# ==============================================================================
# Run Lakefetch Upload App
# ==============================================================================
# This script launches the standalone Shiny app where you can upload a CSV
# file with GPS coordinates and analyze fetch interactively.
#
# Usage:
#   1. Open this script in RStudio
#   2. Click "Source" or press Ctrl+Shift+S to run
#   3. The app will open in your browser
#   4. Upload a CSV file with latitude/longitude columns
#   5. Click "Run Analysis" to process
#
# CSV Requirements:
#   - Must have columns starting with 'lat' and 'lon' (case-insensitive)
#   - Optional 'Site' column for point names
# ==============================================================================

# Load the package from source
devtools::load_all(".")

# Launch the upload app
fetch_app_upload()
