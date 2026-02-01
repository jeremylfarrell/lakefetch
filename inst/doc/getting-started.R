## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----install------------------------------------------------------------------
# # Install from local source
# devtools::install("path/to/lakefetch")
# 
# # Or install dependencies first
# install.packages(c("sf", "osmdata", "ggplot2"))
# 
# # Optional: for NHD integration
# install.packages("nhdplusTools")
# 
# # Optional: for interactive app
# install.packages(c("shiny", "leaflet", "base64enc"))

## ----load-sites---------------------------------------------------------------
# library(lakefetch)
# 
# # From CSV file
# sites <- load_sites("my_lake_sites.csv")
# 
# # Or create manually
# sites <- data.frame(
#   Site = c("Site_A", "Site_B", "Site_C"),
#   latitude = c(43.42, 43.43, 43.41),
#   longitude = c(-73.69, -73.68, -73.70)
# )
# sites <- load_sites(sites)

## ----get-lake-----------------------------------------------------------------
# # Automatically download from OpenStreetMap
# lake <- get_lake_boundary(sites)
# 
# # Or load from a local shapefile
# lake <- get_lake_boundary(sites, file = "lake_boundary.shp")

## ----calculate----------------------------------------------------------------
# results <- fetch_calculate(sites, lake)
# 
# # View results
# head(results$results)

## ----visualize----------------------------------------------------------------
# # Map of sites
# plot_fetch_map(results)
# 
# # Bar chart
# plot_fetch_bars(results)
# 
# # Interactive app
# fetch_app(results)

## ----options------------------------------------------------------------------
# # View current options
# lakefetch_options()
# 
# # Change options
# lakefetch_options(
#   buffer_distance_m = 20,      # GPS accuracy buffer
#   angle_resolution_deg = 10,   # Direction resolution (degrees)
#   max_fetch_m = 30000,         # Maximum fetch distance
#   use_parallel = TRUE          # Parallel processing for multiple lakes
# )
# 
# # Reset to defaults
# lakefetch_reset_options()

## ----nhd----------------------------------------------------------------------
# # NHD context is added automatically if available
# results <- fetch_calculate(sites, lake, add_context = TRUE)
# 
# # Check connectivity
# table(results$results$connectivity_class)

## ----multi-lake---------------------------------------------------------------
# # Sites are assigned to their containing lake polygon
# sites_with_lakes <- assign_sites_to_lakes(lake$sites, lake$all_lakes)
# 
# # Check assignment
# table(sites_with_lakes$lake_name)

## ----export-------------------------------------------------------------------
# # To CSV (without geometry)
# write.csv(sf::st_drop_geometry(results$results),
#           "fetch_results.csv", row.names = FALSE)
# 
# # To GeoPackage (with geometry)
# sf::st_write(results$results, "fetch_results.gpkg")
# 
# # Export ray geometries for GIS
# rays <- create_ray_geometries(results)
# sf::st_write(rays, "fetch_rays.gpkg")

