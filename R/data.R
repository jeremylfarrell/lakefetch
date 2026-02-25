# ==============================================================================
# Dataset Documentation
# ==============================================================================
# Documentation for example datasets included with the lakefetch package.
# ==============================================================================

#' Adirondack Lake Sampling Sites
#'
#' A dataset containing example lake sampling sites from the Adirondack region
#' of New York State. These synthetic but realistic coordinates demonstrate
#' typical multi-lake sampling scenarios.
#'
#' @name adirondack_sites
#' @docType data
#' @format A data frame with 12 rows and 5 variables:
#' \describe{
#'   \item{Site}{Unique site identifier}
#'   \item{lake.name}{Name of the lake}
#'   \item{latitude}{Latitude in decimal degrees (WGS84)}
#'   \item{longitude}{Longitude in decimal degrees (WGS84)}
#'   \item{datetime}{Date and time of sampling (POSIXct)}
#' }
#'
#' @details
#' The dataset includes sites from four Adirondack lakes:
#' \itemize{
#'   \item Blue Mountain Lake (3 sites)
#'   \item Raquette Lake (4 sites)
#'   \item Long Lake (2 sites)
#'   \item Tupper Lake (3 sites)
#' }
#'
#' @examples
#' # Load the dataset
#' data(adirondack_sites)
#'
#' # View structure
#' str(adirondack_sites)
#'
#' # Use with lakefetch
#' \donttest{
#' sites <- load_sites(adirondack_sites)
#' lake_data <- get_lake_boundary(sites)
#' results <- fetch_calculate(sites, lake_data)
#' }
#'
#' @source Synthetic data for demonstration purposes
"adirondack_sites"


#' Example Circular Lake Polygon
#'
#' A simple circular lake polygon for demonstration and testing purposes.
#' This synthetic lake has known geometry (1 km radius) which makes it
#' useful for validating fetch calculations.
#'
#' @name example_lake
#' @docType data
#' @format An sf object with 1 row and 3 variables:
#' \describe{
#'   \item{osm_id}{Identifier (synthetic)}
#'   \item{name}{Lake name}
#'   \item{area_km2}{Surface area in square kilometers (~3.14 km²)}
#'   \item{geometry}{POLYGON geometry in UTM Zone 18N (EPSG:32618)}
#' }
#'
#' @details
#' The lake is centered at UTM coordinates (500000, 4800000) with a radius
#' of 1000 meters. For a site at the center, fetch should equal 1000 m in
#' all directions.
#'
#' @examples
#' # Load the dataset
#' data(example_lake)
#'
#' # View structure
#' print(example_lake)
#'
#' # Plot the lake
#' \donttest{
#' library(ggplot2)
#' ggplot(example_lake) + geom_sf()
#' }
#'
#' @source Synthetic data for demonstration and validation
"example_lake"


#' Wisconsin Lake Sampling Sites
#'
#' A dataset containing example sampling sites from well-known Wisconsin lakes.
#' These coordinates are useful for testing with real lake boundaries from
#' OpenStreetMap.
#'
#' @name wisconsin_lakes
#' @docType data
#' @format A data frame with 8 rows and 4 variables:
#' \describe{
#'   \item{Site}{Unique site identifier}
#'   \item{lake.name}{Name of the lake}
#'   \item{latitude}{Latitude in decimal degrees (WGS84)}
#'   \item{longitude}{Longitude in decimal degrees (WGS84)}
#' }
#'
#' @details
#' The dataset includes sites from three Wisconsin lakes:
#' \itemize{
#'   \item Lake Mendota (3 sites) - Madison's largest lake, well-studied
#'   \item Lake Monona (2 sites) - Connected to Mendota via Yahara River
#'   \item Geneva Lake (3 sites) - Popular recreational lake in SE Wisconsin
#' }
#'
#' @examples
#' # Load the dataset
#' data(wisconsin_lakes)
#'
#' # View the data
#' head(wisconsin_lakes)
#'
#' # Use with lakefetch
#' \donttest{
#' sites <- load_sites(wisconsin_lakes)
#' lake_data <- get_lake_boundary(sites)
#' results <- fetch_calculate(sites, lake_data)
#' }
#'
#' @source Synthetic data based on real lake locations
"wisconsin_lakes"
