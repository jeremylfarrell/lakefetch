# ==============================================================================
# Lake Depth and Bathymetry Functions
# ==============================================================================
# Functions to estimate lake depth using HydroLAKES database and empirical
# relationships. Used for wave orbital velocity calculations.
# ==============================================================================

#' Get Lake Depth Estimates
#'
#' Retrieves or estimates lake depth for wave calculations. Uses multiple
#' methods in order of preference: user-provided depth, HydroLAKES database,
#' or empirical estimation from lake area.
#'
#' @param lake_polygon sf polygon of the lake
#' @param site_coords Coordinates of the sample site (optional, for future
#'   bathymetry grid support)
#' @param user_depth User-provided depth in meters (highest priority)
#' @param method Method for depth estimation: "auto" (try all), "hydrolakes",
#'   or "empirical"
#'
#' @return A list with elements:
#'   \item{depth_mean}{Estimated mean depth in meters}
#'   \item{depth_max}{Estimated maximum depth in meters (if available)}
#'   \item{source}{Source of the estimate ("user", "hydrolakes", "empirical")}
#'   \item{confidence}{Confidence level ("high", "medium", "low")}
#'
#' @details
#' Depth estimation methods:
#' \enumerate{
#'   \item User-provided: Direct input, highest confidence
#'   \item HydroLAKES: Global database with depth estimates for lakes >10 ha
#'   \item Empirical: Estimated from lake surface area using published relationships
#' }
#'
#' The empirical method uses the relationship from Cael et al. (2017):
#' mean_depth ~ 10.3 * area_km2^0.25
#'
#' @examples
#' \dontrun{
#' # With user-provided depth
#' depth <- get_lake_depth(lake_poly, user_depth = 8.5)
#'
#' # Auto-detect from HydroLAKES or estimate
#' depth <- get_lake_depth(lake_poly)
#' }
#'
#' @references
#' Messager, M.L., Lehner, B., Grill, G., Nedeva, I., Schmitt, O. (2016):
#' Estimating the volume and age of water stored in global lakes using a
#' geo-statistical approach. Nature Communications, 7: 13603.
#'
#' Cael, B.B., Heathcote, A.J., Seekell, D.A. (2017): The volume and mean
#' depth of Earth's lakes. Geophysical Research Letters, 44: 209-218.
#'
#' @export
get_lake_depth <- function(lake_polygon, site_coords = NULL, user_depth = NULL,
                           method = "auto") {


  # If user provided depth, use it directly

if (!is.null(user_depth) && !is.na(user_depth) && user_depth > 0) {
    return(list(
      depth_mean = user_depth,
      depth_max = NA_real_,
      source = "user",
      confidence = "high"
    ))
  }

  # Calculate lake area for empirical estimation
  lake_area_m2 <- as.numeric(sf::st_area(sf::st_union(lake_polygon)))
  lake_area_km2 <- lake_area_m2 / 1e6

  # Try HydroLAKES if method allows
  if (method %in% c("auto", "hydrolakes")) {
    hydrolakes_result <- try_hydrolakes_lookup(lake_polygon, lake_area_km2)
    if (!is.null(hydrolakes_result)) {
      return(hydrolakes_result)
    }
  }

  # Fall back to empirical estimation
  if (method %in% c("auto", "empirical")) {
    return(estimate_depth_empirical(lake_area_km2))
  }

  # No method succeeded
  return(list(
    depth_mean = NA_real_,
    depth_max = NA_real_,
    source = "none",
    confidence = "none"
  ))
}


#' Try to Look Up Lake in HydroLAKES Database
#'
#' Attempts to match a lake to the HydroLAKES database and retrieve depth.
#'
#' @param lake_polygon sf polygon of the lake
#' @param lake_area_km2 Lake area in square kilometers
#'
#' @return List with depth info or NULL if not found
#'
#' @noRd
try_hydrolakes_lookup <- function(lake_polygon, lake_area_km2) {
  # HydroLAKES lookup currently disabled - hydrolinks package not on CRAN

  # Falls back to empirical depth estimation based on lake area
  # TODO: Re-enable when hydrolinks becomes available on CRAN or implement
  #       direct HydroLAKES API access
  return(NULL)
}


#' Estimate Lake Depth from Surface Area
#'
#' Uses empirical relationships between lake surface area and depth.
#'
#' @param lake_area_km2 Lake surface area in square kilometers
#'
#' @return List with depth estimates
#'
#' @details
#' Uses the global empirical relationship from Cael et al. (2017):
#' mean_depth (m) = 10.3 * area (km2)^0.25
#'
#' Maximum depth is estimated as approximately 2.5-3x mean depth based on
#' typical lake morphometry.
#'
#' @noRd
estimate_depth_empirical <- function(lake_area_km2) {

  if (is.na(lake_area_km2) || lake_area_km2 <= 0) {
    return(list(
      depth_mean = NA_real_,
      depth_max = NA_real_,
      source = "empirical",
      confidence = "none"
    ))
  }

  # Cael et al. (2017) global relationship
  # Mean depth (m) = 10.3 * Area (km2)^0.25
  depth_mean <- 10.3 * (lake_area_km2 ^ 0.25)

  # Max depth typically 2-3x mean depth
  # Using 2.5 as middle estimate
  depth_max <- depth_mean * 2.5

  # Cap estimates at reasonable values
  depth_mean <- min(depth_mean, 200)  # Few lakes >200m mean depth
  depth_max <- min(depth_max, 500)    # Few lakes >500m max depth

  message("  Lake depth estimated from area (", round(lake_area_km2, 2),
          " km2): mean ~ ", round(depth_mean, 1), "m, max ~ ",
          round(depth_max, 1), "m")

  return(list(
    depth_mean = depth_mean,
    depth_max = depth_max,
    source = "empirical",
    confidence = "low"
  ))
}


#' Add Depth Information to Fetch Results
#'
#' Looks up or estimates depth for each lake in the fetch results and
#' adds depth columns.
#'
#' @param fetch_results sf object with fetch results
#' @param lakes sf object with lake polygons
#' @param user_depths Named vector of user-provided depths (names = lake IDs)
#'
#' @return fetch_results with added depth columns
#'
#' @examples
#' \dontrun{
#' # After running fetch_calculate
#' results <- fetch_calculate(sites, lake)
#'
#' # Add depth estimates
#' results$results <- add_lake_depth(results$results, results$lakes)
#'
#' # Or provide known depths
#' depths <- c("12345" = 15.5)  # lake_osm_id = depth in meters
#' results$results <- add_lake_depth(results$results, results$lakes, user_depths = depths)
#' }
#'
#' @export
add_lake_depth <- function(fetch_results, lakes, user_depths = NULL) {

  # Get unique lakes
  lake_ids <- unique(fetch_results$lake_osm_id)
  lake_ids <- lake_ids[!is.na(lake_ids)]

  # Look up depth for each lake
  depth_lookup <- list()

  for (lake_id in lake_ids) {
    lake_poly <- lakes[lakes$osm_id == lake_id, ]

    # Check for user-provided depth
    user_d <- if (!is.null(user_depths) && lake_id %in% names(user_depths)) {
      user_depths[[lake_id]]
    } else {
      NULL
    }

    depth_info <- get_lake_depth(lake_poly, user_depth = user_d)
    depth_lookup[[lake_id]] <- depth_info
  }

  # Add depth columns to results
  fetch_results$depth_mean_m <- sapply(fetch_results$lake_osm_id, function(id) {
    if (is.na(id) || is.null(depth_lookup[[id]])) NA_real_
    else depth_lookup[[id]]$depth_mean
  })

  fetch_results$depth_max_m <- sapply(fetch_results$lake_osm_id, function(id) {
    if (is.na(id) || is.null(depth_lookup[[id]])) NA_real_
    else depth_lookup[[id]]$depth_max
  })

  fetch_results$depth_source <- sapply(fetch_results$lake_osm_id, function(id) {
    if (is.na(id) || is.null(depth_lookup[[id]])) NA_character_
    else depth_lookup[[id]]$source
  })

  return(fetch_results)
}


#' Estimate Depth at Specific Site Location
#'
#' For lakes with bathymetry data, estimates depth at a specific site.
#' Currently uses mean depth as proxy; future versions may support
#' bathymetry grids.
#'
#' @param site_point sf point of the site location
#' @param lake_polygon sf polygon of the lake
#' @param depth_mean Mean depth of the lake
#' @param depth_max Maximum depth of the lake
#' @param position Relative position in lake ("shore", "middle", "unknown")
#'
#' @return Estimated depth at site in meters
#'
#' @details
#' Without detailed bathymetry, estimates site depth based on:
#' - Distance from shore (closer = shallower)
#' - Lake mean/max depth relationship
#'
#' Uses a simple linear interpolation assuming depth increases
#' linearly from shore (0m) to center (max_depth).
#'
#' @noRd
estimate_site_depth <- function(site_point, lake_polygon, depth_mean,
                                depth_max = NULL, position = "unknown") {

  if (is.na(depth_mean)) return(NA_real_)
  if (is.null(depth_max) || is.na(depth_max)) depth_max <- depth_mean * 2.5

  # Calculate relative distance from shore to center
  # Get distance to shore
  shore <- sf::st_boundary(lake_polygon)
  dist_to_shore <- as.numeric(sf::st_distance(site_point, shore))

  # Get distance to centroid (approximate center)
  center <- sf::st_centroid(sf::st_union(lake_polygon))
  dist_to_center <- as.numeric(sf::st_distance(site_point, center))

  # Total distance shore to center
  dist_shore_to_center <- as.numeric(sf::st_distance(shore, center))

  # Relative position (0 = at shore, 1 = at center)
  if (dist_shore_to_center > 0) {
    relative_pos <- dist_to_shore / (dist_to_shore + dist_to_center)
  } else {
    relative_pos <- 0.5  # Default to middle
  }

  # Estimate depth using parabolic profile

  # Depth at position r: D(r) = Dmax * (1 - (1-r)^2) approximately
  # This gives shallower near shore, deeper in middle
  estimated_depth <- depth_max * (1 - (1 - relative_pos)^2)

  # Don't let it exceed max or go below 0.5m
  estimated_depth <- max(0.5, min(estimated_depth, depth_max))

  return(estimated_depth)
}
