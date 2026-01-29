# ==============================================================================
# Core Fetch Calculation Functions
# ==============================================================================

#' Calculate Fetch for Lake Sampling Sites
#'
#' Main entry point for fetch calculation. Takes sites and lake boundaries,
#' calculates directional fetch using ray-casting, and returns results with
#' exposure metrics.
#'
#' @param sites Data frame or sf object with site locations
#' @param lake Lake boundary data from \code{\link{get_lake_boundary}}
#' @param add_context Logical; add NHD context if available (default TRUE)
#'
#' @return A list with elements:
#'   \item{results}{sf object with fetch results for each site}
#'   \item{lakes}{sf object with lake polygons used}
#'   \item{angles}{Vector of angles used for fetch calculation}
#'
#' @details
#' For each site, the function:
#' \enumerate{
#'   \item Assigns the site to its containing lake polygon
#'   \item Buffers the site inward from shore (GPS accuracy adjustment)
#'   \item Casts rays in all directions at specified angle resolution
#'   \item Measures distance to shore in each direction
#'   \item Calculates summary metrics (mean, max, effective fetch)
#'   \item Derives exposure category (Sheltered/Moderate/Exposed)
#' }
#'
#' @examples
#' \dontrun{
#' sites <- load_sites("my_sites.csv")
#' lake <- get_lake_boundary(sites)
#' results <- fetch_calculate(sites, lake)
#'
#' # Access results
#' results$results  # sf with all fetch data
#' results$lakes    # lake polygons
#' }
#'
#' @export
fetch_calculate <- function(sites, lake, add_context = TRUE) {

  # Extract components from lake data
  all_lakes <- lake$all_lakes
  sites_sf <- lake$sites
  utm_epsg <- lake$utm_epsg

  # Assign sites to lakes
  sites_with_lakes <- assign_sites_to_lakes(
    sites_sf,
    all_lakes,
    tolerance_m = get_opt("gps_tolerance_m")
  )

  # Calculate fetch (handles multiple lakes)
  fetch_data <- calculate_fetch_multi_lake(
    sites_with_lakes,
    all_lakes,
    utm_epsg
  )

  # Add lake context if requested and available
  if (add_context && nhd_available()) {
    fetch_data$results <- add_lake_context(
      fetch_data$results,
      fetch_data$lakes,
      utm_epsg
    )
  }

  return(fetch_data)
}

#' Calculate Fetch for Multiple Lakes
#'
#' @param sites_with_lakes sf object with sites assigned to lakes
#' @param all_lakes sf object with all lake polygons
#' @param utm_epsg EPSG code for UTM projection
#'
#' @return List with results, lakes, and angles
#' @noRd
calculate_fetch_multi_lake <- function(sites_with_lakes, all_lakes, utm_epsg) {

  message("Calculating fetch for multiple lakes...")
  message("  Buffering sites ", get_opt("buffer_distance_m"), "m inward")
  message("  Angle resolution: ", get_opt("angle_resolution_deg"), " degrees")

  # Get unique lakes that have sites
  lakes_with_sites <- unique(sites_with_lakes$lake_osm_id)
  lakes_with_sites <- lakes_with_sites[!is.na(lakes_with_sites)]

  # Warn about unmatched sites
  unmatched <- sites_with_lakes[is.na(sites_with_lakes$lake_osm_id), ]
  if (nrow(unmatched) > 0) {
    warning("Skipping ", nrow(unmatched), " sites not matched to any lake: ",
            paste(unmatched$Site, collapse = ", "))
  }

  if (length(lakes_with_sites) == 0) {
    stop("No sites matched to any lake. Cannot calculate fetch.")
  }

  message("Processing ", length(lakes_with_sites), " lake(s)...")

  # Define function to process one lake
  process_one_lake <- function(lake_id, all_lakes_data, sites_data, utm_code) {
    lake_poly <- all_lakes_data[which(all_lakes_data$osm_id == lake_id), ]
    lake_sites <- sites_data[which(sites_data$lake_osm_id == lake_id), ]
    lake_nm <- lake_sites$lake_name[1]

    result <- calculate_fetch_single_lake(
      sites = lake_sites,
      lake_polygon = lake_poly,
      utm_epsg = utm_code,
      lake_name = lake_nm,
      lake_osm_id = lake_id
    )

    return(result)
  }

  # Process lakes (parallel if multiple lakes and parallel is available)
  if (parallel_available() && length(lakes_with_sites) > 1) {
    n_cores <- min(length(lakes_with_sites), max(1, parallel::detectCores() - 1))
    message("Using parallel processing with ", n_cores, " cores for ",
            length(lakes_with_sites), " lakes")

    cl <- parallel::makeCluster(n_cores)

    # Export required functions and data
    parallel::clusterExport(cl, c(
      "calculate_fetch_single_lake", "get_highres_fetch", "nudge_inward",
      "calc_orbital", "get_opt"
    ), envir = environment())

    parallel::clusterExport(cl, c("all_lakes", "sites_with_lakes", "utm_epsg",
                                   "process_one_lake"),
                             envir = environment())

    parallel::clusterEvalQ(cl, library(sf))

    results_list <- parallel::parLapply(cl, lakes_with_sites, function(lid) {
      process_one_lake(lid, all_lakes, sites_with_lakes, utm_epsg)
    })

    parallel::stopCluster(cl)
  } else {
    message("Using sequential processing")
    results_list <- lapply(lakes_with_sites, function(lid) {
      process_one_lake(lid, all_lakes, sites_with_lakes, utm_epsg)
    })
  }

  message("Fetch calculation complete.")

  # Combine results from all lakes
  all_results <- do.call(rbind, lapply(results_list, function(x) x$results))
  all_lake_polys <- do.call(rbind, lapply(results_list, function(x) x$lake))
  angles <- results_list[[1]]$angles

  return(list(
    results = all_results,
    lakes = all_lake_polys,
    angles = angles
  ))
}

#' Calculate Fetch for a Single Lake
#'
#' @param sites sf object with site points
#' @param lake_polygon sf object with lake polygon
#' @param utm_epsg EPSG code for UTM projection
#' @param lake_name Character lake name
#' @param lake_osm_id Character OSM ID
#'
#' @return List with results, lake, and angles
#' @noRd
calculate_fetch_single_lake <- function(sites, lake_polygon, utm_epsg,
                                         lake_name = NULL, lake_osm_id = NULL) {

  n_sites <- nrow(sites)
  if (n_sites == 0) {
    return(NULL)
  }

  message("  Processing ", n_sites, " samples in lake: ",
          ifelse(is.null(lake_name) || is.na(lake_name), lake_osm_id, lake_name))

  # Optimize: identify unique coordinates to avoid recalculating fetch
  coords_all <- sf::st_coordinates(sites)
  coord_key <- paste(round(coords_all[, 1], 6), round(coords_all[, 2], 6), sep = "_")
  unique_coords <- !duplicated(coord_key)
  n_unique <- sum(unique_coords)

  if (n_unique < n_sites) {
    message("    (", n_unique, " unique locations, ", n_sites - n_unique, " repeat samples)")
  }

  # If multiple polygons (duplicates), union them into one
  if (nrow(lake_polygon) > 1) {
    unified_geom <- sf::st_union(lake_polygon)
    lake_polygon <- sf::st_sf(
      data.frame(osm_id = lake_osm_id, name = lake_name, stringsAsFactors = FALSE),
      geometry = sf::st_sfc(unified_geom, crs = utm_epsg)
    )
  }

  # Ensure valid geometry
  if (!all(sf::st_is_valid(lake_polygon))) {
    lake_polygon <- sf::st_make_valid(lake_polygon)
  }

  lake_boundary <- tryCatch({
    sf::st_cast(lake_polygon, "MULTILINESTRING")
  }, error = function(e) {
    tryCatch({
      sf::st_cast(sf::st_boundary(lake_polygon), "MULTILINESTRING")
    }, error = function(e2) {
      sf::st_boundary(lake_polygon)
    })
  })

  # Buffer all sites inward (only unique locations need nudging)
  sites_buffered <- sites
  new_geoms <- vector("list", n_sites)
  nudged_cache <- list()  # Cache nudged positions by coord_key

  for (i in seq_len(n_sites)) {
    key <- coord_key[i]
    if (is.null(nudged_cache[[key]])) {
      # First time seeing this coordinate - calculate nudge
      nudged_cache[[key]] <- nudge_inward(sf::st_geometry(sites)[i],
                                           lake_boundary,
                                           lake_polygon)
    }
    new_geoms[[i]] <- nudged_cache[[key]]
  }

  sf::st_geometry(sites_buffered) <- sf::st_sfc(new_geoms, crs = utm_epsg)

  # Calculate fetch (only for unique coordinates)
  angle_res <- get_opt("angle_resolution_deg")
  angles <- seq(0, 360 - angle_res, by = angle_res)

  fetch_cache <- list()  # Cache fetch results by coord_key
  fetch_list <- vector("list", n_sites)

  for (i in seq_len(n_sites)) {
    key <- coord_key[i]
    if (is.null(fetch_cache[[key]])) {
      # First time seeing this coordinate - calculate fetch
      fetch_cache[[key]] <- get_highres_fetch(sites_buffered[i, ],
                                               lake_boundary,
                                               lake_polygon,
                                               angles)
    }
    fetch_list[[i]] <- fetch_cache[[key]]
  }

  # Convert to data frame
  fetch_matrix <- do.call(rbind, fetch_list)
  colnames(fetch_matrix) <- paste0("fetch_", angles)
  fetch_df <- as.data.frame(fetch_matrix)

  # Combine with site data
  results <- cbind(sf::st_drop_geometry(sites_buffered), fetch_df)
  results_sf <- sf::st_sf(results, geometry = sf::st_geometry(sites_buffered))

  # Calculate metrics
  fetch_cols <- grep("^fetch_[0-9]", names(results), value = TRUE)
  fetch_values <- as.matrix(results[, fetch_cols])

  results$fetch_mean <- rowMeans(fetch_values, na.rm = TRUE)
  results$fetch_max <- apply(fetch_values, 1, max, na.rm = TRUE)

  # Effective fetch (mean of top 3)
  results$fetch_effective <- apply(fetch_values, 1, function(x) {
    mean(sort(x, decreasing = TRUE)[1:3], na.rm = TRUE)
  })

  results$orbital_effective <- calc_orbital(results$fetch_effective)

  results$exposure_category <- ifelse(results$fetch_effective < 2500, "Sheltered",
                                      ifelse(results$fetch_effective > 5000, "Exposed",
                                             "Moderate"))

  results_sf <- sf::st_sf(results, geometry = sf::st_geometry(sites_buffered))

  return(list(
    results = results_sf,
    lake = lake_polygon,
    angles = angles
  ))
}

#' Get Directional Fetch via Ray-Casting
#'
#' @param pt sf point geometry
#' @param boundary sf line geometry (lake boundary)
#' @param poly sf polygon geometry (lake)
#' @param angles Numeric vector of angles to cast rays
#'
#' @return Numeric vector of fetch distances
#' @noRd
get_highres_fetch <- function(pt, boundary, poly, angles) {

  max_d <- get_opt("max_fetch_m")
  validation_buffer <- get_opt("validation_buffer_m")

  coords <- sf::st_coordinates(pt)
  x0 <- coords[1]
  y0 <- coords[2]
  f_dists <- numeric(length(angles))
  pt_crs <- sf::st_crs(pt)

  for (i in seq_along(angles)) {
    deg <- angles[i]
    rad <- deg * (pi / 180)

    x1 <- x0 + max_d * sin(rad)
    y1 <- y0 + max_d * cos(rad)
    ray_geom <- sf::st_sfc(sf::st_linestring(rbind(c(x0, y0), c(x1, y1))), crs = pt_crs)

    # Calculate intersection with error handling
    inter <- tryCatch({
      suppressWarnings(sf::st_intersection(ray_geom, boundary))
    }, error = function(e) {
      sf::st_sfc(crs = pt_crs)
    })

    if (length(inter) == 0) {
      f_dists[i] <- max_d
    } else {
      dists <- sort(as.numeric(sf::st_distance(pt, inter)))
      dists <- dists[dists > 1]

      final_dist <- max_d

      if (length(dists) > 0) {
        final_dist <- dists[1]

        for (d in dists) {
          check_dist <- d + validation_buffer
          if (check_dist >= max_d) break

          xt <- x0 + check_dist * sin(rad)
          yt <- y0 + check_dist * cos(rad)
          test_pt <- sf::st_sfc(sf::st_point(c(xt, yt)), crs = pt_crs)

          is_water <- length(sf::st_intersects(test_pt, poly)[[1]]) > 0

          if (is_water) {
            next
          } else {
            final_dist <- d
            break
          }
        }
      }
      f_dists[i] <- final_dist
    }
  }

  return(f_dists)
}

#' Nudge Point Inward from Shore
#'
#' Buffer a point inward from the nearest shore to account for GPS inaccuracy.
#' Only nudges points that are already close to the shoreline (within the
#' buffer distance threshold). Points far from shore are left at their
#' original location.
#'
#' @param point_geom sf point geometry
#' @param poly_boundary sf line geometry (lake boundary)
#' @param poly_full sf polygon geometry (lake)
#' @param dist Buffer distance in meters
#'
#' @return sf point geometry
#' @noRd
nudge_inward <- function(point_geom, poly_boundary, poly_full, dist = NULL) {

  if (is.null(dist)) {
    dist <- get_opt("buffer_distance_m")
  }

  # Extract the sfg point from sfc if needed (for consistent return type)
  original_point <- if (inherits(point_geom, "sfc")) point_geom[[1]] else point_geom
  original_coords <- sf::st_coordinates(point_geom)

  # Handle empty or invalid geometries
  if (is.null(poly_boundary) || sf::st_is_empty(poly_boundary) ||
      is.null(poly_full) || sf::st_is_empty(poly_full)) {
    return(original_point)
  }

  is_inside <- length(sf::st_intersects(point_geom, poly_full)[[1]]) > 0

  nearest_on_shore <- tryCatch({
    sf::st_nearest_points(point_geom, poly_boundary)
  }, error = function(e) NULL)

  if (is.null(nearest_on_shore) || length(nearest_on_shore) == 0) {
    return(original_point)
  }

  coords <- sf::st_coordinates(nearest_on_shore)

  if (is.null(coords) || nrow(coords) < 2) {
    return(original_point)
  }

  p_x <- coords[1, 1]
  p_y <- coords[1, 2]
  s_x <- coords[2, 1]
  s_y <- coords[2, 2]

  dx <- p_x - s_x
  dy <- p_y - s_y
  len <- sqrt(dx^2 + dy^2)

  if (len == 0) return(original_point)

  # Only nudge points that are close to the shoreline (within buffer distance)
  # Points far from shore are left at their original location
  if (len > dist) {
    return(original_point)
  }

  if (!is_inside) {
    dx <- -dx
    dy <- -dy
  }

  new_x <- s_x + (dx / len) * dist
  new_y <- s_y + (dy / len) * dist

  return(sf::st_point(c(new_x, new_y)))
}

#' Calculate Orbital Velocity Approximation
#'
#' Estimate wave-induced orbital velocity based on fetch and wind speed.
#'
#' @param fetch_m Fetch distance in meters
#' @param wind_speed Wind speed in m/s
#'
#' @return Orbital velocity in m/s
#' @noRd
calc_orbital <- function(fetch_m, wind_speed = NULL) {
  if (is.null(wind_speed)) {
    wind_speed <- get_opt("default_wind_speed_ms")
  }
  H_sig <- 0.0016 * sqrt(fetch_m) * wind_speed
  return(H_sig)
}
