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
#' @param depth_m Water depth in meters for orbital velocity calculation.
#'   Can be a single value (applied to all sites), a vector (one per site),
#'   or NULL to use depth from sites data or default from options.
#' @param fetch_method Method for calculating effective fetch. Options:
#'   \describe{
#'     \item{"top3"}{Mean of the 3 highest directional fetch values (default)}
#'     \item{"max"}{Maximum directional fetch value}
#'     \item{"cosine"}{SPM/CERC cosine-weighted average. Uses 9 radials centered
#'       on the direction of maximum fetch at 6-degree intervals, weighted by
#'       cosine of angle from center. Based on Shore Protection Manual (1984).}
#'   }
#'   If NULL, uses the value from \code{\link{lakefetch_options}}.
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
#'   \item Calculates orbital velocity using depth
#'   \item Derives exposure category (Sheltered/Moderate/Exposed)
#' }
#'
#' Exposure thresholds can be configured via \code{\link{lakefetch_options}}.
#'
#' @references
#' Shore Protection Manual (1984). U.S. Army Corps of Engineers, Coastal
#' Engineering Research Center. 4th Edition.
#'
#' @examples
#' \dontrun{
#' sites <- load_sites("my_sites.csv")
#' lake <- get_lake_boundary(sites)
#' results <- fetch_calculate(sites, lake)
#'
#' # With explicit depth
#' results <- fetch_calculate(sites, lake, depth_m = 5)
#'
#' # Using cosine-weighted effective fetch (SPM method)
#' results <- fetch_calculate(sites, lake, fetch_method = "cosine")
#'
#' # Access results
#' results$results  # sf with all fetch data
#' results$lakes    # lake polygons
#' }
#'
#' @export
fetch_calculate <- function(sites, lake, depth_m = NULL, fetch_method = NULL,
                            add_context = TRUE) {

  # Extract components from lake data
  all_lakes <- lake$all_lakes
  sites_sf <- lake$sites
  utm_epsg <- lake$utm_epsg

  # Resolve fetch method
  if (is.null(fetch_method)) {
    fetch_method <- get_opt("fetch_method")
  }
  fetch_method <- match.arg(fetch_method, c("top3", "max", "cosine"))
  message("Effective fetch method: ", fetch_method)

  # Resolve depth values
  n_sites <- nrow(sites_sf)
  if (!is.null(depth_m)) {
    # User provided depth
    if (length(depth_m) == 1) {
      depth_vec <- rep(depth_m, n_sites)
    } else if (length(depth_m) == n_sites) {
      depth_vec <- depth_m
    } else {
      stop("depth_m must be a single value or a vector with one value per site")
    }
    message("Using provided depth: ", paste(unique(depth_vec), collapse = ", "), " m")
  } else if ("depth_m" %in% names(sites_sf)) {
    # Depth from sites data
    depth_vec <- sites_sf$depth_m
    depth_vec[is.na(depth_vec)] <- get_opt("default_depth_m")
    message("Using depth from sites data (default for missing: ", get_opt("default_depth_m"), " m)")
  } else {
    # Use default
    depth_vec <- rep(get_opt("default_depth_m"), n_sites)
    message("Using default depth: ", get_opt("default_depth_m"), " m")
  }

  # Assign sites to lakes
  sites_with_lakes <- assign_sites_to_lakes(
    sites_sf,
    all_lakes,
    tolerance_m = get_opt("gps_tolerance_m")
  )

  # Add depth to sites

  sites_with_lakes$depth_m <- depth_vec

  # Calculate fetch (handles multiple lakes)
  fetch_data <- calculate_fetch_multi_lake(
    sites_with_lakes,
    all_lakes,
    utm_epsg,
    fetch_method = fetch_method
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
#' @param fetch_method Method for effective fetch: "top3", "max", or "cosine"
#'
#' @return List with results, lakes, and angles
#' @noRd
calculate_fetch_multi_lake <- function(sites_with_lakes, all_lakes, utm_epsg,
                                        fetch_method = "top3") {

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
  process_one_lake <- function(lake_id, all_lakes_data, sites_data, utm_code,
                                eff_fetch_method) {
    lake_poly <- all_lakes_data[which(all_lakes_data$osm_id == lake_id), ]
    lake_sites <- sites_data[which(sites_data$lake_osm_id == lake_id), ]
    lake_nm <- lake_sites$lake_name[1]

    result <- calculate_fetch_single_lake(
      sites = lake_sites,
      lake_polygon = lake_poly,
      utm_epsg = utm_code,
      lake_name = lake_nm,
      lake_osm_id = lake_id,
      fetch_method = eff_fetch_method
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
      "calc_orbital", "calc_effective_fetch", "get_opt"
    ), envir = environment())

    parallel::clusterExport(cl, c("all_lakes", "sites_with_lakes", "utm_epsg",
                                   "fetch_method", "process_one_lake"),
                             envir = environment())

    parallel::clusterEvalQ(cl, library(sf))

    results_list <- parallel::parLapply(cl, lakes_with_sites, function(lid) {
      process_one_lake(lid, all_lakes, sites_with_lakes, utm_epsg, fetch_method)
    })

    parallel::stopCluster(cl)
  } else {
    message("Using sequential processing")
    results_list <- lapply(lakes_with_sites, function(lid) {
      process_one_lake(lid, all_lakes, sites_with_lakes, utm_epsg, fetch_method)
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
#' @param fetch_method Method for effective fetch: "top3", "max", or "cosine"
#'
#' @return List with results, lake, and angles
#' @noRd
calculate_fetch_single_lake <- function(sites, lake_polygon, utm_epsg,
                                         lake_name = NULL, lake_osm_id = NULL,
                                         fetch_method = "top3") {

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

  # Calculate effective fetch using specified method
  results$fetch_effective <- calc_effective_fetch(fetch_values, angles, fetch_method)

  # Calculate orbital velocity using depth (depth_m column should exist from sites_with_lakes)
  site_depth <- if ("depth_m" %in% names(results)) results$depth_m else get_opt("default_depth_m")
  results$orbital_effective <- calc_orbital(results$fetch_effective, depth_m = site_depth)

  # Exposure classification using configurable thresholds
  sheltered_threshold <- get_opt("exposure_sheltered_m")
  exposed_threshold <- get_opt("exposure_exposed_m")
  results$exposure_category <- ifelse(results$fetch_effective < sheltered_threshold, "Sheltered",
                                      ifelse(results$fetch_effective > exposed_threshold, "Exposed",
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

#' Calculate Effective Fetch
#'
#' Calculate effective fetch using one of three methods.
#'
#' @param fetch_matrix Matrix of directional fetch values (rows = sites, cols = angles)
#' @param angles Numeric vector of angles in degrees
#' @param method Method: "top3", "max", or "cosine"
#'
#' @return Numeric vector of effective fetch values
#' @noRd
calc_effective_fetch <- function(fetch_matrix, angles, method = "top3") {

  if (method == "max") {
    # Simple maximum
    return(apply(fetch_matrix, 1, max, na.rm = TRUE))

  } else if (method == "top3") {
    # Mean of 3 highest values
    return(apply(fetch_matrix, 1, function(x) {
      mean(sort(x, decreasing = TRUE)[1:3], na.rm = TRUE)
    }))

  } else if (method == "cosine") {
    # SPM/CERC cosine-weighted method
    # Uses 9 radials centered on direction of maximum fetch, at 6-degree intervals
    # Weighted by cosine of angle from center direction
    # Reference: Shore Protection Manual (1984), Equation 3-36

    angle_resolution <- if (length(angles) > 1) angles[2] - angles[1] else 5
    n_radials <- 9
    radial_spacing <- 6  # degrees

    return(apply(fetch_matrix, 1, function(fetch_row) {
      # Find direction of maximum fetch
      max_idx <- which.max(fetch_row)
      max_angle <- angles[max_idx]

      # Calculate angles for 9 radials centered on max direction
      offsets <- seq(-(n_radials - 1) / 2, (n_radials - 1) / 2) * radial_spacing
      radial_angles <- (max_angle + offsets) %% 360

      # Get fetch values at these angles (interpolate if needed)
      radial_fetches <- sapply(radial_angles, function(a) {
        # Find closest angle in our measurements
        angle_diff <- abs(angles - a)
        angle_diff <- pmin(angle_diff, 360 - angle_diff)  # Handle wrap-around
        closest_idx <- which.min(angle_diff)
        fetch_row[closest_idx]
      })

      # Cosine weights (in radians)
      cos_weights <- cos(offsets * pi / 180)

      # Weighted average
      sum(radial_fetches * cos_weights) / sum(cos_weights)
    }))
  } else {
    stop("Unknown fetch method: ", method)
  }
}

#' Calculate Orbital Velocity
#'
#' Estimate wave-induced near-bottom orbital velocity based on fetch, wind speed,
#' and water depth using SMB (Sverdrup-Munk-Bretschneider) wave equations.
#'
#' @param fetch_m Fetch distance in meters
#' @param depth_m Water depth in meters
#' @param wind_speed Wind speed in m/s
#'
#' @return Orbital velocity in m/s
#'
#' @details
#' Uses simplified SMB equations from the Shore Protection Manual (1984):
#' \itemize{
#'   \item Wave height: H_s = 0.0016 * sqrt(F) * U (simplified deep-water form)
#'   \item Wave period: T_p = 0.286 * F^0.33 * U^0.33
#'   \item Orbital velocity: U_b = (pi * H / T) * exp(-k * d)
#' }
#'
#' The full SMB equations use hyperbolic tangent functions for finite-depth
#' effects. This simplified version is appropriate for small to medium lakes
#' where fetch-limited conditions dominate.
#'
#' @references
#' Shore Protection Manual (1984). U.S. Army Corps of Engineers, Coastal
#' Engineering Research Center. 4th Edition.
#'
#' Sverdrup, H. U., & Munk, W. H. (1947). Wind, sea, and swell: theory of
#' relations for forecasting. U.S. Navy Hydrographic Office, Pub. No. 601.
#'
#' @noRd
calc_orbital <- function(fetch_m, depth_m = NULL, wind_speed = NULL) {
  if (is.null(wind_speed)) {
    wind_speed <- get_opt("default_wind_speed_ms")
  }
  if (is.null(depth_m)) {
    depth_m <- get_opt("default_depth_m")
  }


  # SMB wave hindcast equations
  # Significant wave height (m): H_s = 0.0016 * sqrt(F) * U
  H_sig <- 0.0016 * sqrt(fetch_m) * wind_speed

  # Wave period (s): T_p = 0.286 * F^0.33 * U^0.33
  T_p <- 0.286 * (fetch_m^0.33) * (wind_speed^0.33)

  # Avoid division by zero

  T_p <- pmax(T_p, 0.1)

  # Deep water wavelength: L = 1.56 * T^2
  wavelength <- 1.56 * T_p^2

  # Wave number: k = 2*pi / L
  k <- 2 * pi / wavelength

  # Near-bottom orbital velocity: U_b = (pi * H / T) * exp(-k * d)
  orbital_velocity <- (pi * H_sig / T_p) * exp(-k * depth_m)

  return(orbital_velocity)
}
