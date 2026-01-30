# ==============================================================================
# Lake Boundary Acquisition Functions
# ==============================================================================

#' Get Lake Boundary
#'
#' Get lake boundary polygon(s) either from OpenStreetMap or from a local file.
#'
#' @param sites A data.frame with latitude and longitude columns, or an sf object.
#' @param file Optional file path to a shapefile or geopackage with lake boundaries.
#'
#' @return A list with elements:
#'   \item{all_lakes}{sf object with lake polygons in UTM projection}
#'   \item{sites}{sf object with site points in UTM projection}
#'   \item{utm_epsg}{EPSG code for the UTM projection used}
#'
#' @details
#' If \code{file} is provided, the lake boundary is loaded from the file.
#' Otherwise, the function downloads lake boundaries from OpenStreetMap
#' based on the bounding box of the provided sites.
#'
#' @examples
#' \dontrun{
#' sites <- load_sites("my_sites.csv")
#' lake_data <- get_lake_boundary(sites)
#'
#' # Or with a local file
#' lake_data <- get_lake_boundary(sites, file = "lake_boundary.shp")
#' }
#'
#' @export
get_lake_boundary <- function(sites, file = NULL) {
  if (!is.null(file)) {
    return(load_lake_file(sites, file))
  } else {
    return(download_lake_osm(sites))
  }
}

#' Download Lake Boundary from OpenStreetMap
#'
#' @param sites_df Data frame with latitude and longitude columns
#'
#' @return A list with all_lakes, sites, and utm_epsg
#'
#' @noRd
download_lake_osm <- function(sites_df) {

  message("Converting to spatial format...")

  # Disable S2 spherical geometry to avoid topology errors
  sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(TRUE), add = TRUE)

  # Convert to sf object (WGS84)
  if (inherits(sites_df, "sf")) {
    sites_sf <- sf::st_transform(sites_df, 4326)
  } else {
    sites_sf <- sf::st_as_sf(sites_df,
                              coords = c("longitude", "latitude"),
                              crs = 4326)
  }

  # Create bounding box with buffer for better OSM coverage
  bbox <- sf::st_bbox(sites_sf)
  bbox_buffer <- 0.15
  bbox[1] <- bbox[1] - bbox_buffer
  bbox[2] <- bbox[2] - bbox_buffer
  bbox[3] <- bbox[3] + bbox_buffer
  bbox[4] <- bbox[4] + bbox_buffer

  # Convert bbox to vector format for osmdata
  bbox_vec <- c(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])
  names(bbox_vec) <- c("left", "bottom", "right", "top")

  message("Downloading lake boundaries from OpenStreetMap...")
  message("  Bounding box: [", paste(round(bbox_vec, 4), collapse = ", "), "]")

  # Helper to extract polygons from OSM response
  extract_osm_polys <- function(osm_data) {
    result <- list()
    if (!is.null(osm_data$osm_polygons) && nrow(osm_data$osm_polygons) > 0) {
      std_poly <- standardize_osm_sf(osm_data$osm_polygons)
      if (!is.null(std_poly) && nrow(std_poly) > 0) {
        result[[length(result) + 1]] <- std_poly
        message("    Found ", nrow(std_poly), " polygons")
      }
    }
    if (!is.null(osm_data$osm_multipolygons) && nrow(osm_data$osm_multipolygons) > 0) {
      std_mpoly <- standardize_osm_sf(osm_data$osm_multipolygons)
      if (!is.null(std_mpoly) && nrow(std_mpoly) > 0) {
        result[[length(result) + 1]] <- std_mpoly
        message("    Found ", nrow(std_mpoly), " multipolygons")
      }
    }
    return(result)
  }

  # Overpass servers to try (main server often overloaded)
  overpass_servers <- c(
    "https://overpass-api.de/api/interpreter",
    "https://overpass.kumi.systems/api/interpreter",
    "https://maps.mail.ru/osm/tools/overpass/api/interpreter"
  )

  # Helper to query OSM with retries across multiple servers
  query_osm_robust <- function(bbox, key, value, max_attempts = 3) {
    for (attempt in seq_len(max_attempts)) {
      # Rotate through servers
      server <- overpass_servers[((attempt - 1) %% length(overpass_servers)) + 1]

      tryCatch({
        # Set the Overpass server
        osmdata::set_overpass_url(server)

        osm_query <- osmdata::opq(bbox = bbox, timeout = 90)
        osm_query <- osmdata::add_osm_feature(osm_query, key = key, value = value)
        result <- osmdata::osmdata_sf(osm_query)

        # Reset to default server
        osmdata::set_overpass_url(overpass_servers[1])
        return(result)

      }, error = function(e) {
        msg <- conditionMessage(e)
        if (attempt < max_attempts) {
          # Check if it's a server error worth retrying
          if (grepl("500|502|503|504|timeout|Timeout", msg, ignore.case = TRUE)) {
            wait_time <- attempt * 3
            message("    Server error, trying another server in ", wait_time, "s...")
            Sys.sleep(wait_time)
          } else {
            message("    Error: ", msg)
          }
        } else {
          message("    Failed after ", max_attempts, " attempts: ", msg)
        }
        NULL
      })
    }
    # Reset to default server on failure
    tryCatch(osmdata::set_overpass_url(overpass_servers[1]), error = function(e) NULL)
    return(NULL)
  }

  water_list <- list()

  # Query for natural=water (catches most lakes, ponds, reservoirs)
  message("  Querying natural=water...")
  osm_result <- query_osm_robust(bbox_vec, "natural", "water")
  if (!is.null(osm_result)) {
    water_list <- c(water_list, extract_osm_polys(osm_result))
  }

  # Also query water=lake (some lakes only have this tag)
  message("  Querying water=lake...")
  osm_result <- query_osm_robust(bbox_vec, "water", "lake")
  if (!is.null(osm_result)) {
    water_list <- c(water_list, extract_osm_polys(osm_result))
  }

  # Auto-detect UTM zone from sites
  site_coords <- if (inherits(sites_df, "sf")) {
    sf::st_coordinates(sf::st_centroid(sf::st_union(sites_df)))
  } else {
    c(mean(sites_df$longitude), mean(sites_df$latitude))
  }
  utm_zone <- floor((site_coords[1] + 180) / 6) + 1
  utm_epsg <- ifelse(site_coords[2] >= 0,
                     as.numeric(paste0("326", sprintf("%02d", utm_zone))),
                     as.numeric(paste0("327", sprintf("%02d", utm_zone))))

  # Check if we found ANY water bodies
  if (length(water_list) == 0) {
    warning("No water bodies found in OpenStreetMap - creating approximate boundary")

    # FALLBACK: Create a buffer around all points
    sites_utm_temp <- sf::st_transform(sites_sf, utm_epsg)
    buffer_dist <- 5000
    buffered <- sf::st_buffer(sites_utm_temp, dist = buffer_dist)
    lake_approx_utm <- sf::st_union(buffered)
    lake_approx_utm <- sf::st_convex_hull(lake_approx_utm)

    lake_approx_utm <- sf::st_sf(
      name = "Approximate Boundary",
      osm_id = "fallback",
      area_km2 = as.numeric(sf::st_area(lake_approx_utm)) / 1e6,
      geometry = sf::st_geometry(lake_approx_utm)
    )

    return(list(
      all_lakes = lake_approx_utm,
      sites = sites_utm_temp,
      utm_epsg = utm_epsg
    ))
  }

  # Combine all water polygons
  message("  Combining results...")
  all_water <- do.call(rbind, water_list)
  message("  Total water bodies found: ", nrow(all_water))

  message("  Auto-detected UTM Zone: ", utm_zone,
          ifelse(site_coords[2] >= 0, "N", "S"))

  # Transform EVERYTHING to UTM before spatial operations
  message("  Transforming to UTM for analysis...")
  all_water_utm <- sf::st_transform(all_water, utm_epsg)
  sites_utm <- sf::st_transform(sites_sf, utm_epsg)

  # Process each lake polygon - extract largest part if multipolygon
  message("  Processing lake polygons...")
  processed_lakes <- list()

  for (i in seq_len(nrow(all_water_utm))) {
    lake_poly <- all_water_utm[i, ]
    geom_type <- sf::st_geometry_type(lake_poly, by_geometry = FALSE)

    if (geom_type %in% c("MULTIPOLYGON", "GEOMETRYCOLLECTION")) {
      lake_parts <- sf::st_cast(lake_poly, "POLYGON")
      lake_areas <- sf::st_area(lake_parts)
      lake_poly <- lake_parts[which.max(lake_areas), ]
    }

    if (!all(sf::st_is_valid(lake_poly))) {
      lake_poly <- sf::st_make_valid(lake_poly)
    }

    processed_lakes[[i]] <- lake_poly
  }

  all_lakes_utm <- do.call(rbind, processed_lakes)

  # Calculate areas for reference
  all_lakes_utm$area_km2 <- as.numeric(sf::st_area(all_lakes_utm)) / 1e6

  # Deduplicate by osm_id (keep largest polygon if duplicates)
  unique_ids <- unique(all_lakes_utm$osm_id)
  if (length(unique_ids) < nrow(all_lakes_utm)) {
    message("  Deduplicating ", nrow(all_lakes_utm) - length(unique_ids),
            " duplicate lake entries...")
    deduped_list <- lapply(unique_ids, function(id) {
      matches <- all_lakes_utm[all_lakes_utm$osm_id == id, ]
      if (nrow(matches) > 1) {
        matches[which.max(matches$area_km2), ]
      } else {
        matches
      }
    })
    all_lakes_utm <- do.call(rbind, deduped_list)
  }

  message("  Processed ", nrow(all_lakes_utm), " unique lake polygons")
  message("  Total area: ", round(sum(all_lakes_utm$area_km2), 2), " km2")

  return(list(
    all_lakes = all_lakes_utm,
    sites = sites_utm,
    utm_epsg = utm_epsg
  ))
}

#' Standardize and Validate OSM sf Data
#'
#' @param osm_sf sf object from osmdata
#' @return Standardized sf object with osm_id, name, geometry
#' @noRd
standardize_osm_sf <- function(osm_sf) {
  if (is.null(osm_sf) || nrow(osm_sf) == 0) return(NULL)

  # Make geometries valid first
  osm_sf <- sf::st_make_valid(osm_sf)

  # Find columns case-insensitively
  col_names_lower <- tolower(names(osm_sf))
  osm_id_col <- which(col_names_lower == "osm_id")[1]
  name_col <- which(col_names_lower == "name")[1]

  # Extract only essential columns
  result <- data.frame(
    osm_id = if (!is.na(osm_id_col)) as.character(osm_sf[[osm_id_col]]) else NA_character_,
    name = if (!is.na(name_col)) as.character(osm_sf[[name_col]]) else NA_character_,
    stringsAsFactors = FALSE
  )
  result <- sf::st_sf(result, geometry = sf::st_geometry(osm_sf))

  # Remove any invalid geometries
  valid_mask <- sf::st_is_valid(result)
  if (!all(valid_mask)) {
    message("      (Removed ", sum(!valid_mask), " invalid geometries)")
    result <- result[valid_mask, ]
  }

  return(result)
}

#' Load Lake from Local File
#'
#' @param sites_df Data frame with latitude and longitude columns
#' @param lake_file_path Path to shapefile or geopackage
#'
#' @return A list with all_lakes, sites, and utm_epsg
#'
#' @noRd
load_lake_file <- function(sites_df, lake_file_path) {

  message("Loading lake boundary from file: ", lake_file_path)

  # Convert sites to sf
  if (inherits(sites_df, "sf")) {
    sites_sf <- sf::st_transform(sites_df, 4326)
  } else {
    sites_sf <- sf::st_as_sf(sites_df,
                              coords = c("longitude", "latitude"),
                              crs = 4326)
  }

  # Load lake shapefile
  lake_wgs84 <- sf::st_read(lake_file_path, quiet = TRUE)

  if (!sf::st_crs(lake_wgs84)$input %in% c("EPSG:4326", "WGS 84")) {
    lake_wgs84 <- sf::st_transform(lake_wgs84, 4326)
  }

  # Auto-detect UTM zone
  lake_centroid <- sf::st_coordinates(sf::st_centroid(sf::st_union(lake_wgs84)))
  utm_zone <- floor((lake_centroid[1] + 180) / 6) + 1
  utm_epsg <- ifelse(lake_centroid[2] >= 0,
                     as.numeric(paste0("326", sprintf("%02d", utm_zone))),
                     as.numeric(paste0("327", sprintf("%02d", utm_zone))))

  # Transform to UTM
  lake_utm <- sf::st_transform(lake_wgs84, utm_epsg)
  sites_utm <- sf::st_transform(sites_sf, utm_epsg)

  # Extract largest polygon if needed
  geom_type <- sf::st_geometry_type(lake_utm, by_geometry = FALSE)
  if (any(geom_type %in% c("MULTIPOLYGON", "GEOMETRYCOLLECTION"))) {
    lake_parts <- sf::st_cast(lake_utm, "POLYGON")
    lake_areas <- sf::st_area(lake_parts)
    lake_utm <- lake_parts[which.max(lake_areas), ]
  }

  if (!all(sf::st_is_valid(lake_utm))) {
    lake_utm <- sf::st_make_valid(lake_utm)
  }

  lake_area_km2 <- as.numeric(sf::st_area(lake_utm)) / 1e6
  message("  Lake area: ", round(lake_area_km2, 2), " km2")

  # Add required columns for multi-lake compatibility
  lake_utm$osm_id <- "local_file"
  lake_utm$name <- basename(lake_file_path)
  lake_utm$area_km2 <- lake_area_km2

  return(list(
    all_lakes = lake_utm,
    sites = sites_utm,
    utm_epsg = utm_epsg
  ))
}

#' Assign Sites to Lakes
#'
#' Perform spatial join to assign each site to its containing lake polygon.
#'
#' @param sites_sf sf object with site points
#' @param water_polygons sf object with lake polygons
#' @param tolerance_m Buffer distance for matching sites near lake edges
#'
#' @return sf object with sites and added columns for lake_osm_id, lake_name, lake_area_km2
#'
#' @export
assign_sites_to_lakes <- function(sites_sf, water_polygons, tolerance_m = NULL) {

  if (is.null(tolerance_m)) {
    tolerance_m <- get_opt("gps_tolerance_m")
  }

  message("Assigning sites to lakes...")

  n_sites <- nrow(sites_sf)
  sites_sf$lake_osm_id <- NA_character_
  sites_sf$lake_name <- NA_character_
  sites_sf$lake_area_km2 <- NA_real_

  # First pass: direct intersection
  message("  Checking direct intersections...")
  sites_in_lakes <- sf::st_join(sites_sf, water_polygons, join = sf::st_intersects, left = TRUE)

  # Handle sites that matched directly
  direct_matches <- !is.na(sites_in_lakes$osm_id)
  if (any(direct_matches)) {
    for (i in which(direct_matches)) {
      site_name <- sites_in_lakes$Site[i]
      if (is.na(sites_sf$lake_osm_id[sites_sf$Site == site_name])) {
        sites_sf$lake_osm_id[sites_sf$Site == site_name] <- sites_in_lakes$osm_id[i]
        sites_sf$lake_name[sites_sf$Site == site_name] <- sites_in_lakes$name[i]
        lake_idx <- which(water_polygons$osm_id == sites_in_lakes$osm_id[i])[1]
        if (!is.na(lake_idx) && "area_km2" %in% names(water_polygons)) {
          sites_sf$lake_area_km2[sites_sf$Site == site_name] <- water_polygons$area_km2[lake_idx]
        }
      }
    }
    message("    ", sum(!is.na(sites_sf$lake_osm_id)), " sites matched directly")
  }

  # Second pass: buffer check for unmatched sites
  # Only match sites that are close to the SHORELINE of a lake (not just near the lake polygon)
  # This prevents assigning points to the wrong lake when multiple lakes are nearby
  unmatched_idx <- which(is.na(sites_sf$lake_osm_id))

  if (length(unmatched_idx) > 0 && tolerance_m > 0) {
    message("  Checking ", length(unmatched_idx),
            " unmatched sites with ", tolerance_m, "m tolerance...")

    for (i in unmatched_idx) {
      site_geom <- sf::st_geometry(sites_sf)[i]

      # Find distance to the BOUNDARY (shoreline) of each lake, not just the polygon
      # This ensures we only match sites that are genuinely close to a lake edge
      shoreline_distances <- sapply(seq_len(nrow(water_polygons)), function(j) {
        lake_boundary <- tryCatch({
          sf::st_boundary(water_polygons[j, ])
        }, error = function(e) {
          sf::st_cast(water_polygons[j, ], "MULTILINESTRING")
        })
        as.numeric(sf::st_distance(site_geom, lake_boundary))
      })

      # Find lakes within tolerance distance of their shoreline
      within_tolerance <- which(shoreline_distances <= tolerance_m)

      if (length(within_tolerance) > 0) {
        # Pick the lake with the closest shoreline
        closest_idx <- within_tolerance[which.min(shoreline_distances[within_tolerance])]
        lake_match <- water_polygons[closest_idx, ]

        sites_sf$lake_osm_id[i] <- lake_match$osm_id
        sites_sf$lake_name[i] <- lake_match$name
        if ("area_km2" %in% names(lake_match)) {
          sites_sf$lake_area_km2[i] <- lake_match$area_km2
        }
      }
    }

    buffer_matches <- sum(!is.na(sites_sf$lake_osm_id)) - sum(direct_matches)
    message("    ", buffer_matches, " additional sites matched within tolerance")
  }

  # Third pass: name-based matching for remaining unmatched sites
  # If sites have a lake.name or similar column, try to match by name
  unmatched_idx <- which(is.na(sites_sf$lake_osm_id))
  if (length(unmatched_idx) > 0) {
    # Check if sites have a lake name column we can use
    site_lake_col <- NULL
    for (col in c("lake.name", "lake_name", "lakename", "Lake", "lake", "waterbody")) {
      if (col %in% names(sites_sf)) {
        site_lake_col <- col
        break
      }
    }

    if (!is.null(site_lake_col)) {
      message("  Trying name-based matching using '", site_lake_col, "' column...")

      # Get unique lake names from unmatched sites
      unmatched_lake_names <- unique(sites_sf[[site_lake_col]][unmatched_idx])
      unmatched_lake_names <- unmatched_lake_names[!is.na(unmatched_lake_names)]

      # Get OSM lake names (lowercase for comparison)
      osm_names <- tolower(trimws(water_polygons$name))
      osm_names[is.na(osm_names)] <- ""

      for (site_lake_name in unmatched_lake_names) {
        site_lake_lower <- tolower(trimws(site_lake_name))

        # Try exact match first
        exact_match <- which(osm_names == site_lake_lower)

        # If no exact match, try partial/fuzzy matching
        if (length(exact_match) == 0) {
          # Check if OSM name contains site lake name or vice versa
          partial_match <- which(
            grepl(site_lake_lower, osm_names, fixed = TRUE) |
            sapply(osm_names, function(x) grepl(x, site_lake_lower, fixed = TRUE) && nchar(x) > 3)
          )
          if (length(partial_match) > 0) {
            exact_match <- partial_match[1]
          }
        }

        if (length(exact_match) > 0) {
          # Found a matching lake - assign all unmatched sites with this lake name
          lake_match <- water_polygons[exact_match[1], ]
          site_indices <- unmatched_idx[sites_sf[[site_lake_col]][unmatched_idx] == site_lake_name]

          for (idx in site_indices) {
            sites_sf$lake_osm_id[idx] <- lake_match$osm_id
            sites_sf$lake_name[idx] <- lake_match$name
            if ("area_km2" %in% names(lake_match)) {
              sites_sf$lake_area_km2[idx] <- lake_match$area_km2
            }
          }
          message("    Matched ", length(site_indices), " sites to '", lake_match$name,
                  "' via name matching from '", site_lake_name, "'")
        } else {
          # Report unmatched lake name
          n_unmatched_this_lake <- sum(sites_sf[[site_lake_col]][unmatched_idx] == site_lake_name)
          message("    WARNING: No OSM lake found matching '", site_lake_name,
                  "' (", n_unmatched_this_lake, " sites)")
        }
      }
    }
  }

  # Summary
  matched <- sum(!is.na(sites_sf$lake_osm_id))
  unmatched <- sum(is.na(sites_sf$lake_osm_id))

  message("  Site assignment summary:")
  message("    Matched: ", matched, "/", n_sites)

  if (unmatched > 0) {
    message("    Unmatched: ", unmatched)

    # Provide diagnostic information for unmatched sites
    unmatched_sites <- sites_sf[is.na(sites_sf$lake_osm_id), ]
    unmatched_coords <- sf::st_coordinates(unmatched_sites)

    # Check if unmatched sites have a lake name column
    lake_col <- NULL
    for (col in c("lake.name", "lake_name", "lakename", "Lake", "lake", "waterbody")) {
      if (col %in% names(unmatched_sites)) {
        lake_col <- col
        break
      }
    }

    if (!is.null(lake_col)) {
      unmatched_lakes <- unique(unmatched_sites[[lake_col]])
      message("    Unmatched sites claim to be in: ", paste(unmatched_lakes, collapse = ", "))
    }

    # Show coordinate range of unmatched sites
    message("    Unmatched coordinate range:")
    message("      Lat: ", round(min(unmatched_coords[, 2]), 4), " to ",
            round(max(unmatched_coords[, 2]), 4))
    message("      Lon: ", round(min(unmatched_coords[, 1]), 4), " to ",
            round(max(unmatched_coords[, 1]), 4))

    # Suggest increasing tolerance or checking OSM
    message("    TIP: Try lakefetch_options(gps_tolerance_m = 100) for larger buffer")
    message("    TIP: Check if the lake exists in OpenStreetMap at openstreetmap.org")
  }

  # Clean up lake names - look up from water_polygons if name is NA
  for (i in which(is.na(sites_sf$lake_name) & !is.na(sites_sf$lake_osm_id))) {
    lake_id <- sites_sf$lake_osm_id[i]
    lake_area <- sites_sf$lake_area_km2[i]

    # First try: Find any polygon with this osm_id that has a name
    matching_polys <- water_polygons[water_polygons$osm_id == lake_id, ]
    if (nrow(matching_polys) > 0) {
      poly_names <- matching_polys$name[!is.na(matching_polys$name) & matching_polys$name != ""]
      if (length(poly_names) > 0) {
        sites_sf$lake_name[i] <- poly_names[1]
        next
      }
    }

    # Second try: Find another lake with very similar area (likely duplicate polygon)
    # This handles cases where OSM has the same lake with different IDs
    if (!is.na(lake_area) && lake_area > 0) {
      area_tolerance <- 0.01  # 1% tolerance
      similar_area_idx <- which(
        abs(water_polygons$area_km2 - lake_area) / lake_area < area_tolerance &
        !is.na(water_polygons$name) &
        water_polygons$name != ""
      )
      if (length(similar_area_idx) > 0) {
        sites_sf$lake_name[i] <- water_polygons$name[similar_area_idx[1]]
      }
    }
  }

  # Final fallback for any still-missing names
  sites_sf$lake_name <- ifelse(
    is.na(sites_sf$lake_name) & !is.na(sites_sf$lake_osm_id),
    paste0("Unknown Lake (ID: ", sites_sf$lake_osm_id, ")"),
    sites_sf$lake_name
  )

  # Show lakes with sites
  if (matched > 0) {
    lake_counts <- table(sites_sf$lake_osm_id, useNA = "no")
    message("  Sites per lake:")
    for (lake_id in names(lake_counts)) {
      lake_nm <- sites_sf$lake_name[sites_sf$lake_osm_id == lake_id][1]
      if (is.na(lake_nm)) lake_nm <- paste0("ID: ", lake_id)
      message("    ", lake_nm, ": ", lake_counts[lake_id], " sites")
    }
  }

  return(sites_sf)
}
