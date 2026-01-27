# ==============================================================================
# UNIVERSAL FETCH CALCULATOR - BASE R VERSION
# ==============================================================================
# Robust implementation using base R functions
# User provides points (CSV or manual), script downloads lake from OSM
# ==============================================================================

# 1. Setup & Libraries ----------------------------------------------------
required_packages <- c("sf", "osmdata", "leaflet", "ggplot2", "shiny", "viridis", "base64enc")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Optional: parallel processing (faster but not required)
if (!require("parallel", quietly = TRUE)) {
  use_parallel <- FALSE
} else {
  use_parallel <- TRUE
  library(parallel)
}

# ==============================================================================
# 2. USER INPUT SECTION - MODIFY THIS
# ==============================================================================

# Option 1: Load from CSV file (RECOMMENDED)
INPUT_FILE <- "2020 Chautauqua Lake.csv"  # Your CSV file path

# Option 2: Manual entry (comment out INPUT_FILE line above and uncomment below)
# sites_input <- data.frame(
#   Site = c("Site1", "Site2", "Site3"),
#   latitude = c(43.4253, 43.4305, 43.4198),
#   longitude = c(-73.6932, -73.6845, -73.7012)
# )

# Fetch Calculation Parameters
BUFFER_DISTANCE_M <- 10           # GPS accuracy buffer (meters)
ANGLE_RESOLUTION_DEG <- 5         # Direction resolution (degrees)
MAX_FETCH_M <- 50000              # Maximum fetch distance (meters)
VALIDATION_BUFFER_M <- 10         # Shore detection validation (meters)
DEFAULT_WIND_SPEED_MS <- 10       # Default wind speed (m/s)
DEFAULT_DEPTH_M <- 10             # Default water depth (meters)

# ==============================================================================
# 3. ROBUST DATA LOADING FUNCTION
# ==============================================================================

load_and_clean_sites <- function(file_path = NULL, manual_data = NULL) {
  
  cat("==============================================================================\n")
  cat("UNIVERSAL FETCH CALCULATOR (Base R Version)\n")
  cat("==============================================================================\n\n")
  
  # Load data
  if (!is.null(file_path)) {
    cat("Loading data from:", file_path, "\n")
    
    # Try multiple encoding options
    sites_raw <- tryCatch({
      read.csv(file_path, stringsAsFactors = FALSE, strip.white = TRUE, 
               fileEncoding = "UTF-8")
    }, error = function(e) {
      tryCatch({
        read.csv(file_path, stringsAsFactors = FALSE, strip.white = TRUE,
                 fileEncoding = "latin1")
      }, error = function(e2) {
        read.csv(file_path, stringsAsFactors = FALSE, strip.white = TRUE)
      })
    })
  } else if (!is.null(manual_data)) {
    sites_raw <- manual_data
  } else {
    stop("ERROR: Must provide either file_path or manual_data")
  }
  
  cat("  Loaded", nrow(sites_raw), "rows with columns:", 
      paste(names(sites_raw), collapse = ", "), "\n\n")
  
  # Find coordinate columns (flexible naming)
  col_names_lower <- tolower(names(sites_raw))
  lat_col_idx <- grep("^lat", col_names_lower)[1]
  lon_col_idx <- grep("^lon", col_names_lower)[1]
  
  if (is.na(lat_col_idx) || is.na(lon_col_idx)) {
    stop("ERROR: Could not find latitude/longitude columns.\n",
         "       Column names should start with 'lat' and 'lon'\n",
         "       Found: ", paste(names(sites_raw), collapse = ", "))
  }
  
  lat_col <- names(sites_raw)[lat_col_idx]
  lon_col <- names(sites_raw)[lon_col_idx]
  
  cat("Using columns: Latitude =", lat_col, ", Longitude =", lon_col, "\n")
  
  # Extract and clean coordinates (ROBUST)
  lat_raw <- as.character(sites_raw[[lat_col]])
  lon_raw <- as.character(sites_raw[[lon_col]])
  
  # Remove all non-numeric characters except minus and decimal
  lat_clean <- gsub("[^0-9.-]", "", lat_raw)
  lon_clean <- gsub("[^0-9.-]", "", lon_raw)
  
  # Convert to numeric
  latitude <- suppressWarnings(as.numeric(lat_clean))
  longitude <- suppressWarnings(as.numeric(lon_clean))
  
  # Find Site column or create one
  site_col_idx <- grep("^site$", col_names_lower)[1]
  if (!is.na(site_col_idx)) {
    Site <- as.character(sites_raw[[names(sites_raw)[site_col_idx]]])
  } else if ("Site" %in% names(sites_raw)) {
    Site <- as.character(sites_raw$Site)
  } else {
    Site <- paste0("Site_", seq_len(nrow(sites_raw)))
    cat("  No 'Site' column found, created generic names\n")
  }
  
  # Create cleaned data frame
  sites_clean <- data.frame(
    Site = Site,
    latitude = latitude,
    longitude = longitude,
    stringsAsFactors = FALSE
  )
  
  # Report problems
  na_mask <- is.na(sites_clean$latitude) | is.na(sites_clean$longitude)
  if (any(na_mask)) {
    cat("\nWARNING: Found", sum(na_mask), "rows with invalid coordinates:\n")
    print(sites_raw[na_mask, c(1, lat_col_idx, lon_col_idx)])
    cat("\n")
  }
  
  # Remove invalid coordinates
  sites_clean <- sites_clean[!na_mask, ]
  
  # Validate ranges
  invalid_lat <- sites_clean$latitude < -90 | sites_clean$latitude > 90
  invalid_lon <- sites_clean$longitude < -180 | sites_clean$longitude > 180
  
  if (any(invalid_lat)) {
    cat("WARNING: Removing", sum(invalid_lat), "rows with invalid latitude\n")
    sites_clean <- sites_clean[!invalid_lat, ]
  }
  if (any(invalid_lon)) {
    cat("WARNING: Removing", sum(invalid_lon), "rows with invalid longitude\n")
    sites_clean <- sites_clean[!invalid_lon, ]
  }
  
  # Remove duplicates
  sites_clean <- sites_clean[!duplicated(sites_clean$Site), ]
  
  cat("\nFinal valid sites:", nrow(sites_clean), "\n")
  print(head(sites_clean))
  cat("\n")
  
  if (nrow(sites_clean) == 0) {
    stop("ERROR: No valid coordinates remaining after cleaning")
  }
  
  return(sites_clean)
}

# ==============================================================================
# 4. LAKE DOWNLOAD FUNCTION - Fixed S2 geometry errors
# ==============================================================================

download_lake_boundary <- function(sites_df) {

  cat("Converting to spatial format...\n")

  # IMPORTANT: Disable S2 spherical geometry to avoid topology errors
  # We'll work in projected coordinates instead
  sf_use_s2(FALSE)

  # Convert to sf object (WGS84)
  sites_sf <- st_as_sf(sites_df,
                       coords = c("longitude", "latitude"),
                       crs = 4326)

  # Create bounding box with LARGER buffer for better OSM coverage
  bbox <- st_bbox(sites_sf)
  bbox_buffer <- 0.15  # ~15 km
  bbox[1] <- bbox[1] - bbox_buffer
  bbox[2] <- bbox[2] - bbox_buffer
  bbox[3] <- bbox[3] + bbox_buffer
  bbox[4] <- bbox[4] + bbox_buffer

  # Convert bbox to vector format for osmdata
  bbox_vec <- c(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])
  names(bbox_vec) <- c("left", "bottom", "right", "top")

  cat("\nDownloading lake boundary from OpenStreetMap...\n")
  cat("  Bounding box: [", paste(round(bbox_vec, 4), collapse = ", "), "]\n")

  # Helper function to standardize and validate OSM data
  standardize_osm_sf <- function(osm_sf) {
    if (is.null(osm_sf) || nrow(osm_sf) == 0) return(NULL)

    # Make geometries valid first
    osm_sf <- st_make_valid(osm_sf)

    # Extract only essential columns: osm_id, name, geometry
    result <- data.frame(
      osm_id = if("osm_id" %in% names(osm_sf)) as.character(osm_sf$osm_id) else NA_character_,
      name = if("name" %in% names(osm_sf)) as.character(osm_sf$name) else NA_character_,
      stringsAsFactors = FALSE
    )
    result <- st_sf(result, geometry = st_geometry(osm_sf))

    # Remove any invalid geometries
    valid_mask <- st_is_valid(result)
    if (!all(valid_mask)) {
      cat("      (Removed", sum(!valid_mask), "invalid geometries)\n")
      result <- result[valid_mask, ]
    }

    return(result)
  }

  # Try MULTIPLE OSM queries with different tags
  water_list <- list()

  # 1. Try natural=water
  cat("  Trying natural=water...\n")
  tryCatch({
    osm_query <- opq(bbox = bbox_vec, timeout = 120)
    osm_query <- add_osm_feature(osm_query, key = "natural", value = "water")
    osm_water <- osmdata_sf(osm_query)

    if (!is.null(osm_water$osm_polygons) && nrow(osm_water$osm_polygons) > 0) {
      std_poly <- standardize_osm_sf(osm_water$osm_polygons)
      if (!is.null(std_poly) && nrow(std_poly) > 0) {
        water_list[[length(water_list) + 1]] <- std_poly
        cat("    Found", nrow(std_poly), "polygons\n")
      }
    }
    if (!is.null(osm_water$osm_multipolygons) && nrow(osm_water$osm_multipolygons) > 0) {
      std_mpoly <- standardize_osm_sf(osm_water$osm_multipolygons)
      if (!is.null(std_mpoly) && nrow(std_mpoly) > 0) {
        water_list[[length(water_list) + 1]] <- std_mpoly
        cat("    Found", nrow(std_mpoly), "multipolygons\n")
      }
    }
  }, error = function(e) {
    cat("    Error with natural=water:", conditionMessage(e), "\n")
  })

  # 2. Try water=lake specifically
  cat("  Trying water=lake...\n")
  tryCatch({
    osm_query <- opq(bbox = bbox_vec, timeout = 120)
    osm_query <- add_osm_feature(osm_query, key = "water", value = "lake")
    osm_lake <- osmdata_sf(osm_query)

    if (!is.null(osm_lake$osm_polygons) && nrow(osm_lake$osm_polygons) > 0) {
      std_poly <- standardize_osm_sf(osm_lake$osm_polygons)
      if (!is.null(std_poly) && nrow(std_poly) > 0) {
        water_list[[length(water_list) + 1]] <- std_poly
        cat("    Found", nrow(std_poly), "polygons\n")
      }
    }
    if (!is.null(osm_lake$osm_multipolygons) && nrow(osm_lake$osm_multipolygons) > 0) {
      std_mpoly <- standardize_osm_sf(osm_lake$osm_multipolygons)
      if (!is.null(std_mpoly) && nrow(std_mpoly) > 0) {
        water_list[[length(water_list) + 1]] <- std_mpoly
        cat("    Found", nrow(std_mpoly), "multipolygons\n")
      }
    }
  }, error = function(e) {
    cat("    Error with water=lake:", conditionMessage(e), "\n")
  })
  
  # Check if we found ANY water bodies
  if (length(water_list) == 0) {
    cat("\n=============================================================\n")
    cat("WARNING: No water bodies found in OpenStreetMap!\n")
    cat("=============================================================\n")
    
    # Auto-detect UTM zone from sites
    site_centroid <- c(mean(sites_df$longitude), mean(sites_df$latitude))
    utm_zone <- floor((site_centroid[1] + 180) / 6) + 1
    utm_epsg <- ifelse(site_centroid[2] >= 0,
                       paste0("326", sprintf("%02d", utm_zone)),
                       paste0("327", sprintf("%02d", utm_zone)))
    
    # FALLBACK: Create a buffer around all points
    sites_utm_temp <- st_transform(sites_sf, as.numeric(utm_epsg))
    buffer_dist <- 5000
    buffered <- st_buffer(sites_utm_temp, dist = buffer_dist)
    lake_approx_utm <- st_union(buffered)
    lake_approx_utm <- st_convex_hull(lake_approx_utm)
    
    lake_approx_utm <- st_sf(
      name = "Approximate Boundary",
      osm_id = "fallback",
      geometry = st_geometry(lake_approx_utm)
    )
    
    sf_use_s2(TRUE)  # Re-enable S2
    
    return(list(
      lake = lake_approx_utm,
      sites = sites_utm_temp,
      lake_name = "Approximate Boundary (NO OSM DATA)",
      utm_epsg = as.numeric(utm_epsg),
      is_approximate = TRUE
    ))
  }
  
  # Combine all water polygons
  cat("\n  Combining results...\n")
  all_water <- do.call(rbind, water_list)
  cat("  Total water bodies found:", nrow(all_water), "\n")
  
  # Auto-detect UTM zone BEFORE doing spatial operations
  site_centroid <- c(mean(sites_df$longitude), mean(sites_df$latitude))
  utm_zone <- floor((site_centroid[1] + 180) / 6) + 1
  utm_epsg <- ifelse(site_centroid[2] >= 0,
                     paste0("326", sprintf("%02d", utm_zone)),
                     paste0("327", sprintf("%02d", utm_zone)))
  
  cat("  Auto-detected UTM Zone:", utm_zone, 
      ifelse(site_centroid[2] >= 0, "N", "S"), "\n")
  
  # Transform EVERYTHING to UTM before spatial operations
  # This avoids S2 spherical geometry issues
  cat("  Transforming to UTM for analysis...\n")
  all_water_utm <- st_transform(all_water, as.numeric(utm_epsg))
  sites_utm <- st_transform(sites_sf, as.numeric(utm_epsg))
  
  # Find which water body contains the most points OR is largest
  cat("  Finding best matching lake...\n")
  
  # Method 1: Check which contains the most points (now in UTM)
  sites_in_water <- st_join(sites_utm, all_water_utm, join = st_intersects)
  osm_counts <- table(sites_in_water$osm_id, useNA = "no")
  
  if (length(osm_counts) > 0 && max(osm_counts) > 0) {
    # Use water body with most points
    selected_osm_id <- names(osm_counts)[which.max(osm_counts)]
    cat("    Selected by point count (", max(osm_counts), " points inside)\n", sep = "")
  } else {
    # Method 2: Find nearest large water body
    cat("    No points inside water bodies. Finding nearest large lake...\n")
    
    # Calculate areas and find large ones (> 1 km²)
    areas_m2 <- as.numeric(st_area(all_water_utm))
    large_water_idx <- which(areas_m2 > 1e6)  # > 1 km²
    
    if (length(large_water_idx) > 0) {
      cat("      Found", length(large_water_idx), "large water bodies (> 1 km²)\n")
      large_water <- all_water_utm[large_water_idx, ]
      
      # Find nearest among large water bodies
      nearest_idx <- st_nearest_feature(sites_utm, large_water)[1]
      selected_osm_id <- large_water$osm_id[nearest_idx]
      
      # Get the area of selected lake
      selected_area_km2 <- areas_m2[large_water_idx[nearest_idx]] / 1e6
      cat("      Selected lake area:", round(selected_area_km2, 2), "km²\n")
    } else {
      # Just use nearest regardless of size
      nearest_idx <- st_nearest_feature(sites_utm, all_water_utm)[1]
      selected_osm_id <- all_water_utm$osm_id[nearest_idx]
      cat("      Selected nearest water body\n")
    }
  }
  
  # Extract selected lake (already in UTM)
  lake_utm <- all_water_utm[all_water_utm$osm_id == selected_osm_id, ]
  lake_name <- lake_utm$name[1]
  
  if (length(lake_name) == 0 || is.na(lake_name)) {
    lake_name <- "Unknown Lake"
  }
  
  cat("\n  Selected lake:", lake_name, "\n")
  cat("  OSM ID:", selected_osm_id, "\n")
  
  # Extract largest polygon if multipolygon
  geom_type <- st_geometry_type(lake_utm, by_geometry = FALSE)
  if (geom_type %in% c("MULTIPOLYGON", "GEOMETRYCOLLECTION")) {
    cat("  Extracting main lake body...\n")
    lake_parts <- st_cast(lake_utm, "POLYGON")
    lake_areas <- st_area(lake_parts)
    lake_utm <- lake_parts[which.max(lake_areas), ]
  }
  
  # Final validation
  if (!all(st_is_valid(lake_utm))) {
    lake_utm <- st_make_valid(lake_utm)
  }
  
  lake_area_km2 <- as.numeric(st_area(lake_utm)) / 1e6
  cat("  Final lake area:", round(lake_area_km2, 2), "km²\n\n")
  
  # Re-enable S2 for future operations
  sf_use_s2(TRUE)
  
  return(list(
    lake = lake_utm,
    sites = sites_utm,
    lake_name = lake_name,
    utm_epsg = as.numeric(utm_epsg),
    is_approximate = FALSE
  ))
}

# ==============================================================================
# 5. HELPER FUNCTIONS FOR FETCH CALCULATION
# ==============================================================================
# ==============================================================================
# ALTERNATIVE: LOAD LAKE FROM LOCAL SHAPEFILE
# ==============================================================================

load_lake_from_file <- function(sites_df, lake_file_path) {
  
  cat("Loading lake boundary from file:", lake_file_path, "\n")
  
  # Convert sites to sf
  sites_sf <- st_as_sf(sites_df, 
                       coords = c("longitude", "latitude"), 
                       crs = 4326)
  
  # Load lake shapefile
  lake_wgs84 <- st_read(lake_file_path, quiet = TRUE)
  
  if (st_crs(lake_wgs84)$input != "EPSG:4326") {
    lake_wgs84 <- st_transform(lake_wgs84, 4326)
  }
  
  # Auto-detect UTM zone
  lake_centroid <- st_coordinates(st_centroid(st_union(lake_wgs84)))
  utm_zone <- floor((lake_centroid[1] + 180) / 6) + 1
  utm_epsg <- ifelse(lake_centroid[2] >= 0,
                     paste0("326", sprintf("%02d", utm_zone)),
                     paste0("327", sprintf("%02d", utm_zone)))
  
  # Transform to UTM
  lake_utm <- st_transform(lake_wgs84, as.numeric(utm_epsg))
  sites_utm <- st_transform(sites_sf, as.numeric(utm_epsg))
  
  # Extract largest polygon if needed
  if (st_geometry_type(lake_utm, by_geometry = FALSE) %in% 
      c("MULTIPOLYGON", "GEOMETRYCOLLECTION")) {
    lake_parts <- st_cast(lake_utm, "POLYGON")
    lake_areas <- st_area(lake_parts)
    lake_utm <- lake_parts[which.max(lake_areas), ]
  }
  
  if (!all(st_is_valid(lake_utm))) {
    lake_utm <- st_make_valid(lake_utm)
  }
  
  lake_area_km2 <- as.numeric(st_area(lake_utm)) / 1e6
  cat("  Lake area:", round(lake_area_km2, 2), "km²\n\n")
  
  return(list(
    lake = lake_utm,
    sites = sites_utm,
    lake_name = basename(lake_file_path),
    utm_epsg = as.numeric(utm_epsg),
    is_approximate = FALSE
  ))
}

# Calculate orbital velocity approximation
calc_orbital <- function(fetch_m, wind_speed = DEFAULT_WIND_SPEED_MS) {
  H_sig <- 0.0016 * sqrt(fetch_m) * wind_speed
  return(H_sig)
}

# Buffer point inward from shore
nudge_inward <- function(point_geom, poly_boundary, poly_full, dist = BUFFER_DISTANCE_M) {
  
  is_inside <- length(st_intersects(point_geom, poly_full)[[1]]) > 0
  nearest_on_shore <- st_nearest_points(point_geom, poly_boundary)
  coords <- st_coordinates(nearest_on_shore)
  
  p_x <- coords[1, 1]; p_y <- coords[1, 2]
  s_x <- coords[2, 1]; s_y <- coords[2, 2]
  
  dx <- p_x - s_x
  dy <- p_y - s_y
  len <- sqrt(dx^2 + dy^2)
  
  if (len == 0) return(point_geom)
  
  if (!is_inside) {
    dx <- -dx
    dy <- -dy
  }
  
  new_x <- s_x + (dx / len) * dist
  new_y <- s_y + (dy / len) * dist
  
  return(st_point(c(new_x, new_y)))
}

# Calculate fetch in all directions with validation
get_highres_fetch <- function(pt, boundary, poly, angles, max_d = MAX_FETCH_M) {
  
  coords <- st_coordinates(pt)
  x0 <- coords[1]; y0 <- coords[2]
  f_dists <- numeric(length(angles))
  pt_crs <- st_crs(pt)
  
  for (i in seq_along(angles)) {
    deg <- angles[i]
    rad <- deg * (pi / 180)
    
    x1 <- x0 + max_d * sin(rad)
    y1 <- y0 + max_d * cos(rad)
    ray_geom <- st_sfc(st_linestring(rbind(c(x0, y0), c(x1, y1))), crs = pt_crs)
    
    # Calculate intersection with error handling
    inter <- tryCatch({
      suppressWarnings(st_intersection(ray_geom, boundary))
    }, error = function(e) {
      st_sfc(crs = pt_crs)
    })
    
    if (length(inter) == 0) {
      f_dists[i] <- max_d
    } else {
      dists <- sort(as.numeric(st_distance(pt, inter)))
      dists <- dists[dists > 1]
      
      final_dist <- max_d
      
      if (length(dists) > 0) {
        final_dist <- dists[1]
        
        for (d in dists) {
          check_dist <- d + VALIDATION_BUFFER_M
          if (check_dist >= max_d) break
          
          xt <- x0 + check_dist * sin(rad)
          yt <- y0 + check_dist * cos(rad)
          test_pt <- st_sfc(st_point(c(xt, yt)), crs = pt_crs)
          
          is_water <- length(st_intersects(test_pt, poly)[[1]]) > 0
          
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

# ==============================================================================
# 6. MAIN FETCH CALCULATION FUNCTION
# ==============================================================================

calculate_fetch <- function(lake_data) {
  
  lake_utm <- lake_data$lake
  sites <- lake_data$sites
  utm_epsg <- lake_data$utm_epsg
  
  cat("Buffering sites", BUFFER_DISTANCE_M, "m inward...\n")
  
  lake_boundary <- st_cast(lake_utm, "MULTILINESTRING")
  
  # Buffer all sites
  sites_buffered <- sites
  new_geoms <- vector("list", nrow(sites))
  
  for (i in seq_len(nrow(sites))) {
    new_geoms[[i]] <- nudge_inward(st_geometry(sites)[i], 
                                   lake_boundary, 
                                   lake_utm)
  }
  
  st_geometry(sites_buffered) <- st_sfc(new_geoms, crs = utm_epsg)
  cat("  Buffering complete.\n\n")
  
  # Calculate fetch
  angles <- seq(0, 360 - ANGLE_RESOLUTION_DEG, by = ANGLE_RESOLUTION_DEG)
  n_sites <- nrow(sites_buffered)
  
  cat("Calculating fetch with", ANGLE_RESOLUTION_DEG, "° resolution...\n")
  cat("  Processing", n_sites, "sites...\n")
  
  # Check for parallel processing
  if (use_parallel && n_sites > 5) {
    n_cores <- max(1, detectCores() - 1)
    cat("  Using parallel processing with", n_cores, "cores\n")

    cl <- makeCluster(n_cores)
    clusterExport(cl, c("get_highres_fetch", "lake_boundary", "lake_utm",
                        "angles", "MAX_FETCH_M", "VALIDATION_BUFFER_M",
                        "sites_buffered", "n_sites"),
                  envir = environment())
    clusterEvalQ(cl, library(sf))

    fetch_list <- parLapply(cl, seq_len(n_sites), function(i) {
      get_highres_fetch(sites_buffered[i, ], lake_boundary, lake_utm, angles)
    })

    stopCluster(cl)
    
  } else {
    # Sequential processing
    cat("  Using sequential processing\n")
    fetch_list <- vector("list", n_sites)
    
    for (i in seq_len(n_sites)) {
      if (i %% 5 == 0) cat("  Processed", i, "/", n_sites, "sites\n")
      fetch_list[[i]] <- get_highres_fetch(sites_buffered[i, ], 
                                           lake_boundary, 
                                           lake_utm, 
                                           angles)
    }
  }
  
  cat("  Fetch calculation complete.\n\n")
  
  # Convert to data frame
  fetch_matrix <- do.call(rbind, fetch_list)
  colnames(fetch_matrix) <- paste0("fetch_", angles)
  fetch_df <- as.data.frame(fetch_matrix)
  
  # Combine with site data
  results <- cbind(st_drop_geometry(sites_buffered), fetch_df)
  results_sf <- st_sf(results, geometry = st_geometry(sites_buffered))
  
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
  
  results_sf <- st_sf(results, geometry = st_geometry(sites_buffered))
  
  return(list(
    results = results_sf,
    lake = lake_utm,
    angles = angles
  ))
}

# ==============================================================================
# 7. VISUALIZATION FUNCTIONS
# ==============================================================================

# Create base64-encoded rose plot for popup
make_rose_plot_base64 <- function(site_row, site_name) {
  # Extract fetch values
  fetch_cols <- grep("^fetch_[0-9]+$", names(site_row), value = TRUE)

  if (length(fetch_cols) == 0) {
    return("")
  }

  # Get angles from column names
  angles <- as.numeric(gsub("fetch_", "", fetch_cols))
  fetch_vals <- as.numeric(st_drop_geometry(site_row)[, fetch_cols])

  # Create temporary file
  tmp_file <- tempfile(fileext = ".png")

  png(tmp_file, width = 300, height = 300, bg = "transparent")
  par(mar = c(1, 1, 2, 1))

  # Convert to radians (0 = North, clockwise)
  angles_rad <- (90 - angles) * pi / 180

  # Normalize fetch values for plotting
  max_fetch <- max(fetch_vals, na.rm = TRUE)
  fetch_norm <- fetch_vals / max_fetch

  # Plot
  plot(NULL, xlim = c(-1.3, 1.3), ylim = c(-1.3, 1.3),
       asp = 1, axes = FALSE, xlab = "", ylab = "")
  title(main = site_name, cex.main = 0.9)

  # Draw concentric circles
  for (r in c(0.25, 0.5, 0.75, 1)) {
    theta <- seq(0, 2*pi, length.out = 100)
    lines(r * cos(theta), r * sin(theta), col = "gray80", lty = 2)
  }

  # Draw direction lines
  for (a in seq(0, 315, by = 45)) {
    a_rad <- (90 - a) * pi / 180
    lines(c(0, cos(a_rad)), c(0, sin(a_rad)), col = "gray80")
  }

  # Draw fetch polygon
  x_pts <- fetch_norm * cos(angles_rad)
  y_pts <- fetch_norm * sin(angles_rad)
  polygon(c(x_pts, x_pts[1]), c(y_pts, y_pts[1]),
          col = rgb(0.2, 0.5, 0.8, 0.4), border = "steelblue", lwd = 2)

  # Add cardinal directions
  text(0, 1.15, "N", cex = 0.8, font = 2)
  text(1.15, 0, "E", cex = 0.8, font = 2)
  text(0, -1.15, "S", cex = 0.8, font = 2)
  text(-1.15, 0, "W", cex = 0.8, font = 2)

  dev.off()

  # Convert to base64
  img_raw <- readBin(tmp_file, "raw", file.info(tmp_file)$size)
  img_base64 <- base64enc::base64encode(img_raw)
  unlink(tmp_file)

  return(paste0("data:image/png;base64,", img_base64))
}

# Create ray geometries for map visualization
create_ray_geometries <- function(fetch_data) {
  results <- fetch_data$results
  angles <- fetch_data$angles
  utm_crs <- st_crs(results)

  all_rays <- list()

  for (i in seq_len(nrow(results))) {
    site_row <- results[i, ]
    site_name <- site_row$Site
    coords <- st_coordinates(site_row)
    x0 <- coords[1]; y0 <- coords[2]

    for (angle in angles) {
      col_name <- paste0("fetch_", angle)
      if (!col_name %in% names(site_row)) next

      dist <- as.numeric(st_drop_geometry(site_row)[, col_name])
      if (is.na(dist) || dist <= 0) next

      # Calculate endpoint
      rad <- angle * (pi / 180)
      x1 <- x0 + dist * sin(rad)
      y1 <- y0 + dist * cos(rad)

      ray_line <- st_linestring(rbind(c(x0, y0), c(x1, y1)))

      all_rays[[length(all_rays) + 1]] <- list(
        Site = site_name,
        Angle = angle,
        Distance = dist,
        geometry = ray_line
      )
    }
  }

  # Convert to sf
  rays_df <- data.frame(
    Site = sapply(all_rays, function(x) x$Site),
    Angle = sapply(all_rays, function(x) x$Angle),
    Distance = sapply(all_rays, function(x) x$Distance),
    stringsAsFactors = FALSE
  )

  rays_sf <- st_sf(rays_df,
                   geometry = st_sfc(lapply(all_rays, function(x) x$geometry),
                                     crs = utm_crs))

  return(rays_sf)
}

# ==============================================================================
# 8. MAIN EXECUTION
# ==============================================================================

# Load and clean sites
if (exists("INPUT_FILE")) {
  sites_clean <- load_and_clean_sites(file_path = INPUT_FILE)
} else if (exists("sites_input")) {
  sites_clean <- load_and_clean_sites(manual_data = sites_input)
} else {
  stop("ERROR: No input data provided. Set INPUT_FILE or define sites_input")
}

# Download lake boundary
lake_data <- download_lake_boundary(sites_clean)

# Calculate fetch
fetch_data <- calculate_fetch(lake_data)

# Generate rose plots
cat("Generating rose diagrams...\n")
rose_plots <- vector("list", nrow(fetch_data$results))
for (i in seq_len(nrow(fetch_data$results))) {
  rose_plots[[i]] <- make_rose_plot_base64(fetch_data$results[i, ], 
                                           fetch_data$results$Site[i])
}
fetch_data$results$rose_plot <- unlist(rose_plots)
cat("  Rose plots complete\n\n")

# Create ray geometries for visualization
all_rays_sf <- create_ray_geometries(fetch_data)

# Save results
cat("Saving results...\n")
write.csv(st_drop_geometry(fetch_data$results), 
          "fetch_results.csv", 
          row.names = FALSE)
st_write(fetch_data$results, "fetch_results.gpkg", append = FALSE, quiet = TRUE)
cat("  Saved: fetch_results.csv and fetch_results.gpkg\n\n")

# ==============================================================================
# 9. SHINY APP (only runs in interactive mode)
# ==============================================================================

# Skip Shiny app if running non-interactively (e.g., via Rscript)
if (!interactive()) {
  cat("Script complete! Results saved to fetch_results.csv and fetch_results.gpkg\n")
  cat("To view the interactive map, run this script in RStudio.\n")
  quit(save = "no", status = 0)
}

cat("Launching Shiny App...\n")

ui <- fluidPage(
  titlePanel(paste("Fetch Analysis:", lake_data$lake_name)),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Instructions"),
      p("Click any site marker to view fetch rays."),
      hr(),
      h5("Selected Site:"),
      textOutput("selected_site"),
      hr(),
      h5("Color Legend:"),
      p(style = "color: firebrick;", "Red: > 5 km (Exposed)"),
      p(style = "color: gold;", "Gold: 2.5-5 km (Moderate)"),
      p(style = "color: forestgreen;", "Green: < 2.5 km (Sheltered)")
    ),
    mainPanel(
      leafletOutput("map", height = "800px")
    )
  )
)

server <- function(input, output, session) {
  
  # Color palettes
  exposure_pal <- colorFactor(
    palette = c("firebrick", "goldenrod", "forestgreen"),
    levels = c("Exposed", "Moderate", "Sheltered")
  )
  
  ray_pal <- colorBin(
    palette = c("forestgreen", "gold", "firebrick"),
    domain = c(0, 10000),
    bins = c(0, 2500, 5000, 50000)
  )
  
  # Base map
  output$map <- renderLeaflet({
    
    results_wgs <- st_transform(fetch_data$results, 4326)
    lake_wgs <- st_transform(fetch_data$lake, 4326)
    
    popup_html <- sprintf(
      "<div style='font-family:sans-serif; width:200px;'>
       <h4 style='margin:0; border-bottom:1px solid #ccc;'>%s</h4>
       <span style='background:#eee; font-size:0.9em;'>%s</span>
       <center><img src='%s' width='100%%' style='margin:5px 0;'/></center>
       <b>Effective Fetch:</b> %.1f km<br/>
       <b>Orbital Velocity:</b> %.3f m/s
       </div>",
      results_wgs$Site,
      results_wgs$exposure_category,
      results_wgs$rose_plot,
      results_wgs$fetch_effective / 1000,
      results_wgs$orbital_effective
    )
    
    leaflet(results_wgs) %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addPolygons(data = lake_wgs,
                  fill = FALSE, color = "white", 
                  weight = 1, opacity = 0.3) %>%
      addCircleMarkers(
        radius = 6,
        stroke = TRUE, color = "white", weight = 1.5,
        fillOpacity = 0.9,
        fillColor = ~exposure_pal(exposure_category),
        popup = popup_html,
        layerId = ~Site
      ) %>%
      addLegend("bottomright", 
                pal = exposure_pal, 
                values = ~exposure_category,
                title = "Wave Exposure")
  })
  
  # Click handler
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    site_id <- click$id
    
    if (is.null(site_id)) return()
    
    output$selected_site <- renderText(site_id)
    
    # Filter rays
    site_rays <- all_rays_sf[all_rays_sf$Site == site_id, ]
    site_rays_wgs <- st_transform(site_rays, 4326)
    
    # Update map
    leafletProxy("map") %>%
      clearGroup("rays") %>%
      addPolylines(
        data = site_rays_wgs,
        group = "rays",
        color = ~ray_pal(Distance),
        weight = 2,
        opacity = 0.8,
        popup = ~sprintf("Angle: %d°<br>Distance: %d m", Angle, round(Distance))
      )
  })
}

shinyApp(ui, server)