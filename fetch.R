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

# Optional: LAGOS and NHD integration for lake context
use_lagos <- FALSE
use_nhd <- FALSE

if (require("nhdplusTools", quietly = TRUE)) {
  use_nhd <- TRUE
  cat("nhdplusTools loaded - outlet/inlet detection enabled\n")
} else {
  cat("Note: Install 'nhdplusTools' for outlet/inlet detection\n")
}

# Check for LAGOS data (we'll use direct download rather than LAGOSUS package)
LAGOS_CACHE_DIR <- file.path(tempdir(), "lagos_cache")
if (!dir.exists(LAGOS_CACHE_DIR)) {
  dir.create(LAGOS_CACHE_DIR, recursive = TRUE)
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
GPS_TOLERANCE_M <- 50             # Buffer for matching sites to lakes (meters)

# ==============================================================================
# 3. DETECT LOCATION NAME FROM COLUMN NAMES
# ==============================================================================

detect_location_name <- function(sites_raw) {
  # Look for columns that might contain lake/site/location names
  col_names_lower <- tolower(names(sites_raw))

  # Priority order for location name columns
  location_patterns <- c(
    "^lake[._]?name$",     # lake.name, lake_name, lakename
    "^lake$",              # lake
    "^location[._]?name$", # location.name, location_name
    "^location$",          # location
    "^site[._]?name$",     # site.name, site_name
    "^place[._]?name$",    # place.name, place_name
    "^place$",             # place
    "^water[._]?body$",    # water.body, water_body
    "^waterbody$",         # waterbody
    "^reservoir$",         # reservoir
    "^pond$"               # pond
  )

  detected_name <- NULL
  detected_col <- NULL

  for (pattern in location_patterns) {
    col_idx <- grep(pattern, col_names_lower)[1]
    if (!is.na(col_idx)) {
      detected_col <- names(sites_raw)[col_idx]
      # Get unique non-NA values from this column
      unique_names <- unique(sites_raw[[detected_col]])
      unique_names <- unique_names[!is.na(unique_names) & unique_names != ""]

      if (length(unique_names) == 1) {
        # Single lake - use its name
        detected_name <- as.character(unique_names[1])
      } else if (length(unique_names) > 1) {
        # Multiple lakes - create a combined name
        detected_name <- paste(length(unique_names), "lakes")
      }
      break
    }
  }

  # If no location column found, try to extract from filename
  if (is.null(detected_name)) {
    cat("  No location column detected in data\n")
  } else {
    cat("  Detected location from column '", detected_col, "': ", detected_name, "\n", sep = "")
  }

  return(list(
    name = detected_name,
    column = detected_col
  ))
}

# Clean a name for use in filenames
sanitize_filename <- function(name) {
  if (is.null(name) || is.na(name)) return("fetch_results")

  # Remove or replace invalid filename characters
  clean <- gsub("[<>:\"/\\|?*]", "", name)
  clean <- gsub("\\s+", "_", clean)  # Replace spaces with underscores
  clean <- gsub("_+", "_", clean)    # Remove multiple underscores
  clean <- gsub("^_|_$", "", clean)  # Remove leading/trailing underscores

  if (nchar(clean) == 0) return("fetch_results")
  return(clean)
}

# ==============================================================================
# 3b. LAGOS INTEGRATION FUNCTIONS
# ==============================================================================

# Download and cache LAGOS-US LOCUS lake data
get_lagos_lakes <- function(bbox_wgs84, cache_dir = LAGOS_CACHE_DIR) {

  if (!use_nhd) {
    cat("  LAGOS integration requires nhdplusTools - skipping\n")
    return(NULL)
  }

  cat("Fetching LAGOS-US lake data...\n")

  # Use nhdplusTools to get NHD waterbodies which have LAGOS linkage
  # LAGOS lakes are linked via Permanent_Identifier in NHD

  tryCatch({
    # Get NHD waterbodies in the area
    nhd_waterbodies <- nhdplusTools::get_waterbodies(AOI = bbox_wgs84)

    if (is.null(nhd_waterbodies) || nrow(nhd_waterbodies) == 0) {
      cat("  No NHD waterbodies found in area\n")
      return(NULL)
    }

    cat("  Found", nrow(nhd_waterbodies), "NHD waterbodies\n")
    return(nhd_waterbodies)

  }, error = function(e) {
    cat("  Error fetching LAGOS/NHD data:", conditionMessage(e), "\n")
    return(NULL)
  })
}

# Match a lake polygon to LAGOS/NHD via spatial intersection
match_lake_to_nhd <- function(lake_polygon_wgs84, nhd_waterbodies) {

  if (is.null(nhd_waterbodies) || nrow(nhd_waterbodies) == 0) {
    return(NULL)
  }

  # Find NHD waterbodies that intersect with our lake
  intersects <- st_intersects(lake_polygon_wgs84, nhd_waterbodies)[[1]]

  if (length(intersects) == 0) {
    return(NULL)
  }

  # If multiple matches, pick the one with largest overlap
  if (length(intersects) > 1) {
    overlaps <- sapply(intersects, function(i) {
      tryCatch({
        inter <- st_intersection(lake_polygon_wgs84, nhd_waterbodies[i, ])
        as.numeric(st_area(inter))
      }, error = function(e) 0)
    })
    best_match <- intersects[which.max(overlaps)]
  } else {
    best_match <- intersects[1]
  }

  return(nhd_waterbodies[best_match, ])
}

# Get outlet and inlet locations for a lake using NHDPlus flowlines
get_lake_outlets_inlets <- function(lake_nhd, lake_polygon_wgs84) {

  if (!use_nhd || is.null(lake_nhd)) {
    return(list(outlet = NULL, inlets = NULL, outlet_flowline = NULL))
  }

  tryCatch({
    # Get the permanent identifier or comid
    if ("permanent_identifier" %in% tolower(names(lake_nhd))) {
      lake_id_col <- names(lake_nhd)[tolower(names(lake_nhd)) == "permanent_identifier"]
      lake_id <- lake_nhd[[lake_id_col]]
    } else if ("comid" %in% tolower(names(lake_nhd))) {
      lake_id_col <- names(lake_nhd)[tolower(names(lake_nhd)) == "comid"]
      lake_id <- lake_nhd[[lake_id_col]]
    } else {
      cat("  Could not find lake identifier in NHD data\n")
      return(list(outlet = NULL, inlets = NULL, outlet_flowline = NULL))
    }

    # Get flowlines connected to this waterbody
    # Create a bounding box around the lake for flowline search
    lake_bbox <- st_bbox(lake_polygon_wgs84)
    lake_bbox[1] <- lake_bbox[1] - 0.02  # Expand slightly
    lake_bbox[2] <- lake_bbox[2] - 0.02
    lake_bbox[3] <- lake_bbox[3] + 0.02
    lake_bbox[4] <- lake_bbox[4] + 0.02

    # Get flowlines in the area
    flowlines <- nhdplusTools::get_nhdplus(AOI = st_as_sfc(lake_bbox),
                                            realization = "flowline")

    if (is.null(flowlines) || nrow(flowlines) == 0) {
      cat("  No flowlines found near lake\n")
      return(list(outlet = NULL, inlets = NULL, outlet_flowline = NULL))
    }

    # Find flowlines that touch the lake boundary
    lake_boundary <- st_boundary(lake_polygon_wgs84)
    touching <- st_intersects(flowlines, st_buffer(lake_boundary, 0.0001))
    connected_idx <- which(sapply(touching, length) > 0)

    if (length(connected_idx) == 0) {
      cat("  No connected flowlines found\n")
      return(list(outlet = NULL, inlets = NULL, outlet_flowline = NULL))
    }

    connected_flowlines <- flowlines[connected_idx, ]

    # Classify as inlet or outlet based on flow direction
    # In NHDPlus, we can use the geometry - outlet flows away, inlet flows toward
    outlet_list <- list()
    inlet_list <- list()
    outlet_flowline_list <- list()

    lake_centroid <- st_centroid(lake_polygon_wgs84)

    for (i in seq_len(nrow(connected_flowlines))) {
      fl <- connected_flowlines[i, ]
      fl_coords <- st_coordinates(fl)

      if (nrow(fl_coords) < 2) next

      # Get start and end points of flowline
      start_pt <- st_point(fl_coords[1, 1:2])
      end_pt <- st_point(fl_coords[nrow(fl_coords), 1:2])

      # Check which end is closer to lake
      start_in_lake <- st_intersects(st_sfc(start_pt, crs = 4326),
                                      st_buffer(lake_polygon_wgs84, 0.0001))[[1]]
      end_in_lake <- st_intersects(st_sfc(end_pt, crs = 4326),
                                    st_buffer(lake_polygon_wgs84, 0.0001))[[1]]

      if (length(start_in_lake) > 0 && length(end_in_lake) == 0) {
        # Flowline starts in lake, ends outside = OUTLET
        outlet_list[[length(outlet_list) + 1]] <- start_pt
        outlet_flowline_list[[length(outlet_flowline_list) + 1]] <- fl
      } else if (length(end_in_lake) > 0 && length(start_in_lake) == 0) {
        # Flowline ends in lake, starts outside = INLET
        inlet_list[[length(inlet_list) + 1]] <- end_pt
      }
    }

    # Convert to sf points
    outlet_sf <- NULL
    inlet_sf <- NULL
    outlet_flowline <- NULL

    if (length(outlet_list) > 0) {
      outlet_sf <- st_sf(
        type = rep("outlet", length(outlet_list)),
        geometry = st_sfc(outlet_list, crs = 4326)
      )
      # Keep the first outlet flowline for stream order
      outlet_flowline <- outlet_flowline_list[[1]]
    }

    if (length(inlet_list) > 0) {
      inlet_sf <- st_sf(
        type = rep("inlet", length(inlet_list)),
        geometry = st_sfc(inlet_list, crs = 4326)
      )
    }

    cat("  Found", length(outlet_list), "outlet(s) and", length(inlet_list), "inlet(s)\n")

    return(list(outlet = outlet_sf, inlets = inlet_sf, outlet_flowline = outlet_flowline))

  }, error = function(e) {
    cat("  Error finding outlets/inlets:", conditionMessage(e), "\n")
    return(list(outlet = NULL, inlets = NULL, outlet_flowline = NULL))
  })
}

# Get stream order from outlet flowline
get_stream_order <- function(outlet_flowline) {
  if (is.null(outlet_flowline)) {
    return(NA_integer_)
  }

  col_names_lower <- tolower(names(outlet_flowline))

  # Try different column names for stream order
  order_cols <- c("streamorde", "stream_order", "strahler", "streamorder")

  for (col in order_cols) {
    if (col %in% col_names_lower) {
      idx <- which(col_names_lower == col)
      return(as.integer(outlet_flowline[[names(outlet_flowline)[idx]]]))
    }
  }

  return(NA_integer_)
}

# Get watershed area for a lake using NLDI basin delineation
get_watershed_area <- function(lake_nhd, lake_polygon_wgs84) {

  if (!use_nhd) {
    return(NA_real_)
  }

  tryCatch({
    # Get the lake centroid as a pour point
    lake_centroid <- st_centroid(lake_polygon_wgs84)
    coords <- st_coordinates(lake_centroid)

    # Try to get basin using NLDI
    # First, we need to find the nearest flowline comid
    start_point <- st_sfc(st_point(c(coords[1], coords[2])), crs = 4326)

    # Use discover_nhdplus_id to find nearest comid
    comid <- tryCatch({
      nhdplusTools::discover_nhdplus_id(point = start_point)
    }, error = function(e) NULL)

    if (is.null(comid) || length(comid) == 0) {
      return(NA_real_)
    }

    # Get the basin for this comid
    basin <- tryCatch({
      nhdplusTools::get_nldi_basin(list(featureSource = "comid",
                                         featureID = as.character(comid)))
    }, error = function(e) NULL)

    if (is.null(basin)) {
      return(NA_real_)
    }

    # Calculate area in hectares
    basin_utm <- st_transform(basin, 32618)  # UTM zone 18N for northeastern US
    area_ha <- as.numeric(st_area(basin_utm)) / 10000

    return(area_ha)

  }, error = function(e) {
    return(NA_real_)
  })
}

# Derive connectivity class from inlet/outlet presence
get_connectivity_class <- function(has_outlet, has_inlet) {
  if (is.na(has_outlet) && is.na(has_inlet)) {
    return(NA_character_)
  }

  has_outlet <- isTRUE(has_outlet)
  has_inlet <- isTRUE(has_inlet)

  if (!has_outlet && !has_inlet) {
    return("Isolated")
  } else if (!has_inlet && has_outlet) {
    return("Headwater")
  } else if (has_inlet && has_outlet) {
    return("Drainage")
  } else if (has_inlet && !has_outlet) {
    return("Terminal")
  } else {
    return(NA_character_)
  }
}

# Calculate distance and bearing from a point to another point
calc_distance_bearing <- function(from_pt, to_pt, utm_crs) {

  if (is.null(to_pt) || length(to_pt) == 0) {
    return(list(dist_m = NA_real_, bearing = NA_character_))
  }

  # Transform to UTM for accurate distance
  from_utm <- st_transform(from_pt, utm_crs)
  to_utm <- st_transform(to_pt, utm_crs)

  # Calculate distance
  dist_m <- as.numeric(st_distance(from_utm, to_utm))

  # Calculate bearing (in WGS84 for proper bearing)
  from_coords <- st_coordinates(from_pt)
  to_coords <- st_coordinates(to_pt)

  # Calculate bearing using spherical geometry
  lon1 <- from_coords[1] * pi / 180
  lat1 <- from_coords[2] * pi / 180
  lon2 <- to_coords[1] * pi / 180
  lat2 <- to_coords[2] * pi / 180

  dlon <- lon2 - lon1
  x <- sin(dlon) * cos(lat2)
  y <- cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(dlon)
  bearing_rad <- atan2(x, y)
  bearing_deg <- (bearing_rad * 180 / pi + 360) %% 360

  # Convert to compass direction
  directions <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N")
  dir_idx <- round(bearing_deg / 45) + 1
  if (dir_idx > 9) dir_idx <- 1
  bearing_compass <- directions[dir_idx]

  return(list(
    dist_m = dist_m,
    bearing_deg = bearing_deg,
    bearing = bearing_compass
  ))
}

# Get lake context from NHD (watershed area, connectivity, etc.)
get_lake_context <- function(lake_nhd) {

  if (is.null(lake_nhd)) {
    return(list(
      nhd_permanent_id = NA_character_,
      nhd_gnis_name = NA_character_,
      nhd_areasqkm = NA_real_,
      nhd_ftype = NA_character_,
      nhd_fcode = NA_integer_
    ))
  }

  # Extract available attributes from NHD waterbody
  result <- list(
    nhd_permanent_id = NA_character_,
    nhd_gnis_name = NA_character_,
    nhd_areasqkm = NA_real_,
    nhd_ftype = NA_character_,
    nhd_fcode = NA_integer_
  )

  col_names_lower <- tolower(names(lake_nhd))

  # Permanent ID
  if ("permanent_identifier" %in% col_names_lower) {
    idx <- which(col_names_lower == "permanent_identifier")
    result$nhd_permanent_id <- as.character(lake_nhd[[names(lake_nhd)[idx]]])
  }

  # GNIS Name
  if ("gnis_name" %in% col_names_lower) {
    idx <- which(col_names_lower == "gnis_name")
    result$nhd_gnis_name <- as.character(lake_nhd[[names(lake_nhd)[idx]]])
  }

  # Area
  if ("areasqkm" %in% col_names_lower) {
    idx <- which(col_names_lower == "areasqkm")
    result$nhd_areasqkm <- as.numeric(lake_nhd[[names(lake_nhd)[idx]]])
  }

  # Feature type
  if ("ftype" %in% col_names_lower) {
    idx <- which(col_names_lower == "ftype")
    result$nhd_ftype <- as.character(lake_nhd[[names(lake_nhd)[idx]]])
  }

  # Feature code
  if ("fcode" %in% col_names_lower) {
    idx <- which(col_names_lower == "fcode")
    result$nhd_fcode <- as.integer(lake_nhd[[names(lake_nhd)[idx]]])
  }

  return(result)
}

# Main function to add lake context to fetch results
add_lake_context <- function(fetch_results, lake_polygons_wgs84, utm_epsg) {

  if (!use_nhd) {
    cat("\nSkipping lake context (nhdplusTools not available)\n")
    cat("Install with: install.packages('nhdplusTools')\n\n")

    # Add NA columns so output format is consistent
    fetch_results$nhd_permanent_id <- NA_character_
    fetch_results$nhd_gnis_name <- NA_character_
    fetch_results$nhd_areasqkm <- NA_real_
    fetch_results$outlet_dist_m <- NA_real_
    fetch_results$outlet_bearing <- NA_character_
    fetch_results$inlet_nearest_dist_m <- NA_real_
    fetch_results$inlet_nearest_bearing <- NA_character_
    fetch_results$inlet_count <- NA_integer_
    fetch_results$connectivity_class <- NA_character_
    fetch_results$outlet_stream_order <- NA_integer_
    fetch_results$watershed_area_ha <- NA_real_
    fetch_results$lake_watershed_ratio <- NA_real_

    return(fetch_results)
  }

  cat("\n==============================================================================\n")
  cat("ADDING LAKE CONTEXT (NHD/LAGOS Integration)\n")
  cat("==============================================================================\n\n")

  # Get bounding box for all lakes
  all_lakes_wgs84 <- st_transform(lake_polygons_wgs84, 4326)
  bbox <- st_bbox(all_lakes_wgs84)
  bbox[1] <- bbox[1] - 0.1
  bbox[2] <- bbox[2] - 0.1
  bbox[3] <- bbox[3] + 0.1
  bbox[4] <- bbox[4] + 0.1

  # Fetch NHD waterbodies for the area
  nhd_waterbodies <- get_lagos_lakes(st_as_sfc(bbox))

  # Initialize new columns
  fetch_results$nhd_permanent_id <- NA_character_
  fetch_results$nhd_gnis_name <- NA_character_
  fetch_results$nhd_areasqkm <- NA_real_
  fetch_results$outlet_dist_m <- NA_real_
  fetch_results$outlet_bearing <- NA_character_
  fetch_results$inlet_nearest_dist_m <- NA_real_
  fetch_results$inlet_nearest_bearing <- NA_character_
  fetch_results$inlet_count <- NA_integer_
  fetch_results$connectivity_class <- NA_character_
  fetch_results$outlet_stream_order <- NA_integer_
  fetch_results$watershed_area_ha <- NA_real_
  fetch_results$lake_watershed_ratio <- NA_real_

  # Process each unique lake
  unique_lake_ids <- unique(fetch_results$lake_osm_id)
  unique_lake_ids <- unique_lake_ids[!is.na(unique_lake_ids)]

  for (lake_id in unique_lake_ids) {
    cat("Processing lake:", lake_id, "\n")

    # Get the lake polygon
    lake_idx <- which(lake_polygons_wgs84$osm_id == lake_id)[1]
    if (is.na(lake_idx)) next

    lake_poly_wgs84 <- st_transform(lake_polygons_wgs84[lake_idx, ], 4326)

    # Match to NHD
    matched_nhd <- match_lake_to_nhd(lake_poly_wgs84, nhd_waterbodies)

    # Get lake context
    context <- get_lake_context(matched_nhd)

    # Get outlets and inlets
    outlets_inlets <- get_lake_outlets_inlets(matched_nhd, lake_poly_wgs84)

    # Get stream order from outlet flowline
    stream_order <- get_stream_order(outlets_inlets$outlet_flowline)

    # Derive connectivity class
    has_outlet <- !is.null(outlets_inlets$outlet) && nrow(outlets_inlets$outlet) > 0
    has_inlet <- !is.null(outlets_inlets$inlets) && nrow(outlets_inlets$inlets) > 0
    connectivity <- get_connectivity_class(has_outlet, has_inlet)

    # Get watershed area (do once per lake, not per site)
    watershed_ha <- get_watershed_area(matched_nhd, lake_poly_wgs84)

    # Calculate lake:watershed ratio
    lake_area_ha <- ifelse(!is.na(context$nhd_areasqkm),
                            context$nhd_areasqkm * 100,  # Convert km² to ha
                            NA_real_)
    lake_ws_ratio <- ifelse(!is.na(lake_area_ha) && !is.na(watershed_ha) && watershed_ha > 0,
                             lake_area_ha / watershed_ha,
                             NA_real_)

    # Find sites in this lake
    site_idx <- which(fetch_results$lake_osm_id == lake_id)

    # Apply context to all sites in this lake
    for (i in site_idx) {
      fetch_results$nhd_permanent_id[i] <- context$nhd_permanent_id
      fetch_results$nhd_gnis_name[i] <- context$nhd_gnis_name
      fetch_results$nhd_areasqkm[i] <- context$nhd_areasqkm
      fetch_results$connectivity_class[i] <- connectivity
      fetch_results$outlet_stream_order[i] <- stream_order
      fetch_results$watershed_area_ha[i] <- watershed_ha
      fetch_results$lake_watershed_ratio[i] <- lake_ws_ratio

      # Get site location
      site_pt_wgs84 <- st_transform(fetch_results[i, ], 4326)

      # Calculate distance to outlet
      if (has_outlet) {
        # Use first outlet (most lakes have one)
        outlet_pt <- outlets_inlets$outlet[1, ]
        outlet_calc <- calc_distance_bearing(
          st_geometry(site_pt_wgs84),
          st_geometry(outlet_pt),
          utm_epsg
        )
        fetch_results$outlet_dist_m[i] <- outlet_calc$dist_m
        fetch_results$outlet_bearing[i] <- outlet_calc$bearing
      }

      # Calculate distance to nearest inlet
      if (has_inlet) {
        fetch_results$inlet_count[i] <- nrow(outlets_inlets$inlets)

        # Find nearest inlet
        inlet_dists <- sapply(seq_len(nrow(outlets_inlets$inlets)), function(j) {
          calc_distance_bearing(
            st_geometry(site_pt_wgs84),
            st_geometry(outlets_inlets$inlets[j, ]),
            utm_epsg
          )$dist_m
        })

        nearest_inlet_idx <- which.min(inlet_dists)
        nearest_inlet <- outlets_inlets$inlets[nearest_inlet_idx, ]
        inlet_calc <- calc_distance_bearing(
          st_geometry(site_pt_wgs84),
          st_geometry(nearest_inlet),
          utm_epsg
        )
        fetch_results$inlet_nearest_dist_m[i] <- inlet_calc$dist_m
        fetch_results$inlet_nearest_bearing[i] <- inlet_calc$bearing
      } else {
        fetch_results$inlet_count[i] <- 0L
      }
    }
  }

  cat("\nLake context complete.\n\n")
  return(fetch_results)
}

# ==============================================================================
# 4. ROBUST DATA LOADING FUNCTION
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

  # Detect location name from columns
  location_info <- detect_location_name(sites_raw)

  # Also try to extract from filename if no column found
  if (is.null(location_info$name) && !is.null(file_path)) {
    filename <- basename(file_path)
    filename_no_ext <- tools::file_path_sans_ext(filename)
    # Clean up common prefixes/suffixes
    clean_filename <- gsub("^[0-9]{4}\\s*", "", filename_no_ext)  # Remove year prefix
    clean_filename <- gsub("_+", " ", clean_filename)
    clean_filename <- trimws(clean_filename)
    if (nchar(clean_filename) > 0) {
      location_info$name <- clean_filename
      cat("  Location name from filename: ", location_info$name, "\n", sep = "")
    }
  }

  attr(sites_clean, "location_name") <- location_info$name
  attr(sites_clean, "location_column") <- location_info$column

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
      area_km2 = as.numeric(st_area(lake_approx_utm)) / 1e6,
      geometry = st_geometry(lake_approx_utm)
    )

    sf_use_s2(TRUE)  # Re-enable S2

    return(list(
      all_lakes = lake_approx_utm,
      sites = sites_utm_temp,
      utm_epsg = as.numeric(utm_epsg)
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
  
  # Process each lake polygon - extract largest part if multipolygon
  cat("  Processing lake polygons...\n")
  processed_lakes <- list()

  for (i in seq_len(nrow(all_water_utm))) {
    lake_poly <- all_water_utm[i, ]
    geom_type <- st_geometry_type(lake_poly, by_geometry = FALSE)

    if (geom_type %in% c("MULTIPOLYGON", "GEOMETRYCOLLECTION")) {
      # Extract largest polygon from multipolygon
      lake_parts <- st_cast(lake_poly, "POLYGON")
      lake_areas <- st_area(lake_parts)
      lake_poly <- lake_parts[which.max(lake_areas), ]
    }

    # Validate geometry
    if (!all(st_is_valid(lake_poly))) {
      lake_poly <- st_make_valid(lake_poly)
    }

    processed_lakes[[i]] <- lake_poly
  }

  all_lakes_utm <- do.call(rbind, processed_lakes)

  # Calculate areas for reference
  all_lakes_utm$area_km2 <- as.numeric(st_area(all_lakes_utm)) / 1e6

  # Deduplicate by osm_id (keep largest polygon if duplicates)
  unique_ids <- unique(all_lakes_utm$osm_id)
  if (length(unique_ids) < nrow(all_lakes_utm)) {
    cat("  Deduplicating", nrow(all_lakes_utm) - length(unique_ids), "duplicate lake entries...\n")
    deduped_list <- lapply(unique_ids, function(id) {
      matches <- all_lakes_utm[all_lakes_utm$osm_id == id, ]
      if (nrow(matches) > 1) {
        # Keep the one with largest area
        matches[which.max(matches$area_km2), ]
      } else {
        matches
      }
    })
    all_lakes_utm <- do.call(rbind, deduped_list)
  }

  cat("  Processed", nrow(all_lakes_utm), "unique lake polygons\n")
  cat("  Total area:", round(sum(all_lakes_utm$area_km2), 2), "km²\n\n")

  # Re-enable S2 for future operations
  sf_use_s2(TRUE)

  return(list(
    all_lakes = all_lakes_utm,
    sites = sites_utm,
    utm_epsg = as.numeric(utm_epsg)
  ))
}

# ==============================================================================
# 5. ASSIGN SITES TO LAKES
# ==============================================================================

assign_sites_to_lakes <- function(sites_sf, water_polygons, tolerance_m = 50) {

  cat("Assigning sites to lakes...\n")

  n_sites <- nrow(sites_sf)
  sites_sf$lake_osm_id <- NA_character_
  sites_sf$lake_name <- NA_character_
  sites_sf$lake_area_km2 <- NA_real_

  # First pass: direct intersection
  cat("  Checking direct intersections...\n")
  sites_in_lakes <- st_join(sites_sf, water_polygons, join = st_intersects, left = TRUE)

  # Handle sites that matched directly
  direct_matches <- !is.na(sites_in_lakes$osm_id)
  if (any(direct_matches)) {
    # For sites that matched, take the first match (in case of multiple)
    for (i in which(direct_matches)) {
      site_name <- sites_in_lakes$Site[i]
      if (is.na(sites_sf$lake_osm_id[sites_sf$Site == site_name])) {
        sites_sf$lake_osm_id[sites_sf$Site == site_name] <- sites_in_lakes$osm_id[i]
        sites_sf$lake_name[sites_sf$Site == site_name] <- sites_in_lakes$name[i]
        # Get lake area from water_polygons
        lake_idx <- which(water_polygons$osm_id == sites_in_lakes$osm_id[i])[1]
        if (!is.na(lake_idx) && "area_km2" %in% names(water_polygons)) {
          sites_sf$lake_area_km2[sites_sf$Site == site_name] <- water_polygons$area_km2[lake_idx]
        }
      }
    }
    cat("    ", sum(!is.na(sites_sf$lake_osm_id)), " sites matched directly\n", sep = "")
  }

  # Second pass: buffer check for unmatched sites
  unmatched_idx <- which(is.na(sites_sf$lake_osm_id))

  if (length(unmatched_idx) > 0 && tolerance_m > 0) {
    cat("  Checking", length(unmatched_idx), "unmatched sites with", tolerance_m, "m tolerance...\n")

    for (i in unmatched_idx) {
      site_geom <- st_geometry(sites_sf)[i]

      # Create buffer around site
      site_buffer <- st_buffer(site_geom, dist = tolerance_m)

      # Find lakes that intersect the buffer
      intersects <- st_intersects(site_buffer, water_polygons)[[1]]

      if (length(intersects) > 0) {
        # If multiple lakes, pick the nearest one
        if (length(intersects) > 1) {
          candidate_lakes <- water_polygons[intersects, ]
          nearest_idx <- st_nearest_feature(site_geom, candidate_lakes)
          lake_match <- candidate_lakes[nearest_idx, ]
        } else {
          lake_match <- water_polygons[intersects[1], ]
        }

        sites_sf$lake_osm_id[i] <- lake_match$osm_id
        sites_sf$lake_name[i] <- lake_match$name
        if ("area_km2" %in% names(lake_match)) {
          sites_sf$lake_area_km2[i] <- lake_match$area_km2
        }
      }
    }

    buffer_matches <- sum(!is.na(sites_sf$lake_osm_id)) - sum(direct_matches)
    cat("    ", buffer_matches, " additional sites matched within tolerance\n", sep = "")
  }

  # Summary
  matched <- sum(!is.na(sites_sf$lake_osm_id))
  unmatched <- sum(is.na(sites_sf$lake_osm_id))

  cat("\n  Site assignment summary:\n")
  cat("    Matched:", matched, "/", n_sites, "\n")

  if (unmatched > 0) {
    cat("    Unmatched:", unmatched, "\n")
  }

  # Clean up lake names
  sites_sf$lake_name <- ifelse(
    is.na(sites_sf$lake_name) & !is.na(sites_sf$lake_osm_id),
    paste0("Unknown Lake (ID: ", sites_sf$lake_osm_id, ")"),
    sites_sf$lake_name
  )

  # Show lakes with sites
  if (matched > 0) {
    lake_counts <- table(sites_sf$lake_osm_id, useNA = "no")
    cat("\n  Sites per lake:\n")
    for (lake_id in names(lake_counts)) {
      lake_nm <- sites_sf$lake_name[sites_sf$lake_osm_id == lake_id][1]
      if (is.na(lake_nm)) lake_nm <- paste0("ID: ", lake_id)
      cat("    ", lake_nm, ": ", lake_counts[lake_id], " sites\n", sep = "")
    }
  }

  cat("\n")
  return(sites_sf)
}

# ==============================================================================
# 6. HELPER FUNCTIONS FOR FETCH CALCULATION
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

  # Add required columns for multi-lake compatibility
  lake_utm$osm_id <- "local_file"
  lake_utm$name <- basename(lake_file_path)
  lake_utm$area_km2 <- lake_area_km2

  return(list(
    all_lakes = lake_utm,
    sites = sites_utm,
    utm_epsg = as.numeric(utm_epsg)
  ))
}

# Calculate orbital velocity approximation
calc_orbital <- function(fetch_m, wind_speed = DEFAULT_WIND_SPEED_MS) {
  H_sig <- 0.0016 * sqrt(fetch_m) * wind_speed
  return(H_sig)
}

# Buffer point inward from shore
nudge_inward <- function(point_geom, poly_boundary, poly_full, dist = BUFFER_DISTANCE_M) {

  # Handle empty or invalid geometries
  if (is.null(poly_boundary) || st_is_empty(poly_boundary) ||
      is.null(poly_full) || st_is_empty(poly_full)) {
    return(point_geom)
  }

  is_inside <- length(st_intersects(point_geom, poly_full)[[1]]) > 0

  nearest_on_shore <- tryCatch({
    st_nearest_points(point_geom, poly_boundary)
  }, error = function(e) NULL)

  if (is.null(nearest_on_shore) || length(nearest_on_shore) == 0) {
    return(point_geom)
  }

  coords <- st_coordinates(nearest_on_shore)

  # Check that we have valid coordinates (need at least 2 points)
  if (is.null(coords) || nrow(coords) < 2) {
    return(point_geom)
  }

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
# 7. MAIN FETCH CALCULATION FUNCTIONS
# ==============================================================================

# Calculate fetch for a single lake
calculate_fetch_single_lake <- function(sites, lake_polygon, utm_epsg, lake_name = NULL, lake_osm_id = NULL) {

  n_sites <- nrow(sites)
  if (n_sites == 0) {
    return(NULL)
  }

  cat("  Processing", n_sites, "sites in lake:",
      ifelse(is.null(lake_name) || is.na(lake_name), lake_osm_id, lake_name), "\n")

  # If multiple polygons (duplicates), union them into one
  if (nrow(lake_polygon) > 1) {
    unified_geom <- st_union(lake_polygon)
    lake_polygon <- st_sf(
      data.frame(osm_id = lake_osm_id, name = lake_name, stringsAsFactors = FALSE),
      geometry = st_sfc(unified_geom, crs = utm_epsg)
    )
  }

  # Ensure valid geometry
  if (!all(st_is_valid(lake_polygon))) {
    lake_polygon <- st_make_valid(lake_polygon)
  }

  lake_boundary <- tryCatch({
    st_cast(lake_polygon, "MULTILINESTRING")
  }, error = function(e) {
    tryCatch({
      st_cast(st_boundary(lake_polygon), "MULTILINESTRING")
    }, error = function(e2) {
      st_boundary(lake_polygon)
    })
  })

  # Buffer all sites inward
  sites_buffered <- sites
  new_geoms <- vector("list", n_sites)

  for (i in seq_len(n_sites)) {
    new_geoms[[i]] <- nudge_inward(st_geometry(sites)[i],
                                   lake_boundary,
                                   lake_polygon)
  }

  st_geometry(sites_buffered) <- st_sfc(new_geoms, crs = utm_epsg)

  # Calculate fetch
  angles <- seq(0, 360 - ANGLE_RESOLUTION_DEG, by = ANGLE_RESOLUTION_DEG)

  # Sequential processing for single lake (parallelization is at lake level)
  fetch_list <- vector("list", n_sites)

  for (i in seq_len(n_sites)) {
    fetch_list[[i]] <- get_highres_fetch(sites_buffered[i, ],
                                         lake_boundary,
                                         lake_polygon,
                                         angles)
  }

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
    lake = lake_polygon,
    angles = angles
  ))
}

# Calculate fetch for multiple lakes
calculate_fetch_multi_lake <- function(sites_with_lakes, all_lakes, utm_epsg) {

  cat("Calculating fetch for multiple lakes...\n")
  cat("  Buffering sites", BUFFER_DISTANCE_M, "m inward\n")
  cat("  Angle resolution:", ANGLE_RESOLUTION_DEG, "°\n\n")

  # Get unique lakes that have sites
  lakes_with_sites <- unique(sites_with_lakes$lake_osm_id)
  lakes_with_sites <- lakes_with_sites[!is.na(lakes_with_sites)]

  # Warn about unmatched sites
  unmatched <- sites_with_lakes[is.na(sites_with_lakes$lake_osm_id), ]
  if (nrow(unmatched) > 0) {
    warning("Skipping ", nrow(unmatched), " sites not matched to any lake: ",
            paste(unmatched$Site, collapse = ", "))
    cat("WARNING: Skipping", nrow(unmatched), "unmatched sites:",
        paste(unmatched$Site, collapse = ", "), "\n\n")
  }

  if (length(lakes_with_sites) == 0) {
    stop("No sites matched to any lake. Cannot calculate fetch.")
  }

  cat("Processing", length(lakes_with_sites), "lake(s)...\n\n")

  # Define function to process one lake (takes all data as arguments for parallel safety)
  process_one_lake <- function(lake_id, all_lakes_data, sites_data, utm_code) {
    # Use which() to properly handle NA values in filtering
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
  if (use_parallel && length(lakes_with_sites) > 1) {
    n_cores <- min(length(lakes_with_sites), max(1, detectCores() - 1))
    cat("Using parallel processing with", n_cores, "cores for",
        length(lakes_with_sites), "lakes\n\n")

    cl <- makeCluster(n_cores)

    # Export functions from global environment
    clusterExport(cl, c(
      "calculate_fetch_single_lake", "get_highres_fetch", "nudge_inward",
      "calc_orbital", "BUFFER_DISTANCE_M", "ANGLE_RESOLUTION_DEG",
      "MAX_FETCH_M", "VALIDATION_BUFFER_M", "DEFAULT_WIND_SPEED_MS"
    ), envir = globalenv())

    # Export data from local environment
    clusterExport(cl, c("all_lakes", "sites_with_lakes", "utm_epsg", "process_one_lake"),
                  envir = environment())

    clusterEvalQ(cl, library(sf))

    # Run parallel processing
    results_list <- parLapply(cl, lakes_with_sites, function(lid) {
      process_one_lake(lid, all_lakes, sites_with_lakes, utm_epsg)
    })

    stopCluster(cl)
  } else {
    # Sequential processing
    cat("Using sequential processing\n\n")
    results_list <- lapply(lakes_with_sites, function(lid) {
      process_one_lake(lid, all_lakes, sites_with_lakes, utm_epsg)
    })
  }

  cat("\nFetch calculation complete.\n\n")

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

# ==============================================================================
# 8. VISUALIZATION FUNCTIONS
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
# 9. MAIN EXECUTION
# ==============================================================================

# Load and clean sites
if (exists("INPUT_FILE")) {
  sites_clean <- load_and_clean_sites(file_path = INPUT_FILE)
} else if (exists("sites_input")) {
  sites_clean <- load_and_clean_sites(manual_data = sites_input)
} else {
  stop("ERROR: No input data provided. Set INPUT_FILE or define sites_input")
}

# Download ALL lake boundaries
lake_data <- download_lake_boundary(sites_clean)

# Assign sites to their lakes
sites_with_lakes <- assign_sites_to_lakes(
  lake_data$sites,
  lake_data$all_lakes,
  tolerance_m = GPS_TOLERANCE_M
)

# Calculate fetch (handles multiple lakes)
fetch_data <- calculate_fetch_multi_lake(
  sites_with_lakes,
  lake_data$all_lakes,
  lake_data$utm_epsg
)

# Generate rose plots
cat("Generating rose diagrams...\n")
rose_plots <- vector("list", nrow(fetch_data$results))
for (i in seq_len(nrow(fetch_data$results))) {
  rose_plots[[i]] <- make_rose_plot_base64(fetch_data$results[i, ], 
                                           fetch_data$results$Site[i])
}
fetch_data$results$rose_plot <- unlist(rose_plots)
cat("  Rose plots complete\n\n")

# Add lake context (NHD/LAGOS integration)
fetch_data$results <- add_lake_context(
  fetch_data$results,
  fetch_data$lakes,
  lake_data$utm_epsg
)

# Create ray geometries for visualization
all_rays_sf <- create_ray_geometries(fetch_data)

# Determine output filename from detected location name
location_name <- attr(sites_clean, "location_name")
if (!is.null(location_name) && !is.na(location_name)) {
  output_base <- sanitize_filename(location_name)
  output_csv <- paste0(output_base, "_fetch.csv")
  output_gpkg <- paste0(output_base, "_fetch.gpkg")
  cat("Output named based on detected location: ", location_name, "\n", sep = "")
} else {
  # Fallback: use OSM lake name if all sites are in the same lake
  unique_osm_names <- unique(fetch_data$results$lake_name)
  unique_osm_names <- unique_osm_names[!is.na(unique_osm_names)]
  if (length(unique_osm_names) == 1) {
    output_base <- sanitize_filename(unique_osm_names[1])
    output_csv <- paste0(output_base, "_fetch.csv")
    output_gpkg <- paste0(output_base, "_fetch.gpkg")
    cat("Output named based on OSM lake name: ", unique_osm_names[1], "\n", sep = "")
  } else {
    output_base <- "fetch_results"
    output_csv <- "fetch_results.csv"
    output_gpkg <- "fetch_results.gpkg"
  }
}

# Save results
cat("Saving results...\n")

# Show what data is included
cat("  Output includes OSM data:\n")
if ("lake_name" %in% names(fetch_data$results)) {
  osm_names <- unique(fetch_data$results$lake_name)
  osm_names <- osm_names[!is.na(osm_names)]
  cat("    - Lake name(s): ", paste(osm_names, collapse = ", "), "\n", sep = "")
}
if ("lake_area_km2" %in% names(fetch_data$results)) {
  areas <- unique(fetch_data$results$lake_area_km2)
  areas <- areas[!is.na(areas)]
  cat("    - Lake area(s): ", paste(round(areas, 2), "km²", collapse = ", "), "\n", sep = "")
}

# Show NHD context data
if (use_nhd) {
  cat("  Output includes NHD context:\n")
  if ("nhd_gnis_name" %in% names(fetch_data$results)) {
    nhd_names <- unique(fetch_data$results$nhd_gnis_name)
    nhd_names <- nhd_names[!is.na(nhd_names)]
    if (length(nhd_names) > 0) {
      cat("    - NHD name(s): ", paste(nhd_names, collapse = ", "), "\n", sep = "")
    }
  }
  if ("connectivity_class" %in% names(fetch_data$results)) {
    conn_table <- table(fetch_data$results$connectivity_class, useNA = "no")
    if (length(conn_table) > 0) {
      conn_str <- paste(names(conn_table), conn_table, sep = ": ", collapse = ", ")
      cat("    - Connectivity: ", conn_str, "\n", sep = "")
    }
  }
  if ("outlet_stream_order" %in% names(fetch_data$results)) {
    orders <- unique(fetch_data$results$outlet_stream_order)
    orders <- orders[!is.na(orders)]
    if (length(orders) > 0) {
      cat("    - Stream orders: ", paste(sort(orders), collapse = ", "), "\n", sep = "")
    }
  }
  if ("watershed_area_ha" %in% names(fetch_data$results)) {
    ws_areas <- unique(fetch_data$results$watershed_area_ha)
    ws_areas <- ws_areas[!is.na(ws_areas)]
    if (length(ws_areas) > 0) {
      cat("    - Watershed areas: ", length(ws_areas), " lakes with data\n", sep = "")
    }
  }
  if ("outlet_dist_m" %in% names(fetch_data$results)) {
    has_outlet <- sum(!is.na(fetch_data$results$outlet_dist_m))
    cat("    - Outlet distance: available for ", has_outlet, " sites\n", sep = "")
  }
  if ("inlet_count" %in% names(fetch_data$results)) {
    total_inlets <- sum(fetch_data$results$inlet_count, na.rm = TRUE)
    cat("    - Inlet count: ", total_inlets, " inlet(s) detected\n", sep = "")
  }
}

write.csv(st_drop_geometry(fetch_data$results),
          output_csv,
          row.names = FALSE)
st_write(fetch_data$results, output_gpkg, append = FALSE, quiet = TRUE)
cat("  Saved: ", output_csv, " and ", output_gpkg, "\n\n", sep = "")

# ==============================================================================
# 10. STATIC VISUALIZATIONS
# ==============================================================================

cat("Generating static visualizations...\n")

# Transform data to WGS84 for plotting
results_wgs <- st_transform(fetch_data$results, 4326)
lakes_wgs <- st_transform(fetch_data$lakes, 4326)

# Color palette for exposure categories
exposure_colors <- c("Exposed" = "#D73027", "Moderate" = "#FEE08B", "Sheltered" = "#1A9850")

# Determine visualization output prefix
viz_prefix <- if (exists("output_base")) output_base else "fetch"

# --- 1. Map of all sites colored by exposure ---
cat("  Creating site map...\n")
map_file <- paste0(viz_prefix, "_map.png")
png(map_file, width = 1200, height = 900, res = 120)

# Get bounding box
bbox <- st_bbox(results_wgs)
xlim <- c(bbox["xmin"] - 0.02, bbox["xmax"] + 0.02)
ylim <- c(bbox["ymin"] - 0.02, bbox["ymax"] + 0.02)

p1 <- ggplot() +
  geom_sf(data = lakes_wgs, fill = "lightblue", color = "steelblue", alpha = 0.5) +
  geom_sf(data = results_wgs, aes(color = exposure_category), size = 3) +
  scale_color_manual(values = exposure_colors, name = "Exposure") +
  coord_sf(xlim = xlim, ylim = ylim) +
  labs(
    title = "Fetch Analysis - Site Locations",
    subtitle = paste(nrow(results_wgs), "sites across", length(unique(results_wgs$lake_name)), "lakes"),
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right"
  )
print(p1)
dev.off()
cat("    Saved:", map_file, "\n")

# --- 2. Bar chart of effective fetch by site ---
cat("  Creating fetch bar chart...\n")
barchart_file <- paste0(viz_prefix, "_barchart.png")
png(barchart_file, width = 1400, height = 800, res = 120)

results_df <- st_drop_geometry(results_wgs)
results_df <- results_df[order(results_df$fetch_effective, decreasing = TRUE), ]
results_df$Site <- factor(results_df$Site, levels = results_df$Site)

# Truncate long lake names for labels
results_df$lake_short <- ifelse(
  nchar(results_df$lake_name) > 20,
  paste0(substr(results_df$lake_name, 1, 17), "..."),
  results_df$lake_name
)

p2 <- ggplot(results_df, aes(x = Site, y = fetch_effective / 1000, fill = exposure_category)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 2.5, linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = 5, linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = exposure_colors, name = "Exposure") +
  labs(
    title = "Effective Fetch by Site",
    subtitle = "Dashed lines show exposure thresholds (2.5 km, 5 km)",
    x = "Site",
    y = "Effective Fetch (km)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right"
  )
print(p2)
dev.off()
cat("    Saved:", barchart_file, "\n")

# --- 3. Summary by lake ---
cat("  Creating lake summary chart...\n")
lake_file <- paste0(viz_prefix, "_by_lake.png")
png(lake_file, width = 1400, height = 800, res = 120)

# Aggregate by lake
lake_summary <- aggregate(
  cbind(fetch_effective, fetch_mean, fetch_max) ~ lake_name,
  data = results_df,
  FUN = mean
)
lake_summary$n_sites <- as.numeric(table(results_df$lake_name)[lake_summary$lake_name])
lake_summary <- lake_summary[order(lake_summary$fetch_effective, decreasing = TRUE), ]
lake_summary$lake_name <- factor(lake_summary$lake_name, levels = lake_summary$lake_name)

# Truncate long names
lake_summary$lake_short <- ifelse(
  nchar(as.character(lake_summary$lake_name)) > 25,
  paste0(substr(as.character(lake_summary$lake_name), 1, 22), "..."),
  as.character(lake_summary$lake_name)
)
lake_summary$lake_short <- factor(lake_summary$lake_short,
                                   levels = lake_summary$lake_short)

# Assign exposure category
lake_summary$exposure <- ifelse(lake_summary$fetch_effective < 2500, "Sheltered",
                                ifelse(lake_summary$fetch_effective > 5000, "Exposed", "Moderate"))

p3 <- ggplot(lake_summary, aes(x = lake_short, y = fetch_effective / 1000, fill = exposure)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 2.5, linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = 5, linetype = "dashed", color = "gray40") +
  scale_fill_manual(values = exposure_colors, name = "Exposure") +
  labs(
    title = "Average Effective Fetch by Lake",
    subtitle = paste(nrow(lake_summary), "lakes analyzed"),
    x = "Lake",
    y = "Average Effective Fetch (km)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right"
  )
print(p3)
dev.off()
cat("    Saved:", lake_file, "\n")

# --- 4. Exposure summary pie chart ---
cat("  Creating exposure summary...\n")
exposure_file <- paste0(viz_prefix, "_exposure_summary.png")
png(exposure_file, width = 800, height = 600, res = 120)

exposure_counts <- table(results_df$exposure_category)
exposure_df <- data.frame(
  category = names(exposure_counts),
  count = as.numeric(exposure_counts)
)
exposure_df$pct <- round(100 * exposure_df$count / sum(exposure_df$count), 1)
exposure_df$label <- paste0(exposure_df$category, "\n", exposure_df$count, " (", exposure_df$pct, "%)")

p4 <- ggplot(exposure_df, aes(x = "", y = count, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = exposure_colors, name = "Exposure") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4) +
  labs(
    title = "Site Exposure Distribution",
    subtitle = paste("Total:", sum(exposure_df$count), "sites")
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  )
print(p4)
dev.off()
cat("    Saved:", exposure_file, "\n")

cat("  Visualizations complete!\n\n")

# ==============================================================================
# 11. SHINY APP (only runs in interactive mode)
# ==============================================================================

# Skip Shiny app if running non-interactively (e.g., via Rscript)
if (!interactive()) {
  cat("Script complete!\n")
  cat("  Results:", output_csv, ",", output_gpkg, "\n")
  cat("  Visualizations:", map_file, ",", barchart_file, ",", lake_file, ",", exposure_file, "\n")
  cat("To view the interactive map, run this script in RStudio.\n")
  quit(save = "no", status = 0)
}

cat("Launching Shiny App...\n")

# Get title for app - use detected location name first, then OSM names
if (!is.null(location_name) && !is.na(location_name)) {
  app_title <- paste("Fetch Analysis:", location_name)
} else {
  unique_lakes <- unique(fetch_data$results$lake_name)
  unique_lakes <- unique_lakes[!is.na(unique_lakes)]
  app_title <- if (length(unique_lakes) == 1) {
    paste("Fetch Analysis:", unique_lakes[1])
  } else {
    paste("Fetch Analysis:", length(unique_lakes), "Lakes")
  }
}

ui <- fluidPage(
  titlePanel(app_title),
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
    lakes_wgs <- st_transform(fetch_data$lakes, 4326)
    
    # Build outlet/inlet info strings
    outlet_info <- ifelse(
      !is.na(results_wgs$outlet_dist_m),
      sprintf("<b>Outlet:</b> %.0f m %s<br/>",
              results_wgs$outlet_dist_m,
              ifelse(is.na(results_wgs$outlet_bearing), "", results_wgs$outlet_bearing)),
      ""
    )
    inlet_info <- ifelse(
      !is.na(results_wgs$inlet_nearest_dist_m),
      sprintf("<b>Nearest Inlet:</b> %.0f m %s<br/>",
              results_wgs$inlet_nearest_dist_m,
              ifelse(is.na(results_wgs$inlet_nearest_bearing), "", results_wgs$inlet_nearest_bearing)),
      ""
    )

    popup_html <- sprintf(
      "<div style='font-family:sans-serif; width:220px;'>
       <h4 style='margin:0; border-bottom:1px solid #ccc;'>%s</h4>
       <span style='background:#eee; font-size:0.9em;'>%s</span><br/>
       <span style='color:#666; font-size:0.8em;'>%s</span>
       <center><img src='%s' width='100%%' style='margin:5px 0;'/></center>
       <b>Effective Fetch:</b> %.1f km<br/>
       <b>Orbital Velocity:</b> %.3f m/s<br/>
       %s%s
       </div>",
      results_wgs$Site,
      results_wgs$exposure_category,
      ifelse(is.na(results_wgs$lake_name), "", results_wgs$lake_name),
      results_wgs$rose_plot,
      results_wgs$fetch_effective / 1000,
      results_wgs$orbital_effective,
      outlet_info,
      inlet_info
    )
    
    leaflet(results_wgs) %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addPolygons(data = lakes_wgs,
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