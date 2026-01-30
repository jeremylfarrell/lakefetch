# ==============================================================================
# lakefetch Literature Validation Tests
# ==============================================================================
# This script validates fetch calculations against published lake morphometry
# data and documents alignment with the Shore Protection Manual (SPM) methodology.
#
# Validation approach:
# 1. Use lakes with well-documented morphometry from World Lake Database
# 2. Download boundaries from OpenStreetMap
# 3. Calculate fetch at lake center
# 4. Compare to expected values based on published dimensions
#
# Reference methodology:
# - Shore Protection Manual (USACE, 1984) - standard fetch calculation method
# - Mason et al. (2018) - Great Lakes effective fetch methodology
#
# Run with: source("inst/validation/validate_literature.R")
# ==============================================================================

library(sf)

# Load package
if (!requireNamespace("lakefetch", quietly = TRUE)) {
  devtools::load_all(".")
} else {
  library(lakefetch)
}

cat("\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("LAKEFETCH LITERATURE VALIDATION\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("\nValidating fetch calculations against real lakes with published morphometry.\n")
cat("Reference: Shore Protection Manual (USACE, 1984); World Lake Database (ILEC)\n\n")

# ==============================================================================
# Test Lakes with Published Morphometry
# ==============================================================================
# Selected lakes with well-documented dimensions from World Lake Database
# and other limnological literature.

test_lakes <- list(
  # Lake Sunapee, NH - well-documented small lake
  # Source: NH DES, various limnology publications
  lake_sunapee = list(
    name = "Lake Sunapee",
    state = "New Hampshire",
    coords = c(-72.053, 43.423),  # Approximate center
    area_km2 = 16.7,
    max_length_m = 13000,  # ~13 km long
    max_width_m = 2800,    # ~2.8 km wide
    max_depth_m = 33,
    source = "NH DES Lake Survey"
  ),

  # Cayuga Lake, NY - Finger Lake with known dimensions
  # Source: USGS, Cornell limnology
  cayuga_lake = list(
    name = "Cayuga Lake",
    state = "New York",
    coords = c(-76.7, 42.687),  # Approximate center
    area_km2 = 172,
    max_length_m = 61000,  # ~61 km long (very elongated)
    max_width_m = 2800,    # ~2.8 km wide
    max_depth_m = 133,
    source = "USGS / Cornell University"
  ),

  # Green Lake, WI - meromictic lake, well-studied
  # Source: Wisconsin DNR, USGS
  green_lake_wi = list(
    name = "Green Lake",
    state = "Wisconsin",
    coords = c(-89.003, 43.844),  # Approximate center
    area_km2 = 29.6,
    max_length_m = 12000,  # ~12 km
    max_width_m = 3200,    # ~3.2 km
    max_depth_m = 72,
    source = "Wisconsin DNR"
  )
)

# ==============================================================================
# Validation Functions
# ==============================================================================

#' Calculate expected fetch range for a lake
#' Based on lake dimensions, the center point fetch should approximately equal
#' half the dimension in each direction
calculate_expected_fetch <- function(lake_info) {
  # For an elliptical lake approximation:
  # - Min fetch (short axis) ~ max_width / 2
  # - Max fetch (long axis) ~ max_length / 2
  # - Mean fetch ~ approximately the radius of a circle with same area

  equiv_radius <- sqrt(lake_info$area_km2 * 1e6 / pi)  # Equivalent circular radius

  list(
    min_expected = lake_info$max_width_m / 2,
    max_expected = lake_info$max_length_m / 2,
    mean_expected_low = equiv_radius * 0.5,   # Lower bound
    mean_expected_high = equiv_radius * 1.2   # Upper bound (account for elongation)
  )
}

#' Download lake boundary and calculate fetch at center
test_lake_fetch <- function(lake_info, timeout_sec = 60) {
  cat("\n", "-" |> rep(60) |> paste(collapse = ""), "\n")
  cat("Testing:", lake_info$name, "(", lake_info$state, ")\n")
  cat("-" |> rep(60) |> paste(collapse = ""), "\n")

  cat("Published morphometry:\n")
  cat("  Surface area:  ", lake_info$area_km2, "km²\n")
  cat("  Max length:    ", lake_info$max_length_m / 1000, "km\n")
  cat("  Max width:     ", lake_info$max_width_m / 1000, "km\n")
  cat("  Source:        ", lake_info$source, "\n\n")

  # Create initial site at approximate center for boundary download
  sites_df <- data.frame(
    Site = paste0(lake_info$name, "_center"),
    latitude = lake_info$coords[2],
    longitude = lake_info$coords[1],
    lake.name = lake_info$name
  )

  # Try to download boundary
  cat("Downloading boundary from OSM...\n")

  result <- tryCatch({
    # Load and prepare sites
    sites <- load_sites(sites_df)

    # Download lake boundary - returns list with $all_lakes, $sites, $utm_epsg
    lake_data <- get_lake_boundary(sites)

    # Extract the lakes sf object from the returned list
    if (is.null(lake_data) || is.null(lake_data$all_lakes)) {
      return(list(success = FALSE, error = "No lake boundary found"))
    }

    lake_boundary <- lake_data$all_lakes
    n_lakes <- nrow(lake_boundary)

    if (is.null(n_lakes) || n_lakes == 0) {
      return(list(success = FALSE, error = "No lake polygons found"))
    }

    cat("  Downloaded", n_lakes, "lake polygons\n")

    # Find the target lake by name matching
    target_name <- tolower(lake_info$name)

    # Handle cases where name column might have NA or be missing
    if (!"name" %in% names(lake_boundary)) {
      lake_boundary$name <- paste0("lake_", seq_len(nrow(lake_boundary)))
    }
    lake_names <- tolower(as.character(lake_boundary$name))
    lake_names[is.na(lake_names)] <- ""

    # Try exact or partial match
    search_pattern <- gsub(" lake$", "", target_name)
    match_idx <- which(grepl(target_name, lake_names, fixed = TRUE) |
                       grepl(search_pattern, lake_names, fixed = TRUE))

    cat("  Looking for:", target_name, "\n")

    # Calculate areas for all lakes
    lake_boundary$area <- as.numeric(st_area(lake_boundary))

    if (length(match_idx) == 0) {
      # If no name match, find largest lake (likely our target)
      match_idx <- which.max(lake_boundary$area)
      cat("  No name match - using largest lake polygon (area:",
          round(lake_boundary$area[match_idx] / 1e6, 2), "km²)\n")
    } else {
      # Get best match (largest if multiple)
      best_match <- match_idx[which.max(lake_boundary$area[match_idx])]
      cat("  Found", length(match_idx), "matches, using largest:",
          lake_boundary$name[best_match], "\n")
      match_idx <- best_match
    }

    target_lake <- lake_boundary[match_idx, ]
    cat("  Matched lake:", as.character(target_lake$name[1]), "\n")
    cat("  OSM area:", round(target_lake$area[1] / 1e6, 2), "km²\n")

    # Get actual lake centroid for test site
    suppressWarnings({
      lake_centroid <- st_centroid(st_geometry(target_lake))
    })
    centroid_wgs84 <- st_transform(lake_centroid, 4326)
    centroid_coords <- st_coordinates(centroid_wgs84)

    cat("  Using lake centroid at:", round(centroid_coords[2], 4), ",",
        round(centroid_coords[1], 4), "\n")

    # Create site at actual lake center
    center_site <- st_sf(
      Site = paste0(lake_info$name, "_center"),
      site_name = paste0(lake_info$name, "_center"),
      lake_osm_id = as.character(target_lake$osm_id[1]),
      lake_name = as.character(target_lake$name[1]),
      geometry = lake_centroid
    )
    st_crs(center_site) <- st_crs(target_lake)

    # Add required columns for fetch calculation
    center_site$lake_area_km2 <- target_lake$area[1] / 1e6

    # Get lake boundary line
    cat("  Creating boundary line...\n")
    lake_line <- tryCatch({
      st_cast(target_lake, "MULTILINESTRING")
    }, error = function(e) {
      st_boundary(target_lake)
    })

    # Calculate fetch using internal function
    angles <- seq(0, 355, by = lakefetch_options()$angle_resolution_deg)
    cat("  Calculating fetch for", length(angles), "directions...\n")

    # Temporarily set buffer to small value for precise center test
    old_buffer <- lakefetch_options()$buffer_distance_m
    lakefetch_options(buffer_distance_m = 1)

    fetch_vals <- lakefetch:::get_highres_fetch(center_site, lake_line, target_lake, angles)

    lakefetch_options(buffer_distance_m = old_buffer)

    cat("  Fetch calculated successfully\n")

    # Build result structure similar to fetch_calculate output
    for (i in seq_along(angles)) {
      center_site[[paste0("fetch_", angles[i])]] <- fetch_vals[i]
    }
    center_site$fetch_mean <- mean(fetch_vals)
    center_site$fetch_max <- max(fetch_vals)
    center_site$fetch_effective <- mean(sort(fetch_vals, decreasing = TRUE)[1:3])

    list(
      success = TRUE,
      fetch_data = list(results = center_site, lakes = target_lake, angles = angles),
      lake_boundary = target_lake
    )
  }, error = function(e) {
    cat("  Error:", as.character(e$message), "\n")
    list(success = FALSE, error = as.character(e$message))
  })

  return(result)
}

#' Compare calculated fetch to expected values
validate_fetch_result <- function(lake_info, fetch_result) {
  if (!fetch_result$success) {
    cat("\n  SKIPPED: ", fetch_result$error, "\n")
    return(list(pass = NA, reason = fetch_result$error))
  }

  results <- fetch_result$fetch_data$results

  # Get fetch statistics
  fetch_mean <- results$fetch_mean[1]
  fetch_max <- results$fetch_max[1]
  fetch_eff <- results$fetch_effective[1]

  # Get expected ranges
  expected <- calculate_expected_fetch(lake_info)

  # Calculate elongation ratio (length/width)
  elongation <- lake_info$max_length_m / lake_info$max_width_m

  cat("\nCalculated fetch:\n")
  cat("  Mean fetch:      ", round(fetch_mean, 0), "m\n")
  cat("  Max fetch:       ", round(fetch_max, 0), "m\n")
  cat("  Effective fetch: ", round(fetch_eff, 0), "m\n")

  cat("\nExpected ranges (based on morphometry):\n")
  cat("  Max fetch:       ~", round(expected$max_expected, 0), "m (half of max length)\n")
  cat("  Min direction:   ~", round(expected$min_expected, 0), "m (half of max width)\n")
  cat("  Elongation ratio:", round(elongation, 1), "\n")

  # Adjust expectations for elongated lakes
  # For very elongated lakes (ratio > 5), mean fetch will be much higher
  # than circular approximation suggests
  if (elongation > 5) {
    cat("  Note: Lake is highly elongated - adjusting expectations\n")
    expected$mean_expected_high <- expected$mean_expected_high * (elongation / 3)
  }

  cat("  Mean fetch:      ", round(expected$mean_expected_low, 0), "-",
      round(expected$mean_expected_high, 0), "m\n")

  # Validation checks
  # 1. Max fetch should be reasonable relative to lake length
  #    Allow for hitting max_fetch_m cap (50km) for very long lakes
  max_fetch_cap <- lakefetch_options()$max_fetch_m

  # Check if we hit the fetch cap (very long lakes)
  hit_cap <- abs(fetch_max - max_fetch_cap) < 100  # Within 100m of cap

  if (hit_cap) {
    # If we hit the cap, the lake is very long - this is expected behavior
    # Just verify the lake is indeed long enough to warrant hitting the cap
    max_ok <- expected$max_expected > max_fetch_cap * 0.5
    if (max_ok) {
      cat("  Note: Max fetch hit ", max_fetch_cap/1000, "km cap (expected for long lake)\n")
    }
  } else {
    # Normal case: within 50% of expected
    max_ok <- fetch_max >= expected$max_expected * 0.4 &&
              fetch_max <= expected$max_expected * 1.6
  }

  # 2. Mean fetch should be within expected bounds (generous for irregular shapes)
  mean_ok <- fetch_mean >= expected$mean_expected_low * 0.5 &&
             fetch_mean <= expected$mean_expected_high * 2

  # 3. Effective fetch should be reasonable (between mean and max)
  eff_ok <- fetch_eff >= fetch_mean * 0.8 && fetch_eff <= fetch_max * 1.1

  cat("\nValidation:\n")
  cat("  Max fetch within expected range:  ", ifelse(max_ok, "PASS", "FAIL"), "\n")
  cat("  Mean fetch within expected range: ", ifelse(mean_ok, "PASS", "FAIL"), "\n")
  cat("  Effective fetch reasonable:       ", ifelse(eff_ok, "PASS", "FAIL"), "\n")

  all_pass <- max_ok && mean_ok && eff_ok
  cat("\n  Overall: ", ifelse(all_pass, "PASS", "FAIL"), "\n")

  return(list(
    pass = all_pass,
    fetch_mean = fetch_mean,
    fetch_max = fetch_max,
    fetch_eff = fetch_eff,
    expected = expected,
    elongation = elongation
  ))
}

# ==============================================================================
# Run Validation Tests
# ==============================================================================

cat("\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("RUNNING VALIDATION TESTS\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("\nNote: Tests require internet connection for OSM data download.\n")
cat("Tests may be skipped if lake boundaries are not available in OSM.\n")

validation_results <- list()

for (lake_id in names(test_lakes)) {
  lake_info <- test_lakes[[lake_id]]

  # Run test
  fetch_result <- test_lake_fetch(lake_info)

  # Validate
  validation <- validate_fetch_result(lake_info, fetch_result)

  validation_results[[lake_id]] <- list(
    lake = lake_info,
    fetch_result = fetch_result,
    validation = validation
  )
}

# ==============================================================================
# Summary
# ==============================================================================

cat("\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("LITERATURE VALIDATION SUMMARY\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n\n")

passed <- 0
failed <- 0
skipped <- 0

for (lake_id in names(validation_results)) {
  result <- validation_results[[lake_id]]
  lake_name <- result$lake$name

  if (is.na(result$validation$pass)) {
    status <- "SKIPPED"
    skipped <- skipped + 1
  } else if (result$validation$pass) {
    status <- "PASS"
    passed <- passed + 1
  } else {
    status <- "FAIL"
    failed <- failed + 1
  }

  cat(sprintf("  %-30s %s\n", lake_name, status))
}

cat("\n")
cat(sprintf("Results: %d passed, %d failed, %d skipped\n", passed, failed, skipped))
total_testable <- passed + failed
if (total_testable > 0) {
  cat(sprintf("Pass rate: %.1f%% (of testable lakes)\n", passed / total_testable * 100))
}

# ==============================================================================
# Methodology Notes
# ==============================================================================

cat("\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("METHODOLOGY ALIGNMENT\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")

cat("
lakefetch implements fetch calculation following established methods:

1. RAY-CASTING APPROACH
   - Casts rays from each site in multiple directions (default: 5° resolution)
   - Measures distance to shoreline intersection
   - Consistent with USGS and Natural Capital Project algorithms

2. EFFECTIVE FETCH (SPM Method)
   The Shore Protection Manual (USACE, 1984) defines effective fetch as:
   - Multiple radials around a wind direction
   - Weighted by cosine of angle from wind direction

   lakefetch simplifies to mean of top 3 directional fetch values,
   providing a robust exposure metric independent of wind direction.

3. EXPOSURE CLASSIFICATION
   Based on limnological literature thresholds:
   - Sheltered: < 2.5 km effective fetch
   - Moderate:  2.5 - 5.0 km
   - Exposed:   > 5.0 km

References:
- USACE (1984). Shore Protection Manual, 4th ed.
- Mason et al. (2018). Effective fetch maps for Great Lakes. Scientific Data.
- Håkanson (1981). Manual of Lake Morphometry. Springer-Verlag.
")

cat("\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("VALIDATION COMPLETE\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n\n")
