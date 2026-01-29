# ==============================================================================
# Data Loading and Validation Functions
# ==============================================================================

#' Load Sites from CSV or Data Frame
#'
#' Load and validate site data for fetch calculation. Automatically detects
#' coordinate columns (latitude/longitude) and cleans the data.
#'
#' @param x Either a file path to a CSV file or a data.frame with site data.
#'
#' @return A data.frame with columns Site, latitude, longitude, and any
#'   additional columns from the input. Includes attributes "location_name"
#'   and "location_column" if a location was detected.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Detects latitude/longitude columns (names starting with "lat"/"lon")
#'   \item Cleans coordinate values (removes non-numeric characters)
#'   \item Creates Site column if not present
#'   \item Removes rows with invalid or missing coordinates
#'   \item Detects location name from data columns or filename
#' }
#'
#' @examples
#' \dontrun{
#' # Load from CSV
#' sites <- load_sites("my_lake_sites.csv")
#'
#' # Load from data frame
#' df <- data.frame(
#'   Site = c("A", "B", "C"),
#'   latitude = c(43.42, 43.43, 43.41),
#'   longitude = c(-73.69, -73.68, -73.70)
#' )
#' sites <- load_sites(df)
#' }
#'
#' @export
load_sites <- function(x) {

  if (is.character(x)) {
    # Load from file
    file_path <- x
    message("Loading data from: ", file_path)

    # Try multiple encoding options
    sites_raw <- tryCatch({
      utils::read.csv(file_path, stringsAsFactors = FALSE, strip.white = TRUE,
                      fileEncoding = "UTF-8")
    }, error = function(e) {
      tryCatch({
        utils::read.csv(file_path, stringsAsFactors = FALSE, strip.white = TRUE,
                        fileEncoding = "latin1")
      }, error = function(e2) {
        utils::read.csv(file_path, stringsAsFactors = FALSE, strip.white = TRUE)
      })
    })

  } else if (is.data.frame(x)) {
    sites_raw <- x
    file_path <- NULL
  } else {
    stop("x must be a file path or data.frame")
  }

  message("  Loaded ", nrow(sites_raw), " rows with columns: ",
          paste(names(sites_raw), collapse = ", "))

  # Find coordinate columns (flexible naming)
  col_names_lower <- tolower(names(sites_raw))
  lat_col_idx <- grep("^lat", col_names_lower)[1]
  lon_col_idx <- grep("^lon", col_names_lower)[1]

  if (is.na(lat_col_idx) || is.na(lon_col_idx)) {
    stop("Could not find latitude/longitude columns.\n",
         "Column names should start with 'lat' and 'lon'\n",
         "Found: ", paste(names(sites_raw), collapse = ", "))
  }

  lat_col <- names(sites_raw)[lat_col_idx]
  lon_col <- names(sites_raw)[lon_col_idx]

  message("  Using columns: Latitude = ", lat_col, ", Longitude = ", lon_col)

  # Extract and clean coordinates
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
    site_name <- as.character(sites_raw[[names(sites_raw)[site_col_idx]]])
  } else if ("Site" %in% names(sites_raw)) {
    site_name <- as.character(sites_raw$Site)
  } else {
    site_name <- paste0("Site_", seq_len(nrow(sites_raw)))
    message("  No 'Site' column found, created generic names")
  }

  # Create cleaned data frame with original site name preserved
  sites_clean <- data.frame(
    site_name = site_name,
    latitude = latitude,
    longitude = longitude,
    row_id = seq_len(nrow(sites_raw)),
    stringsAsFactors = FALSE
  )

  # Report problems
  na_mask <- is.na(sites_clean$latitude) | is.na(sites_clean$longitude)
  if (any(na_mask)) {
    warning("Found ", sum(na_mask), " rows with invalid coordinates")
  }

  # Remove invalid coordinates
  sites_clean <- sites_clean[!na_mask, ]

  # Validate ranges
  invalid_lat <- sites_clean$latitude < -90 | sites_clean$latitude > 90
  invalid_lon <- sites_clean$longitude < -180 | sites_clean$longitude > 180

  if (any(invalid_lat)) {
    warning("Removing ", sum(invalid_lat), " rows with invalid latitude")
    sites_clean <- sites_clean[!invalid_lat, ]
  }
  if (any(invalid_lon)) {
    warning("Removing ", sum(invalid_lon), " rows with invalid longitude")
    sites_clean <- sites_clean[!invalid_lon, ]
  }

  # Look for datetime column BEFORE creating unique IDs
  datetime_patterns <- c("^datetime$", "^date[_.]?time$", "^sample[_.]?date",
                          "^sample[_.]?time", "^date$", "^time$", "^timestamp$")
  datetime_col_idx <- NA
  for (pattern in datetime_patterns) {
    idx <- grep(pattern, col_names_lower)[1]
    if (!is.na(idx)) {
      datetime_col_idx <- idx
      break
    }
  }

  # Parse datetime if found
  datetime_parsed <- NULL
  if (!is.na(datetime_col_idx)) {
    datetime_col_name <- names(sites_raw)[datetime_col_idx]
    datetime_raw <- sites_raw[[datetime_col_idx]]

    datetime_parsed <- tryCatch({
      as.POSIXct(datetime_raw)
    }, error = function(e) {
      # Try common formats
      formats <- c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d",
                   "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M", "%m/%d/%Y",
                   "%d-%m-%Y %H:%M:%S", "%d-%m-%Y")
      for (fmt in formats) {
        parsed <- tryCatch(as.POSIXct(datetime_raw, format = fmt), error = function(e) NULL)
        if (!is.null(parsed) && !all(is.na(parsed))) return(parsed)
      }
      return(NULL)
    })

    if (!is.null(datetime_parsed) && !all(is.na(datetime_parsed))) {
      # Add datetime to sites_clean using row_id to match
      sites_clean$datetime <- datetime_parsed[sites_clean$row_id]
      message("  Detected datetime column: ", datetime_col_name)
    } else {
      message("  Warning: Could not parse datetime column '", datetime_col_name, "'")
    }
  }

  # Create unique Site identifiers
  # If we have datetime, use site_name + date; otherwise use site_name + row number
  if ("datetime" %in% names(sites_clean) && !all(is.na(sites_clean$datetime))) {
    # Use date portion for ID (multiple samples same day at same site get numbered)
    date_str <- format(sites_clean$datetime, "%Y%m%d")
    date_str[is.na(date_str)] <- "nodate"

    # Create base ID
    base_id <- paste0(sites_clean$site_name, "_", date_str)

    # Handle duplicates within same site+date by adding sequence number
    sites_clean$Site <- ave(base_id, base_id, FUN = function(x) {
      if (length(x) == 1) return(x)
      paste0(x, "_", seq_along(x))
    })
  } else {
    # No datetime - use row numbers for uniqueness
    sites_clean$Site <- paste0(sites_clean$site_name, "_", sites_clean$row_id)
  }

  # Remove true duplicates (same Site ID, same coordinates)
  dup_key <- paste(sites_clean$Site, sites_clean$latitude, sites_clean$longitude, sep = "_")
  sites_clean <- sites_clean[!duplicated(dup_key), ]

  # Clean up: remove row_id and reorder columns so Site is first
  sites_clean$row_id <- NULL
  col_order <- c("Site", "site_name", "latitude", "longitude")
  if ("datetime" %in% names(sites_clean)) {
    col_order <- c(col_order, "datetime")
  }
  sites_clean <- sites_clean[, col_order]

  message("  Final valid samples: ", nrow(sites_clean))

  if (nrow(sites_clean) == 0) {
    stop("No valid coordinates remaining after cleaning")
  }

  # Detect location name from columns
  location_info <- detect_location_name(sites_raw)

  # Also try to extract from filename if no column found
  if (is.null(location_info$name) && !is.null(file_path)) {
    filename <- basename(file_path)
    filename_no_ext <- tools::file_path_sans_ext(filename)
    # Clean up common prefixes/suffixes
    clean_filename <- gsub("^[0-9]{4}\\s*", "", filename_no_ext)
    clean_filename <- gsub("_+", " ", clean_filename)
    clean_filename <- trimws(clean_filename)
    if (nchar(clean_filename) > 0) {
      location_info$name <- clean_filename
      message("  Location name from filename: ", location_info$name)
    }
  }

  attr(sites_clean, "location_name") <- location_info$name
  attr(sites_clean, "location_column") <- location_info$column

  return(sites_clean)
}

#' Detect Location Name from Column Names
#'
#' Look for columns that might contain lake/site/location names and extract
#' a location name from the data.
#'
#' @param sites_raw Raw data frame with site data
#'
#' @return A list with elements:
#'   \item{name}{Detected location name or NULL}
#'   \item{column}{Name of the column used for detection or NULL}
#'
#' @noRd
detect_location_name <- function(sites_raw) {
  # Look for columns that might contain lake/site/location names
  col_names_lower <- tolower(names(sites_raw))

  # Priority order for location name columns
  location_patterns <- c(
    "^lake[._]?name$",
    "^lake$",
    "^location[._]?name$",
    "^location$",
    "^site[._]?name$",
    "^place[._]?name$",
    "^place$",
    "^water[._]?body$",
    "^waterbody$",
    "^reservoir$",
    "^pond$"
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

  if (!is.null(detected_name)) {
    message("  Detected location from column '", detected_col, "': ", detected_name)
  }

  return(list(
    name = detected_name,
    column = detected_col
  ))
}

#' Sanitize a String for Use in Filenames
#'
#' Remove or replace invalid filename characters.
#'
#' @param name Character string to sanitize
#'
#' @return A sanitized string safe for use as a filename
#'
#' @examples
#' sanitize_filename("Lake O'Brien (2024)")
#' # Returns "Lake_OBrien_2024"
#'
#' @export
sanitize_filename <- function(name) {
  if (is.null(name) || is.na(name)) return("fetch_results")

  # Remove or replace invalid filename characters
  clean <- gsub("[<>:\"/\\|?*']", "", name)
  clean <- gsub("\\s+", "_", clean)
  clean <- gsub("[()]", "", clean)
  clean <- gsub("_+", "_", clean)
  clean <- gsub("^_|_$", "", clean)

  if (nchar(clean) == 0) return("fetch_results")
  return(clean)
}
