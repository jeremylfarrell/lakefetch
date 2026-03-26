# ==============================================================================
# Tests for lake_sources.R with mocked API calls
# ==============================================================================

# Helper to create mock OSM polygon data (what standardize_osm_sf returns)
create_mock_water_polys <- function(center_lon = -74, center_lat = 43,
                                     n_lakes = 1) {
  lakes <- lapply(seq_len(n_lakes), function(i) {
    offset <- (i - 1) * 0.05
    coords <- matrix(c(
      center_lon + offset, center_lat,
      center_lon + offset + 0.01, center_lat,
      center_lon + offset + 0.01, center_lat + 0.01,
      center_lon + offset, center_lat + 0.01,
      center_lon + offset, center_lat
    ), ncol = 2, byrow = TRUE)
    sf::st_polygon(list(coords))
  })

  sf::st_sf(
    osm_id = paste0("lake_", seq_len(n_lakes)),
    name = paste0("Lake ", LETTERS[seq_len(n_lakes)]),
    geometry = sf::st_sfc(lakes, crs = 4326)
  )
}

# --- download_lake_osm with small spread (single bbox path) ---

test_that("download_lake_osm works with mocked single-bbox query", {
  sites <- data.frame(
    Site = c("S1", "S2"),
    latitude = c(43.005, 43.006),
    longitude = c(-74.005, -74.006),
    stringsAsFactors = FALSE
  )
  sites_sf <- sf::st_as_sf(sites, coords = c("longitude", "latitude"), crs = 4326)

  mock_polys <- create_mock_water_polys()
  mock_polys$area_km2 <- as.numeric(sf::st_area(sf::st_transform(mock_polys, 32618))) / 1e6

  local_mocked_bindings(
    download_lake_osm_single = function(bbox_vec, lake_names = NULL,
                                         overpass_servers = NULL,
                                         name_only = FALSE) {
      list(mock_polys)
    },
    .package = "lakefetch"
  )

  result <- lakefetch:::download_lake_osm(sites)

  expect_type(result, "list")
  expect_true("all_lakes" %in% names(result))
  expect_true("sites" %in% names(result))
  expect_true("utm_epsg" %in% names(result))
  expect_true(nrow(result$all_lakes) >= 1)
})

# --- download_lake_osm with large spread (cluster path) ---
# Note: cluster path calls osmdata functions directly (not download_lake_osm_single),
# so we mock extract_osm_polys to intercept at that level.

test_that("download_lake_osm clusters sites when spread > 0.5 degrees", {
  # The cluster path calls osmdata functions directly (not download_lake_osm_single),
  # which cannot be mocked without network access. cluster_sites() logic is tested
  # separately in test-lake-sources-helpers.R.
  skip("Cluster path requires network access; cluster_sites tested separately")
})

test_that("download_lake_osm handles lake_name column for name-based queries", {
  sites <- data.frame(
    Site = c("S1", "S2"),
    latitude = c(43.005, 43.006),
    longitude = c(-74.005, -74.006),
    lake_name = c("Mirror Lake", "Mirror Lake"),
    stringsAsFactors = FALSE
  )

  mock_polys <- create_mock_water_polys()
  mock_polys$name <- "Mirror Lake"
  mock_polys$area_km2 <- as.numeric(sf::st_area(sf::st_transform(mock_polys, 32618))) / 1e6

  captured_names <- NULL
  local_mocked_bindings(
    download_lake_osm_single = function(bbox_vec, lake_names = NULL,
                                         overpass_servers = NULL,
                                         name_only = FALSE) {
      captured_names <<- lake_names
      list(mock_polys)
    },
    .package = "lakefetch"
  )

  result <- lakefetch:::download_lake_osm(sites)

  # Should have passed lake names to the download function
  expect_true("Mirror Lake" %in% captured_names)
})

test_that("download_lake_osm filters small water bodies", {
  sites <- data.frame(
    Site = "S1",
    latitude = 43.005,
    longitude = -74.005,
    stringsAsFactors = FALSE
  )

  # Create two polys - one large, one tiny
  large_poly <- create_mock_water_polys()
  large_poly$area_km2 <- 1.5

  tiny_coords <- matrix(c(
    -74.001, 43.001,
    -74.0009, 43.001,
    -74.0009, 43.0011,
    -74.001, 43.0011,
    -74.001, 43.001
  ), ncol = 2, byrow = TRUE)
  tiny_poly <- sf::st_sf(
    osm_id = "tiny",
    name = "Puddle",
    geometry = sf::st_sfc(sf::st_polygon(list(tiny_coords)), crs = 4326)
  )
  tiny_poly$area_km2 <- 0.00001  # Below minimum threshold

  both <- rbind(large_poly, tiny_poly)

  local_mocked_bindings(
    download_lake_osm_single = function(bbox_vec, lake_names = NULL,
                                         overpass_servers = NULL,
                                         name_only = FALSE) {
      list(both)
    },
    .package = "lakefetch"
  )

  result <- lakefetch:::download_lake_osm(sites)

  # Tiny water bodies should be filtered out
  expect_true(all(result$all_lakes$area_km2 > 0.0001))
})

test_that("download_lake_osm warns and creates fallback when no water bodies found", {
  sites <- data.frame(
    Site = "S1",
    latitude = 43.005,
    longitude = -74.005,
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    download_lake_osm_single = function(bbox_vec, lake_names = NULL,
                                         overpass_servers = NULL,
                                         name_only = FALSE) {
      list()  # No results
    },
    .package = "lakefetch"
  )

  expect_warning(
    result <- lakefetch:::download_lake_osm(sites),
    "No water bodies found"
  )

  # Should return empty lake set with sites, not a fallback boundary
  expect_type(result, "list")
  expect_true("all_lakes" %in% names(result))
  expect_equal(nrow(result$all_lakes), 0)
  expect_true("sites" %in% names(result))
  expect_equal(nrow(result$sites), 1)
})

# --- download_lake_osm_single with mocked queries ---

test_that("download_lake_osm_single tries name query first", {
  mock_osm_result <- list(
    osm_polygons = sf::st_sf(
      osm_id = "123",
      name = "Test Lake",
      geometry = sf::st_sfc(
        sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
                                    ncol = 2, byrow = TRUE))),
        crs = 4326
      )
    ),
    osm_multipolygons = NULL
  )

  local_mocked_bindings(
    query_osm_by_name = function(bbox, names, overpass_servers, max_attempts = 3) {
      mock_osm_result
    },
    .package = "lakefetch"
  )

  bbox <- c(left = -74.1, bottom = 42.9, right = -73.9, top = 43.1)
  result <- lakefetch:::download_lake_osm_single(
    bbox, lake_names = "Test Lake",
    overpass_servers = c("https://overpass-api.de/api/interpreter")
  )

  expect_true(length(result) > 0)
})

test_that("get_lake_boundary dispatches to OSM download when no file", {
  sites <- data.frame(
    Site = "S1",
    latitude = 43.005,
    longitude = -74.005,
    stringsAsFactors = FALSE
  )

  mock_polys <- create_mock_water_polys()
  mock_polys$area_km2 <- as.numeric(sf::st_area(sf::st_transform(mock_polys, 32618))) / 1e6

  local_mocked_bindings(
    download_lake_osm_single = function(bbox_vec, lake_names = NULL,
                                         overpass_servers = NULL,
                                         name_only = FALSE) {
      list(mock_polys)
    },
    .package = "lakefetch"
  )

  result <- lakefetch::get_lake_boundary(sites)

  expect_type(result, "list")
  expect_true("all_lakes" %in% names(result))
})
