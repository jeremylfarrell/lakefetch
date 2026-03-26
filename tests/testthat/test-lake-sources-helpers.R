# ==============================================================================
# Tests for lake_sources.R helper functions (no network required)
# ==============================================================================

# --- cluster_sites ---

test_that("cluster_sites groups nearby sites together", {
  # Create 4 sites: 2 close together, 2 far apart
  sites <- sf::st_sf(
    Site = c("A", "B", "C", "D"),
    geometry = sf::st_sfc(
      sf::st_point(c(-74.00, 43.00)),
      sf::st_point(c(-74.05, 43.05)),  # Same 0.1-degree grid cell as A
      sf::st_point(c(-80.00, 40.00)),  # Different grid cell
      sf::st_point(c(-80.05, 40.05)),  # Same grid cell as C
      crs = 4326
    )
  )

  clusters <- lakefetch:::cluster_sites(sites)

  expect_type(clusters, "list")
  expect_true(length(clusters) >= 2)

  # Total indices should equal total sites
  all_indices <- sort(unlist(clusters))
  expect_equal(all_indices, 1:4)
})

test_that("cluster_sites puts all sites in one cluster when close", {
  sites <- sf::st_sf(
    Site = c("A", "B", "C"),
    geometry = sf::st_sfc(
      sf::st_point(c(-74.01, 43.01)),
      sf::st_point(c(-74.02, 43.02)),
      sf::st_point(c(-74.03, 43.03)),
      crs = 4326
    )
  )

  clusters <- lakefetch:::cluster_sites(sites)
  expect_equal(length(clusters), 1)
  expect_equal(sort(clusters[[1]]), 1:3)
})

test_that("cluster_sites handles single site", {
  sites <- sf::st_sf(
    Site = "A",
    geometry = sf::st_sfc(sf::st_point(c(-74, 43)), crs = 4326)
  )

  clusters <- lakefetch:::cluster_sites(sites)
  expect_equal(length(clusters), 1)
  expect_equal(clusters[[1]], 1)
})

# --- standardize_osm_sf ---

test_that("standardize_osm_sf returns NULL for NULL/empty input", {
  expect_null(lakefetch:::standardize_osm_sf(NULL))
  empty_sf <- sf::st_sf(
    osm_id = character(0),
    geometry = sf::st_sfc(crs = 4326)
  )
  expect_null(lakefetch:::standardize_osm_sf(empty_sf))
})

test_that("standardize_osm_sf extracts osm_id and name columns", {
  mock_osm <- sf::st_sf(
    osm_id = c("123", "456"),
    name = c("Lake A", "Lake B"),
    natural = c("water", "water"),
    water = c("lake", "lake"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(2, 2, 3, 2, 3, 3, 2, 3, 2, 2), ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )

  result <- lakefetch:::standardize_osm_sf(mock_osm)
  expect_equal(nrow(result), 2)
  expect_true("osm_id" %in% names(result))
  expect_true("name" %in% names(result))
  expect_equal(result$osm_id, c("123", "456"))
  expect_equal(result$name, c("Lake A", "Lake B"))
})

test_that("standardize_osm_sf handles missing name column", {
  mock_osm <- sf::st_sf(
    osm_id = "789",
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )

  result <- lakefetch:::standardize_osm_sf(mock_osm)
  expect_equal(nrow(result), 1)
  expect_true(is.na(result$name))
})

# --- extract_osm_polys ---

test_that("extract_osm_polys returns empty list for empty data", {
  mock_data <- list(osm_polygons = NULL, osm_multipolygons = NULL)
  result <- lakefetch:::extract_osm_polys(mock_data)
  expect_equal(length(result), 0)
})

test_that("extract_osm_polys extracts polygons and multipolygons", {
  poly_sf <- sf::st_sf(
    osm_id = "1",
    name = "Lake One",
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )

  mpoly_sf <- sf::st_sf(
    osm_id = "2",
    name = "Lake Two",
    geometry = sf::st_sfc(
      sf::st_multipolygon(list(
        list(matrix(c(3, 3, 4, 3, 4, 4, 3, 4, 3, 3), ncol = 2, byrow = TRUE))
      )),
      crs = 4326
    )
  )

  mock_data <- list(osm_polygons = poly_sf, osm_multipolygons = mpoly_sf)
  result <- lakefetch:::extract_osm_polys(mock_data)

  expect_equal(length(result), 2)
})

# --- assign_sites_to_lakes ---

test_that("assign_sites_to_lakes assigns site inside lake polygon", {
  lake <- create_circular_lake(radius = 1000)
  site <- create_site(500000, 4800000, name = "Center Site")
  # Remove pre-existing lake columns so assign can add them fresh
  site$lake_osm_id <- NULL
  site$lake_name <- NULL

  result <- assign_sites_to_lakes(site, lake, tolerance_m = 100)

  expect_equal(result$lake_osm_id, "test_circle")
  expect_equal(result$lake_name, "Test Circular Lake")
})

test_that("assign_sites_to_lakes matches nearby site with tolerance", {
  lake <- create_circular_lake(radius = 1000)
  # Site 50m outside the lake boundary
  site <- create_site(501050, 4800000, name = "Nearby Site")
  site$lake_osm_id <- NULL
  site$lake_name <- NULL

  result <- assign_sites_to_lakes(site, lake, tolerance_m = 100)
  expect_equal(result$lake_osm_id, "test_circle")
})

test_that("assign_sites_to_lakes does not match distant site", {
  lake <- create_circular_lake(radius = 1000)
  # Site 5km outside the lake
  site <- create_site(506000, 4800000, name = "Far Site")
  site$lake_osm_id <- NULL
  site$lake_name <- NULL

  result <- assign_sites_to_lakes(site, lake, tolerance_m = 100)
  expect_true(is.na(result$lake_osm_id))
})

test_that("assign_sites_to_lakes adds required columns", {
  lake <- create_circular_lake(radius = 1000)
  site <- create_site(500000, 4800000, name = "Test")
  site$lake_osm_id <- NULL
  site$lake_name <- NULL

  result <- assign_sites_to_lakes(site, lake, tolerance_m = 100)

  expect_true("lake_osm_id" %in% names(result))
  expect_true("lake_name" %in% names(result))
  expect_true("lake_area_km2" %in% names(result))
})

test_that("assign_sites_to_lakes uses name-based matching for unmatched sites", {
  # Create a lake at one location
  lake <- create_circular_lake(
    center_x = 500000, center_y = 4800000, radius = 1000
  )

  # Site outside lake but with matching lake_name column
  site <- sf::st_sf(
    Site = "Named Site",
    site_name = "Named Site",
    lake_name = "Test Circular Lake",
    geometry = sf::st_sfc(sf::st_point(c(500500, 4800500)), crs = 32618)
  )

  result <- assign_sites_to_lakes(site, lake, tolerance_m = 100)

  # Name-based match should work within 5x tolerance (500m)
  expect_equal(result$lake_osm_id, "test_circle")
})

test_that("assign_sites_to_lakes handles multiple lakes", {
  lake1 <- create_circular_lake(
    center_x = 500000, center_y = 4800000, radius = 500
  )
  lake2 <- create_circular_lake(
    center_x = 505000, center_y = 4800000, radius = 500
  )
  lake2$osm_id <- "test_circle_2"
  lake2$name <- "Second Lake"

  lakes <- rbind(lake1, lake2)

  site1 <- create_site(500000, 4800000, name = "Site A")
  site2 <- create_site(505000, 4800000, name = "Site B")
  sites <- rbind(site1, site2)
  sites$lake_osm_id <- NULL
  sites$lake_name <- NULL

  result <- assign_sites_to_lakes(sites, lakes, tolerance_m = 100)

  expect_equal(nrow(result), 2)
  expect_equal(result$lake_osm_id[1], "test_circle")
  expect_equal(result$lake_osm_id[2], "test_circle_2")
})

test_that("assign_sites_to_lakes fills in missing lake names from polygons", {
  lake <- create_circular_lake(radius = 1000)
  site <- create_site(500000, 4800000, name = "Center")
  site$lake_osm_id <- NULL
  site$lake_name <- NULL

  result <- assign_sites_to_lakes(site, lake, tolerance_m = 100)

  # Name should be filled from the polygon
  expect_false(is.na(result$lake_name))
  expect_equal(result$lake_name, "Test Circular Lake")
})

# --- load_lake_file ---

test_that("load_lake_file loads a geopackage file", {
  # Create a temporary geopackage from the test lake
  lake <- create_circular_lake(radius = 1000)
  lake_wgs84 <- sf::st_transform(lake, 4326)

  tmp_gpkg <- tempfile(fileext = ".gpkg")
  sf::st_write(lake_wgs84, tmp_gpkg, quiet = TRUE)

  # Create site data
  site_sf <- sf::st_sf(
    Site = "Test",
    geometry = sf::st_sfc(sf::st_point(c(-74, 43)), crs = 4326)
  )

  result <- lakefetch:::load_lake_file(site_sf, tmp_gpkg)

  expect_type(result, "list")
  expect_true("all_lakes" %in% names(result))
  expect_true("sites" %in% names(result))
  expect_true("utm_epsg" %in% names(result))
  expect_true("osm_id" %in% names(result$all_lakes))
  expect_true("area_km2" %in% names(result$all_lakes))
  expect_true(result$all_lakes$area_km2 > 0)

  unlink(tmp_gpkg)
})

test_that("load_lake_file transforms CRS to UTM", {
  lake <- create_circular_lake(radius = 1000)
  lake_wgs84 <- sf::st_transform(lake, 4326)

  tmp_gpkg <- tempfile(fileext = ".gpkg")
  sf::st_write(lake_wgs84, tmp_gpkg, quiet = TRUE)

  site_sf <- sf::st_sf(
    Site = "Test",
    geometry = sf::st_sfc(sf::st_point(c(-74, 43)), crs = 4326)
  )

  result <- lakefetch:::load_lake_file(site_sf, tmp_gpkg)

  # UTM EPSG should be in the 326xx or 327xx range
  expect_true(result$utm_epsg >= 32600 && result$utm_epsg <= 32761)

  # Lake and sites should be in UTM
  lake_crs <- sf::st_crs(result$all_lakes)$epsg
  sites_crs <- sf::st_crs(result$sites)$epsg
  expect_equal(lake_crs, result$utm_epsg)
  expect_equal(sites_crs, result$utm_epsg)

  unlink(tmp_gpkg)
})

test_that("get_lake_boundary dispatches to load_lake_file when file provided", {
  lake <- create_circular_lake(radius = 1000)
  lake_wgs84 <- sf::st_transform(lake, 4326)

  tmp_gpkg <- tempfile(fileext = ".gpkg")
  sf::st_write(lake_wgs84, tmp_gpkg, quiet = TRUE)

  site_sf <- sf::st_sf(
    Site = "Test",
    geometry = sf::st_sfc(sf::st_point(c(-74, 43)), crs = 4326)
  )

  result <- lakefetch::get_lake_boundary(site_sf, file = tmp_gpkg)

  expect_type(result, "list")
  expect_true("all_lakes" %in% names(result))
  expect_equal(result$all_lakes$osm_id, "local_file")

  unlink(tmp_gpkg)
})

test_that("assign_sites_to_lakes reports unmatched sites diagnostics", {
  lake <- create_circular_lake(radius = 100)  # Very small lake
  # Site far from lake
  site <- create_site(510000, 4810000, name = "Very Far")
  site$lake_osm_id <- NULL
  site$lake_name <- NULL

  # Should complete without error and report unmatched
  expect_message(
    result <- assign_sites_to_lakes(site, lake, tolerance_m = 50),
    "Unmatched"
  )
  expect_true(is.na(result$lake_osm_id))
})

test_that("assign_sites_to_lakes handles lake with no name", {
  lake <- create_circular_lake(radius = 1000)
  lake$name <- NA_character_

  site <- create_site(500000, 4800000, name = "Inside")
  site$lake_osm_id <- NULL
  site$lake_name <- NULL

  result <- assign_sites_to_lakes(site, lake, tolerance_m = 100)

  # Should assign to lake even without name
  expect_equal(result$lake_osm_id, "test_circle")
  # Name should get a fallback
  expect_false(is.na(result$lake_name))
})

test_that("assign_sites_to_lakes uses default tolerance from options", {
  lake <- create_circular_lake(radius = 1000)
  site <- create_site(500000, 4800000, name = "Default Tol")
  site$lake_osm_id <- NULL
  site$lake_name <- NULL

  # Call without specifying tolerance - should use option
  result <- assign_sites_to_lakes(site, lake)

  expect_equal(result$lake_osm_id, "test_circle")
})

test_that("assign_sites_to_lakes buffer check matches closest lake shore", {
  # Two lakes close together - site is outside both but closer to one
  lake1 <- create_circular_lake(
    center_x = 500000, center_y = 4800000, radius = 500
  )
  lake2 <- create_circular_lake(
    center_x = 502000, center_y = 4800000, radius = 500
  )
  lake2$osm_id <- "lake_2"
  lake2$name <- "Lake Two"
  lakes <- rbind(lake1, lake2)

  # Site between the two lakes, closer to lake1
  site <- create_site(500600, 4800000, name = "Between")
  site$lake_osm_id <- NULL
  site$lake_name <- NULL

  result <- assign_sites_to_lakes(site, lakes, tolerance_m = 200)

  # Should match to the closer lake
  expect_equal(result$lake_osm_id, "test_circle")
})

# --- load_lake_file edge cases ---

test_that("load_lake_file handles data.frame input (non-sf)", {
  lake <- create_circular_lake(radius = 1000)
  lake_wgs84 <- sf::st_transform(lake, 4326)

  tmp_gpkg <- tempfile(fileext = ".gpkg")
  sf::st_write(lake_wgs84, tmp_gpkg, quiet = TRUE)

  # Pass a plain data.frame with longitude/latitude columns
  site_df <- data.frame(
    Site = "Test",
    latitude = 43.0,
    longitude = -74.0,
    stringsAsFactors = FALSE
  )

  result <- lakefetch:::load_lake_file(site_df, tmp_gpkg)

  expect_type(result, "list")
  expect_true("all_lakes" %in% names(result))
  expect_true("sites" %in% names(result))

  unlink(tmp_gpkg)
})

test_that("load_lake_file handles multipolygon geometry", {
  # Create a multipolygon
  coords1 <- matrix(c(
    -74.01, 43.0,
    -73.99, 43.0,
    -73.99, 43.02,
    -74.01, 43.02,
    -74.01, 43.0
  ), ncol = 2, byrow = TRUE)
  coords2 <- matrix(c(
    -74.05, 43.0,
    -74.03, 43.0,
    -74.03, 43.02,
    -74.05, 43.02,
    -74.05, 43.0
  ), ncol = 2, byrow = TRUE)

  mpoly <- sf::st_multipolygon(list(list(coords1), list(coords2)))
  lake <- sf::st_sf(
    name = "Multi Lake",
    geometry = sf::st_sfc(mpoly, crs = 4326)
  )

  tmp_gpkg <- tempfile(fileext = ".gpkg")
  sf::st_write(lake, tmp_gpkg, quiet = TRUE)

  site_sf <- sf::st_sf(
    Site = "Test",
    geometry = sf::st_sfc(sf::st_point(c(-74.0, 43.01)), crs = 4326)
  )

  result <- lakefetch:::load_lake_file(site_sf, tmp_gpkg)

  # Should extract the largest polygon
  expect_type(result, "list")
  expect_true(nrow(result$all_lakes) >= 1)

  unlink(tmp_gpkg)
})
