# ==============================================================================
# Tests for nhd_integration.R with mocked NHD API calls
# ==============================================================================

# Helper to create mock fetch results for NHD tests
create_mock_nhd_fetch_results <- function(n_sites = 1) {
  sites <- sf::st_sf(
    Site = paste0("Site_", seq_len(n_sites)),
    site_name = paste0("Site ", seq_len(n_sites)),
    lake_osm_id = rep("test_circle", n_sites),
    lake_name = rep("Test Lake", n_sites),
    fetch_effective = rep(1500, n_sites),
    exposure_class = rep("Moderate", n_sites),
    geometry = sf::st_sfc(
      lapply(seq_len(n_sites), function(i) {
        sf::st_point(c(-74.0 + i * 0.001, 43.0))
      }),
      crs = 4326
    )
  )
  sites
}

# Helper to create mock lake polygon
create_mock_nhd_lake <- function() {
  coords <- matrix(c(
    -74.01, 43.0,
    -73.99, 43.0,
    -73.99, 43.02,
    -74.01, 43.02,
    -74.01, 43.0
  ), ncol = 2, byrow = TRUE)

  sf::st_sf(
    osm_id = "test_circle",
    name = "Test Lake",
    area_km2 = 2.0,
    geometry = sf::st_sfc(sf::st_polygon(list(coords)), crs = 4326)
  )
}

# Helper to create mock NHD waterbody
create_mock_nhd_waterbody <- function() {
  coords <- matrix(c(
    -74.01, 43.0,
    -73.99, 43.0,
    -73.99, 43.02,
    -74.01, 43.02,
    -74.01, 43.0
  ), ncol = 2, byrow = TRUE)

  sf::st_sf(
    permanent_identifier = "12345678",
    gnis_name = "Test Lake NHD",
    areasqkm = 2.1,
    ftype = 390L,
    geometry = sf::st_sfc(sf::st_polygon(list(coords)), crs = 4326)
  )
}

# Helper for mock outlet point
create_mock_outlet <- function() {
  sf::st_sf(
    type = "outlet",
    geometry = sf::st_sfc(sf::st_point(c(-73.99, 43.01)), crs = 4326)
  )
}

# Helper for mock inlets
create_mock_inlets <- function(n = 2) {
  pts <- lapply(seq_len(n), function(i) {
    sf::st_point(c(-74.01, 43.0 + i * 0.005))
  })
  sf::st_sf(
    type = rep("inlet", n),
    geometry = sf::st_sfc(pts, crs = 4326)
  )
}

# Helper for mock flowline with stream order
create_mock_flowline <- function() {
  sf::st_sf(
    streamorde = 3L,
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(-73.99, 43.01, -73.98, 43.01), ncol = 2, byrow = TRUE)),
      crs = 4326
    )
  )
}

# --- add_lake_context with full mocks ---

test_that("add_lake_context adds NHD columns with mocked data", {
  fetch_results <- create_mock_nhd_fetch_results(n_sites = 2)
  lake_polys <- create_mock_nhd_lake()
  mock_nhd_wb <- create_mock_nhd_waterbody()
  mock_outlet <- create_mock_outlet()
  mock_inlets <- create_mock_inlets(2)
  mock_flowline <- create_mock_flowline()

  local_mocked_bindings(
    nhd_available = function() TRUE,
    get_nhd_waterbodies = function(bbox_sfc) mock_nhd_wb,
    get_outlets_inlets = function(lake_nhd, lake_polygon_wgs84) {
      list(
        outlet = mock_outlet,
        inlets = mock_inlets,
        outlet_flowline = mock_flowline
      )
    },
    get_watershed_area = function(lake_nhd, lake_polygon_wgs84) 500.0,
    .package = "lakefetch"
  )

  result <- lakefetch::add_lake_context(fetch_results, lake_polys, 32618)

  # Should have NHD context columns
  expect_true("nhd_permanent_id" %in% names(result))
  expect_true("nhd_gnis_name" %in% names(result))
  expect_true("nhd_areasqkm" %in% names(result))
  expect_true("connectivity_class" %in% names(result))
  expect_true("outlet_stream_order" %in% names(result))
  expect_true("watershed_area_ha" %in% names(result))
  expect_true("lake_watershed_ratio" %in% names(result))
  expect_true("outlet_dist_m" %in% names(result))
  expect_true("outlet_bearing" %in% names(result))
  expect_true("inlet_nearest_dist_m" %in% names(result))
  expect_true("inlet_nearest_bearing" %in% names(result))
  expect_true("inlet_count" %in% names(result))

  # Values should be populated (not all NA)
  expect_false(is.na(result$nhd_permanent_id[1]))
  expect_equal(result$nhd_permanent_id[1], "12345678")
  expect_equal(result$nhd_gnis_name[1], "Test Lake NHD")
  expect_true(!is.na(result$nhd_areasqkm[1]))

  # Connectivity should be "Drainage" (has outlet and inlets)
  expect_equal(result$connectivity_class[1], "Drainage")

  # Stream order from mock flowline
  expect_equal(result$outlet_stream_order[1], 3L)

  # Watershed area and ratio
  expect_equal(result$watershed_area_ha[1], 500.0)
  expect_true(!is.na(result$lake_watershed_ratio[1]))

  # Outlet distance should be numeric > 0
  expect_true(!is.na(result$outlet_dist_m[1]))
  expect_true(result$outlet_dist_m[1] > 0)

  # Inlet info
  expect_equal(result$inlet_count[1], 2L)
  expect_true(!is.na(result$inlet_nearest_dist_m[1]))
  expect_true(!is.na(result$inlet_nearest_bearing[1]))

  # Both sites should have same lake-level attributes
  expect_equal(result$nhd_permanent_id[1], result$nhd_permanent_id[2])
  expect_equal(result$connectivity_class[1], result$connectivity_class[2])
})

test_that("add_lake_context handles headwater lake (outlet, no inlets)", {
  fetch_results <- create_mock_nhd_fetch_results(n_sites = 1)
  lake_polys <- create_mock_nhd_lake()
  mock_nhd_wb <- create_mock_nhd_waterbody()
  mock_outlet <- create_mock_outlet()
  mock_flowline <- create_mock_flowline()

  local_mocked_bindings(
    nhd_available = function() TRUE,
    get_nhd_waterbodies = function(bbox_sfc) mock_nhd_wb,
    get_outlets_inlets = function(lake_nhd, lake_polygon_wgs84) {
      list(outlet = mock_outlet, inlets = NULL, outlet_flowline = mock_flowline)
    },
    get_watershed_area = function(lake_nhd, lake_polygon_wgs84) 200.0,
    .package = "lakefetch"
  )

  result <- lakefetch::add_lake_context(fetch_results, lake_polys, 32618)

  expect_equal(result$connectivity_class[1], "Headwater")
  expect_equal(result$inlet_count[1], 0L)
  expect_true(is.na(result$inlet_nearest_dist_m[1]))
  expect_true(!is.na(result$outlet_dist_m[1]))
})

test_that("add_lake_context handles terminal lake (inlets, no outlet)", {
  fetch_results <- create_mock_nhd_fetch_results(n_sites = 1)
  lake_polys <- create_mock_nhd_lake()
  mock_nhd_wb <- create_mock_nhd_waterbody()
  mock_inlets <- create_mock_inlets(1)

  local_mocked_bindings(
    nhd_available = function() TRUE,
    get_nhd_waterbodies = function(bbox_sfc) mock_nhd_wb,
    get_outlets_inlets = function(lake_nhd, lake_polygon_wgs84) {
      list(outlet = NULL, inlets = mock_inlets, outlet_flowline = NULL)
    },
    get_watershed_area = function(lake_nhd, lake_polygon_wgs84) 300.0,
    .package = "lakefetch"
  )

  result <- lakefetch::add_lake_context(fetch_results, lake_polys, 32618)

  expect_equal(result$connectivity_class[1], "Terminal")
  expect_true(is.na(result$outlet_dist_m[1]))
  expect_equal(result$inlet_count[1], 1L)
  expect_true(!is.na(result$inlet_nearest_dist_m[1]))
  expect_true(is.na(result$outlet_stream_order[1]))
})

test_that("add_lake_context handles isolated lake (no outlet, no inlets)", {
  fetch_results <- create_mock_nhd_fetch_results(n_sites = 1)
  lake_polys <- create_mock_nhd_lake()
  mock_nhd_wb <- create_mock_nhd_waterbody()

  local_mocked_bindings(
    nhd_available = function() TRUE,
    get_nhd_waterbodies = function(bbox_sfc) mock_nhd_wb,
    get_outlets_inlets = function(lake_nhd, lake_polygon_wgs84) {
      list(outlet = NULL, inlets = NULL, outlet_flowline = NULL)
    },
    get_watershed_area = function(lake_nhd, lake_polygon_wgs84) NA_real_,
    .package = "lakefetch"
  )

  result <- lakefetch::add_lake_context(fetch_results, lake_polys, 32618)

  expect_equal(result$connectivity_class[1], "Isolated")
  expect_true(is.na(result$outlet_dist_m[1]))
  expect_equal(result$inlet_count[1], 0L)
  expect_true(is.na(result$watershed_area_ha[1]))
})

test_that("add_lake_context returns NAs when NHD not available", {
  fetch_results <- create_mock_nhd_fetch_results(n_sites = 1)
  lake_polys <- create_mock_nhd_lake()

  local_mocked_bindings(
    nhd_available = function() FALSE,
    .package = "lakefetch"
  )

  result <- lakefetch::add_lake_context(fetch_results, lake_polys, 32618)

  # All NHD columns should be NA
  expect_true(is.na(result$nhd_permanent_id[1]))
  expect_true(is.na(result$connectivity_class[1]))
  expect_true(is.na(result$outlet_dist_m[1]))
  expect_true(is.na(result$watershed_area_ha[1]))
})

test_that("add_lake_context handles no NHD match gracefully", {
  fetch_results <- create_mock_nhd_fetch_results(n_sites = 1)
  lake_polys <- create_mock_nhd_lake()

  local_mocked_bindings(
    nhd_available = function() TRUE,
    get_nhd_waterbodies = function(bbox_sfc) NULL,
    .package = "lakefetch"
  )

  result <- lakefetch::add_lake_context(fetch_results, lake_polys, 32618)

  # Should complete without error; NHD columns present
  expect_true("nhd_permanent_id" %in% names(result))
  expect_true(is.na(result$nhd_permanent_id[1]))
  # Connectivity class still gets set to "Isolated" (no outlet, no inlet)
  expect_equal(result$connectivity_class[1], "Isolated")
})

test_that("add_lake_context handles multiple lakes", {
  fetch_results <- create_mock_nhd_fetch_results(n_sites = 2)
  fetch_results$lake_osm_id <- c("lake_1", "lake_2")

  # Two lake polygons
  coords1 <- matrix(c(-74.01,43, -73.99,43, -73.99,43.02, -74.01,43.02, -74.01,43),
                     ncol = 2, byrow = TRUE)
  coords2 <- matrix(c(-74.11,43, -74.09,43, -74.09,43.02, -74.11,43.02, -74.11,43),
                     ncol = 2, byrow = TRUE)
  lake_polys <- sf::st_sf(
    osm_id = c("lake_1", "lake_2"),
    name = c("Lake One", "Lake Two"),
    area_km2 = c(2.0, 1.5),
    geometry = sf::st_sfc(
      sf::st_polygon(list(coords1)),
      sf::st_polygon(list(coords2)),
      crs = 4326
    )
  )

  mock_nhd_wb <- create_mock_nhd_waterbody()
  mock_outlet <- create_mock_outlet()

  local_mocked_bindings(
    nhd_available = function() TRUE,
    get_nhd_waterbodies = function(bbox_sfc) mock_nhd_wb,
    get_outlets_inlets = function(lake_nhd, lake_polygon_wgs84) {
      list(outlet = mock_outlet, inlets = NULL, outlet_flowline = NULL)
    },
    get_watershed_area = function(lake_nhd, lake_polygon_wgs84) 400.0,
    .package = "lakefetch"
  )

  result <- lakefetch::add_lake_context(fetch_results, lake_polys, 32618)

  expect_equal(nrow(result), 2)
  # Both should have been processed
  expect_true("connectivity_class" %in% names(result))
})
