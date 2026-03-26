# ==============================================================================
# Tests for weather_integration.R with mocked API calls
# ==============================================================================

# Helper to create a realistic mock weather data frame
create_mock_weather <- function(sample_time, days = 7) {
  hours <- seq(sample_time - days * 24 * 3600, sample_time, by = "hour")
  n <- length(hours)
  data.frame(
    datetime = hours,
    temp_c = 20 + 5 * sin(seq(0, 2 * pi * days, length.out = n)),
    humidity_pct = rep(65, n),
    precip_mm = ifelse(runif(n) > 0.8, runif(n, 0, 2), 0),
    pressure_hpa = rep(1013, n),
    cloud_cover_pct = rep(50, n),
    wind_speed_ms = abs(5 + 3 * sin(seq(0, 4 * pi * days, length.out = n))),
    wind_dir_deg = rep(c(180, 200, 190, 210), length.out = n),
    wind_gust_ms = abs(8 + 4 * sin(seq(0, 4 * pi * days, length.out = n))),
    solar_rad_wm2 = pmax(0, 400 * sin(seq(0, 2 * pi * days, length.out = n))),
    stringsAsFactors = FALSE
  )
}

# Helper to create minimal fetch results with required columns
create_mock_fetch_results <- function(n_sites = 2) {
  angles <- seq(0, 355, by = 5)
  fetch_cols <- paste0("fetch_", angles)

  sites <- sf::st_sf(
    Site = paste0("Site_", seq_len(n_sites)),
    site_name = paste0("Site ", seq_len(n_sites)),
    lake_osm_id = rep("test", n_sites),
    lake_name = rep("Test Lake", n_sites),
    datetime = rep(as.POSIXct("2024-07-15 12:00:00", tz = "UTC"), n_sites),
    fetch_effective = rep(1500, n_sites),
    geometry = sf::st_sfc(
      lapply(seq_len(n_sites), function(i) {
        sf::st_point(c(500000 + i * 100, 4800000))
      }),
      crs = 32618
    )
  )

  # Add directional fetch columns
  for (col in fetch_cols) {
    sites[[col]] <- rep(1000, n_sites)
  }

  sites
}

test_that("add_weather_context works with mocked fetch_weather_history", {
  results <- create_mock_fetch_results(n_sites = 2)
  sample_time <- as.POSIXct("2024-07-15 12:00:00", tz = "UTC")
  mock_weather <- create_mock_weather(sample_time)

  local_mocked_bindings(
    fetch_weather_history = function(lat, lon, datetime, days_before = 7) {
      mock_weather
    },
    .package = "lakefetch"
  )

  result <- lakefetch::add_weather_context(results, datetime_col = "datetime",
                                 depth_m = 5)

  # Should have weather columns added
  expect_true("wind_mean_24h" %in% names(result))
  expect_true("wind_mean_3d" %in% names(result))
  expect_true("temp_mean_24h" %in% names(result))
  expect_true("precip_total_24h" %in% names(result))
  expect_true("wave_energy_24h" %in% names(result))
  expect_true("wave_energy_3d" %in% names(result))
  expect_true("orbital_velocity_mean_3d" %in% names(result))

  # Values should be numeric and not all NA
  expect_false(all(is.na(result$wind_mean_24h)))
  expect_false(all(is.na(result$temp_mean_24h)))
})

test_that("add_weather_context handles failed weather fetch gracefully", {
  results <- create_mock_fetch_results(n_sites = 1)

  local_mocked_bindings(
    fetch_weather_history = function(lat, lon, datetime, days_before = 7) {
      NULL  # Simulate API failure
    },
    .package = "lakefetch"
  )

  result <- lakefetch::add_weather_context(results, datetime_col = "datetime",
                                 depth_m = 5)

  # Should complete without error, columns should be present but NA
  expect_equal(nrow(result), 1)
})

test_that("add_weather_context uses per-site depth when available", {
  results <- create_mock_fetch_results(n_sites = 1)
  results$depth_mean_m <- 3.5
  sample_time <- as.POSIXct("2024-07-15 12:00:00", tz = "UTC")
  mock_weather <- create_mock_weather(sample_time)

  local_mocked_bindings(
    fetch_weather_history = function(lat, lon, datetime, days_before = 7) {
      mock_weather
    },
    .package = "lakefetch"
  )

  expect_message(
    result <- lakefetch::add_weather_context(results, datetime_col = "datetime"),
    "depth_mean_m"
  )

  expect_true("wave_energy_3d" %in% names(result))
})

test_that("add_weather_context parses non-POSIXct datetime", {
  results <- create_mock_fetch_results(n_sites = 1)
  results$datetime <- "2024-07-15 12:00:00"  # Character, not POSIXct
  sample_time <- as.POSIXct("2024-07-15 12:00:00", tz = "UTC")
  mock_weather <- create_mock_weather(sample_time)

  local_mocked_bindings(
    fetch_weather_history = function(lat, lon, datetime, days_before = 7) {
      mock_weather
    },
    .package = "lakefetch"
  )

  result <- lakefetch::add_weather_context(results, datetime_col = "datetime",
                                 depth_m = 5)

  expect_true("wind_mean_24h" %in% names(result))
})

test_that("add_weather_context custom time windows", {
  results <- create_mock_fetch_results(n_sites = 1)
  sample_time <- as.POSIXct("2024-07-15 12:00:00", tz = "UTC")
  mock_weather <- create_mock_weather(sample_time, days = 14)

  local_mocked_bindings(
    fetch_weather_history = function(lat, lon, datetime, days_before = 7) {
      mock_weather
    },
    .package = "lakefetch"
  )

  result <- lakefetch::add_weather_context(results, datetime_col = "datetime",
                                 windows_hours = c(24, 48),
                                 depth_m = 5)

  expect_true("wind_mean_24h" %in% names(result))
  expect_true("wind_mean_2d" %in% names(result))
})
