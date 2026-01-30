# ==============================================================================
# Tests for data_loading.R - Site data loading functions
# ==============================================================================

test_that("load_sites handles data.frame input", {
  df <- data.frame(
    Site = c("A", "B", "C"),
    latitude = c(43.0, 43.1, 43.2),
    longitude = c(-74.0, -74.1, -74.2)
  )

  result <- load_sites(df)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true("Site" %in% names(result))
  expect_true("latitude" %in% names(result))
  expect_true("longitude" %in% names(result))
})

test_that("load_sites auto-detects lat/lon column names", {
  # Test various column name formats
  df1 <- data.frame(
    Site = "A",
    lat = 43.0,
    lon = -74.0
  )

  df2 <- data.frame(
    Site = "B",
    Latitude = 43.0,
    Longitude = -74.0
  )

  df3 <- data.frame(
    Site = "C",
    LAT = 43.0,
    LONG = -74.0
  )

  result1 <- load_sites(df1)
  result2 <- load_sites(df2)
  result3 <- load_sites(df3)

  expect_s3_class(result1, "data.frame")
  expect_s3_class(result2, "data.frame")
  expect_s3_class(result3, "data.frame")

  # All should have standardized lat/lon columns
  expect_true("latitude" %in% names(result1))
  expect_true("longitude" %in% names(result1))
})

test_that("load_sites creates Site names if missing", {
  df <- data.frame(
    latitude = c(43.0, 43.1),
    longitude = c(-74.0, -74.1)
  )

  result <- load_sites(df)

  expect_true("Site" %in% names(result))
  expect_true(all(grepl("^Site_", result$Site) | grepl("_\\d+$", result$Site)))
})

test_that("load_sites removes invalid coordinates", {
  df <- data.frame(
    Site = c("A", "B", "C", "D"),
    latitude = c(43.0, NA, 200, 43.1),  # NA and invalid lat
    longitude = c(-74.0, -74.1, -74.2, NA)  # NA lon
  )

  expect_warning(result <- load_sites(df))

  # Should only have 1 valid row (A)
  expect_equal(nrow(result), 1)
})

test_that("load_sites preserves datetime column", {
  df <- data.frame(
    Site = c("A", "B"),
    latitude = c(43.0, 43.1),
    longitude = c(-74.0, -74.1),
    datetime = as.POSIXct(c("2024-07-15 10:00:00", "2024-07-15 14:00:00"))
  )

  result <- load_sites(df)

  expect_true("datetime" %in% names(result))
  expect_s3_class(result$datetime, "POSIXct")
})

test_that("load_sites preserves lake.name column", {
  df <- data.frame(
    Site = c("A", "B"),
    latitude = c(43.0, 43.1),
    longitude = c(-74.0, -74.1),
    lake.name = c("Lake One", "Lake Two")
  )

  result <- load_sites(df)

  expect_true("lake.name" %in% names(result))
  expect_equal(result$lake.name, c("Lake One", "Lake Two"))
})

test_that("load_sites handles CSV file path", {
  skip_if_not(file.exists(tempdir()))

  # Create temporary CSV
  tmp_file <- tempfile(fileext = ".csv")
  df <- data.frame(
    Site = c("A", "B"),
    latitude = c(43.0, 43.1),
    longitude = c(-74.0, -74.1)
  )
  write.csv(df, tmp_file, row.names = FALSE)

  result <- load_sites(tmp_file)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)

  unlink(tmp_file)
})

test_that("load_sites errors on missing coordinate columns", {
  df <- data.frame(
    Site = c("A", "B"),
    x = c(43.0, 43.1),
    y = c(-74.0, -74.1)
  )

  expect_error(load_sites(df))
})

test_that("sanitize_filename removes special characters", {
  result1 <- lakefetch:::sanitize_filename("Lake O'Brien (North)")
  result2 <- lakefetch:::sanitize_filename("Test/Lake\\Name")

  # Should not contain special characters
  expect_false(grepl("[^a-zA-Z0-9_]", result1))
  expect_false(grepl("[^a-zA-Z0-9_]", result2))

  # Should preserve alphanumeric content
  expect_true(grepl("Lake", result1))
  expect_true(grepl("OBrien", result1))
})
