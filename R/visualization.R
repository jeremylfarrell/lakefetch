# ==============================================================================
# Visualization Functions
# ==============================================================================

#' Plot Fetch Map
#'
#' Create a map showing site locations colored by exposure category.
#'
#' @param fetch_data Results from \code{\link{fetch_calculate}}
#' @param title Optional plot title
#'
#' @return A ggplot2 object
#'
#' @examples
#' if (FALSE) {
#' results <- fetch_calculate(sites, lake)
#' plot_fetch_map(results)
#' }
#'
#' @export
plot_fetch_map <- function(fetch_data, title = "Fetch Analysis - Site Locations") {

  # Transform data to WGS84 for plotting
  results_wgs <- sf::st_transform(fetch_data$results, 4326)
  lakes_wgs <- sf::st_transform(fetch_data$lakes, 4326)

  # Color palette for exposure categories
  exposure_colors <- c("Exposed" = "#D73027", "Moderate" = "#FEE08B", "Sheltered" = "#1A9850")

  # Get bounding box
  bbox <- sf::st_bbox(results_wgs)
  xlim <- c(bbox["xmin"] - 0.02, bbox["xmax"] + 0.02)
  ylim <- c(bbox["ymin"] - 0.02, bbox["ymax"] + 0.02)

  # Create subtitle
  n_sites <- nrow(results_wgs)
  n_lakes <- length(unique(results_wgs$lake_name))
  subtitle <- paste(n_sites, "sites across", n_lakes, "lakes")

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = lakes_wgs, fill = "lightblue", color = "steelblue", alpha = 0.5) +
    ggplot2::geom_sf(data = results_wgs, ggplot2::aes(color = .data$exposure_category), size = 3) +
    ggplot2::scale_color_manual(values = exposure_colors, name = "Exposure") +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Longitude", y = "Latitude"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      legend.position = "right"
    )

  return(p)
}

#' Plot Fetch Bar Chart
#'
#' Create a bar chart showing effective fetch by site.
#'
#' @param fetch_data Results from \code{\link{fetch_calculate}}
#' @param title Optional plot title
#'
#' @return A ggplot2 object
#'
#' @examples
#' if (FALSE) {
#' results <- fetch_calculate(sites, lake)
#' plot_fetch_bars(results)
#' }
#'
#' @export
plot_fetch_bars <- function(fetch_data, title = "Effective Fetch by Site") {

  exposure_colors <- c("Exposed" = "#D73027", "Moderate" = "#FEE08B", "Sheltered" = "#1A9850")

  results_df <- sf::st_drop_geometry(fetch_data$results)
  results_df <- results_df[order(results_df$fetch_effective, decreasing = TRUE), ]
  results_df$Site <- factor(results_df$Site, levels = results_df$Site)

  p <- ggplot2::ggplot(results_df,
                        ggplot2::aes(x = .data$Site,
                                     y = .data$fetch_effective / 1000,
                                     fill = .data$exposure_category)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_hline(yintercept = 2.5, linetype = "dashed", color = "gray40") +
    ggplot2::geom_hline(yintercept = 5, linetype = "dashed", color = "gray40") +
    ggplot2::scale_fill_manual(values = exposure_colors, name = "Exposure") +
    ggplot2::labs(
      title = title,
      subtitle = "Dashed lines show exposure thresholds (2.5 km, 5 km)",
      x = "Site",
      y = "Effective Fetch (km)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      legend.position = "right"
    )

  return(p)
}

#' Plot Fetch Rose Diagram
#'
#' Create a rose diagram showing directional fetch for a single site.
#'
#' @param fetch_data Results from \code{\link{fetch_calculate}}
#' @param site Site name to plot
#' @param title Optional plot title (defaults to site name)
#'
#' @return Invisible NULL (creates base R plot)
#'
#' @examples
#' if (FALSE) {
#' results <- fetch_calculate(sites, lake)
#' plot_fetch_rose(results, "Site1")
#' }
#'
#' @export
plot_fetch_rose <- function(fetch_data, site, title = NULL) {

  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))

  results <- fetch_data$results

  # Find the site
  site_idx <- which(results$Site == site)
  if (length(site_idx) == 0) {
    stop("Site '", site, "' not found in results")
  }

  site_row <- results[site_idx[1], ]

  if (is.null(title)) {
    title <- site
  }

  # Extract fetch values
  fetch_cols <- grep("^fetch_[0-9]+$", names(site_row), value = TRUE)

  if (length(fetch_cols) == 0) {
    stop("No fetch columns found in results")
  }

  # Get angles from column names
  angles <- as.numeric(gsub("fetch_", "", fetch_cols))
  fetch_vals <- as.numeric(sf::st_drop_geometry(site_row)[, fetch_cols])

  # Set up plot
  graphics::par(mar = c(1, 1, 2, 1))

  # Convert to radians (0 = North, clockwise)
  angles_rad <- (90 - angles) * pi / 180

  # Normalize fetch values for plotting
  max_fetch <- max(fetch_vals, na.rm = TRUE)
  fetch_norm <- fetch_vals / max_fetch

  # Plot
  graphics::plot(NULL, xlim = c(-1.3, 1.3), ylim = c(-1.3, 1.3),
       asp = 1, axes = FALSE, xlab = "", ylab = "")
  graphics::title(main = title, cex.main = 0.9)

  # Draw concentric circles
  for (r in c(0.25, 0.5, 0.75, 1)) {
    theta <- seq(0, 2*pi, length.out = 100)
    graphics::lines(r * cos(theta), r * sin(theta), col = "gray80", lty = 2)
  }

  # Draw direction lines
  for (a in seq(0, 315, by = 45)) {
    a_rad <- (90 - a) * pi / 180
    graphics::lines(c(0, cos(a_rad)), c(0, sin(a_rad)), col = "gray80")
  }

  # Draw fetch polygon
  x_pts <- fetch_norm * cos(angles_rad)
  y_pts <- fetch_norm * sin(angles_rad)
  graphics::polygon(c(x_pts, x_pts[1]), c(y_pts, y_pts[1]),
          col = grDevices::rgb(0.2, 0.5, 0.8, 0.4), border = "steelblue", lwd = 2)

  # Add cardinal directions
  graphics::text(0, 1.15, "N", cex = 0.8, font = 2)
  graphics::text(1.15, 0, "E", cex = 0.8, font = 2)
  graphics::text(0, -1.15, "S", cex = 0.8, font = 2)
  graphics::text(-1.15, 0, "W", cex = 0.8, font = 2)

  # Add scale label
  graphics::text(0, -1.4, paste("Max:", round(max_fetch/1000, 1), "km"),
       cex = 0.7, col = "gray40")

  invisible(NULL)
}

#' Create Ray Geometries for Map Visualization
#'
#' Create line geometries representing fetch rays from each site.
#' Useful for detailed visualization of the ray-casting results.
#'
#' @param fetch_data Results from \code{\link{fetch_calculate}}
#'
#' @return An sf object with ray line geometries
#'
#' @examples
#' if (FALSE) {
#' results <- fetch_calculate(sites, lake)
#' rays <- create_ray_geometries(results)
#'
#' # Plot rays for a specific site
#' library(ggplot2)
#' site_rays <- rays[rays$Site == "Site1", ]
#' ggplot() + geom_sf(data = site_rays, aes(color = Distance))
#' }
#'
#' @export
create_ray_geometries <- function(fetch_data) {

  results <- fetch_data$results
  angles <- fetch_data$angles
  utm_crs <- sf::st_crs(results)

  all_rays <- list()

  for (i in seq_len(nrow(results))) {
    site_row <- results[i, ]
    site_name <- site_row$Site
    coords <- sf::st_coordinates(site_row)
    x0 <- coords[1]
    y0 <- coords[2]

    for (angle in angles) {
      col_name <- paste0("fetch_", angle)
      if (!col_name %in% names(site_row)) next

      dist <- as.numeric(sf::st_drop_geometry(site_row)[, col_name])
      if (is.na(dist) || dist <= 0) next

      # Calculate endpoint
      rad <- angle * (pi / 180)
      x1 <- x0 + dist * sin(rad)
      y1 <- y0 + dist * cos(rad)

      ray_line <- sf::st_linestring(rbind(c(x0, y0), c(x1, y1)))

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

  rays_sf <- sf::st_sf(rays_df,
                        geometry = sf::st_sfc(lapply(all_rays, function(x) x$geometry),
                                              crs = utm_crs))

  return(rays_sf)
}

#' Create Base64-Encoded Rose Plot for Popup
#'
#' Create a rose plot as a base64-encoded PNG for embedding in HTML popups.
#' Used internally by the Shiny app.
#'
#' @param site_row Single row from fetch results
#' @param site_name Site name for title
#'
#' @return Character string with base64-encoded PNG data URI
#' @noRd
make_rose_plot_base64 <- function(site_row, site_name) {

  if (!requireNamespace("base64enc", quietly = TRUE)) {
    return("")
  }

  # Extract fetch values
  fetch_cols <- grep("^fetch_[0-9]+$", names(site_row), value = TRUE)

  if (length(fetch_cols) == 0) {
    return("")
  }

  # Get angles from column names
  angles <- as.numeric(gsub("fetch_", "", fetch_cols))
  fetch_vals <- as.numeric(sf::st_drop_geometry(site_row)[, fetch_cols])

  # Create temporary file
  tmp_file <- tempfile(fileext = ".png")

  grDevices::png(tmp_file, width = 300, height = 300, bg = "transparent")
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar), add = TRUE)
  graphics::par(mar = c(1, 1, 2, 1))

  # Convert to radians (0 = North, clockwise)
  angles_rad <- (90 - angles) * pi / 180

  # Normalize fetch values for plotting
  max_fetch <- max(fetch_vals, na.rm = TRUE)
  fetch_norm <- fetch_vals / max_fetch

  # Plot
  graphics::plot(NULL, xlim = c(-1.3, 1.3), ylim = c(-1.3, 1.3),
       asp = 1, axes = FALSE, xlab = "", ylab = "")
  graphics::title(main = site_name, cex.main = 0.9)

  # Draw concentric circles
  for (r in c(0.25, 0.5, 0.75, 1)) {
    theta <- seq(0, 2*pi, length.out = 100)
    graphics::lines(r * cos(theta), r * sin(theta), col = "gray80", lty = 2)
  }

  # Draw direction lines
  for (a in seq(0, 315, by = 45)) {
    a_rad <- (90 - a) * pi / 180
    graphics::lines(c(0, cos(a_rad)), c(0, sin(a_rad)), col = "gray80")
  }

  # Draw fetch polygon
  x_pts <- fetch_norm * cos(angles_rad)
  y_pts <- fetch_norm * sin(angles_rad)
  graphics::polygon(c(x_pts, x_pts[1]), c(y_pts, y_pts[1]),
          col = grDevices::rgb(0.2, 0.5, 0.8, 0.4), border = "steelblue", lwd = 2)

  # Add cardinal directions
  graphics::text(0, 1.15, "N", cex = 0.8, font = 2)
  graphics::text(1.15, 0, "E", cex = 0.8, font = 2)
  graphics::text(0, -1.15, "S", cex = 0.8, font = 2)
  graphics::text(-1.15, 0, "W", cex = 0.8, font = 2)

  grDevices::dev.off()

  # Convert to base64
  img_raw <- readBin(tmp_file, "raw", file.info(tmp_file)$size)
  img_base64 <- base64enc::base64encode(img_raw)
  unlink(tmp_file)

  return(paste0("data:image/png;base64,", img_base64))
}
