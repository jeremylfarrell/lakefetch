# ==============================================================================
# Shiny Interactive App
# ==============================================================================

#' Launch Interactive Fetch App
#'
#' Launch a Shiny app for interactive exploration of fetch calculation results.
#' Click on site markers to view fetch rays and detailed information.
#' Click anywhere on the map to analyze a new point.
#'
#' @param fetch_data Results from \code{\link{fetch_calculate}}
#' @param title Optional app title
#'
#' @return Launches a Shiny app (does not return)
#'
#' @details
#' Requires the shiny, leaflet, and base64enc packages (suggested dependencies).
#'
#' The app displays:
#' \itemize{
#'   \item Interactive map with satellite imagery
#'   \item Site markers colored by exposure category
#'   \item Click markers to see fetch rays
#'   \item Popup with rose diagram and metrics
#'   \item Click anywhere on the map to analyze a new point
#' }
#'
#' @examples
#' \dontrun{
#' sites <- load_sites("my_sites.csv")
#' lake <- get_lake_boundary(sites)
#' results <- fetch_calculate(sites, lake)
#' fetch_app(results)
#' }
#'
#' @export
fetch_app <- function(fetch_data, title = NULL) {


  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for the interactive app.\n",
         "Install with: install.packages('shiny')")
  }
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' is required for the interactive app.\n",
         "Install with: install.packages('leaflet')")
  }

  # Generate rose plots for popups
  message("Generating rose diagrams...")
  rose_plots <- vector("list", nrow(fetch_data$results))
  for (i in seq_len(nrow(fetch_data$results))) {
    rose_plots[[i]] <- make_rose_plot_base64(fetch_data$results[i, ],
                                              fetch_data$results$Site[i])
  }
  fetch_data$results$rose_plot <- unlist(rose_plots)

  # Create ray geometries
  all_rays_sf <- create_ray_geometries(fetch_data)

  # Determine app title
  if (is.null(title)) {
    unique_lakes <- unique(fetch_data$results$lake_name)
    unique_lakes <- unique_lakes[!is.na(unique_lakes)]
    title <- if (length(unique_lakes) == 1) {
      paste("Fetch Analysis:", unique_lakes[1])
    } else {
      paste("Fetch Analysis:", length(unique_lakes), "Lakes")
    }
  }

  # Store lake data for click analysis
  lakes_utm <- fetch_data$lakes
  utm_epsg <- sf::st_crs(lakes_utm)$epsg

  # UI
  ui <- shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::h4("Instructions"),
        shiny::p("Click any site marker to view fetch rays."),
        shiny::p(shiny::strong("Click anywhere on a lake"), " to analyze a new point."),
        shiny::hr(),
        shiny::h5("Selected Site:"),
        shiny::textOutput("selected_site"),
        shiny::uiOutput("click_results"),
        shiny::hr(),
        shiny::h5("Color Legend:"),
        shiny::p(style = "color: firebrick;", "Red: > 5 km (Exposed)"),
        shiny::p(style = "color: gold;", "Gold: 2.5-5 km (Moderate)"),
        shiny::p(style = "color: forestgreen;", "Green: < 2.5 km (Sheltered)"),
        shiny::hr(),
        shiny::actionButton("clear_click", "Clear Custom Point", class = "btn-sm")
      ),
      shiny::mainPanel(
        leaflet::leafletOutput("map", height = "800px")
      )
    )
  )

  # Server
  server <- function(input, output, session) {

    # Color palettes
    exposure_pal <- leaflet::colorFactor(
      palette = c("firebrick", "goldenrod", "forestgreen"),
      levels = c("Exposed", "Moderate", "Sheltered")
    )

    ray_pal <- leaflet::colorBin(
      palette = c("forestgreen", "gold", "firebrick"),
      domain = c(0, 10000),
      bins = c(0, 2500, 5000, 50000)
    )

    # Base map
    output$map <- leaflet::renderLeaflet({

      results_wgs <- sf::st_transform(fetch_data$results, 4326)
      lakes_wgs <- sf::st_transform(fetch_data$lakes, 4326)

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

      leaflet::leaflet(results_wgs) |>
        leaflet::addProviderTiles("Esri.WorldImagery") |>
        leaflet::addPolygons(data = lakes_wgs,
                    fill = FALSE, color = "white",
                    weight = 1, opacity = 0.3) |>
        leaflet::addCircleMarkers(
          radius = 6,
          stroke = TRUE, color = "white", weight = 1.5,
          fillOpacity = 0.9,
          fillColor = ~exposure_pal(exposure_category),
          popup = popup_html,
          layerId = ~Site
        ) |>
        leaflet::addLegend("bottomright",
                  pal = exposure_pal,
                  values = ~exposure_category,
                  title = "Wave Exposure")
    })

    # Click handler for existing markers
    shiny::observeEvent(input$map_marker_click, {
      click <- input$map_marker_click
      site_id <- click$id

      if (is.null(site_id)) return()

      output$selected_site <- shiny::renderText(site_id)

      # Filter rays
      site_rays <- all_rays_sf[all_rays_sf$Site == site_id, ]
      site_rays_wgs <- sf::st_transform(site_rays, 4326)

      # Update map
      leaflet::leafletProxy("map") |>
        leaflet::clearGroup("rays") |>
        leaflet::addPolylines(
          data = site_rays_wgs,
          group = "rays",
          color = ~ray_pal(Distance),
          weight = 2,
          opacity = 0.8,
          popup = ~sprintf("Angle: %d deg<br>Distance: %d m", Angle, round(Distance))
        )
    })

    # Store click analysis results
    click_result <- shiny::reactiveVal(NULL)

    # Click handler for map (new point analysis)
    shiny::observeEvent(input$map_click, {
      click <- input$map_click
      if (is.null(click)) return()

      # Create point in WGS84
      click_pt_wgs <- sf::st_sfc(sf::st_point(c(click$lng, click$lat)), crs = 4326)

      # Transform to UTM
      click_pt_utm <- sf::st_transform(click_pt_wgs, utm_epsg)

      # Check if point is inside any lake
      lakes_contain <- sf::st_intersects(click_pt_utm, lakes_utm)[[1]]

      if (length(lakes_contain) == 0) {
        # Point is not in a lake
        output$selected_site <- shiny::renderText("Click is not inside a lake")
        click_result(NULL)
        return()
      }

      # Get the lake polygon
      lake_idx <- lakes_contain[1]
      lake_poly <- lakes_utm[lake_idx, ]
      lake_name <- if (!is.na(lake_poly$name)) lake_poly$name else "Unknown Lake"

      output$selected_site <- shiny::renderText(
        sprintf("Analyzing point in %s...", lake_name)
      )

      # Run fetch calculation for this single point
      tryCatch({
        # Create a minimal site data frame
        site_sf <- sf::st_sf(
          Site = "Custom Point",
          lake_osm_id = lake_poly$osm_id,
          lake_name = lake_name,
          geometry = click_pt_utm
        )

        # Get lake boundary
        lake_boundary <- tryCatch({
          sf::st_cast(lake_poly, "MULTILINESTRING")
        }, error = function(e) {
          sf::st_boundary(lake_poly)
        })

        # Calculate fetch
        angle_res <- get_opt("angle_resolution_deg")
        angles <- seq(0, 360 - angle_res, by = angle_res)

        # Nudge point if needed
        nudged_pt <- nudge_inward(
          click_pt_utm,
          lake_boundary,
          lake_poly,
          get_opt("buffer_distance_m")
        )
        nudged_sf <- sf::st_sf(
          Site = "Custom Point",
          geometry = sf::st_sfc(nudged_pt, crs = utm_epsg)
        )

        # Get fetch distances
        fetch_dists <- get_highres_fetch(nudged_sf, lake_boundary, lake_poly, angles)

        # Calculate metrics
        fetch_mean <- mean(fetch_dists, na.rm = TRUE)
        fetch_max <- max(fetch_dists, na.rm = TRUE)
        fetch_effective <- mean(sort(fetch_dists, decreasing = TRUE)[1:3], na.rm = TRUE)
        orbital <- calc_orbital(fetch_effective)
        exposure <- if (fetch_effective < 2500) "Sheltered" else if (fetch_effective > 5000) "Exposed" else "Moderate"

        # Create rays for visualization
        pt_coords <- sf::st_coordinates(nudged_sf)
        ray_lines <- lapply(seq_along(angles), function(i) {
          rad <- angles[i] * pi / 180
          end_x <- pt_coords[1] + fetch_dists[i] * sin(rad)
          end_y <- pt_coords[2] + fetch_dists[i] * cos(rad)
          sf::st_linestring(rbind(pt_coords[1:2], c(end_x, end_y)))
        })
        rays_sf <- sf::st_sf(
          Angle = angles,
          Distance = fetch_dists,
          geometry = sf::st_sfc(ray_lines, crs = utm_epsg)
        )
        rays_wgs <- sf::st_transform(rays_sf, 4326)

        # Store results
        click_result(list(
          lake_name = lake_name,
          fetch_mean = fetch_mean,
          fetch_max = fetch_max,
          fetch_effective = fetch_effective,
          orbital = orbital,
          exposure = exposure,
          rays = rays_wgs,
          point = sf::st_transform(nudged_sf, 4326)
        ))

        # Update selected site text
        output$selected_site <- shiny::renderText(
          sprintf("Custom Point in %s", lake_name)
        )

        # Get exposure color
        exp_color <- switch(exposure,
          "Exposed" = "firebrick",
          "Moderate" = "goldenrod",
          "Sheltered" = "forestgreen"
        )

        # Update map with new point and rays
        leaflet::leafletProxy("map") |>
          leaflet::clearGroup("rays") |>
          leaflet::clearGroup("custom_point") |>
          leaflet::addPolylines(
            data = rays_wgs,
            group = "rays",
            color = ~ray_pal(Distance),
            weight = 2,
            opacity = 0.8,
            popup = ~sprintf("Angle: %d deg<br>Distance: %d m", Angle, round(Distance))
          ) |>
          leaflet::addCircleMarkers(
            data = sf::st_transform(nudged_sf, 4326),
            group = "custom_point",
            radius = 8,
            stroke = TRUE,
            color = "white",
            weight = 2,
            fillColor = exp_color,
            fillOpacity = 0.9,
            popup = sprintf(
              "<b>Custom Point</b><br>Lake: %s<br>Effective Fetch: %.1f km<br>Exposure: %s",
              lake_name, fetch_effective / 1000, exposure
            )
          )

      }, error = function(e) {
        output$selected_site <- shiny::renderText(
          sprintf("Error analyzing point: %s", conditionMessage(e))
        )
        click_result(NULL)
      })
    })

    # Display click results in sidebar
    output$click_results <- shiny::renderUI({
      res <- click_result()
      if (is.null(res)) return(NULL)

      shiny::tagList(
        shiny::hr(),
        shiny::h5("Analysis Results:"),
        shiny::p(shiny::strong("Lake: "), res$lake_name),
        shiny::p(shiny::strong("Mean Fetch: "), sprintf("%.1f m", res$fetch_mean)),
        shiny::p(shiny::strong("Max Fetch: "), sprintf("%.1f m", res$fetch_max)),
        shiny::p(shiny::strong("Effective Fetch: "), sprintf("%.1f km", res$fetch_effective / 1000)),
        shiny::p(shiny::strong("Orbital Velocity: "), sprintf("%.3f m/s", res$orbital)),
        shiny::p(shiny::strong("Exposure: "),
          shiny::span(res$exposure, style = sprintf("color: %s; font-weight: bold;",
            switch(res$exposure,
              "Exposed" = "firebrick",
              "Moderate" = "goldenrod",
              "Sheltered" = "forestgreen"
            )
          ))
        )
      )
    })

    # Clear custom point
    shiny::observeEvent(input$clear_click, {
      click_result(NULL)
      output$selected_site <- shiny::renderText("")
      leaflet::leafletProxy("map") |>
        leaflet::clearGroup("rays") |>
        leaflet::clearGroup("custom_point")
    })
  }

  shiny::shinyApp(ui, server)
}
