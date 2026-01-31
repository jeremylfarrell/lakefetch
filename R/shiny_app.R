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
        # Use default depth for click analysis in fetch_app
        orbital <- calc_orbital(fetch_effective, depth_m = get_opt("default_depth_m"))
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


#' Launch Interactive Fetch App with File Upload
#'
#' Launch a standalone Shiny app where users can upload a CSV file with GPS
#' coordinates, and the app will automatically download lake boundaries,
#' calculate fetch, and display interactive results.
#'
#' @param title Optional app title (default: "Lake Fetch Calculator")
#'
#' @return Launches a Shiny app (does not return)
#'
#' @details
#' Requires the shiny, leaflet, and base64enc packages (suggested dependencies).
#'
#' The app workflow:
#' \enumerate{
#'   \item Upload a CSV file with latitude/longitude columns
#'   \item App downloads lake boundaries from OpenStreetMap
#'   \item Calculates fetch for all uploaded points
#'   \item Displays interactive map with results
#'   \item Click anywhere on a lake to analyze additional points
#'   \item Download results as CSV or GeoPackage
#' }
#'
#' CSV file requirements:
#' \itemize{
#'   \item Must have columns starting with "lat" and "lon" (case-insensitive)
#'   \item Optional "Site" column for point names
#'   \item Additional columns are preserved in output
#' }
#'
#' @examples
#' \dontrun{
#' # Launch the upload app
#' fetch_app_upload()
#' }
#'
#' @export
fetch_app_upload <- function(title = "Lake Fetch Calculator") {

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for the interactive app.\n",
         "Install with: install.packages('shiny')")
  }
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' is required for the interactive app.\n",
         "Install with: install.packages('leaflet')")
  }

  # UI
  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        .progress-message { color: #666; font-style: italic; margin: 10px 0; }
        .error-message { color: firebrick; font-weight: bold; }
        .success-message { color: forestgreen; font-weight: bold; }
      "))
    ),
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::conditionalPanel(
          condition = "!output.has_results",
          shiny::h4("Upload GPS Points"),
          shiny::fileInput("file_upload", "Choose CSV File",
                           accept = c("text/csv", ".csv")),
          shiny::helpText("CSV must have columns starting with 'lat' and 'lon'"),
          shiny::helpText("Optional: include a 'datetime' column for weather analysis"),
          shiny::hr(),
          shiny::h5("Options"),
          shiny::numericInput("water_depth", "Water depth (m)", value = 5, min = 0.5, max = 100),
          shiny::helpText("Used for orbital velocity calculation"),
          shiny::checkboxInput("add_nhd", "Add NHD context (outlets/inlets)", value = FALSE),
          shiny::checkboxInput("add_weather", "Add historical weather data", value = FALSE),
          shiny::conditionalPanel(
            condition = "input.add_weather",
            shiny::helpText("Requires 'datetime' column in CSV")
          ),
          shiny::hr(),
          shiny::actionButton("run_analysis", "Run Analysis",
                              class = "btn-primary btn-block",
                              disabled = TRUE),
          shiny::hr(),
          shiny::uiOutput("status_message")
        ),
        shiny::conditionalPanel(
          condition = "output.has_results",
          shiny::h4("Results"),
          shiny::p(shiny::textOutput("results_summary")),
          shiny::hr(),
          shiny::h5("Instructions"),
          shiny::p("Click any marker to view fetch rays."),
          shiny::p(shiny::strong("Click on a lake"), " to analyze a new point."),
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
          shiny::h5("Download Results"),
          shiny::downloadButton("download_csv", "Download CSV", class = "btn-sm"),
          shiny::downloadButton("download_gpkg", "Download GeoPackage", class = "btn-sm"),
          shiny::hr(),
          shiny::actionButton("clear_click", "Clear Custom Point", class = "btn-sm"),
          shiny::actionButton("reset_app", "Start Over", class = "btn-sm btn-warning")
        )
      ),
      shiny::mainPanel(
        width = 9,
        shiny::conditionalPanel(
          condition = "!output.has_results",
          shiny::div(
            style = "text-align: center; padding: 100px; color: #999;",
            shiny::h3("Upload a CSV file to begin"),
            shiny::p("Your file should contain latitude and longitude columns."),
            shiny::p("The app will automatically find lakes and calculate fetch.")
          )
        ),
        shiny::conditionalPanel(
          condition = "output.has_results",
          leaflet::leafletOutput("map", height = "800px")
        )
      )
    )
  )

  # Server
  server <- function(input, output, session) {

    # Reactive values
    rv <- shiny::reactiveValues(
      sites = NULL,
      lake_data = NULL,
      fetch_data = NULL,
      all_rays_sf = NULL,
      click_result = NULL,
      status = NULL,
      error = NULL,
      has_datetime = FALSE,
      has_weather = FALSE
    )

    # Track if we have results
    output$has_results <- shiny::reactive({
      !is.null(rv$fetch_data)
    })
    shiny::outputOptions(output, "has_results", suspendWhenHidden = FALSE)

    # Handle file upload
    shiny::observeEvent(input$file_upload, {
      req(input$file_upload)

      tryCatch({
        rv$sites <- load_sites(input$file_upload$datapath)

        # Check if datetime column was detected
        rv$has_datetime <- "datetime" %in% names(rv$sites)

        if (rv$has_datetime) {
          rv$status <- sprintf("Loaded %d sites with datetime. Click 'Run Analysis' to continue.",
                               nrow(rv$sites))
        } else {
          rv$status <- sprintf("Loaded %d sites. Click 'Run Analysis' to continue.",
                               nrow(rv$sites))
        }
        rv$error <- NULL

        # Enable the run button
        shiny::updateActionButton(session, "run_analysis", disabled = FALSE)

      }, error = function(e) {
        rv$error <- sprintf("Error loading file: %s", conditionMessage(e))
        rv$status <- NULL
        rv$sites <- NULL
        rv$has_datetime <- FALSE
      })
    })

    # Status message display
    output$status_message <- shiny::renderUI({
      if (!is.null(rv$error)) {
        shiny::div(class = "error-message", rv$error)
      } else if (!is.null(rv$status)) {
        shiny::div(class = "progress-message", rv$status)
      }
    })

    # Run analysis
    shiny::observeEvent(input$run_analysis, {
      req(rv$sites)

      # Show progress
      shiny::withProgress(message = "Processing...", value = 0, {

        tryCatch({
          # Step 1: Get lake boundaries
          shiny::incProgress(0.15, detail = "Downloading lake boundaries from OSM...")
          rv$lake_data <- get_lake_boundary(rv$sites)

          # Step 2: Calculate fetch
          shiny::incProgress(0.25, detail = "Calculating fetch...")
          rv$fetch_data <- fetch_calculate(rv$sites, rv$lake_data,
                                            depth_m = input$water_depth,
                                            add_context = input$add_nhd)

          # Step 3: Add weather data if requested and datetime available
          rv$has_weather <- FALSE
          if (input$add_weather && rv$has_datetime) {
            shiny::incProgress(0.25, detail = "Fetching historical weather data...")
            tryCatch({
              rv$fetch_data$results <- add_weather_context(
                rv$fetch_data$results,
                datetime_col = "datetime",
                windows_hours = c(24, 72, 168),
                depth_m = input$water_depth
              )
              rv$has_weather <- TRUE
            }, error = function(e) {
              message("Weather data error: ", conditionMessage(e))
            })
          } else {
            shiny::incProgress(0.25)
          }

          # Step 4: Generate rose plots
          shiny::incProgress(0.25, detail = "Generating visualizations...")
          rose_plots <- vector("list", nrow(rv$fetch_data$results))
          for (i in seq_len(nrow(rv$fetch_data$results))) {
            rose_plots[[i]] <- make_rose_plot_base64(
              rv$fetch_data$results[i, ],
              rv$fetch_data$results$Site[i]
            )
          }
          rv$fetch_data$results$rose_plot <- unlist(rose_plots)

          # Step 5: Create ray geometries
          rv$all_rays_sf <- create_ray_geometries(rv$fetch_data)

          shiny::incProgress(0.1, detail = "Done!")
          rv$status <- if (rv$has_weather) "Analysis complete with weather data!" else "Analysis complete!"
          rv$error <- NULL

        }, error = function(e) {
          rv$error <- sprintf("Error during analysis: %s", conditionMessage(e))
          rv$fetch_data <- NULL
        })
      })
    })

    # Results summary
    output$results_summary <- shiny::renderText({
      req(rv$fetch_data)
      n_sites <- nrow(rv$fetch_data$results)
      n_lakes <- length(unique(rv$fetch_data$results$lake_osm_id))
      base_text <- sprintf("%d sites across %d lake(s)", n_sites, n_lakes)
      if (rv$has_weather) {
        paste0(base_text, " + weather")
      } else {
        base_text
      }
    })

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

    # Render map
    output$map <- leaflet::renderLeaflet({
      req(rv$fetch_data)

      results_wgs <- sf::st_transform(rv$fetch_data$results, 4326)
      lakes_wgs <- sf::st_transform(rv$fetch_data$lakes, 4326)

      # Build weather info string if available
      weather_info <- ""
      if (rv$has_weather && "wind_mean_24h" %in% names(results_wgs)) {
        weather_info <- sprintf(
          "<hr style='margin:5px 0;'/>
           <b>Weather (24h/3d):</b><br/>
           Wind: %.1f / %.1f m/s<br/>
           Wave Energy: %.0f / %.0f<br/>
           Temp: %.1f C | Precip: %.1f mm",
          ifelse(is.na(results_wgs$wind_mean_24h), 0, results_wgs$wind_mean_24h),
          ifelse(is.na(results_wgs$wind_mean_3d), 0, results_wgs$wind_mean_3d),
          ifelse(is.na(results_wgs$wave_energy_24h), 0, results_wgs$wave_energy_24h),
          ifelse(is.na(results_wgs$wave_energy_3d), 0, results_wgs$wave_energy_3d),
          ifelse(is.na(results_wgs$temp_mean_24h), 0, results_wgs$temp_mean_24h),
          ifelse(is.na(results_wgs$precip_total_3d), 0, results_wgs$precip_total_3d)
        )
      }

      # Build popup HTML
      popup_html <- sprintf(
        "<div style='font-family:sans-serif; width:220px;'>
         <h4 style='margin:0; border-bottom:1px solid #ccc;'>%s</h4>
         <span style='background:#eee; font-size:0.9em;'>%s</span><br/>
         <span style='color:#666; font-size:0.8em;'>%s</span>
         <center><img src='%s' width='100%%' style='margin:5px 0;'/></center>
         <b>Effective Fetch:</b> %.1f km<br/>
         <b>Orbital Velocity:</b> %.3f m/s<br/>
         %s
         </div>",
        results_wgs$Site,
        results_wgs$exposure_category,
        ifelse(is.na(results_wgs$lake_name), "", results_wgs$lake_name),
        results_wgs$rose_plot,
        results_wgs$fetch_effective / 1000,
        results_wgs$orbital_effective,
        weather_info
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
      req(rv$all_rays_sf)
      click <- input$map_marker_click
      site_id <- click$id

      if (is.null(site_id)) return()

      output$selected_site <- shiny::renderText(site_id)

      site_rays <- rv$all_rays_sf[rv$all_rays_sf$Site == site_id, ]
      site_rays_wgs <- sf::st_transform(site_rays, 4326)

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

    # Click handler for map (new point analysis)
    shiny::observeEvent(input$map_click, {
      req(rv$fetch_data)
      click <- input$map_click
      if (is.null(click)) return()

      lakes_utm <- rv$fetch_data$lakes
      utm_epsg <- sf::st_crs(lakes_utm)$epsg

      click_pt_wgs <- sf::st_sfc(sf::st_point(c(click$lng, click$lat)), crs = 4326)
      click_pt_utm <- sf::st_transform(click_pt_wgs, utm_epsg)

      lakes_contain <- sf::st_intersects(click_pt_utm, lakes_utm)[[1]]

      if (length(lakes_contain) == 0) {
        output$selected_site <- shiny::renderText("Click is not inside a lake")
        rv$click_result <- NULL
        return()
      }

      lake_idx <- lakes_contain[1]
      lake_poly <- lakes_utm[lake_idx, ]
      lake_name <- if (!is.na(lake_poly$name)) lake_poly$name else "Unknown Lake"

      output$selected_site <- shiny::renderText(
        sprintf("Analyzing point in %s...", lake_name)
      )

      tryCatch({
        lake_boundary <- tryCatch({
          sf::st_cast(lake_poly, "MULTILINESTRING")
        }, error = function(e) {
          sf::st_boundary(lake_poly)
        })

        angle_res <- get_opt("angle_resolution_deg")
        angles <- seq(0, 360 - angle_res, by = angle_res)

        nudged_pt <- nudge_inward(
          click_pt_utm, lake_boundary, lake_poly,
          get_opt("buffer_distance_m")
        )
        nudged_sf <- sf::st_sf(
          Site = "Custom Point",
          geometry = sf::st_sfc(nudged_pt, crs = utm_epsg)
        )

        fetch_dists <- get_highres_fetch(nudged_sf, lake_boundary, lake_poly, angles)

        fetch_mean <- mean(fetch_dists, na.rm = TRUE)
        fetch_max <- max(fetch_dists, na.rm = TRUE)
        fetch_effective <- mean(sort(fetch_dists, decreasing = TRUE)[1:3], na.rm = TRUE)
        # Use user-specified depth from input
        orbital <- calc_orbital(fetch_effective, depth_m = input$water_depth)
        exposure <- if (fetch_effective < 2500) "Sheltered" else if (fetch_effective > 5000) "Exposed" else "Moderate"

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

        rv$click_result <- list(
          lake_name = lake_name,
          fetch_mean = fetch_mean,
          fetch_max = fetch_max,
          fetch_effective = fetch_effective,
          orbital = orbital,
          exposure = exposure,
          rays = rays_wgs,
          point = sf::st_transform(nudged_sf, 4326)
        )

        output$selected_site <- shiny::renderText(
          sprintf("Custom Point in %s", lake_name)
        )

        exp_color <- switch(exposure,
          "Exposed" = "firebrick",
          "Moderate" = "goldenrod",
          "Sheltered" = "forestgreen"
        )

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
          sprintf("Error: %s", conditionMessage(e))
        )
        rv$click_result <- NULL
      })
    })

    # Display click results
    output$click_results <- shiny::renderUI({
      res <- rv$click_result
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

    # Download CSV
    output$download_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("fetch_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        req(rv$fetch_data)
        results_wgs <- sf::st_transform(rv$fetch_data$results, 4326)
        coords <- sf::st_coordinates(results_wgs)
        output_df <- sf::st_drop_geometry(results_wgs)
        output_df$longitude <- coords[, 1]
        output_df$latitude <- coords[, 2]
        # Remove rose_plot column (base64 data)
        output_df$rose_plot <- NULL
        utils::write.csv(output_df, file, row.names = FALSE)
      }
    )

    # Download GeoPackage
    output$download_gpkg <- shiny::downloadHandler(
      filename = function() {
        paste0("fetch_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".gpkg")
      },
      content = function(file) {
        req(rv$fetch_data)
        results_wgs <- sf::st_transform(rv$fetch_data$results, 4326)
        results_wgs$rose_plot <- NULL
        sf::st_write(results_wgs, file, driver = "GPKG", delete_dsn = TRUE)
      }
    )

    # Clear custom point
    shiny::observeEvent(input$clear_click, {
      rv$click_result <- NULL
      output$selected_site <- shiny::renderText("")
      leaflet::leafletProxy("map") |>
        leaflet::clearGroup("rays") |>
        leaflet::clearGroup("custom_point")
    })

    # Reset app
    shiny::observeEvent(input$reset_app, {
      rv$sites <- NULL
      rv$lake_data <- NULL
      rv$fetch_data <- NULL
      rv$all_rays_sf <- NULL
      rv$click_result <- NULL
      rv$status <- NULL
      rv$error <- NULL
      shiny::updateActionButton(session, "run_analysis", disabled = TRUE)
    })
  }

  shiny::shinyApp(ui, server)
}
