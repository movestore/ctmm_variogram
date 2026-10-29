library(shiny)
library(ctmm)
library(dplyr)

# Determine plot grid parameters for the given plot width and number of panels.
# Caps the grid at `max_cols` columns if there are more plots than `max_cols`.
# Ensures that plots do not have height collapsed as fewer columns/more rows
# are added. Returns the number of columns, number of rows, and the total
# plot height. These are used later when drawing the plot based on the
# user inputs (e.g. selected animals, n columns, etc.)
plot_layout <- function(n_panels, max_cols, width) {
  min_panel_width <- 175
  min_panel_height <- 300
  max_panel_height <- 500

  n_panels <- max(n_panels, 1)
  cols <- min(max_cols, n_panels)
  
  # The width is unknown (NULL or 0) until the browser has drawn the plot area
  if (!is.null(width) && width > 0) {
    # Determine the number of columns for the plot. Use only as many as fit
    # across the plot at the `min_panel_width` or wider. Never use fewer than 1.
    cols <- max(1, min(cols, floor(width / min_panel_width)))
    
    # Make plots roughly square, within a capped min/max panel height
    panel_height <- min(max(width / cols, min_panel_height), max_panel_height)
  } else {
    panel_height <- min_panel_height
  }
  
  # Rows implied by the number of panels and number of columns as computed above
  rows <- ceiling(n_panels / cols)

  list(cols = cols, rows = rows, height = rows * panel_height)
}

# Use zoom based on ctmm's `zoom()`. Usually this only works inside RStudio,
# so we reimplement for this app. Slider shows time lags up to 4^(z - 1) times
# the longest time lag in the data
zoom_base <- 4
zoom_default <- 1 + log(0.5, zoom_base) # shows half the longest time lag

zoom_range <- function(data) {
  max_lag <- max(sapply(data, function(x) diff(range(x$t))))
  min_lag <- min(sapply(data, function(x) stats::median(diff(x$t))))
  min_fraction <- min(0.5, 10 * min_lag / max_lag)
  list(min = 1 + log(min_fraction, zoom_base), max_lag = max_lag)
}

zoom_cutoff <- function(z, max_lag) {
  zoom_base^(z - 1) * max_lag
}

# Turns a time lag in seconds into readable text, e.g. "3.5 days".
format_lag <- function(seconds) {
  units <- c(seconds = 1, minutes = 60, hours = 3600, days = 86400,
             months = 365.25 * 86400 / 12, years = 365.25 * 86400)
  unit <- names(units)[max(1, which(units <= seconds))]
  value <- signif(seconds / units[[unit]], 2)
  if (value == 1) unit <- sub("s$", "", unit)
  paste(value, unit)
}

shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id) ## all IDs of UI functions need to be wrapped in ns()
  tagList(
    titlePanel("Variogram"),
    # The zoom slider's own numbers mean nothing to users, so they are
    # hidden and the time-lag cutoff is shown as text underneath instead
    tags$style(".zoom-slider .irs-min, .zoom-slider .irs-max,
                .zoom-slider .irs-single { display: none; }"),
    wellPanel(
               fluidRow(
                 column(5,
                        uiOutput(ns("zoom_ui")),
                        div(style = "display: flex; align-items: center; gap: 10px;",
                            textOutput(ns("zoom_label")),
                            actionButton(ns("zoom_reset"), "Reset to default",
                                         class = "btn-sm"))
               ),
               column(5, sliderInput(
                 ns("columns"),
                 "Max columns:",
                 min = 1, max = 10, value = 8
               )),
               column(2,
                      checkboxInput(
                         ns("plot_axes"),
                         "Same axes for all animals", value = FALSE
                       ),
                      radioButtons(
                        ns("view_mode"),
                        "View:",
                        choices = c("Per individual", "Population variogram"),
                        selected = "Per individual"
                      ),
                      uiOutput(ns("selectvar"))
               ))
    ),
    plotOutput(ns("plot"), height = "auto")
  )
}

shinyModule <- function(input, output, session, data){ ## The parameter "data" is reserved for the data object passed on from the previous app
  ns <- session$ns 
  
  output$selectvar <- renderUI({
    checkboxGroupInput(
      ns("selected_ids"),
      "Animals:",
      choices = names(data),
      selected = names(data)
    )
  })

  zoom <- zoom_range(data)

  output$zoom_ui <- renderUI({
    div(class = "zoom-slider", sliderInput(
      ns("zoom"),
      "Zoom:",
      min = zoom$min, max = 1, value = zoom_default,
      step = 0.01, ticks = FALSE
    ))
  })

  # The reset button is left out of saved settings. Otherwise restoring them
  # would count as a click and undo the restored zoom.
  setBookmarkExclude("zoom_reset")
  observeEvent(input$zoom_reset, {
    updateSliderInput(session, "zoom", value = zoom_default)
  }, ignoreInit = TRUE)

  cutoff <- reactive({
    zoom_cutoff(req(input$zoom), zoom$max_lag)
  })

  output$zoom_label <- renderText({
    paste("Time lags shown: up to", format_lag(cutoff()))
  })

  svf1 <- reactive({
    req(input$selected_ids)
    svf <- lapply(data[input$selected_ids], variogram)
    if (input$view_mode == "Population variogram") {
      mean(svf)
    } else {
      svf
    }
  })

  layout <- reactive({
    if (input$view_mode == "Population variogram") {
      n_panels <- 1
    } else {
      n_panels <- length(input$selected_ids)
    }
    
    width <- session$clientData[[paste0("output_", ns("plot"), "_width")]]
    
    plot_layout(n_panels, input$columns, width)
  })

  output$plot <- renderPlot({
    svf_sel <- req(svf1())
    max_lag <- req(cutoff())

    # Share of a variogram's own lag range that falls within the zoom cutoff
    zoom_fraction <- function(svf) {
      min(1, max_lag / utils::tail(svf$lag, 1))
    }

    if (input$view_mode == "Population variogram") {
      plot(svf_sel, fraction = zoom_fraction(svf_sel))
    } else {
      graphics::par(mfrow = c(layout()$rows, layout()$cols),
                    mar = c(5, 5, 4, 1), ps = 18, cex = 1, cex.main = 0.9)
      # ctmm picks long or short axis labels from the size of the panel drawn
      # last, so on the first panel it would measure the whole page instead.
      # Starting an empty first panel and stepping back to it makes every
      # panel choose its labels from the same panel size.
      graphics::plot.new()
      graphics::par(mfg = c(1, 1))

      if (input$plot_axes) {
        # Shared axes cover the zoomed part of every selected variogram
        zoomed <- lapply(svf_sel, function(svf) svf[svf$lag <= max_lag, ])
        extent_tele <- ctmm::extent(zoomed)
        for (i in seq_along(zoomed)) {
          plot(zoomed[[i]],
               fraction = 1,
               xlim = c(0, extent_tele["max", "x"]),
               ylim = c(0, extent_tele["max", "y"]),
               bty = "n")
          graphics::title(names(zoomed[i]))
        }
      } else {
        for (i in seq_along(svf_sel)) {
          plot(svf_sel[[i]],
               fraction = zoom_fraction(svf_sel[[i]]),
               bty = "n")
          graphics::title(names(svf_sel[i]))
        }
      }
    }
  }, height = function() layout()$height)

  

  
  return(reactive({ 
    data
  })) ## if data are not modified, the unmodified input data must be returned
}
