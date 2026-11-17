library(shiny)
library(ctmm)
library(shinycssloaders)
library(shinyjs)

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
# so we reimplement for this app. The slider value z shows the first
# 4^(z - 1) of each animal's own time lags (ctmm's `fraction`). A log scale
# leaves room on the slider to zoom in to very short time lags.
zoom_base <- 4
zoom_default <- 1 + log(0.5, zoom_base) # shows the first half of the time lags

zoom_to_fraction <- function(z) {
  zoom_base^(z - 1)
}

# Lowest slider value. Lets the user zoom in to about 10 sampling intervals
# on the animal whose track is longest relative to its sampling interval.
zoom_min <- function(data) {
  fractions <- sapply(data, function(x) {
    10 * stats::median(diff(x$t)) / diff(range(x$t))
  })
  1 + log(min(0.5, fractions), zoom_base)
}

# Tick marks with fraction labels under the zoom slider. The slider's own
# ticks can only be evenly spaced, so these are placed by hand. Positions are
# percentages of the slider track. Only a few ticks are used so the labels
# don't overlap on a narrow slider.
zoom_ticks <- function(z_min) {
  fractions <- c(0.1, 0.5, 1)
  z <- 1 + log(fractions, zoom_base)
  keep <- z >= z_min
  div(
    class = "zoom-ticks",
    lapply(which(keep), function(i) {
      span(
        class = "zoom-tick",
        style = sprintf("left: %.2f%%;", 100 * (z[i] - z_min) / (1 - z_min)),
        format(fractions[i], scientific = FALSE, drop0trailing = TRUE)
      )
    })
  )
}

shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id) ## all IDs of UI functions need to be wrapped in ns()
  tagList(
    titlePanel("Variogram"),
    # The zoom slider's own numbers are on a log scale and mean nothing to
    # users, so they are hidden and labelled ticks are drawn underneath
    # instead. The ticks are inset by half the handle width (22px), because
    # that is where the slider's track starts and ends.
    tags$style("
      .zoom-slider .irs-min, .zoom-slider .irs-max,
      .zoom-slider .irs-single { display: none; }
      .zoom-slider .form-group { margin-bottom: 0; }
      .zoom-ticks { position: relative; height: 22px; margin: -6px 11px 8px; }
      .zoom-tick {
        position: absolute; transform: translateX(-50%);
        padding-top: 5px; font-size: 11px; color: #777777;
      }
      .zoom-tick::before {
        content: ''; position: absolute; top: 0; left: 50%;
        height: 4px; border-left: 1px solid #999999;
      }
    "),
    # Always keep room for the page's scrollbar. Otherwise a scrollbar that
    # appears when the plot overflows the page forces the margins inward,
    # triggering a trivial redraw.
    tags$style("html { scrollbar-gutter: stable; }"),
    # On wide screens the sidebar stays in view while the plots scroll. Its
    # height is capped at the window height so a long list of animals scrolls
    # inside the sidebar instead of running off the bottom of the page.
    tags$style("
      @media (min-width: 768px) {
        .variogram-sidebar {
          position: sticky; top: 10px;
          max-height: calc(100vh - 20px); overflow-y: auto;
        }
      }
    "),
    # Grey out the label of a disabled checkbox, not just the box itself
    tags$style(".checkbox input:disabled + span { color: #999999; }"),
    useShinyjs(),
    sidebarLayout(
      tagAppendAttributes(class = "variogram-sidebar", sidebarPanel(
        width = 3,
        radioButtons(
          ns("view_mode"),
          "View:",
          choices = c("Per individual", "Population variogram"),
          selected = "Per individual"
        ),
        checkboxInput(ns("plot_axes"), "Use same axes for all animals", value = FALSE),
        hr(),
        sliderInput(
          ns("columns"),
          "Max columns:",
          min = 1, max = 6, value = 3, width = "100%"
        ),
        hr(),
        uiOutput(ns("zoom_ui")),
        actionButton(ns("zoom_reset"), "Reset to default", class = "btn-sm"),
        hr(),
        uiOutput(ns("selectvar")),
        hr(),
        tags$label(class = "control-label", "Output:"),
        helpText(
          "Check this box to remove any unselected animals in the list above ",
          "from this App's output data."
        ),
        checkboxInput(
          ns("remove_unselected"),
          "Remove unselected animals from output",
          value = FALSE
        ),
        uiOutput(ns("removed_note"))
      )),
      mainPanel(
        width = 9,
        # Show a spinner while the plot is calculated or redrawn.
        # min-height gives the spinner room before the first plot is drawn.
        tags$style(paste0("#", ns("plot"), " { min-height: 300px; }")),
        withSpinner(plotOutput(ns("plot"), height = "auto"), hide.ui = FALSE)
      )
    )
  )
}

shinyModule <- function(input, output, session, data){ ## The parameter "data" is reserved for the data object passed on from the previous app
  ns <- session$ns 
  
  # The same-axes checkbox and the column slider only affect the per-animal
  # plots, so disable them while the population view is selected
  observeEvent(input$view_mode, {
    per_individual <- input$view_mode == "Per individual"
    toggleState("plot_axes", condition = per_individual)
    toggleState("columns", condition = per_individual)
  })

  output$selectvar <- renderUI({
    checkboxGroupInput(
      ns("selected_ids"),
      "Animals:",
      choices = names(data),
      selected = names(data)
    )
  })

  z_min <- zoom_min(data)

  output$zoom_ui <- renderUI({
    div(
      class = "zoom-slider",
      tags$label(class = "control-label", `for` = ns("zoom"), "Zoom:"),
      helpText("Proportion of each variogram's time lags shown"),
      sliderInput(
        ns("zoom"),
        label = NULL,
        min = z_min, max = 1, value = zoom_default,
        step = 0.01, ticks = FALSE, width = "100%"
      ),
      zoom_ticks(z_min)
    )
  })

  # The reset button is left out of saved settings. Otherwise restoring them
  # would count as a click and undo the restored zoom.
  setBookmarkExclude("zoom_reset")
  observeEvent(input$zoom_reset, {
    updateSliderInput(session, "zoom", value = zoom_default)
  }, ignoreInit = TRUE)

  # Debounce the settings needed for plot rendering. This prevents laggy
  # plot redraws when settings are changed in succession. Also prevents
  # two loads on session start if bookmarks are being restored.
  settings <- debounce(
    reactive(
      list(
        selected_ids = input$selected_ids,
        view_mode = input$view_mode,
        zoom = input$zoom,
        plot_axes = input$plot_axes,
        columns = input$columns
      )
    ),
    millis = 250
  )

  svf1 <- reactive({
    selected_ids <- req(settings()$selected_ids)
    svf <- lapply(data[selected_ids], variogram)
    if (settings()$view_mode == "Population variogram") {
      mean(svf)
    } else {
      svf
    }
  })

  layout <- reactive({
    if (settings()$view_mode == "Population variogram") {
      n_panels <- 1
    } else {
      n_panels <- length(settings()$selected_ids)
    }

    width <- session$clientData[[paste0("output_", ns("plot"), "_width")]]

    plot_layout(n_panels, settings()$columns, width)
  })

  output$plot <- renderPlot({
    svf_sel <- req(svf1())
    fraction <- zoom_to_fraction(req(settings()$zoom))

    if (settings()$view_mode == "Population variogram") {
      plot(svf_sel, fraction = fraction)
    } else {
      graphics::par(mfrow = c(layout()$rows, layout()$cols),
                    mar = c(5, 5, 4, 1), ps = 18, cex = 1, cex.main = 0.9)
      # ctmm picks long or short axis labels from the size of the panel drawn
      # last, so on the first panel it would measure the whole page instead.
      # Starting an empty first panel and stepping back to it makes every
      # panel choose its labels from the same panel size.
      graphics::plot.new()
      graphics::par(mfg = c(1, 1))

      if (isTRUE(settings()$plot_axes)) {
        # Shared axes cover the zoomed part of every selected variogram
        zoomed <- lapply(svf_sel, function(svf) {
          svf[svf$lag <= fraction * utils::tail(svf$lag, 1), ]
        })
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
               fraction = fraction,
               bty = "n")
          graphics::title(names(svf_sel[i]))
        }
      }
    }
  }, height = function() layout()$height)

  # Record the animals that should be removed from the output data
  removed_ids <- reactive({
    if (!isTRUE(input$remove_unselected) || !"selected_ids" %in% names(input)) {
      return(character(0))
    }
    setdiff(names(data), input$selected_ids)
  })

  # Lists the animals that will be removed from the output, if any
  output$removed_note <- renderUI({
    ids <- removed_ids()
    if (length(ids) == 0) return(NULL)
    p(
      class = "text-warning",
      paste0(
        "These animals will not be passed to the next App: ",
        paste(ids, collapse = ", "), "."
      )
    )
  })

  filtered_data <- reactive({
    ids <- removed_ids()
    
    if (length(ids) == 0) {
      return(data)
    }
    
    logger.warn("Removed unselected animals from the output: %s",
                paste(ids, collapse = ", "))
    
    kept <- data[setdiff(names(data), ids)]
    
    # Next Apps may fail on an empty list, so pass on nothing instead
    if (length(kept) == 0) {
      NULL
    } else {
      kept
    }
  })

  return(filtered_data)
}
