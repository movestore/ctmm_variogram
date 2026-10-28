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

shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id) ## all IDs of UI functions need to be wrapped in ns()
  tagList(
    titlePanel("Variogram"),
    wellPanel(
               fluidRow(
                 column(5, sliderInput(
                 ns("fraction"),
                 "Fraction:",
                 min = 0, max = 1, value = .5
               )),
               column(5, sliderInput(
                 ns("columns"),
                 "Max columns:",
                 min = 1, max = 10, value = 8
               )),
               column(2,
                      checkboxInput(
                         ns("plot_axes"),
                         "Use full data extent for axes", value = FALSE
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
    plotOutput(ns("plot"))
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

    if (input$view_mode == "Population variogram") {
      plot(svf_sel, fraction = input$fraction)
    } else {
      graphics::par(mfrow = c(layout()$rows, layout()$cols),
                    mar = c(5, 5, 4, 1), ps = 18, cex = 1, cex.main = 0.9)

      if (input$plot_axes) {
        extent_tele <- ctmm::extent(svf_sel)
        for (i in seq_along(svf_sel)) {
          plot(svf_sel[[i]],
               fraction = input$fraction,
               xlim = c(0, extent_tele["max", "x"]),
               ylim = c(0, extent_tele["max", "y"]),
               bty = "n")
          graphics::title(names(svf_sel[i]))
        }
      } else {
        for (i in seq_along(svf_sel)) {
          plot(svf_sel[[i]],
               fraction = input$fraction,
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
