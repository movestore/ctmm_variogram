library(shiny)
library(ctmm)
library(dplyr)

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
                 "Columns:",
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

  row_count <- reactive({
    ceiling(length(svf1()) / input$columns)
  })

  output$plot <- renderPlot({
    svf_sel <- req(svf1())

    if (input$view_mode == "Population variogram") {
      plot(svf_sel, fraction = input$fraction)
    } else {
      graphics::par(mfrow = c(row_count(), input$columns),
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
  })

  

  
  return(reactive({ 
    data
  })) ## if data are not modified, the unmodified input data must be returned
}
