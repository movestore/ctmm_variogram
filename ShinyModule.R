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
                         "Same axes for all individuals", value = FALSE
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
    selectInput(ns("select_input"), "Select animal(s):",
                c("All", "Population Variogram", names(data)))
  })
  
  filter_id <- reactive({
    input$select_input
  })
  observeEvent(input$select_input, {
    svf <- lapply(data, variogram)
    
    svf1 <- if (filter_id() == "All") {
      svf
    } else if (filter_id() == "Population Variogram") {
      mean(svf)
    } else {
      svf[grep(filter_id(), names(svf))]
    }
    
    row_count <- reactive({
      ceiling(length(svf1) / input$columns)
    })
    
    
    # Individual plots
    output$plot <- renderPlot({
      req(svf1)
      
      if(filter_id() == "All") {
      max.lag <- max(sapply(svf1, function(v){ last(v$lag) } ))
      xlim <- max.lag * input$fraction
      svf <- lapply(svf1,
                    function(svf) svf[svf$lag <= xlim, ])
      
      graphics::par(mfrow = c(row_count(), input$columns),
                    mar = c(5, 5, 4, 1), ps = 18, cex = 1, cex.main = 0.9)
      
      if (input$plot_axes) {
        extent_tele <- ctmm::extent(svf1)
        for (i in seq_along(svf1)) {
          plot(svf1[[i]], 
               fraction = input$fraction,
               xlim = c(0, extent_tele["max", "x"]),
               ylim = c(0, extent_tele["max", "y"]),
               bty="n")
          graphics::title(names(svf1[i]))
        }
      }  else {
        for (i in seq_along(svf1)) {
          plot(svf1[[i]], 
               fraction = input$fraction,
               bty="n")
          graphics::title(names(svf1[i]))
        }
      }
    } else {
      plot(svf1, fraction = input$fraction)
    }
  })
})

  

  
  return(reactive({ 
    data
  })) ## if data are not modified, the unmodified input data must be returned
}
