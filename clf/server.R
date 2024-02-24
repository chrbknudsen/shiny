#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(bslib)
library(dplyr)

# Define server logic required to draw a histogram
function(input, output, session) {
    distrib <- reactive(input$distribution)
    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')

    })
    output$distriPlot <- renderPlot({
      
      # distrib <- input$distribution
      range <- case_when(distrib() == "dnorm" ~ c(-3,3),
                         distrib() == "dunif" ~ c(-3,3),
                         distrib() == "dlnorm" ~ c(0,3),
                         .default =  c(-3,3))
      
      argumenter <- case_when(distrib() == "dnorm" ~ list(0,1),
                              distrib() == "dunif" ~ list(-3,3),
                              distrib() == "dlnorm" ~ list(0,1),
                              .default =  list(-3,3))
      
      # plot fordelingen
      ggplot(data.frame(x=range), aes(x)) + 
        stat_function(fun=match.fun(distrib()), args=argumenter, aes(fill = "orange"), geom  ="area") +
        scale_fill_manual(values = "orange", guide  =F)
      
    })

    output$samplePlot <- renderPlot({
      
      # distrib <- input$distribution
      range <- case_when(distrib() == "dnorm" ~ c(-3,3),
                         distrib() == "dunif" ~ c(-3,3),
                         distrib() == "dlnorm" ~ c(0,3),
                         .default =  c(-3,3))
      
      argumenter <- case_when(distrib() == "dnorm" ~ list(0,1),
                              distrib() == "dunif" ~ list(-3,3),
                              distrib() == "dlnorm" ~ list(0,1),
                              .default =  list(-3,3))
      
      # plot fordelingen
      data.frame(x = rnorm(100, 0, 1)) %>% 
      ggplot(aes(x)) + 
        geom_histogram()

      
    })
}
