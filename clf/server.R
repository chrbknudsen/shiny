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

    output$distriPlot <- renderPlot({
      
      # distrib <- input$distribution
      range <- switch(distrib(),
                      dnorm = c(-3,3),
                      dunif = c(-3,3),
                      dlnorm = c(0,3),
                      dweibull = c(0,10),
                      c(-3,3)
                      )
      
      argumenter <- case_when(distrib() == "dnorm" ~ list(0,1),
                              distrib() == "dunif" ~ list(-3,3),
                              distrib() == "dlnorm" ~ list(0,1),
                              distrib() == "dweibull" ~ list(2, 2.5),
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
                         distrib() == "dweibull" ~ c(0,10),
                         .default =  c(-3,3))
      
      argumenter <- case_when(distrib() == "dnorm" ~ list(0,1),
                              distrib() == "dunif" ~ list(-3,3),
                              distrib() == "dlnorm" ~ list(0,1),
                              distrib() == "dweibull" ~ list(2, 2.5),
                              .default =  list(0,1))
      
      sample_data <- switch(distrib(),
                     dnorm = rnorm(input$sample_size, 0, 1),
                     dunif = runif(input$sample_size, -3, 3),
                     dlnorm = rlnorm(input$sample_size, 0,1),
                     dweibull = rweibull(input$sample_size, 2, 2.5),
                     rnorm(input$sample_size, 0,1)
                     )
      
      # plot sample
      

      data.frame(x = sample_data) %>% 
      ggplot(aes(x)) + 
        geom_histogram()

      
    })
    
    output$flere_samples <- renderPlot({
      
      # distrib <- input$distribution
      range <- case_when(distrib() == "dnorm" ~ c(-3,3),
                         distrib() == "dunif" ~ c(-3,3),
                         distrib() == "dlnorm" ~ c(0,3),
                         distrib() == "dweibull" ~ c(0,10),
                         .default =  c(-3,3))
      
      argumenter <- case_when(distrib() == "dnorm" ~ list(0,1),
                              distrib() == "dunif" ~ list(-3,3),
                              distrib() == "dlnorm" ~ list(0,1),
                              distrib() == "dweibull" ~ list(2, 2.5),
                              .default =  list(0,1))
      
      sample_data <- switch(distrib(),
                            dnorm = rnorm(input$sample_size, 0, 1),
                            dunif = runif(input$sample_size, -3, 3),
                            dlnorm = rlnorm(input$sample_size, 0,1),
                            dweibull = rweibull(input$sample_size, 2, 2.5),
                            rnorm(input$sample_size, 0,1)
      )
      collated_samples <- replicate(input$sample_n, mean(switch(distrib(),
                                                                dnorm = rnorm(input$sample_size, 0, 1),
                                                                dunif = runif(input$sample_size, -3, 3),
                                                                dlnorm = rlnorm(input$sample_size, 0,1),
                                                                dweibull = rweibull(input$sample_size, 2, 2.5),
                                                                rnorm(input$sample_size, 0,1))))
      # plot sample
      
      
      data.frame(x = collated_samples) %>% 
        ggplot(aes(x)) + 
        geom_histogram()
      
      
    })
}
