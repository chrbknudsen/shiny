#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("CLF"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("distribution", "Vælg fordeling", 
                        choices = c("uniform" = "dunif", 
                                    "normal" = "dnorm", 
                                    "lnorm" = "dlnorm",
                                    "Weibull" = "dweibull"), selected = "dnorm"),
            sliderInput("sample_size",
                        "sample size:",
                        min = 1,
                        max = 500,
                        value = 30),
            sliderInput("sample_n",
                        "sample n:",
                        min = 1,
                        max = 500,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distriPlot"),
            plotOutput("samplePlot"),
            plotOutput("flere_samples")
            
        )
    )
)
