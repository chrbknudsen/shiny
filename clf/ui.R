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
page_sidebar(
  theme = bs_theme(version = 5),
  title = "CLF",
  sidebar = sidebar(
    title = "kontrol",
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
  card(card_header("Fordelingen"),
                   plotOutput("distriPlot")),
  card(card_header("Tilfældigt sample"),
       plotOutput("samplePlot")
       ),
  card(card_header("Middelværdi af x tilfældige samples"),
       plotOutput("flere_samples"))
)
