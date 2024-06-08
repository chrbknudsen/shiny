#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

valg <- c(
  "Grays",         
  "Blues 2",      
  "Blues 3" ,   
  "Purples 2",     
  "Purples 3",     
  "Oslo"    ,     
  "Inferno"  ,     
  "Mako"      ,    
  "Teal"       ,   
  "Purp"        , 
  "BrwnYl"       , 
  "YlOrRd",
  "YlGn",
  "Blues",         
  "Lajolla",      
  "Turku"   ,      
  "Blue-Red" ,    
  "Blue-Yellow 2", 
  "Broc"       ,
  "Cork"  ,
  "Vik"    ,       
  "Lisbon"  ,     
  "Tofino"   ,     
  "Cividis"  
)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Vælg farve"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("palette",
                        "palette:",
                        choices = valg)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
)
