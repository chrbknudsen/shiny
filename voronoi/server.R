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
library(deldir)
library(tidyverse)

set.seed(42)
tiler <- 50
x <- rnorm(tiler)
y <- rnorm(tiler)

tesselation <- deldir(x, y)
tiles <- tile.list(tesselation)


points <- data.frame(x=x, y=y)


paletter <- hcl.pals()


tiles_df <- lapply(tiles, function(tile){
  data.frame(x=tile$x, y = tile$y)
}) %>% 
  bind_rows(.id = "tile_id")


# Define server logic required to draw a histogram
function(input, output, session) {

    output$distPlot <- renderPlot({

      tiles_df %>% 
        ggplot(aes(x = x, y = y, group = tile_id, fill = tile_id)) +
        geom_polygon() +
        theme_minimal() +
        coord_polar(start = 0.47) +
        theme(legend.position = "none" ,
              axis.text = element_blank(),
              axis.title = element_blank(),
              axis.line = element_blank(),
              axis.ticks = element_blank(),
              panel.grid = element_blank()) +
        scale_fill_manual(values = hcl.colors(50, input$palette)) +
        theme(legend.position = "none")

    })

}
