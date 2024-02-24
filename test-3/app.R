# Shiny example
ui <- page_sidebar(
  title = "My app"
)

server <- function(input, output) {
  bs_theme_update(theme, preset = "cyborg")
  
}

shinyApp(ui, server)