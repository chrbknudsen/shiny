library(shiny)
library(hexSticker)
library(colorpicker)  # Bibliotek for farvevælger

# UI
ui <- fluidPage(
  titlePanel("Hexsticker Generator"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Vælg SVG-fil", accept = c(".svg", ".png", ".jpg")),
      textInput("url", "URL tekst", value = "KUB Datalab"),
      numericInput("s_x", "s_x", value = 1, min = 0, max = 2),
      numericInput("s_y", "s_y", value = 1, min = 0, max = 2),
      numericInput("s_height", "s_height", value = 0.6, min = 0.1, max = 2),
      numericInput("s_width", "s_width", value = 0.6, min = 0.1, max = 2),
      colourInput("h_fill", "Hintergrund farve", value = "black"),  # Farvevælger for baggrund
      colourInput("h_color", "Kantfarve", value = "black"),          # Farvevælger for kant
      numericInput("u_size", "URL tekststørrelse", value = 10, min = 1, max = 50),
      colourInput("u_color", "URL tekstfarve", value = "green"),     # Farvevælger for URL tekstfarve
      numericInput("u_x", "URL x position", value = 1.15, min = 0, max = 2),
      numericInput("u_y", "URL y position", value = 0.16, min = 0, max = 2),
      actionButton("generate", "Generer Hexsticker")
    ),
    mainPanel(
      imageOutput("sticker_img")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  observeEvent(input$generate, {
    req(input$file)
    
    # Gem SVG-fil til en midlertidig placering
    svg_path <- input$file$datapath
    
    # Valider inputværdierne for at undgå fejl
    if (input$s_x < 0 || input$s_x > 2 || input$s_y < 0 || input$s_y > 2 ||
        input$s_height <= 0 || input$s_width <= 0) {
      showNotification("Fejl: Tjek dine skaleringsværdier.", type = "error")
      return(NULL)
    }
    
    # Generer hexsticker
    sticker <- hexSticker::sticker(svg_path, package = "", 
                                   filename = "sticker_output.png",
                                   url = input$url,
                                   s_x = input$s_x,
                                   s_y = input$s_y,
                                   s_height = input$s_height,
                                   s_width = input$s_width,
                                   h_fill = input$h_fill,
                                   h_color = input$h_color,
                                   u_size = input$u_size,
                                   u_color = input$u_color,
                                   u_x = input$u_x,
                                   u_y = input$u_y
    )
    
    # Vis billedet
    output$sticker_img <- renderImage({
      list(src = "sticker_output.png", contentType = "image/png")
    }, deleteFile = FALSE)
  })
}

# Kør appen
shinyApp(ui = ui, server = server)
