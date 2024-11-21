#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(magrittr)
flammebound <- 2.5

# find dagens juledag
today <- Sys.Date()

# Start- og slutdato for de 12 juledage
start_date <- lubridate::ymd(paste0(lubridate::year(today), "-12-25"))
end_date <- start_date + lubridate::days(11)

# Beregning af juledag eller 0
juledag <- ifelse(today >= start_date & today <= end_date, 
                  as.integer(today - start_date + 1), 
                  0)


lys <- tibble::tibble(
  x = letters[1:12],
  y = purrr::map(1:12, ~ abs(rnorm(2000)))
) %>% 
  tidyr::unnest(y)

generate_flammer <- function(dage) {
  tibble(
    x = letters[1:dage],
    y = map(1:dage, ~ c(
      rnorm(30, mean = 0, sd = 1),       # Nederste del
      rnorm(30, mean = 0.5, sd = 0.7),   # Midten
      rnorm(30, mean = 1, sd = 0.5)      # Øverste del
    ))
  ) %>%
    tidyr::unnest(y) %>%
    dplyr::filter(y > 0) %>% 
    dplyr::filter(x %in% letters[0:dage])
}



# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: black; /* Sort baggrund */
        color: green; /* Hvid tekst som standard */
      }
      h2 {
        color: green; /* Grøn titel */
        text-align: center; /* Centreret titel (valgfrit) */
      }
      .well {
        background-color: black !important; /* Sort baggrund for inputfeltet */
        color: green !important; /* Grøn tekst */
        border: 0px solid green !important; /* Grøn kant */
      }
      .selectize-input {
        background-color: black !important; /* Sort baggrund for inputfeltet */
        color: green !important; /* Grøn tekst */
        border: 2px solid green !important; /* Grøn kant */
      }
      .selectize-dropdown {
        background-color: black !important; /* Sort baggrund for dropdown */
        color: green !important; /* Grøn tekst i dropdown */
      }
      .selectize-dropdown-content .option:hover {
        background-color: green !important; /* Grøn baggrund når du holder musen over */
        color: black !important; /* Sort tekst når musen er over */
      }
    "))
  ),
    # Application title
    titlePanel("De tolv juledage"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "dage",
                             label = "Antal juledage",
                             choices = 0:12,
                             selected = juledag)

        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic 
server <- function(input, output) {
  # definer funktion til at generere data til flammer.
    flammer <- reactive({
    generate_flammer(input$dage)
  })
    output$distPlot <- renderPlot({

        # lav plottet
        ggplot2::ggplot() +
          ggplot2::geom_violin(data = flammer(), ggplot2::aes(x=x,y=y+flammebound), fill = "yellow", trim = FALSE, bounds = c(flammebound, Inf)) +
          ggplot2::geom_jitter(data = lys, mapping = ggplot2::aes(x = x, y=y),color = "red") +
                    ggplot2::theme_void() +
          ggplot2::theme(
            plot.background = ggplot2::element_rect(fill = "black")
          )
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
