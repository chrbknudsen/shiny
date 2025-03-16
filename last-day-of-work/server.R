library(shiny)
library(shinyjs)
library(future)
library(promises)


function(input, output, session) {
  vis_hvilket <- reactiveVal(1)                 # Step-tracker
  resultat_funktion_4 <- reactiveVal(NULL)     # Gemmer funktionens resultat
  funktion_4_igang <- reactiveVal(FALSE)       # Om funktion er startet
  
  # Klik: Øg step op til 6
  observeEvent(input$knap, {
    if (vis_hvilket() < 6) {
      vis_hvilket(vis_hvilket() + 1)
    }
  })
  
  # Disable knap ved sidste step
  observe({
    if (vis_hvilket() == 6) shinyjs::disable("knap")
  })
  
  # Når vi rammer step 4, start funktion, men kun én gang
  observeEvent(vis_hvilket(), {
    if (vis_hvilket() == 4 && !funktion_4_igang()) {
      funktion_4_igang(TRUE)  # Marker at vi er i gang
      start_tid <- Sys.time()  # Husk hvornår vi startede
      
      # Start future med korrekt seed
      future({
        Sys.sleep(1.5)  # Eksempel på tung funktion
        sum(rnorm(100)) # Resultat
      }, seed = TRUE) %...>%
        # Når resultatet kommer:
        (function(resultat) {
          tid_brugt <- as.numeric(difftime(Sys.time(), start_tid, units = "secs"))
          vent_tid <- max(0, 2 - tid_brugt)  # Hvis mindre end 2 sek, vent resten
          
          # Brug later til at udsætte visning hvis nødvendigt
          promise <- promise(function(resolve, reject) {
            later::later(function() {
              resolve(round(resultat, 2))  # Returner afrundet resultat
            }, delay = vent_tid)
          })
          return(promise)  # Returner som promise
        }) %...>%
        # Når alt er klar, gem som reaktiv værdi
        (function(final_result) {
          resultat_funktion_4(final_result)
        })
    }
  })
  
  # Dynamisk tekst output
  output$output_tekst <- renderUI({
    switch(vis_hvilket(),
           h3("Output 1: Velkommen til app'en!"),
           h3("Output 2: Her kommer et flot plot."),
           h3("Output 3: Her vises en simpel tabel."),
           if (is.null(resultat_funktion_4())) {
             h3("Output 4: Funktion kører, vent venligst...")
           } else {
             h3(paste("Output 4: Funktion kørt! Resultat:", resultat_funktion_4()))
           },
           h3("Output 5: Nu får du et billede at se."),
           h3("Output 6: Det var sidste output. Tak fordi du kiggede med!")
    )
  })
  
  # Plot til step 2
  output$output_plot <- renderPlot({
    if (vis_hvilket() == 2) {
      hist(rnorm(100), col = "skyblue", main = "Et simpelt histogram", xlab = "Tilfældige værdier")
    }
  })
  
  # Tabel til step 3
  output$output_tabel <- renderTable({
    if (vis_hvilket() == 3) {
      head(mtcars)
    }
  })
  
  # Billede til step 5
  output$output_billede <- renderUI({
    if (vis_hvilket() == 5) {
      tags$img(src = "https://www.r-project.org/logo/Rlogo.png", width = "200px")
    }
  })
}
