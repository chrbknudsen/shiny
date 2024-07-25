library(glue)
library(tidyverse)
library(tibble)
library(rvest)
#datoen er i præcist det format der angives i resultatet.
url <- "https://www.dbu.dk/landshold/landsholdsdatabasen/MatchSearchResult?DateFrom="
library(lubridate)


# returnerer tabel med kampdata fra url
hent_tabel <- function(url){rvest::read_html(url) %>% 
    rvest::html_table() %>% 
    unlist(recursive = FALSE) %>% 
    as_tibble() %>% 
    mutate(date = as_date(Dato, format = "%d-%m-%Y"))
}

# Henter seneste kampdato
seneste_kamp_dato <- function(df){
  df %>% 
    slice_max(date) %>% 
    pull(date) %>% 
    unique()
}

# Fjerner evt dubblerede kampe - for det tilfælde at der er 
# kampe hvor data ikke er opdateret endnu.
saniter_data <- function(df){
  df %>% 
    group_by(across(-Tilskuere)) %>% 
    slice_max(Tilskuere) %>% 
    slice(1) %>% 
    ungroup() %>% 
    filter(date < today())
}

fix_dato <- function(d){
  if(day(d)>12){
    day(d) <- 12
  }
  d
}

# returnerer den næste url baseret på sidste dato i dataframen. Med et 
# optional offset på et antal dage.
next_url <- function(df, offset =0){
  df %>% 
    slice_max(date) %>% 
    transmute(date = date + days(offset)) %>% 
    distinct() %>% 
    transmute(date = fix_dato(date)) %>% 
    transmute(Dato = format(date, "%d-%m-%Y")) %>% 
    transmute(next_url = glue(url, Dato)) %>% 
    pull(next_url)
}


# Hvis filen ikke eksisterer allerede - så henter vi 
# de første halvtres resultater og initialiserer filen.
if(!file.exists("audience/kampdata.csv")){
  hent_tabel(url) %>% 
    write_csv("audience/kampdata.csv")
}
# Indlæs data
df <- read_csv("audience/kampdata.csv")
# Opdaterer data, og gemmer ny udgave.
if(seneste_kamp_dato(df)<=today()){
  ny_df <- hent_tabel(next_url(df, -10)) # går ti dage tilbage for at sikre dækning.
  df <- df %>% rbind(ny_df)
    
  df <- saniter_data(df)   # saniterer data.
  write_csv(df, "audience/kampdata.csv")
}

df %>% 
  mutate(team = glue("{Hold} - {Køn}")) %>% 
  ggplot(aes(x = date, y = Tilskuere, color = team)) +
  geom_smooth()

range(df$date)

# vi forventer ca. 4861 rækker
