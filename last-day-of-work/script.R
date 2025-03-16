library(httr)
library(tidyverse)
library(jsonlite)
library(magrittr)
endpoint <- "http://api.statbank.dk/v1/tableinfo"

our_body <- list(lang = "en", table = "AKU410A")
data <- POST(endpoint, body=our_body, encode = "json") %>% 
  content() %>% 
  fromJSON()

data

endpoint <- "http://api.statbank.dk/v1/data"


variables <- list(list(code = "ARBEJDSTID", values = I("022")),
                    list(code = "ALDER", values = I("*")),
                    list(code = "KOEN", values = c("M", "K")),
                    list(code = "Tid", values = I("*"))
  )

our_body <- list(table = "AKU410A", lang = "da", format = "CSV", variables = variables)

data <- POST(endpoint, body=our_body, encode = "json")

data <- data %>% content(type="text") %>% read_csv2()  
data %>% 
  select(-ARBEJDSTID) %>% 
  ggplot(aes(x = TID, y = INDHOLD, colour = ALDER)) +
  geom_line() +
  facet_wrap(~KOEN)


data %>% 
  select(-ARBEJDSTID) %>% 
  pivot_wider(names_from = KOEN, values_from = INDHOLD) %>% 
  mutate(dif = Mænd - Kvinder) %>% 
  ggplot(aes(x=TID, y = dif, colour = ALDER)) +
  geom_line()


# der er håb - for differencen for 25-34 årige
# falder ret voldsomt.

# Men hvornår er vi i mål?

data %>% 
  select(-ARBEJDSTID) %>% 
  pivot_wider(names_from = KOEN, values_from = INDHOLD) %>% 
  mutate(dif = Mænd - Kvinder) %>% 
  filter(ALDER == "Alder i alt") %>% 
  lm(formula = dif~TID, data = .) %>% 
  coef() %>% 
  { -.[["(Intercept)"]] / .[["TID"]] } %>% 
  round()


