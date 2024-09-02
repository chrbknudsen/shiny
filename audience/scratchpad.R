source("audience/funcs.R")



df <- update_og_hent()

df

df %>% 
  filter(Hold == "A-Landshold") %>% 
  mutate(team = glue("{Hold} - {Køn}")) %>% 
  ggplot(aes(x = date, y = Tilskuere, color = team)) +
  geom_ma(n=100)


df %>% 
  filter(Hold == "A-Landshold") %>% 
  mutate(team = glue("{Hold} - {Køn}")) %>% 
  ggplot(aes(x = Tilskuere, color = team)) +
  geom_boxplot()

geom_ma
library(tidyverse)
install.packages("tidyquant")
library(tidyquant)
