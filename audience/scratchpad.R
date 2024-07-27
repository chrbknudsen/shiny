source("audience/funcs.R")



df <- update_og_hent()



df %>% 
  mutate(team = glue("{Hold} - {Køn}")) %>% 
  ggplot(aes(x = date, y = Tilskuere, color = team)) +
  geom_smooth()



