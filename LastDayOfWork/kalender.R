library(ggplot2)
library(lubridate)

# Parametre
special_date <- as.Date("2022-05-15")  # Datoen, hvor der skal sættes et rødt kryds

year <- year(special_date)
month <- month(special_date)

# Bestem første og sidste dag i måneden
first_date <- as.Date(paste(year, month, "1", sep = "-"))
last_date <- ceiling_date(first_date, "month") - days(1)

# Opret en data frame med alle datoer i måneden
dates <- seq(first_date, last_date, by = "day")
calendar_df <- data.frame(date = dates)
calendar_df$day <- day(calendar_df$date)
calendar_df$wday <- wday(calendar_df$date, week_start = 1)  # Mandag = 1

# Beregn hvilken uge i måneden dagen tilhører
first_wday <- wday(first_date, week_start = 1)
calendar_df$week <- ceiling((calendar_df$day + first_wday - 1) / 7)

# Plot kalenderen
p <- ggplot(calendar_df, aes(x = wday, y = -week)) + 
  geom_tile(fill = "white", color = "black") +
  geom_text(aes(label = day), size = 5) +
  scale_x_continuous(breaks = 1:7, labels = c("Man", "Tir", "Ons", "Tor", "Fre", "Lør", "Søn")) +
  theme_minimal() +
  labs(title = paste(month.name[month], year)) +
  theme(axis.title = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank())

# Hvis special_date er i den viste måned, tilføj et rødt kryds
if (special_date >= first_date && special_date <= last_date) {
  special_day <- day(special_date)
  special_wday <- wday(special_date, week_start = 1)
  special_week <- ceiling((special_day + first_wday - 1) / 7)
  
  p <- p + annotate("text", 
                    x = special_wday, 
                    y = -special_week, 
                    label = "×", 
                    color = "red", 
                    size = 10, 
                      fontface = "bold")
}

p
