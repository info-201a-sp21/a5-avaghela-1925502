# Analysis File

# Load the `dplyr` package
library("dplyr")
library("leaflet")
library("ggplot2")
library("plotly")
library("lintr")
library("tidyverse")

# Load the dataset
shootings_dataset <- read.csv("/Users/Arjun/Desktop/Info 201/a5-avaghela-1925502/data/shootings-2018.csv")


# Total Shootings
total_shootings <- nrow(shootings_dataset)

# Total Lives Lost
total_lives_lost <- sum(shootings_dataset$num_killed)

# City Impacted by shootings
most_shooting_occurrences <- shootings_dataset %>%
  mutate(count = 1) %>%
    group_by(city) %>%
      summarise(count = sum(count, na.rm = T)) %>%
        filter(count == max(count, na.rm = T)) %>%
          pull(city)

# Chicago injured
chicago_injured <- shootings_dataset %>%
  filter(city == "Chicago") %>%
    select(num_injured) %>%
      summarise(total_hurt = sum(num_injured))

# Chicago killed
chicago_killed <- shootings_dataset %>%
  filter(city == "Chicago") %>%
    select(num_killed) %>%
      summarise(total_hurt = sum(num_killed))

# Summary Table
summary_table <- shootings_dataset %>%
    select(2, 5, 6) %>%
      group_by(state) %>%
        summarise(Deaths = sum(num_killed),
                  Injured = sum(num_injured),
                  Total = (sum(num_killed) + sum(num_injured)))

# Description of a Particular Incident
parkland_date <- shootings_dataset[312, 1]

parkland_killed <- shootings_dataset[312, 5]

parkland_injured <- shootings_dataset[312, 6]

parkland_lat <- round(shootings_dataset[312, 7], digits = 4)

parkland_long <- shootings_dataset[312, 8]

parkland_city <- shootings_dataset[312, 3]

parkland_state <- shootings_dataset[312, 2]

# Interactive Map
interactive_map <- leaflet(data = shootings_dataset) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircles(
    lat = ~lat,
    lng = ~long,
    weight = ~ (num_killed + num_injured),
    fillColor = "#ff0000",
    color = "#ff0000",
    stroke = TRUE,
    popup = ~paste0(date, "<br/>", city, ", ", state, "<br/>", address, "<br/>",
                    num_killed, " Killed <br/>", num_injured, " Injured")
  )
  
# States with Highest Shootings (Bar Graph)
total_filtered <- summary_table[order(-summary_table$Deaths), ]
top_6 <- total_filtered[1:6, ]

chart <- ggplot(data = top_6, aes(x = reorder(state, -Deaths), y = Deaths,
                                  fill = state)) +
  geom_bar(
    stat = "identity",
    width = 0.5,
  ) +
  theme_minimal() +
  ggtitle("Top 6 States with Highest Deaths Related to Shootings") +
  xlab("States") +
  labs(fill = "States") +
  theme(legend.position = "none") +
  geom_text(aes(label = Deaths, vjust = -0.25))


lint("analysis.R")

