# Title     : TODO
# Objective : TODO
# Created by: fabian
# Created on: 15.01.21

library(dplyr)

load("data/Apps.rda")

ratings_by_app <- Apps %>%
  group_by(App) %>%
  summarise(mean(Rating))

ratings_by_version <- Apps %>%
  group_by(App, App_Version) %>%
  summarise(mean(Rating))