# Title     : TODO
# Objective : TODO
# Created by: fabian
# Created on: 15.01.21

library(dplyr)
library(ggplot2)

load("data/Apps.rda")

# rating by app
ratings_by_app <- Apps %>%
  group_by(App) %>%
  summarise(mean(Rating))

# rating by app and version
ratings_by_version <- Apps %>%
  group_by(App, App_Version) %>%
  summarise(mean(Rating))

# rating over time by app
p <- ggplot(Apps, aes(x = Review_date, y = Rating))
p <- p + facet_wrap(App ~ .)
p <- p + geom_line()
p