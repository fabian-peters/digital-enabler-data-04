# Title     : TODO
# Objective : TODO
# Created by: fabian
# Created on: 15.01.21

library(dplyr)
library(ggplot2)

load("data/Apps.rda")
Apps$Rating_factor <- as.factor(Apps$Rating)
Apps$Timeofday_hour_factor <- as.factor(Apps$Timeofday_hour)

# rating by app
ratings_by_app <- Apps %>%
  group_by(App) %>%
  summarise(mean(Rating))

p <- ggplot(Apps, aes(x = Rating_factor))
p <- p + facet_wrap(App ~ .)
p <- p + geom_histogram(stat = "count")
p <- p + xlab("Rating")
p

# rating by app and version
ratings_by_version <- Apps %>%
  group_by(App, App_Version) %>%
  summarise(mean(Rating))

# rating over time by app
p <- ggplot(Apps, aes(x = Review_date, y = Rating))
p <- p + facet_wrap(App ~ .)
p <- p + geom_smooth()
p

# time needed to receive 490 reviews
time_needed_for_reviews <- Apps %>%
  group_by(App) %>%
  summarise(max(Review_minute) - min(Review_minute))

# authors with multiple reviews
authors_with_multiple_review <- Apps %>%
  group_by(Author_Name) %>%
  count() %>%
  filter(n > 1)

# number of reviews per time of day
reviews_by_time_of_day <- Apps %>%
group_by(Timeofday_hour) %>%
count()

p <- ggplot(Apps, aes(x = Timeofday_hour))
p <- p + geom_histogram(bins = 24)
p

p <- p + facet_wrap(App ~ .)
p

p <- ggplot(reviews_by_time_of_day, aes(x = Timeofday_hour, y = n))
p <- p + geom_line()
p

# sentiment per time of day
sentiment_by_time_of_day <- Apps %>%
  group_by(Timeofday_hour) %>%
  summarise(mean(Sentiment_nrc), min(Sentiment_nrc), max(Sentiment_nrc))

sentiment_by_time_of_day$Timeofday_hour <- as.factor(sentiment_by_time_of_day$Timeofday_hour)
p <- ggplot(sentiment_by_time_of_day, aes(x = Timeofday_hour, y = `mean(Sentiment_nrc)`, ymin = `min(Sentiment_nrc)`, ymax = `max(Sentiment_nrc)`))
p <- p + xlab("Time of Day")
p <- p + geom_pointrange()
p

p <- ggplot(Apps, aes(x = Timeofday_hour_factor, y = Sentiment_nrc))
p <- p + geom_boxplot()
p <- p + xlab("Time of Day")
p


p <- ggplot(Apps, aes(x = Timeofday_hour, y = Rating))
p <- p + geom_smooth()
p <- p + xlab("Time of Day")
p <- p + facet_wrap(App ~ .)
p


