# Title     : TODO
# Objective : TODO
# Created by: fabian
# Created on: 15.01.21

library(dplyr)
library(ggplot2)
library(reshape2)
library(psych)
library(tidyr)
library (stringr)
library(tm)
library(wordcloud)
library(RColorBrewer)

load("data/Apps.rda")
Apps$Rating_factor <- as.factor(Apps$Rating)
Apps$Timeofday_hour_factor <- as.factor(Apps$Timeofday_hour)

# verify data set has the correct size (490 reviews per app)
review_count_per_app <- Apps %>%
  group_by(App) %>%
  count()

# rating by app
ratings_by_app <- Apps %>%
  group_by(App) %>%
  summarise(mean(Rating))

p <- ggplot(Apps, aes(x = Rating_factor, fill=App))
p <- p + facet_wrap(App ~ .)
p <- p + geom_histogram(stat = "count", fill = 'seagreen', color = 'red') + theme_minimal()
p <- p + xlab("Rating")
p

#neu mit Farbe
p <- ggplot(Apps, aes(x = Rating_factor,fill = cut(val, 100)))
p <- p + facet_wrap(App ~ .)
p <- p + geom_histogram(stat = "count", aes(fill=..count..)) + theme_minimal() +scale_fill_gradient("Count", low="green", high="red")
p <- p + xlab("Rating")
p

#neu mit Farbe und Rating
p <- ggplot(Apps, aes(x = Rating_factor))
p <- p + facet_wrap(App ~ .)
p <- p + geom_histogram(stat = "count", aes(fill=Rating)) + theme_minimal() +scale_fill_gradient("Rating", low="red", high="green")
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

# reviews per day
review_count_per_app_and_day <- Apps %>%
  group_by(App, Review_date) %>%
  count()

mean_review_count_per_app_and_day <- review_count_per_app_and_day %>%
  group_by(App) %>%
  summarise(mean(n))

# number of review over time
number_reviews_over_time <- Apps %>%
  group_by(App, Review_date) %>%
  count()

p <- ggplot(number_reviews_over_time, aes(x = Review_date, y = n))
p <- p + facet_wrap(App ~ .)
p <- p + geom_line()
p <- p + xlab("Date")
p

p <- ggplot(number_reviews_over_time, aes(x = Review_date, y = n))
p <- p + facet_wrap(App ~ .)
p <- p + geom_line()
p <- p + geom_smooth()
p <- p + xlab("Date")
p

# stacked area for magenta ratings
magenta_stack_data <- Apps %>%
  filter(App == "MeinMagenta") %>%
  group_by(Review_date, Rating_factor) %>%
  count()

p <- ggplot(magenta_stack_data, aes(x = Review_date, y = n, fill = Rating_factor))
p <- p + geom_area(position = "stack")
p <- p + xlab("Date")
p

# authors with multiple reviews
authors_with_multiple_review <- Apps %>%
  group_by(Author_Name) %>%
  count() %>%
  filter(n > 1)

# number of reviews per time of day
reviews_by_time_of_day <- Apps %>%
group_by(Timeofday_hour) %>%
count()

p <- ggplot(Apps, aes(x = Timeofday_hour_factor))
p <- p + geom_histogram(stat = "count")
p <- p + theme_minimal()
p <- p + xlab("Time of Day")
p

p <- ggplot(Apps, aes(x = Timeofday_hour))
p <- p + geom_histogram(bins = 24,color = 'black',fill = 'white')
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

# sentiment per app
sentiment_by_app <- Apps %>%
  group_by(App) %>%
  summarise(mean(Sentiment_nrc), min(Sentiment_nrc), max(Sentiment_nrc))

#wordcloud filter
MeinMagenta<- Apps %>%
  filter(App=='MeinMagenta')

MeinVodafone<- Apps %>%
  filter(App=='MeinVodafone')

MeinO2<- Apps %>%
  filter(App=='MeinO2')

#wordcloud 
corpus <- Corpus(VectorSource(MeinO2$Review))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus,content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, c ("app", "funktioniert", "mehr", "seit", "immer", "gut", "update", "vodafone"))
corpus <- tm_map(corpus, removeWords, stopwords ("german"))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
#corpus <- tm_map(corpus,stemDocument)
#Kreiere Term-Document Matrix
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
df <- data.frame(word = names(v),freq=v)
#Kreiere Wordcloud
set.seed(1001)
wordcloud(words = df$word, freq = df$freq, min.freq = 2,
          max.words=500, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(n=8,name="Spectral"))