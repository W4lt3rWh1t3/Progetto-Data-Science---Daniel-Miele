setwd("C:/Users/danie/OneDrive - Università degli Studi di Udine/uni/primo anno/data science/RStudio_project")

library("dplyr")
library(ggplot2)
library(gganimate)
library(gapminder)
library(tidyr)
library(readr)
library(stringr)
library("scales")
library(ggrepel)

blockbuster = read.csv("movie_metadata.csv", header = TRUE, sep = ",")
blockbuster$genres = sapply(strsplit(blockbuster$genres, "\\|"), function(x) x[1])


principal_table = blockbuster %>%
  filter(genres == "Action" | genres == "Horror" | genres == "Thriller" | genres == "Comedy" | genres == "Drama" | genres == "Animation" | genres == "Adventure" | genres == "Sci-Fi")


sostituzioni = c("Action" = "Azione", "Horror" = "Horror", "Thriller" = "Thriller", "Comedy" = "Commedia", "Drama" = "Drammatico", "Animation" = "Animazione", "Adventure" = "Avventura", "Sci-Fi" = "Fantascienza")


principal_table = principal_table %>%
  mutate(genres = sostituzioni[genres])


ggplot(data = principal_table) +
  geom_boxplot(mapping = aes(x = reorder(genres, imdb_score, FUN = median), y = imdb_score)) +
  labs(title = "Il miglior genere è: Dramattico", caption = "Fonte: Kaggle.com", x = "Generi", y = "Valutazione di IMDb", colour = "Generi")


drama_films = principal_table %>%
  filter(genres == "Drammatico")


drama_directors = principal_table %>%
  filter(genres == "Drammatico") %>%
  filter(director_name == "James Cameron" | director_name == "Quentin Tarantino" | director_name == "Ridley Scott" | director_name == "Christopher Nolan" | director_name == "Steven Spielberg" | director_name == "Martin Scorsese" | director_name == "Clint Eastwood")


ggplot(data = drama_directors) +
  geom_freqpoly(mapping = aes(x = gross, color = director_name)) +
  scale_x_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",", scale = 1e-6)) +
  labs(title = "Il migliore regista di film drammatici è: James Cameron", caption = "Fonte: Kaggle.com", x = "Fatturato in milioni di $", y = "Conto", colour = "Registi")


drama_actor1 = principal_table %>%
  filter(actor_1_name == "Leonardo DiCaprio" | actor_1_name == "Brad Pitt" | actor_1_name == "Tom Hanks" | actor_1_name == "Christian Bale" | actor_1_name == "Johnny Depp" | actor_1_name == "Tom Cruise" | actor_1_name == "Scarlett Johansson" | actor_1_name == "Robert Downey Jr." | actor_1_name == "Kevin Spacey" | actor_1_name == "Emma Stone" | actor_1_name == "Ryan Gosling" | actor_1_name == "Matt Damon")


ggplot(data = drama_actor1, mapping = aes(x = gross, y = imdb_score, color = actor_1_name)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",", scale = 1e-6)) +
  labs(title = "Il miglior attore è: Christian Bale.", caption = "Fonte: Kaggle.com", x = "Fatturato in milioni di $", y = "Valutazione di IMDb", colour = "Attori")


ggplot(data = drama_films, mapping = aes(x = imdb_score, y = duration)) +
  geom_point() +
  geom_smooth() +
  labs(title = "In generale, non è vero che un film è più apprezzato se dura di più.", caption = "Fonte: Kaggle.com", x = "Valutazione di IMDb", y = "Durata in minuti")


ggplot(data = drama_films, mapping = aes(x = budget, y = imdb_score)) +
  geom_point() +
  geom_smooth() +
  labs(title = "...", caption = "Fonte: Kaggle.com", x = "Budget in miliardi di $", y = "Valutazione di IMDb") +
  scale_x_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",", scale = 1e-9))


duration_table = blockbuster %>%
  filter(duration < 60 | duration >= 60 & duration < 90 | duration >= 90 & duration < 120 | duration >= 120 & duration < 150 | duration >= 150 & duration < 180 | duration >= 180 & duration < 200 | duration >= 200)


duration_table <- blockbuster %>%
  mutate(duration_category = case_when(
    duration < 60 ~ "0-59",
    duration >= 60 & duration < 90 ~ "60-89",
    duration >= 90 & duration < 120 ~ "90-119",
    duration >= 120 & duration < 150 ~ "120-149",
    duration >= 150 & duration < 180 ~ "150-179",
    duration >= 180 & duration < 200 ~ "180-199",
    duration >= 200 ~ "200+"
  ))





ggplot(data = blockbuster, mapping = aes(x = gross, y = imdb_score)) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",", scale = 1e-6)) +
  labs(title = "C'è correlazione tra la valutazione di IMDb e il fatturato", caption = "Fonte: Kaggle.com", x = "Fatturato in milioni di $", y = "Valutazione di IMDb")



ggplot(data = blockbuster, mapping = aes(x = gross, y = imdb_score)) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",", scale = 1e-6)) +
  labs(title = "C'è correlazione tra la valutazione di IMDb e il fatturato", caption = "Fonte: Kaggle.com", x = "Fatturato in milioni di $", y = "Valutazione di IMDb")
