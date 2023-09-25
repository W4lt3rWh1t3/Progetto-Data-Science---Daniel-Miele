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
blockbuster = na.omit(blockbuster)


principal_table = blockbuster %>%
  filter(genres == "Action" | genres == "Horror" | genres == "Thriller" | genres == "Comedy" | genres == "Drama" | genres == "Animation" | genres == "Adventure" | genres == "Sci-Fi")


sostituzioni = c("Action" = "Azione", "Horror" = "Horror", "Thriller" = "Thriller", "Comedy" = "Commedia", "Drama" = "Drammatico", "Animation" = "Animazione", "Adventure" = "Avventura", "Sci-Fi" = "Fantascienza")


principal_table = principal_table %>%
  mutate(genres = sostituzioni[genres])


drama_films = principal_table %>%
  filter(genres == "Drammatico")


drama_directors = blockbuster %>%
  filter(director_name == "Quentin Tarantino" | director_name == "Ridley Scott" | director_name == "Christopher Nolan" | director_name == "Michael Bay" | director_name == "Tim Burton")


drama_actor1 = principal_table %>%
  filter(actor_1_name == "Leonardo DiCaprio" | actor_1_name == "Christian Bale" | actor_1_name == "Tom Hanks" | actor_1_name == "Scarlett Johansson" | actor_1_name == "Robert Downey Jr." | actor_1_name == "Matt Damon")


duration_table = blockbuster %>%
  mutate(duration_category = case_when(
    duration > 90 & duration <= 120 ~ "90-120",
    duration > 120 & duration <= 150 ~ "120-150",
    duration > 150 & duration <= 180 ~ "150-180",
    duration > 180 ~ "180+"
  ))


duration_table = duration_table %>%
  filter(duration_category == "90-120" | duration_category == "120-150" | duration_category == "150-180" | duration_category == "180+")


order_of_categories <- c("90-120", "120-150", "150-180", "180+")
duration_table$duration_category <- factor(duration_table$duration_category, levels = order_of_categories)


ggplot(data = principal_table) +
  geom_boxplot(mapping = aes(x = reorder(genres, imdb_score, FUN = median), y = imdb_score)) +
  labs(title = "Il miglior genere è: Dramattico", caption = "Fonte: Kaggle.com", x = "Generi", y = "Valutazione di IMDb", colour = "Generi")


ggplot(data = drama_directors, aes(x = gross, y = imdb_score)) +
  geom_point(aes(shape = director_name), alpha = 0.5, size = 4) +
  labs(x = "Fatturato in milioni di $",
       y = "Valutazione di IMDb",
       shape = "Regista") +
  scale_x_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",", scale = 1e-6)) +
  theme_minimal()


mean_directors = drama_directors %>%
  group_by(director_name) %>%
  summarize(mean_imdbscore = mean(imdb_score),
            mean_gross = mean(gross))


colnames(mean_directors)[colnames(mean_directors) == "director_name"] <- "Registi"


ggplot(mean_directors, aes(x = mean_gross, y = mean_imdbscore)) +
  geom_point(aes(shape = Registi, color = Registi), size = 5) +
  theme_minimal() +
  labs(title = "Il regista migliore è: Christopher Nolan.", x = "Fatturato in milioni di $", y = "Valutazione di IMDb") +
  scale_x_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",", scale = 1e-6))


ggplot(data = drama_actor1, aes(x = gross, y = imdb_score)) +
  geom_point(aes(shape = actor_1_name), alpha = 0.5, size = 4) +
  labs(x = "Fatturato in milioni di $",
       y = "Valutazione di IMDb",
       shape = "Attori") +
  scale_x_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",", scale = 1e-6)) +
  theme_minimal()


mean_actors = drama_actor1 %>%
  group_by(actor_1_name) %>%
  summarize(mean_imdbscore = mean(imdb_score),
            mean_gross = mean(gross))

  
colnames(mean_actors)[colnames(mean_actors) == "actor_1_name"] <- "Attori"


ggplot(mean_actors, aes(x = mean_gross, y = mean_imdbscore)) +
  geom_point(aes(shape = Attori, color = Attori), size = 5) +
  theme_minimal() +
  labs(title = "L'attore migliore è: Tom Hanks.", x = "Fatturato in milioni di $", y = "Valutazione di IMDb") +
  scale_x_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",", scale = 1e-6))


ggplot(data = duration_table) +
  geom_boxplot(mapping = aes(x = reorder(duration_category, imdb_score, FUN = median), y = imdb_score)) +
  labs(title = "In generale, un film di successo deve durare almeno 180 minuti.", caption = "Fonte: Kaggle.com", x = "Intervallo di durata in minuti", y = "Valutazione di IMDb")


