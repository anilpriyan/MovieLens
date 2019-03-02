#############################################################
# Create edx set, validation set, and submission file
#############################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)

colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(ratings, movies, test_index, temp, movielens, removed)

# RMSE function
RMSE <- function(true_ratings, predicted_ratings) {
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# average rating among all movies
mu <- mean(edx$rating)

# regularization parameter
lamda <- 5

# regularized movie averages
movie_reg_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n() + lamda), n_i = n())

# regularized user averages
user_reg_avgs <- edx %>%
  left_join(movie_reg_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n() + lamda))

# movie genres separated into rows and split by individual genre
movie_data_listed_by_genre <- edx %>% 
  separate_rows(genres, sep = "\\|") %>%
  split(., .$genres)

rm(edx)

# genre effect of movie
genre_classified_movie_reg_avgs <- movie_data_listed_by_genre %>%
  lapply(function(x) {
    genre <- x$genres[1] 
    x %>% 
      left_join(movie_reg_avgs, by = "movieId") %>%
      left_join(user_reg_avgs, by = "userId") %>%
      group_by(movieId) %>% 
      summarize(b_ig = sum(rating - mu - b_i - b_u)/(n() + lamda), g = genre) 
  })

# genre effect of user
genre_classified_user_reg_avgs <- movie_data_listed_by_genre %>%
  lapply(function(x) {
    genre <- x$genres[1] 
    x %>% 
      left_join(movie_reg_avgs, by = "movieId") %>%
      left_join(user_reg_avgs, by = "userId") %>%
      left_join(genre_classified_movie_reg_avgs[[genre]], by = "movieId") %>%
      group_by(userId) %>%
      summarize(b_ug = sum(rating - mu - b_i - b_u - b_ig)/(n() + lamda), g = genre)
  })

# predict rating based on movieId, userId and genre
predicted_rating <- function(movie_id, user_id, genre) {
  # if the user has not rated any movies in this genre, then the genre effect for user is 0
  b_ug <- b_ug <- genre_classified_user_reg_avgs[[genre]] %>% 
    filter(userId == user_id) %>% .$b_ug
  if(length(b_ug) == 0) {
    b_ug <- 0
  }
  mu + b_ug +
    movie_reg_avgs %>% filter(movieId == movie_id) %>% .$b_i +
    user_reg_avgs %>% filter(userId == user_id) %>% .$b_u +
    genre_classified_movie_reg_avgs[[genre]] %>% 
      filter(movieId == movie_id) %>% .$b_ig
}

# prepare data and call function to predict  
predicted_ratings_split_by_genre <- function(userId, movieId, genres) {
  genre <- strsplit(genres, "\\|")[[1]][1]
  predicted_rating(movieId, userId, genre)
}

# apply vectors to predict functions
predicted_model_rating <- mapply(predicted_ratings_split_by_genre, 
                                 validation$userId,
                                 validation$movieId,
                                 validation$genres)

# calculated RMSE
rmse_calculated <- RMSE(validation$rating, predicted_model_rating)
rmse_results <- data_frame(method = "RMSE for Movie Recommendation Algorithm", RMSE = rmse_calculated)
rmse_results %>% knitr::kable()

# visualizations - Movie effect
movie_reg_avgs %>% ggplot(aes(x = b_i)) + geom_histogram() +
  labs(x = "Movie Effect") 

# visualizations - User effect
user_reg_avgs %>% ggplot(aes(x = b_u)) + geom_histogram() +
  labs(x = "User Effect") 

# functions to generate dataframes for visualizations
df <- function(x) {
  data.frame(genre = x[3], effect = x[2]) 
}

dfs <- function(y) {
  z <- data.frame()
  for (i in 1 : length(y)) {
    x <- df(y[[i]])
    z <- rbind(z, x)
  }
  z
}

# visualizations - Movie effect for individual movies within a genre
movie_genre_effs_df <- dfs(genre_classified_movie_reg_avgs)

movie_genre_effs_df %>% ggplot(aes(x = g, y = b_ig)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(x = "Genre", y = "Movie Effect for Genre") 

# visualizations - User effect for individual users within a genre
user_genre_effs_df <- dfs(genre_classified_user_reg_avgs)

user_genre_effs_df %>% ggplot(aes(x = g, y = b_ug)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Genre", y = "User Effect for Genre") 

