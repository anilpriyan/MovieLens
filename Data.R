#############################################################
# Create edx set, validation set, and submission file
#############################################################

# Note: this process could take a couple of minutes

# if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
# if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
# 
# # MovieLens 10M dataset:
# # https://grouplens.org/datasets/movielens/10m/
# # http://files.grouplens.org/datasets/movielens/ml-10m.zip
# 
# dl <- tempfile()
# download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
# 

library(tidyverse)
library(caret)

# ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
#                        col.names = c("userId", "movieId", "rating", "timestamp"))
# 
# 
# 
# movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)

ratings <- read.table(text = gsub("::", "\t", readLines("ml-10M100K/ratings.dat")),
                       col.names = c("userId", "movieId", "rating", "timestamp"))



movies <- str_split_fixed(readLines("ml-10M100K/movies.dat"), "\\::", 3)


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

# Learners will develop their algorithms on the edx set
# For grading, learners will run algorithm on validation set to generate ratings
validation <- validation %>% select(-rating)

# Ratings will go into the CSV submission file below:
write.csv(validation %>% select(userId, movieId) %>% mutate(rating = NA),
          "submission.csv", na = "", row.names=FALSE)
rm(dl, ratings, movies, test_index, temp, movielens, removed)

# dim(edx)
# edx %>% filter(rating == 0) %>% tally()
# edx %>% filter(rating == 3) %>% tally()
# n_distinct(edx$movieId)
# n_distinct(edx$userId)
# 
# sum(sapply(edx$genres, function(x) {"Comedy" %in% unlist(strsplit(x, "[|]"))}))
# sum(sapply(edx$genres, function(x) {"Drama" %in% unlist(strsplit(x, "[|]"))}))
# sum(sapply(edx$genres, function(x) {"Thriller" %in% unlist(strsplit(x, "[|]"))}))
# sum(sapply(edx$genres, function(x) {"Romance" %in% unlist(strsplit(x, "[|]"))}))
# 
# edx %>% separate_rows(genres, sep = "\\|") %>%
#   summarize(count = n()) %>%
#   arrange(desc(count))
# 
# edx %>% group_by(movieId, title) %>%
#   summarize(count = n()) %>%
#   top_n(3) %>%
#   arrange(desc(count))
# 
# edx %>% group_by(rating) %>%
#   summarize(count = n()) %>%
#   ggplot(aes(x = rating, y = count)) +
#   geom_line()

# medx <- edx[1:10,] %>% spread(movieId, rating)

RMSE <- function(true_ratings, predicted_ratings) {
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

set.seed(755)
test_index <- createDataPartition(y = edx$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

test_set <- test_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# mu <- mean(train_set$rating)
# 
# naive_rmse <- RMSE(test_set$rating, mu)
# 
# rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)
# 
# movie_avgs <- train_set %>%
#   group_by(movieId) %>%
#   summarize(b_i = mean(rating - mu))
# 
# predicted_ratings <- mu + test_set %>%
#   left_join(movie_avgs, by = "movieId") %>%
#   .$b_i
# 
# model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
# 
# 
# 
# rmse_results <- bind_rows(rmse_results,
#                           data_frame(method = "Movie Effect Model",
#                                      RMSE = model_1_rmse))
# 
# user_avgs <- train_set %>%
#   left_join(movie_avgs, by = "movieId") %>%
#   group_by(userId) %>%
#   summarize(b_u = mean(rating - mu - b_i))
# 
# predicted_ratings_ui <- test_set %>%
#   left_join(movie_avgs, by = "movieId") %>%
#   left_join(user_avgs, by = "userId") %>%
#   mutate(pred = mu + b_i + b_u) %>%
#   .$pred
# 
# model_2_rmse <- RMSE(predicted_ratings_ui, test_set$rating)
# 
# rmse_results <- bind_rows(rmse_results,
#                           data_frame(method = "Movie + User Effects Model",
#                                      RMSE = model_2_rmse))

# rmse_results %>% knitr::kable()

dim(train_set)

train_small <- train_set %>% 
  group_by(movieId) %>%
  filter(n() > 100 | movieId ==3252) %>% 
  ungroup %>% 
  group_by(userId) %>%
  filter(n() >
           100) %>%
  ungroup()

dim(train_small)

rm(validation)
rm(test_index)
rm(edx)
rm(train_set)
rm(test_set)



y <- train_small %>%
  select(userId, movieId, rating) %>%
  spread(movieId, rating, fill = 0.0) %>%
  as.matrix()

rownames(y) <- y[,1]

y <- y[,-1]

colnames(y) <- with(train_small, title[match(colnames(y), movieId)])

y <- sweep(y, 1, rowMeans(y))
y <- sweep(y, 2, colMeans(y))

# y <- sweep(y, 1, rowMeans(y, na.rm = TRUE))
# y <- sweep(y, 2, colMeans(y, na.rm = TRUE))
# 
# 
# 
# y[is.na(y)] <- 0

rm(train_small)

dim(y)

# nzv <- nearZeroVar(y)

# y <- setdiff(1:ncol(y), nzv)

y <- y[1:100, 1:25]

dim(y)

my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)

pca <- prcomp(y)

rm(y)

plot(pca$sdev)

var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained)

my_image(pca$x[,1,drop=FALSE] %*% t(pca$rotation[,1,drop=FALSE]))