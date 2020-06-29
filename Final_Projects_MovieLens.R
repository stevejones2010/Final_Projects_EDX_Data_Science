################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
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

rm(dl, ratings, movies, test_index, temp, movielens, removed)


###############################
#Dataset
###############################



# edx class
class(edx)

# Display head of edx
head(edx)

# Number of rows in data
nrow(edx)

# Number of columns in data
ncol(edx)

# Number of unique users
length(unique(edx$userId))

# Number of films in dataset
length(unique(edx$movieId))

# Average rating 
mu <- mean(edx$rating)


##############################
#Method
##############################

### Pre-processing data ###

edx <- edx %>% mutate(year_rated = year(as_datetime(timestamp))) 
head(edx)
validation <- validation %>% mutate(year_rated = year(as_datetime(timestamp)))

# Split data into train and test set
test_index <- createDataPartition(y = edx$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

# Ensure test set and train set have same movies 
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")




### Data exploration and visualisations ###

# Check most rated films 
train_set %>% 
  group_by(title) %>% 
  summarize(n_ratings = n()) %>% 
  arrange(desc(n_ratings))

# Highest rated films
train_set %>%
  group_by(title) %>%
  summarize(avg_rating = mean(rating),n_ratings = n()) %>%
  arrange(desc(avg_rating))

# Lowest rates films
train_set %>%
  group_by(title) %>%
  summarize(avg_rating = mean(rating),n_ratings = n()) %>%
  arrange(avg_rating)

# Visualise how many ratings the movies in our dataset have received
train_set %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 10, color = "black") + 
  scale_x_log10() + 
  ggtitle("No. of ratings films receive")

# Visualised how many movies our users rate on average
train_set %>% 
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 10, color = "black") +
  scale_x_log10() + 
    ggtitle("Average number of movies rated")

# Ratings by year

train_set %>% 
  group_by(year_rated) %>%
  summarize(Average = mean(rating)) %>%
  ggplot(aes(x = year_rated, y = Average, color = year_rated)) +
  geom_point() +
  theme(legend.position="none")+
  ggtitle("Year each of movies rated")

############################
# Results
############################

# RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Naive Bayes 

# Train set average
mu <- mean(train_set$rating)

# Checking RMSE against average
Pred_avg <- RMSE(test_set$rating, mu)

# Creating RMSE results table
rmse_results_table <- data.frame(Methods = "Naive Bayes Average", RMSE = Pred_avg)
rmse_results_table

# Movie effect

movie_effect <- train_set %>%
  group_by(movieId) %>% 
  summarize(b_m = (mean(rating - mu)))

# Creating an object including Movie effect to apply RMSE too Validation set
movie_pred <- left_join(test_set,movie_effect, by = 'movieId') %>%
  mutate(b_m = b_m + mu) %>% .$b_m

# Checking RMSE including Movie effect
Pred_mov <- RMSE(test_set$rating,movie_pred)

# New RMSE
rmse_results_table <- bind_rows(rmse_results_table, data.frame(Methods = "Adding Movie effect", RMSE = Pred_mov))
rmse_results_table

# Movie and user effect
movie_user_effect <- train_set %>%
  left_join(movie_effect,by = 'movieId') %>%
  group_by(userId) %>% 
  summarize(b_u = (mean(rating - mu - b_m)))

# Creating an object including Movie effect to apply RMSE too Validation set
movie_user_pred <- test_set %>%
  left_join(movie_effect, by = 'movieId') %>%
  left_join(movie_user_effect, by = 'userId') %>%
  mutate(b_u = b_u + b_m + mu) %>% 
  .$b_u

# Checking RMSE including Movie effect
Pred_mov_user <- RMSE(test_set$rating,movie_user_pred)

# New RMSE
rmse_results_table <- bind_rows(rmse_results_table, data.frame(Methods = "Adding Movie + user effect", RMSE = Pred_mov_user))
rmse_results_table

# Year effect
movie_user_year_effect <- train_set %>%
  left_join(movie_effect, by= 'movieId') %>% 
  left_join(movie_user_effect, by = 'userId') %>%
  group_by(year_rated) %>%
  summarize(b_y = mean(rating - mu - b_m -b_u))

# Predicting ratings
movie_user_year_pred <- test_set %>%
  left_join(movie_effect, by = "movieId") %>%
  left_join(movie_user_effect, by = "userId") %>%
  left_join(movie_user_year_effect, by = 'year_rated') %>%
  mutate(pred = mu + b_m + b_u + b_y) %>%
  .$pred

# Calculate RMSE
Pred_mov_user_year <- RMSE(test_set$rating,movie_user_year_pred)

# Update table
rmse_results_table <- bind_rows(rmse_results_table, data.frame(Methods = "Adding Movie + user + year effect", RMSE = Pred_mov_user_year))
rmse_results_table


## Regularization

lambda <- seq(0, 10, 0.25)
rmses <- sapply(lambda, function(l){
  b_m <- train_set %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_m, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_m - mu)/(n()+l))
  b_y <- train_set %>%
    left_join(b_m, by= 'movieId') %>% 
    left_join(b_u, by = 'userId') %>%
    group_by(year_rated) %>%
    summarize(b_y = mean(rating - mu - b_m -b_u))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_m, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_y, by = 'year_rated') %>%
    mutate(pred = mu + b_m + b_u + b_y) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambda, rmses)  

low_lambda <- lambda[which.min(rmses)]
low_lambda

####
#final test set results
####
# Apply rgularized movie effect
b_m <- train_set %>%
  group_by(movieId) %>%
  summarize(b_m = sum(rating - mu)/(n()+4.75))
# Apply regularized user effect
b_u <- train_set %>% 
  left_join(b_m, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_m - mu)/(n()+4.75))
# Apply year effect
b_y <- train_set %>%
  left_join(b_m, by= 'movieId') %>% 
  left_join(b_u, by = 'userId') %>%
  group_by(year_rated) %>%
  summarize(b_y = mean(rating - mu - b_m -b_u))
# Calculating predictions on test set
pred_final_test_ratings <- 
  test_set %>% 
  left_join(b_m, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_y, by = 'year_rated') %>%
  mutate(pred = mu + b_m + b_u + b_y) %>%
  .$pred

#Calculate RMSE
final_test_rmse <- RMSE(test_set$rating, pred_final_test_ratings)

# Update table
rmse_results_table <- bind_rows(rmse_results_table, data.frame(Methods = "Adding Movie + user + year + regularization effect", RMSE = final_test_rmse))
rmse_results_table

####
#final results
####

# Apply rgularized movie effect
b_m <- edx %>%
  group_by(movieId) %>%
  summarize(b_m = sum(rating - mu)/(n()+4.75))

# Apply regularized user effect
b_u <- edx %>% 
  left_join(b_m, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_m - mu)/(n()+4.75))

# Apply year effect
b_y <- edx %>%
  left_join(b_m, by= 'movieId') %>% 
  left_join(b_u, by = 'userId') %>%
  group_by(year_rated) %>%
  summarize(b_y = mean(rating - mu - b_m -b_u))
  

# Calculating predictions on validation set
pred_final_ratings <- 
  validation %>% 
  left_join(b_m, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_y, by = 'year_rated') %>%
  mutate(pred = mu + b_m + b_u + b_y) %>%
  .$pred

# Calculate RMSE
final_rmse <- RMSE(validation$rating, pred_final_ratings)

# Update table
rmse_results_table <- bind_rows(rmse_results_table, data.frame(Methods = "Final RMSE", RMSE = final_rmse))
rmse_results_table
