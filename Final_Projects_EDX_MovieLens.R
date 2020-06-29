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

# Looking at data, checking it's in tidy format

class(edx)

table(edx)

head(edx)

# Sampling smaller dataset from edx to test processing speed and time

process_test <- edx[sample(nrow(edx),1000),]

# Analysing datasets

# Number of rows in data

nrow(edx)

# Number of columns in data

ncol(edx)

# Number of unique users

length(unique(edx$userId))

# Number of films in dataset

length(unique(edx$movieId))

# Counting the number of films in the following categories

count(str_count(edx$genres,"Drama"))
count(str_count(edx$genres,"Comedy"))
count(str_count(edx$genres,"Thriller"))
count(str_count(edx$genres,"Romance"))

# Most rated films

most_rated <- edx %>% 
  group_by(title) %>% 
  summarize(n_ratings = n()) %>% 
  arrange(desc(n_ratings))

# Highest rated films



# Which rating is most often given to film? 

most_rates <- edx %>% 
  group_by(rating) %>% 
  summarize(n_rates = n()) %>% 
  arrange(desc(n_rates))

# Visualise how many ratings the movies in our dataset have received

edx %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")

# Visualised how many movies our users rate on average

edx %>% 
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Average number of movies rated")

# Boxplot movie ratings by Genre

#######

# Predicting ratings

# Average rating 

Mean_avg_rating <- mean(edx$rating)


# Defining RMSE function

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Checking RMSE against average

Pred_avg <- RMSE(validation$rating,Mean_avg_rating)

# Creating RMSE results table

rmse_results_table <- data.frame(Methods = "Naive Bayes Average", RMSE = Predicting_average)

# Some movies are just rated higher on average than others. So we're going to add
# the variable to the average rating for each movie to see if it's possible to 
# improve

# Calculating the Movie effect from edx ratings

Lambda <- seq(0,10,0.25)

Avg_movie_rating <- edx %>%
  group_by(movieId) %>% 
  summarize(reg = (mean(rating - Mean_avg_rating, n_i = n())))

# Creating an object including Movie effect to apply RMSE too Validation set

rmses <- sapply(Lambda, function(l){
  Avg_movie_rating_pred <- left_join(validation,Avg_movie_rating, by = 'movieId') %>%
    mutate(b_i = (reg/(n()+l)) + Mean_avg_rating) %>% .$b_i
  return(RMSE(validation$rating,Avg_movie_rating_pred))
})

# Plot Lambda 

qplot(Lambda,rmses)

# Checking which Lambda figure works best

Lambda[which.min(rmses)]

#### Explained by the fact all movies have been rated signifcant number of times

# Using 0 Lambda

Avg_movie_rating_pred <- left_join(validation,Avg_movie_rating, by = 'movieId') %>%
  mutate(b_i = reg + Mean_avg_rating) %>% .$b_i

# Checking RMSE including Movie effect

Pred_avg_mov_rat <- RMSE(validation$rating,Avg_movie_rating_pred)

# New RMSE

rmse_results_table <- bind_rows(rmse_results_table, data.frame(Methods = "Adding Movie effect", RMSE = Pred_avg_mov_rat))

# User effect

Avg_movie_user_rating <- edx %>%
  left_join(Avg_movie_rating, by= 'movieId') %>% 
  group_by(userId) %>%
  summarize(b_u = mean(rating - Mean_avg_rating-b_i,n_i = n()))

# Predicting ratings

rmses <- sapply(Lambda, function(l){
  Avg_movie_user_rating_pred <- validation %>% 
    left_join(Avg_movie_rating, by = 'movieId') %>%
    left_join(Avg_movie_user_rating, by = 'userId') %>%
    mutate(pred = Mean_avg_rating + b_i + (b_u/n()+l)) %>% .$pred
  RMSE(validation$rating,Avg_movie_user_rating_pred)
})

qplot(Lambda,rmses)

# Checking which Lambda figure works best

Lambda[which.min(rmses)]


########### course code####

rmses <- sapply(Lambda, function(l){
  mu <- mean(edx$rating)
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings,validation$rating))
})

rmses

####coursecode###above

# Calculate RMSE

Pred_avg_mov_user_rat <- RMSE(validation$rating,Avg_movie_user_rating_pred)

# Update table

rmse_results_table <- bind_rows(rmse_results_table, data.frame(Methods = "Adding Movie + user effect", RMSE = Pred_avg_mov_user_rat))

#### Adding year column - Deleting as it's going into year effect directly ####

edx <- edx %>% 
  mutate(year = year(as.Date(as.POSIXct(timestamp, origin="1970-01-01"))))

# Year effect

Avg_movie_user_year_rating <- edx %>%
  mutate(year = year(as.Date(as.POSIXct(timestamp, origin="1970-01-01")))) %>%
  left_join(Avg_movie_rating, by= 'movieId') %>% 
  left_join(Avg_movie_user_rating, by = 'userId') %>%
  group_by(year) %>%
  summarize(b_y = mean(rating - Mean_avg_rating-b_i-b_u))

# Predicting ratings

Avg_movie_user_year_rating_pred <- validation %>%
  mutate(year = year(as.Date(as.POSIXct(timestamp, origin="1970-01-01")))) %>%
  left_join(Avg_movie_rating, by = 'movieId') %>%
  left_join(Avg_movie_user_rating, by = 'userId') %>%
  left_join(Avg_movie_user_year_rating, by = 'year') %>%
  mutate(pred = Mean_avg_rating + b_u + b_i + b_y) %>% .$pred

# Calculate RMSE

Pred_avg_mov_user_year_rat <- RMSE(validation$rating,Avg_movie_user_year_rating_pred)

# Update table

rmse_results_table <- bind_rows(rmse_results_table, data.frame(Methods = "Adding Movie + user + year effect", RMSE = Pred_avg_mov_user_year_rat))

########### Genre effect#############

# Number of unique genres in dataset

length(unique(edx$genres))

# Number of genre categories

te <- process_test %>% 
  separate(genres, c("Genre 1", "Genre 2", "Genre 3", "Genre 4","Genre 5", "Genre 6", "Genre 7", "Genre 8","Genre 9", "Genre 10", "Genre 11", "Genre 12"),sep = "|")

# Name of genres



# Genre effect

Avg_movie_user_year_genre_rating <- edx %>%
  mutate(year = year(as.Date(as.POSIXct(timestamp, origin="1970-01-01")))) %>%
  left_join(Avg_movie_rating, by= 'movieId') %>% 
  left_join(Avg_movie_user_rating, by = 'userId') %>%
  left_join(Avg_movie_user_year_rating, by = 'year') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - Mean_avg_rating-b_i-b_u-b_y))

# Predicting ratings

Avg_movie_user_year_genre_rating_pred <- validation %>%
  mutate(year = year(as.Date(as.POSIXct(timestamp, origin="1970-01-01")))) %>%
  left_join(Avg_movie_rating, by = 'movieId') %>%
  left_join(Avg_movie_user_rating, by = 'userId') %>%
  left_join(Avg_movie_user_year_rating, by = 'year') %>%
  left_join(Avg_movie_user_year_genre_rating, by = 'genre') %>%
  mutate(pred = Mean_avg_rating + b_u + b_i + b_y + b_g) %>% .$pred


######## Number of ratings effect ######




####### Matrix factorization #######

#Create matrix

edx_small <- edx %>%
  group_by(userId) %>% 
  filter(n()>400) %>%
  ungroup() %>%
  group_by(movieId) %>%
  filter(n()>2500) %>%
  ungroup()



y <- edx_small %>%
  select(userId,movieId,rating) %>%
  spread(movieId,rating) %>% 
  as.matrix()

# Assign row names

rownames(y) <- y[,1]

#Remove first column as this has been allocated to the row name

y <- y[,-1]

# Give columns movie names

colnames(y) <- with(process_test, title[match(colnames(y), movieId)])

# remove column and row means

y <- sweep(y, 1, rowMeans(y, na.rm=TRUE))
y <- sweep(y, 2, colMeans(y, na.rm=TRUE))

# Assign zero to Na's as we don't want these to have any affect

y[is.na(y)] <- 0

x <- as(y, "realRatingMatrix")

# Using Recommenderlab

scheme <- x %>%
  evaluationScheme(method = "cross",
                   k = 5,
  )

algorithm <- c("LIBMF", "POPULAR","RANDOM","RERECOMMEND","ALS","")

rec <- Recommender(x, method = "LIBMF")

confusionMatrix()



y <- sweep(y, 1, rowMeans(y))
pca <- prcomp(y)

dim(pca$rotation)

dim(pca$x)

plot(pca$sdev)

var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained)

pcs <- data.frame(pca$rotation, name = colnames(y))

pcs %>% select(name, PC2) %>% arrange(PC2) %>% slice(1:10)

#####looks wrong beow####

# Training a logisitc regression model using Carat package

lm_model <- train(rating ~ ,
                  method = "knn",
                  data = process_test)
confusionMatrix(data = predict(lm_model,validation), reference=validation$rating)$overall["Accuracy"]



lda_model <- train(Survived ~ Fare,
                   method = "lda",
                   data = train_set)
confusionMatrix(data = predict(lda_model,test_set), reference=test_set$Survived)$overall["Accuracy"]



# Start by 
