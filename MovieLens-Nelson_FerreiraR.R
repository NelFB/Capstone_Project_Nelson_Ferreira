#Packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(dplyr)) install.packages("dplyr")
if(!require(dslabs)) install.packages("dslabs")
if(!require(tidyr)) install.packages("tidyr")
if(!require(stringr)) install.packages("stringr")
if(!require(forcats)) install.packages("forcats")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggplot2)) install.packages("ggplot2")

# Loading libraries
library(tidyverse)
library(caret)
library(data.table)
library(kableExtra)
library(dplyr)
library(dslabs)
library(stringr)
library(forcats)
library(ggplot2)
library(lubridate)

### Loading data set
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), 
                          "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")


movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")
# Validation set will be 10% of MovieLens data
set.seed(1) # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, 
                                  list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)

# Joining columns
edx <- rbind(edx, removed)


#####################################################
# Test_set set will be 10% of Edx data
test_index2 <- createDataPartition(y = edx$rating, times = 1, p = 0.1, 
                                   list = FALSE)
train_set <- edx[-test_index2,]
temp2 <- edx[test_index2,]

# Make sure userId and movieId in test_set set are also in train_set
test_set <- temp2 %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test_set set back into train_set
removed_2 <- anti_join(temp2, test_set)

# Joining the columns
train_set <- rbind(train_set, removed_2)
####################################################
## Main basic data exploration

# Edx Database
head(edx)
glimpse(edx)
edx %>% summarize(n_users = n_distinct(userId),
                  n_movies = n_distinct(movieId))
###################################################
#Validation Database
glimpse(validation)
validation %>% summarize(n_users = n_distinct(userId),
                         n_movies = n_distinct(movieId))
###################################################
# train_set Database
head(train_set)
glimpse(train_set)
train_set %>% summarize(n_users = n_distinct(userId),
                        n_movies = n_distinct(movieId))
# test_set Database
head(test_set)
glimpse(test_set)
test_set %>% summarize(n_users = n_distinct(userId),
                       n_movies = n_distinct(movieId))

## Rating distribution vs Users and Movies
# Examining distribution of ratings by users.
edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram( bins=30, color = "black") +
  scale_x_log10() +
  ggtitle("User distribution") +
  labs(x="Ratings", y="Users")

# Examining distribution of ratings by movies.
edx %>%
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram( bins=30, color = "black") +
  scale_x_log10() +
  ggtitle("Movie Distribution") +
  labs(x="Ratings" , y="Movies")
##############################################################
# Processing data
# Convert the timestamp to **ratingyear**and ratingmonth in the edx database

edx$date <- as.POSIXct(edx$timestamp, origin="1970-01-01")
edx$ratingyear <- format(edx$date, "%Y")
edx$ratingyear <- as.numeric(edx$ratingyear)
head(edx)

# Convert the timestamp to **ratingyear**and ratingmonth in the validation database
validation$date <- as.POSIXct(validation$timestamp, origin="1970-01-01")
validation$ratingyear <- format(validation$date, "%Y")
validation$ratingyear <- as.numeric(validation$ratingyear)
head(validation)

# # edx database
edx <- edx %>%
  select(-timestamp, -date)

# validation database
validation <- validation %>%
  select(-timestamp, -date)

# Release year in a new column

edx <- edx %>% mutate(year = as.numeric(str_sub(title,-5,-2)))
validation <- validation %>% mutate(year = as.numeric(str_sub(title,-5,-2)))

## Rating exploration
# Data frame that contains movie ratings from edx data
stars <- ifelse((edx$rating == 1 |edx$rating == 2 | edx$rating == 3 |
                   edx$rating == 4 | edx$rating == 5) ,
                "round_star_rating",
                "half_star_rating")

movie_ratings <- data.frame(edx$rating, stars)

# Number of ratings per rating
ggplot(movie_ratings, aes(x = edx.rating, fill = stars)) + 
  geom_histogram(binwidth = 0.2) + 
  scale_x_continuous(name = "rating", breaks = seq(0, 5, by = 0.5)) +
  scale_y_continuous(name = "number_of_ratings") +
  scale_fill_manual(values = c("round_star_rating"="dodgerblue2", 
                               "half_star_rating"="green4")) 

# Summary of rating count
edx %>% group_by(rating) %>% summarize(count = n()) %>%
  arrange(desc(count))
edx %>% group_by(rating, ratingyear) %>% summarize(count = n()) %>%
  arrange(desc(count))
# Top 10 most rated titles
edx %>%
  group_by(title) %>%
  summarize(count=n()) %>%
  arrange(desc(count))
# Top listed genres
edx%>%
  group_by(genres) %>% summarize(Ratings_Sum = n(), 
                                 Average_Rating = mean(rating)) %>%
  arrange(-Ratings_Sum)
# Movies with the major number of ratings
most_rated_titles <- edx %>%
  group_by(title) %>%
  summarize(count=n()) %>%
  top_n(15,count) %>%
  arrange(desc(count))

most_rated_titles %>%
  ggplot(aes(x=reorder(title, count), y=count)) +
  geom_bar(stat= "identity", fill="blue") +
  coord_flip(y=c(0, 35000)) +
  labs(x="", y="Number of ratings") +
  geom_text(aes(label= count), hjust=-0.1, size=3) +
  labs(title="Top 15 movies title with \n most number of ratings")

# Frequency of ratings per year
edx$ratingyear %>%
  hist(main="Frequency", xlab="Years")

##################################################################
## Naive Model
train_mu_hat <- mean(train_set$rating)
train_mu_hat

nrmse <- RMSE(train_set$rating, train_mu_hat)
nrmse

prediction_results <-  data.frame(model="Naive Model", RMSE=nrmse) 

##################################################################
## Movie Effect Model
train_mu_hat <- mean(train_set$rating)
train_mu_hat
bi <- train_set %>% # Average rating by movie
  group_by(movieId) %>%
  summarize(b_i = mean(rating - train_mu_hat))
qplot(b_i, data = bi, bins = 10, color = I("black"))

prediction_bi <-train_mu_hat + test_set %>%
  left_join(bi, by = "movieId") %>% .$b_i
m_rmse <- RMSE(prediction_bi, test_set$rating)
m_rmse

prediction_results <- prediction_results %>% 
  add_row(model = "Movie Effect Model", RMSE = m_rmse)
prediction_results
##################################################################
## Movie + User Effects Model
train_set %>%
  group_by(userId) %>%
  filter(n()>=100) %>%
  summarize(bu = mean(rating)) %>%
  ggplot(aes(bu)) +
  geom_histogram(bins = 30, color = "black")

train_mu_hat <- mean(train_set$rating)
train_mu_hat

bu <-train_set %>%  
  left_join(bi, by = "movieId") %>% 
  group_by(userId) %>%
  summarize(b_u = mean(rating - train_mu_hat - b_i))

prediction_bu <- test_set %>% 
  left_join(bi, by='movieId') %>%
  left_join(bu, by='userId') %>%
  mutate(predictions = train_mu_hat + b_i + b_u) %>%
  pull(predictions)
um_rmse <- RMSE(prediction_bu, test_set$rating)
um_rmse

prediction_results <- prediction_results %>% 
  add_row(model = "Movie + User effects", RMSE = um_rmse)
prediction_results
##################################################################
## Penalized least squares

prediction_results

train_mu_hat <- mean(train_set$rating)
train_mu_hat

lambdas <- seq(0, 10, 0.5)

# Modeling with Regularized Movie + User Effect Model

rmse_mu <- sapply(lambdas, function(l){
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - train_mu_hat)/(n()+l)) # rating mean by movie
  b_u <- train_set %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - train_mu_hat)/(n()+l)) # mean rating by user
  predicted_ratings <- test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(prediction = train_mu_hat + b_i + b_u) %>%
    pull(prediction)  # RMSE in the test_set database
  
  return(RMSE(predicted_ratings, test_set$rating))
})

rmse_mu_pen <- min(rmse_mu)
rmse_mu_pen

prediction_results <- prediction_results %>% 
  add_row(model="Regularized Movie + User Effect Model", RMSE=rmse_mu_pen)
prediction_results

lambdas[which.min(rmse_mu)] # The lambda value that minimize the RMSE

qplot(lambdas, rmse_mu)

###############################################################
# Validation
prediction_results

edx_muhat <- mean(edx$rating)

lambdas <- seq(0, 10, 0.1)

# Modeling with Regularized Movie + User Effect Model

rmse_mu_val <- sapply(lambdas, function(l){
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - edx_muhat)/(n()+l)) # mean rating by movie
  b_u <- edx %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - edx_muhat)/(n()+l))  # mean rating by user
  predicted_ratings <- validation %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(prediction = edx_muhat + b_i + b_u) %>%
    pull(prediction) # RMSE prediction on the validation database
  
  return(RMSE(predicted_ratings, validation$rating))
})

rmse_mu_pen_val <- min(rmse_mu_val)
rmse_mu_pen_val

prediction_results <- prediction_results %>% 
  add_row(model="Regularized Movie + User Effect Model Validated", RMSE=rmse_mu_pen_val)

prediction_results








