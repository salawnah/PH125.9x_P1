if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(stringr)

##########################################################
# The Movielens project has the following sections 
# Section1 : Create edx and final_holdout_test sets, using 
#            the code given in the edx platform 
# Section2 : Preprocess the data sets as the following: 
#            - Extracting the movie release date from the title 
#            - Converting the ratting timestamp to  rating_year, rating_month,  
#            rating_day_of_week, and rating_week_of_year  
#            - splitting the genres to be one column per genre (one-hot-coding)
#              since the movie can belong to many genre, this task is a little complex 
#              and time consuming.
# Section3 : Building the models using the features we 
#            have.
# Section4 : Use regularization to optimize the best model 
#           accuracy 
# Section5 : Test the best regularized model on the final_holdout_test 
##########################################################



##########################################################
# Section1 : Create edx and final_holdout_test sets, using 
#            the code given in the edx platform 
##########################################################

# Note: this process could take a couple of minutes


# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 1200)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


##########################################################
# Section2 : Preprocess the data sets as the following: 
#            - Extracting the movie release date from the title 
#            - Converting the ratting timestamp to  rating_year, rating_month,  
#            rating_day_of_week, and rating_week_of_year  
#            - splitting the genres to be one column per genre (one-hot-coding)
#              since the movie can belong to many genre, this task is a little complex 
#              and time consuming.
##########################################################


## - Extracting the movie release date from the title

## the get_release_date function, to extract the release data 
## from the title, an example title is: 
## Star Wars: Episode IV - A New Hope (a.k.a. Star Wars) (1977)
## the release date is the last value encapsulated between the () 
get_release_date <- function(title){
  ## get all strings encapsulated between the () 
  all_matches <- str_match_all(title, "\\((.*?)\\)")
  ## the matches will be given in all_matches[[1]]
  ## the first dimension of it is the number of matches 
  matches_count <- dim(all_matches[[1]])[1]
  ## the last match is located at the matches_count
  ## the first value will be with (), the second will be without 
  ## the () which what we want 
  as.integer(all_matches[[1]][matches_count,2])
}

## will use the get_release_date to add the release date to the data sets 
edx <- edx %>% mutate(release_year =  get_release_date(title))
final_holdout_test <- final_holdout_test %>% mutate(release_year =  get_release_date(title))

edx <- edx %>% mutate(ratting_date=as.POSIXct(timestamp,origin="1970-01-01")) %>% 
  mutate(rating_year=format(ratting_date,"%Y"),rating_month=format(ratting_date,"%m"),rating_day_of_week=format(ratting_date,"%w"),rating_week_of_year=format(ratting_date,"%W")) 


final_holdout_test <- final_holdout_test %>% mutate(ratting_date=as.POSIXct(timestamp,origin="1970-01-01")) %>% 
  mutate(rating_year=format(ratting_date,"%Y"),rating_month=format(ratting_date,"%m"),rating_day_of_week=format(ratting_date,"%w"),rating_week_of_year=format(ratting_date,"%W"))


## Convert the datasets into one-hot coding for the genres
## This will help us to study the effect of each individual genere 
edx$rowID = seq(1,nrow(edx))
head(edx)
#Split the genres into single values 
edx<- edx %>% separate_rows(genres, sep = "\\|")

row_with_1gener <- edx %>% group_by(rowID)%>%summarise(n=n())%>%filter(n==1)
head(row_with_1gener)
tt <- edx %>% filter(rowID %in% row_with_1gener$rowID) %>% group_by(genres) %>% summarise(n_1=n())
ttt <- edx  %>% group_by(genres) %>% summarise(n_all=n())

tt %>% inner_join(ttt,by='genres')%>% mutate(ratio=n_1/n_all)
#Get the list of unique genres 
gens <- unique(edx$genres)
#Add each invidual genre as column, where it has the value 1 if the movie belongs to the genre, 0 otherwise (one-hot coding)
edx_splitted = edx
for(gen in gens)
{
  edx_splitted <- edx_splitted %>% mutate( "genres_{gen}" := ifelse(genres==gen,1,0))
}
#install.packages("corrplot")
library(corrplot)
#remove the genres columns as it is no longer needed 
#NOTE: there are other methods to create one-hot coding like the one_hot function in the mltools package but it will required huge memory which I don't have 
#edx_splitted <- edx_splitted %>% select(-genres)
edx_splitted <- edx_splitted %>% select(-`genres_(no genres listed)`)
#corrplot(cor(edx_splitted %>% select(-title)))
#cor(edx_splitted %>% select(-title))
#colnames(edx_splitted) = c("userId","movieId","rating","timestamp","title","rowID","genres_Comedy","genres_Romance","genres_Action","genres_Crime","genres_Thriller","genres_Drama","genres_Sci_Fi","genres_Adventure","genres_Children","genres_Fantasy","genres_War","genres_Animation","genres_Musical","genres_Western","genres_Mystery","genres_Film_Noir","genres_Horror","genres_Documentary","genres_IMAX")
#names(edx_splitted)
edx_splitted <- edx_splitted %>% group_by(rowID) %>% summarise(
  userId = first(userId),
  movieId = first(movieId),
  rating = first(rating),
  generes = first(genres),
  timestamp = first(timestamp),
  `genres_Comedy` = sum(`genres_Comedy`),
  genres_Romance = sum(`genres_Romance`),
  genres_Action = sum(`genres_Action`),
  genres_Crime = sum(`genres_Crime`),
  genres_Thriller = sum(`genres_Thriller`),
  genres_Drama = sum(`genres_Drama`),
  `genres_Sci-Fi` = sum(`genres_Sci-Fi`),
  genres_Adventure = sum(`genres_Adventure`),
  genres_Children = sum(`genres_Children`),
  genres_Fantasy = sum(`genres_Fantasy`),
  genres_War = sum(`genres_War`),
  genres_Animation = sum(`genres_Animation`),
  genres_Musical = sum(`genres_Musical`),
  genres_Western = sum(`genres_Western`),
  genres_Mystery = sum(`genres_Mystery`),
  `genres_Film-Noir` = sum(`genres_Film-Noir`),
  genres_Horror = sum(`genres_Horror`),
  genres_Documentary = sum(`genres_Documentary`),
  genres_IMAX = sum(`genres_IMAX`)
  
)
corrplot(cor(edx_splitted))

save(edx_splitted,file = "./capstone_p1_v2_ckpts/edx_splitted.RData")
# See the first few rows of edx 
head(edx_splitted)

## Do the same for final_holdout_test

## Convert the datasets into one-hot coding for the genres
## This will help us to study the effect of each individual genere 
final_holdout_test_splitted <- final_holdout_test
final_holdout_test_splitted$rowID = seq(1,nrow(final_holdout_test_splitted))
head(final_holdout_test_splitted)
#Split the genres into single values 
final_holdout_test_splitted<- final_holdout_test_splitted %>% separate_rows(genres, sep = "\\|")

#Add each invidual genre as column, where it has the value 1 if the movie belongs to the genre, 0 otherwise (one-hot coding)
for(gen in gens)
{
  final_holdout_test_splitted <- final_holdout_test_splitted %>% mutate( "genres_{gen}" := ifelse(genres==gen,1,0))
}

final_holdout_test_splitted <- final_holdout_test_splitted %>% select(-`genres_(no genres listed)`)

final_holdout_test_splitted <- final_holdout_test_splitted %>% group_by(rowID) %>% summarise(
  userId = first(userId),
  movieId = first(movieId),
  rating = first(rating),
  generes = first(genres),
  timestamp = first(timestamp),
  `genres_Comedy` = sum(`genres_Comedy`),
  genres_Romance = sum(`genres_Romance`),
  genres_Action = sum(`genres_Action`),
  genres_Crime = sum(`genres_Crime`),
  genres_Thriller = sum(`genres_Thriller`),
  genres_Drama = sum(`genres_Drama`),
  `genres_Sci-Fi` = sum(`genres_Sci-Fi`),
  genres_Adventure = sum(`genres_Adventure`),
  genres_Children = sum(`genres_Children`),
  genres_Fantasy = sum(`genres_Fantasy`),
  genres_War = sum(`genres_War`),
  genres_Animation = sum(`genres_Animation`),
  genres_Musical = sum(`genres_Musical`),
  genres_Western = sum(`genres_Western`),
  genres_Mystery = sum(`genres_Mystery`),
  `genres_Film-Noir` = sum(`genres_Film-Noir`),
  genres_Horror = sum(`genres_Horror`),
  genres_Documentary = sum(`genres_Documentary`),
  genres_IMAX = sum(`genres_IMAX`)
  
)

save(final_holdout_test_splitted,file = "./capstone_p1_v2_ckpts/final_holdout_test_splitted.RData")
# See the first few rows of edx 
head(edx_splitted)
corrplot(cor(edx_splitted))





### First Approche 
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y = edx_splitted$rating, times = 1, p = 0.2, list = FALSE)
edx_train <- edx_splitted[-test_index,]
temp <- edx_splitted[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
edx_test <- temp %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, edx_test)
edx_train <- rbind(edx_train, removed)


mu <- mean(edx_train$rating) 
movie_avgs <- edx_train %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
movie_avgs
predicted_ratings <- mu + edx_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
model_1_rmse <- RMSE(predicted_ratings, edx_test$rating)
model_1_rmse


user_avgs <- edx_train %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))


predicted_ratings <- edx_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

model_2_rmse <- RMSE(predicted_ratings, edx_test$rating)
model_2_rmse

## effects of year and month 
edx_splitted <- edx_splitted %>% mutate(rel_date=as.POSIXct(timestamp,origin="1970-01-01")) %>% 
  mutate(rel_year=format(rel_date,"%Y"),rel_month=format(rel_date,"%m"))

edx_train <- edx_train %>% mutate(rel_date=as.POSIXct(timestamp,origin="1970-01-01")) %>% 
  mutate(rel_year=format(rel_date,"%Y"),rel_month=format(rel_date,"%m"))

edx_test <- edx_test %>% mutate(rel_date=as.POSIXct(timestamp,origin="1970-01-01")) %>% 
  mutate(rel_year=format(rel_date,"%Y"),rel_month=format(rel_date,"%m"))


final_holdout_test_splitted <- final_holdout_test_splitted %>% mutate(rel_date=as.POSIXct(timestamp,origin="1970-01-01")) %>% 
  mutate(rel_year=format(rel_date,"%Y"),rel_month=format(rel_date,"%m"))


year_avgs <- edx_train %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(rel_year) %>%
  summarize(b_y = mean(rating - mu - b_i-b_u))

predicted_ratings <- edx_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_avgs,by="rel_year") %>%
  mutate(pred = mu + b_i + b_u + b_y) %>%
  pull(pred)

model_3_rmse <- RMSE(predicted_ratings, edx_test$rating)
model_3_rmse


month_avgs <- edx_train %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_avgs,by='rel_year') %>%
  group_by(rel_month) %>%
  summarize(b_m = mean(rating - mu - b_i-b_u-b_y))

predicted_ratings <- edx_test %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_avgs,by="rel_year") %>%
  left_join(month_avgs,by="rel_month") %>%
  mutate(pred = mu + b_i + b_u + b_y + b_m) %>%
  pull(pred)

model_4_rmse <- RMSE(predicted_ratings, edx_test$rating)
model_4_rmse
## effects of the generes 
genres_Comedy_avg <- edx_train %>% filter(genres_Comedy==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=mean(rating-mu-b_i-b_u)) %>% 
  pull(b_g_avg)

genres_Romance_avg <- edx_train %>% filter(genres_Romance==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=mean(rating-mu-b_i-b_u)) %>% 
  pull(b_g_avg)

genres_Action_avg <- edx_train %>% filter(genres_Action==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=mean(rating-mu-b_i-b_u)) %>% 
  pull(b_g_avg)


genres_Crime_avg <- edx_train %>% filter(genres_Crime==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=mean(rating-mu-b_i-b_u)) %>% 
  pull(b_g_avg)

genres_Thriller_avg <- edx_train %>% filter(genres_Thriller==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=mean(rating-mu-b_i-b_u)) %>% 
  pull(b_g_avg)

genres_Drama_avg <- edx_train %>% filter(genres_Drama==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=mean(rating-mu-b_i-b_u)) %>% 
  pull(b_g_avg)

`genres_Sci_Fi_avg` <- edx_train %>% filter(`genres_Sci_Fi`==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=mean(rating-mu-b_i-b_u)) %>% 
  pull(b_g_avg)

`genres_Adventure_avg` <- edx_train %>% filter(`genres_Adventure`==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=mean(rating-mu-b_i-b_u)) %>% 
  pull(b_g_avg)

`genres_Children_avg` <- edx_train %>% filter(`genres_Children`==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=mean(rating-mu-b_i-b_u)) %>% 
  pull(b_g_avg)

`genres_Fantasy_avg` <- edx_train %>% filter(`genres_Fantasy`==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=mean(rating-mu-b_i-b_u)) %>% 
  pull(b_g_avg)

`genres_War_avg` <- edx_train %>% filter(`genres_War`==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=mean(rating-mu-b_i-b_u)) %>% 
  pull(b_g_avg)

`genres_Animation_avg` <- edx_train %>% filter(`genres_Animation`==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=mean(rating-mu-b_i-b_u)) %>% 
  pull(b_g_avg)

`genres_Musical_avg` <- edx_train %>% filter(`genres_Musical`==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=mean(rating-mu-b_i-b_u)) %>% 
  pull(b_g_avg)

`genres_Western_avg` <- edx_train %>% filter(`genres_Western`==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=mean(rating-mu-b_i-b_u)) %>% 
  pull(b_g_avg)

`genres_Mystery_avg` <- edx_train %>% filter(`genres_Mystery`==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=mean(rating-mu-b_i-b_u)) %>% 
  pull(b_g_avg)

`genres_Film_Noir_avg` <- edx_train %>% filter(`genres_Film_Noir`==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=mean(rating-mu-b_i-b_u)) %>% 
  pull(b_g_avg)

`genres_Horror_avg` <- edx_train %>% filter(`genres_Horror`==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=mean(rating-mu-b_i-b_u)) %>% 
  pull(b_g_avg)

`genres_Documentary_avg` <- edx_train %>% filter(`genres_Documentary`==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=mean(rating-mu-b_i-b_u)) %>% 
  pull(b_g_avg)

`genres_IMAX_avg` <- edx_train %>% filter(`genres_IMAX`==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=mean(rating-mu-b_i-b_u)) %>% 
  pull(b_g_avg)



predicted_ratings <- final_holdout_test_splitted %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_avgs,by="rel_year") %>%
  left_join(month_avgs,by="rel_month") %>%
  mutate(pred = mu + b_i + b_u+ b_y + b_m + 
           
           genres_Comedy_avg*genres_Comedy+
           genres_Romance_avg*genres_Romance+
           #genres_Action_avg*genres_Action+
           genres_Crime_avg*genres_Crime+
           genres_Thriller_avg*genres_Thriller+
           genres_Drama_avg*genres_Drama+
           genres_Sci_Fi_avg*`genres_Sci-Fi`+
           
           genres_Adventure_avg*`genres_Adventure`+
           genres_Children_avg*`genres_Children`+
           genres_Fantasy_avg*`genres_Fantasy`+
           genres_War_avg*`genres_War`+
           #genres_Animation_avg*`genres_Animation`+
           genres_Musical_avg*`genres_Musical`+
           genres_Western_avg*`genres_Western`+
           genres_Mystery_avg*`genres_Mystery`+
           genres_Film_Noir_avg*`genres_Film-Noir`+
           genres_Horror_avg*`genres_Horror`+
           genres_Documentary_avg*`genres_Documentary`+
           genres_IMAX_avg*`genres_IMAX`
  ) %>%
  pull(pred)
model_5_rmse <- RMSE(predicted_ratings, final_holdout_test_splitted$rating)
model_5_rmse



## Regulirzation 

lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  
  
  
  mu <- mean(edx_train$rating) 
  movie_avgs <- edx_train %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  user_avgs <- edx_train %>% 
    left_join(movie_avgs, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+l))
  
  
  year_avgs <- edx_train %>% 
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    group_by(rel_year) %>%
    summarize(b_y = sum(rating - mu - b_i-b_u)/(n()+l))
  
  
  month_avgs <- edx_train %>%
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    left_join(year_avgs,by='rel_year') %>%
    group_by(rel_month) %>%
    summarize(b_m = sum(rating - mu - b_i-b_u-b_y)/(n()+l))
  
  
  ## effects of the generes 
  genres_Comedy_avg <- edx_train %>% filter(genres_Comedy==1) %>% 
    left_join(movie_avgs,by='movieId') %>%
    left_join(user_avgs,by='userId') %>%
    summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
    pull(b_g_avg)
  
  genres_Romance_avg <- edx_train %>% filter(genres_Romance==1) %>% 
    left_join(movie_avgs,by='movieId') %>%
    left_join(user_avgs,by='userId') %>%
    summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
    pull(b_g_avg)
  
  genres_Action_avg <- edx_train %>% filter(genres_Action==1) %>% 
    left_join(movie_avgs,by='movieId') %>%
    left_join(user_avgs,by='userId') %>%
    summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
    pull(b_g_avg)
  
  
  genres_Crime_avg <- edx_train %>% filter(genres_Crime==1) %>% 
    left_join(movie_avgs,by='movieId') %>%
    left_join(user_avgs,by='userId') %>%
    summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
    pull(b_g_avg)
  
  genres_Thriller_avg <- edx_train %>% filter(genres_Thriller==1) %>% 
    left_join(movie_avgs,by='movieId') %>%
    left_join(user_avgs,by='userId') %>%
    summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
    pull(b_g_avg)
  
  genres_Drama_avg <- edx_train %>% filter(genres_Drama==1) %>% 
    left_join(movie_avgs,by='movieId') %>%
    left_join(user_avgs,by='userId') %>%
    summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
    pull(b_g_avg)
  
  `genres_Sci_Fi_avg` <- edx_train %>% filter(`genres_Sci_Fi`==1) %>% 
    left_join(movie_avgs,by='movieId') %>%
    left_join(user_avgs,by='userId') %>%
    summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
    pull(b_g_avg)
  
  `genres_Adventure_avg` <- edx_train %>% filter(`genres_Adventure`==1) %>% 
    left_join(movie_avgs,by='movieId') %>%
    left_join(user_avgs,by='userId') %>%
    summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
    pull(b_g_avg)
  
  `genres_Children_avg` <- edx_train %>% filter(`genres_Children`==1) %>% 
    left_join(movie_avgs,by='movieId') %>%
    left_join(user_avgs,by='userId') %>%
    summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
    pull(b_g_avg)
  
  `genres_Fantasy_avg` <- edx_train %>% filter(`genres_Fantasy`==1) %>% 
    left_join(movie_avgs,by='movieId') %>%
    left_join(user_avgs,by='userId') %>%
    summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
    pull(b_g_avg)
  
  `genres_War_avg` <- edx_train %>% filter(`genres_War`==1) %>% 
    left_join(movie_avgs,by='movieId') %>%
    left_join(user_avgs,by='userId') %>%
    summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
    pull(b_g_avg)
  
  `genres_Animation_avg` <- edx_train %>% filter(`genres_Animation`==1) %>% 
    left_join(movie_avgs,by='movieId') %>%
    left_join(user_avgs,by='userId') %>%
    summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
    pull(b_g_avg)
  
  `genres_Musical_avg` <- edx_train %>% filter(`genres_Musical`==1) %>% 
    left_join(movie_avgs,by='movieId') %>%
    left_join(user_avgs,by='userId') %>%
    summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
    pull(b_g_avg)
  
  `genres_Western_avg` <- edx_train %>% filter(`genres_Western`==1) %>% 
    left_join(movie_avgs,by='movieId') %>%
    left_join(user_avgs,by='userId') %>%
    summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
    pull(b_g_avg)
  
  `genres_Mystery_avg` <- edx_train %>% filter(`genres_Mystery`==1) %>% 
    left_join(movie_avgs,by='movieId') %>%
    left_join(user_avgs,by='userId') %>%
    summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
    pull(b_g_avg)
  
  `genres_Film_Noir_avg` <- edx_train %>% filter(`genres_Film_Noir`==1) %>% 
    left_join(movie_avgs,by='movieId') %>%
    left_join(user_avgs,by='userId') %>%
    summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
    pull(b_g_avg)
  
  `genres_Horror_avg` <- edx_train %>% filter(`genres_Horror`==1) %>% 
    left_join(movie_avgs,by='movieId') %>%
    left_join(user_avgs,by='userId') %>%
    summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
    pull(b_g_avg)
  
  `genres_Documentary_avg` <- edx_train %>% filter(`genres_Documentary`==1) %>% 
    left_join(movie_avgs,by='movieId') %>%
    left_join(user_avgs,by='userId') %>%
    summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
    pull(b_g_avg)
  
  `genres_IMAX_avg` <- edx_train %>% filter(`genres_IMAX`==1) %>% 
    left_join(movie_avgs,by='movieId') %>%
    left_join(user_avgs,by='userId') %>%
    summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
    pull(b_g_avg)
  
  
  
  predicted_ratings <- edx_test %>% 
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    left_join(year_avgs,by="rel_year") %>%
    left_join(month_avgs,by="rel_month") %>%
    mutate(pred = mu + b_i + b_u+ b_y + b_m + 
             
             genres_Comedy_avg*genres_Comedy+
             genres_Romance_avg*genres_Romance+
             genres_Action_avg*genres_Action+
             genres_Crime_avg*genres_Crime+
             genres_Thriller_avg*genres_Thriller+
             genres_Drama_avg*genres_Drama+
             genres_Sci_Fi_avg*`genres_Sci_Fi`+
             
             genres_Adventure_avg*`genres_Adventure`+
             genres_Children_avg*`genres_Children`+
             genres_Fantasy_avg*`genres_Fantasy`+
             genres_War_avg*`genres_War`+
             genres_Animation_avg*`genres_Animation`+
             genres_Musical_avg*`genres_Musical`+
             genres_Western_avg*`genres_Western`+
             genres_Mystery_avg*`genres_Mystery`+
             genres_Film_Noir_avg*`genres_Film_Noir`+
             genres_Horror_avg*`genres_Horror`+
             genres_Documentary_avg*`genres_Documentary`+
             genres_IMAX_avg*`genres_IMAX`
    ) %>%
    pull(pred)
  
  print(paste('l:',toString(l)))
  
  return(RMSE(predicted_ratings, edx_test$rating))
})

qplot(lambdas, rmses)  

l = lambdas[which.min(rmses)]

mu <- mean(edx_splitted$rating) 
movie_avgs <- edx_train %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+l))

user_avgs <- edx_splitted %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+l))


year_avgs <- edx_splitted %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(rel_year) %>%
  summarize(b_y = sum(rating - mu - b_i-b_u)/(n()+l))


month_avgs <- edx_splitted %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_avgs,by='rel_year') %>%
  group_by(rel_month) %>%
  summarize(b_m = sum(rating - mu - b_i-b_u-b_y)/(n()+l))


## effects of the generes 
genres_Comedy_avg <- edx_splitted %>% filter(genres_Comedy==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
  pull(b_g_avg)

genres_Romance_avg <- edx_splitted %>% filter(genres_Romance==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
  pull(b_g_avg)

genres_Action_avg <- edx_splitted %>% filter(genres_Action==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
  pull(b_g_avg)


genres_Crime_avg <- edx_splitted %>% filter(genres_Crime==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
  pull(b_g_avg)

genres_Thriller_avg <- edx_splitted %>% filter(genres_Thriller==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
  pull(b_g_avg)

genres_Drama_avg <- edx_splitted %>% filter(genres_Drama==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
  pull(b_g_avg)

`genres_Sci_Fi_avg` <- edx_splitted %>% filter(`genres_Sci_Fi`==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
  pull(b_g_avg)

`genres_Adventure_avg` <- edx_splitted %>% filter(`genres_Adventure`==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
  pull(b_g_avg)

`genres_Children_avg` <- edx_splitted %>% filter(`genres_Children`==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
  pull(b_g_avg)

`genres_Fantasy_avg` <- edx_splitted %>% filter(`genres_Fantasy`==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
  pull(b_g_avg)

`genres_War_avg` <- edx_splitted %>% filter(`genres_War`==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
  pull(b_g_avg)

`genres_Animation_avg` <- edx_splitted %>% filter(`genres_Animation`==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
  pull(b_g_avg)

`genres_Musical_avg` <- edx_splitted %>% filter(`genres_Musical`==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
  pull(b_g_avg)

`genres_Western_avg` <- edx_splitted %>% filter(`genres_Western`==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
  pull(b_g_avg)

`genres_Mystery_avg` <- edx_splitted %>% filter(`genres_Mystery`==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
  pull(b_g_avg)

`genres_Film_Noir_avg` <- edx_splitted %>% filter(`genres_Film_Noir`==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
  pull(b_g_avg)

`genres_Horror_avg` <- edx_splitted %>% filter(`genres_Horror`==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
  pull(b_g_avg)

`genres_Documentary_avg` <- edx_splitted %>% filter(`genres_Documentary`==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
  pull(b_g_avg)

`genres_IMAX_avg` <- edx_splitted %>% filter(`genres_IMAX`==1) %>% 
  left_join(movie_avgs,by='movieId') %>%
  left_join(user_avgs,by='userId') %>%
  summarise(b_g_avg=sum(rating-mu-b_i-b_u)/(n()+l)) %>% 
  pull(b_g_avg)



predicted_ratings <- final_holdout_test_splitted %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_avgs,by="rel_year") %>%
  left_join(month_avgs,by="rel_month") %>%
  mutate(pred = mu + b_i + b_u+ b_y + b_m + 
           
           genres_Comedy_avg*genres_Comedy+
           genres_Romance_avg*genres_Romance+
           genres_Action_avg*genres_Action+
           genres_Crime_avg*genres_Crime+
           genres_Thriller_avg*genres_Thriller+
           genres_Drama_avg*genres_Drama+
           genres_Sci_Fi_avg*`genres_Sci-Fi`+
           
           genres_Adventure_avg*`genres_Adventure`+
           genres_Children_avg*`genres_Children`+
           genres_Fantasy_avg*`genres_Fantasy`+
           genres_War_avg*`genres_War`+
           #genres_Animation_avg*`genres_Animation`+
           genres_Musical_avg*`genres_Musical`+
           genres_Western_avg*`genres_Western`+
           genres_Mystery_avg*`genres_Mystery`+
           genres_Film_Noir_avg*`genres_Film-Noir`+
           genres_Horror_avg*`genres_Horror`+
           genres_Documentary_avg*`genres_Documentary`+
           genres_IMAX_avg*`genres_IMAX`
  ) %>%
  pull(pred)

reg_model_rmse_4 <- RMSE(predicted_ratings, final_holdout_test_splitted$rating)
reg_model_rmse_4

