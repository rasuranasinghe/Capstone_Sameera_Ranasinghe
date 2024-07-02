#Check for duplicate rows based on all columns
duplicates_all <- edx %>%
  group_by(userId, movieId, rating) %>%
  filter(n() > 1) %>%
  ungroup()


# Check for missing values
missing_values <- sum(is.na(edx))

# Check for errors or different formats in numeric columns
Uers_ID_errors <- edx %>%
  filter(!is.numeric(userId))

Movie_ID_errors <- edx %>%
  filter(!is.numeric(movieId))

ratings_errors <- edx %>%
  filter(!is.numeric(rating))







# assesing the unique values of the variables
edx %>% summarize(un_users = length(unique(userId)),
                  un_movies = length(unique(movieId)),
                  un_genres = length(unique(genres)))

# Calculate percentages for the 'type' variable
Percentage_rating <- edx %>%
  count(rating) %>%
  mutate(percentage = n / sum(n) * 100)

#plot %ratings out of total number of ratings
ggplot(Percentage_rating,aes(x=as.factor(rating ), y= percentage,fill= as.factor(rating ))) +
geom_bar(stat="identity",width = 0.7)+
labs(title= "Percentage Distribution of ratings ",y = "percentage" , x= "Ratinng")+
theme_minimal()+
geom_text(aes(label=paste0(round(percentage,1),"%")),vjust= -0.5)+
scale_fill_manual(values = c("1" = "#1f77b4", "2" = "#ff7f0e", "3" = "#2ca02c", "4" = "#d62728", "5" = "#9467bd", "6" = "#8c564b", "7" = "#e377c2", "8" = "#7f7f7f", "9" = "#bcbd22", "10" = "#17becf"))+
theme(axis.text.x = element_text(angle = 0, hjust = 1), axis.ticks = element_blank())

#summarize the data by number of movies
movie_counts <- edx%>% 
  count(movieId) %>%
  rename(count_ratings= n)
#plotting the distribution of mnie IDs
ggplot(movie_counts, aes(x = count_ratings)) +
geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black") +
scale_x_log10() +
labs(title = "Movies Distribution", x = "Number of Ratings", y = "Number of Movies")


#summarize the data by number of USERID
user_counts <- edx%>% 
  count(userId) %>%
  rename(count_ratings= n)
#plotting the distribution of mnie IDs
ggplot(user_counts, aes(x = count_ratings)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black") +
  scale_x_log10() +
  labs(title = "UserID Distribution", x = "Number of Ratings", y = "Number of UserID")

#converting the timestamp and extracting the year
edx <- edx %>% mutate(date = as.Date(as.POSIXct(timestamp, origin = "1970-01-01")))

#providing seperate columns for each genre
edx <- edx %>%
  separate_rows(genres, sep = "\\|") %>%
  mutate(value = 1) %>%
  spread(genres, value, fill = 0)

heheadlibrary(dplyr)
library(tidyr)
library(caret)

#Convert timestamp to Date and extract the Year
edx <- edx %>%
  mutate(date = as.Date(as.POSIXct(timestamp, origin = "1970-01-01")),
         Year = as.numeric(format(date, "%Y")))

#Separate genres and create one-hot encoded genre columns
edx <- edx %>%
  separate_rows(genre, sep = "\\|") %>%
  mutate(value = 1) %>%
  spread(key = genre, value = value, fill = 0)

# Calculate global mean rating
global_mean <- mean(edx$rating)

#Calculate movie effect
movies_effect <- edx %>%
  group_by(movieId) %>%
  summarize(movie_effect = mean(rating - global_mean))

# Calculate user effect
users_effect <- edx %>%
  left_join(movie_effects, by = "movieId") %>%
  group_by(userId) %>%
  summarize(user_effect = mean(rating - global_mean - movies_effect))

# Merge user and movie effects back into the dataset
edx <- edx %>%
  left_join(movie_effects, by = "movieId") %>%
  left_join(user_effects, by = "userId")



# Split data into training and testing sets (70% training, 30% testing)
set.seed(123) # For reproducibility
trainIndex <- createDataPartition(model_data$rating, p = .75, 
                                  list = FALSE, 
                                  times = 1)
trainData <- model_data[ trainIndex,]
testData  <- model_data[-trainIndex,]

# Train the linear model
linear_model <- train(formula, data = trainData, method = "lm")

# View the summary of the model
summary(linear_model)

# Predict on test data
predictions <- predict(linear_model, testData)

# Evaluate the model
model_performance <- postResample(predictions, testData$rating)
print(model_performance)

global_mean <- mean(edx$rating)

# Step 4: Calculate movie effect
movie_effects <- edx %>%
  group_by(movieId) %>%
  summarize(movie_effect = mean(rating - global_mean))

# Step 5: Calculate user effect
user_effects <- edx %>%
  left_join(movie_effects, by = "movieId") %>%
  group_by(userId) %>%
  summarize(user_effect = mean(rating - global_mean - movie_effect))

# Step 6: Merge user and movie effects back into the dataset
edx <- edx %>%
  left_join(movie_effects, by = "movieId") %>%
  left_join(user_effects, by = "userId")
#remove columns with infinite values
edx_filtered <- edx %>% select (-movieId,-userId,-title,-timestamp, -`(no genres listed)`)

#rename column names to avoid errors
edx_filtered <- edx_filtered %>%
  rename(Film_Noir = `Film-Noir`, Sci_Fi = `Sci-Fi`)

# Sample the data
set.seed(125)
sample_size <- 10000  # Adjust based on available memory
sample_indices <- sample(nrow(edx_filtered), sample_size)
edx_sample <- edx_filtered[sample_indices, ]

# Split the data into training and testing sets
set.seed(125)
trainIndex <- createDataPartition(edx_sample$rating, p = 0.75, list = FALSE)
train_data <- edx_sample[trainIndex, ]
test_data <- edx_sample[-trainIndex, ]

#providing seperate columns for each genre for the test dataset
Final_holdout_test <- final_holdout_test %>%
  separate_rows(genres, sep = "\\|") %>%
  mutate(value = 1) %>%
  spread(genres, value, fill = 0)

#time stamp change for test data
Final_holdout_test <- Final_holdout_test %>%
  mutate(date = as.Date(as.POSIXct(timestamp, origin = "1970-01-01")),
         Year = as.numeric(format(date, "%Y")))

global_mean <- mean(Final_holdout_test$rating)

# Calculate movie effect for test data
movie_effects <- Final_holdout_test %>%
  group_by(movieId) %>%
  summarize(movie_effect = mean(rating - global_mean))

# Calculate user effect
user_effects <- Final_holdout_test %>%
  left_join(movie_effects, by = "movieId") %>%
  group_by(userId) %>%
  summarize(user_effect = mean(rating - global_mean - movie_effect))

# Merge user and movie effects back into the dataset
Final_holdout_test <- Final_holdout_test %>%
  left_join(movie_effects, by = "movieId") %>%
  left_join(user_effects, by = "userId")

#remove columns with infinite values
Final_holdout_test<- Final_holdout_test %>% select (-movieId,-userId,-title,-timestamp)

#rename column names to avoid errors
Final_holdout_test <- Final_holdout_test %>%
  rename(Film_Noir = `Film-Noir`, Sci_Fi = `Sci-Fi`)

# Calculate the mean rating (benchmark model)
edx_mean <- mean(train_data$rating)

# Calculate RMSE for the benchmark model
rmse_BM <- RMSE(Final_holdout_test$rating, edx_mean)



#10 fold cross validation was used to run the algorithms
trainControl <- trainControl(method = "cv", number = 10, classProbs = FALSE, summaryFunction = defaultSummary)
#accuracy was selected as the metric to display                               +                              
metric <- "RMSE"

#KNN model
fit_KNN <- train(rating ~ ., data = train_data, method = "knn", metric = metric, preProc = c("center", "scale"), trControl = trainControl)
prediction_KNN <-predict(fit_KNN, newdata=Final_holdout_test)
rmse_KNN <- sqrt (mean((prediction_KNN-Final_holdout_test$rating)^2))



# Train a Random Forest model with a limited number of trees
fit_RF <- randomForest(rating ~ ., data=train_data, ntree=50)
prediction_RF <-predict(fit_RF, newdata=Final_holdout_test)
rmse_RF <- sqrt (mean((prediction_RF-Final_holdout_test$rating)^2))

#train a decision tree model
fit_DecisionTree <- rpart(rating ~ ., data=train_data, method="anova")
prediction_DecisionTree <-predict(fit_DecisionTree, newdata=Final_holdout_test)
rmse_DecisionTree <- sqrt (mean((prediction_DecisionTree-Final_holdout_test$rating)^2))

#train a SVM model
fit_SVM <- train(rating ~ ., data = train_data, method = "svmRadial", metric = metric, preProc = c("center", "scale"), trControl = trainControl)
prediction_SVM <- predict(fit_SVM, newdata=Final_holdout_test)
rmse_SVM <- sqrt (mean((prediction_SVM-Final_holdout_test$rating)^2))

#compare the rmse values
RMSE_comparison <- data.frame (Model=c("Benchmark","Decision Tree","KNN","SVM","Random Forest"),RMSE=c(rmse_BM, rmse_DecisionTree, rmse_KNN, rmse_SVM,rmse_RF))

#plot RMSE values
ggplot(data=RMSE_comparison, aes(x=Model,y=RMSE)) +
geom_point(size= 5) +
labs(title= "RMSE comparison",x="Model", y="RMSE") +
theme_minimal()

