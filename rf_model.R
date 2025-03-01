setwd("~/University/TDK/trump_speech_analysis")
install.packages("gridExtra")
library(dplyr)
library(readxl)
library(ranger)
library(h2o)
library(vip)
library(gridExtra)

#data preprocessing
df <- read_excel("trump-speech-analysis/data_for_rfmodel.xlsx")

df$date <- as.Date(df$date)
df$month <- as.factor(df$month)
#df$day <- as.factor(df$day)
df$day_of_the_week <- as.factor(df$day_of_the_week)
df$state <- as.factor(df$day_of_the_week)
df$nth_speech_in_state <- as.factor(df$nth_speech_in_state)
summary(df)

#----------------------------------------------------------
#building decision tree to predict positive sentiment score
set.seed(2024)
df_pos <- df[,c('positive', 'date', 'month', 'day', 'day_of_the_week', 'state',
                'nth_speech_in_state', 'CTTR', 'scale', 'popularity', 'tfidf1',
                'tfidf2', 'tfidf3', 'tfidf4', 'tfidf5')]

df_pos_train <- slice_sample(df_pos, n = floor(nrow(df_pos)*0.7))
df_pos_test <- anti_join(df_pos, df_pos_train)
n_features <- length(setdiff(names(df_pos), "positive"))

pos_rf1 <- ranger(
  positive ~ ., 
  data = df_pos_train,
  mtry = floor(n_features / 3),
  respect.unordered.factors = "order"
)
default_rmse <- sqrt(pos_rf1$prediction.error)
predictions <- predict(pos_rf1, data = df_pos_test)
df_pos_test$prediction <- predictions$predictions

ggplot(df_pos_test, aes(x=date)) + geom_point(aes(y=positive)) +
  geom_point(aes(y=prediction), color="red")





pos_rf1_impurity <- ranger(
  formula = positive ~ ., 
  data = df_pos_train, 
  num.trees = 2000,
  mtry = floor(n_features / 3),
  min.node.size = 1,
  sample.fraction = .80,
  replace = FALSE,
  importance = "impurity",
  respect.unordered.factors = "order",
  verbose = FALSE
)

pos_rf1_permutation <- ranger(
  formula = positive ~ ., 
  data = df_pos_train, 
  num.trees = 2000,
  mtry = floor(n_features / 3),
  min.node.size = 1,
  sample.fraction = .80,
  replace = FALSE,
  importance = "permutation",
  respect.unordered.factors = "order",
  verbose = FALSE
)
p1 <- vip::vip(pos_rf1_impurity, num_features = 25, bar = FALSE)
p2 <- vip::vip(pos_rf1_permutation, num_features = 25, bar = FALSE)
gridExtra::grid.arrange(p2, nrow = 1)


#----------------------------------------------------------
#building decision tree to predict political scale right leaning
df_scale <- df[,c('positive', 'date', 'day', 'month',
                'nth_speech_in_state', 'CTTR', 'scale', 'popularity', 'tfidf1',
                'tfidf2', 'tfidf3', 'tfidf4', 'tfidf5')]

df_scale_train <- slice_sample(df_scale, n = floor(nrow(df_pos)*0.7))
df_scale_test <- anti_join(df_scale, df_scale_train)
n_features <- length(setdiff(names(df_scale), "scale"))

scale_rf1 <- ranger(
  scale ~ ., 
  data = df_pos_train,
  mtry = floor(n_features / 3),
  respect.unordered.factors = "order"
)
scale_default_rmse <- sqrt(scale_rf1$prediction.error)
predictions <- predict(scale_rf1, data = df_scale_test)
df_scale_test$prediction <- predictions$predictions
scale_rf1$r.squared
ggplot(df_scale_test, aes(x=date)) + geom_point(aes(y=scale)) +
  geom_point(aes(y=prediction), color="red")

scale_rf1_impurity <- ranger(
  formula = scale ~ ., 
  data = df_scale_train, 
  num.trees = 2000,
  mtry = floor(n_features / 3),
  min.node.size = 1,
  sample.fraction = .80,
  replace = FALSE,
  importance = "impurity",
  respect.unordered.factors = "order",
  verbose = FALSE
)

scale_rf1_permutation <- ranger(
  formula = scale ~ ., 
  data = df_scale_train, 
  num.trees = 2000,
  mtry = floor(n_features / 3),
  min.node.size = 1,
  sample.fraction = .80,
  replace = FALSE,
  importance = "permutation",
  respect.unordered.factors = "order",
  verbose = FALSE
)
p1 <- vip::vip(scale_rf1_impurity, num_features = 25, bar = FALSE)
p2 <- vip::vip(scale_rf1_permutation, num_features = 25, bar = FALSE)
gridExtra::grid.arrange(p2, nrow = 1)





