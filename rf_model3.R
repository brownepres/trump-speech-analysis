setwd("~/University/TDK/trump_speech_analysis")
install.packages("iml")
library(readxl)
library(randomForest)
library(iml)
library(ggplot2)

#read and preprocess data
set.seed(2024)
df <- read_excel("trump-speech-analysis/data_for_rfmodel.xlsx")
df$date <- as.Date(df$date)
df$month <- as.factor(df$month)
#df$day <- as.factor(df$day)
#df$day_of_the_week <- as.factor(df$day_of_the_week)
df$state <- as.factor(df$state)
#df$nth_speech_in_state <- as.factor(df$nth_speech_in_state)
summary(df)

calculate_r2 <- function(df) {
  SST <- sum((df$y - mean(df$y))^2)  # Corrected SST calculation
  SSR <- sum(df$residuals^2)  # Residual sum already squared
  r2 <- 1 - (SSR / SST)
  return(r2)
}

#-------------------------------------------------
#build model to predict positive sentiment score
#prepare for model training

df_positive <- df[,c("positive", "popularity", "month", "day", "day_of_the_week", "state",
                      "nth_speech_in_state", "neg_positive", 
                      "neg_negative", "CTTR", "scale", "tfidf1", 
                      "tfidf2", "tfidf3", "tfidf4", "tfidf5", "frequency", 
                     "popularity_tminus1", "popularity_tplus1")]

get_random_forest_positive <- function(df_for_model){
  positive_rf1 <- randomForest(positive ~ ., df_for_model, 
                               ntree=1000, 
                               mtry = floor((ncol(df_for_model)-1)/3)
                               )
  
  # Ensure it's a dataframe
  positive_fitted <- df_for_model[, "positive", drop = FALSE]  
  positive_fitted$y_hat <- positive_rf1$predicted
  positive_fitted$id <- 1:nrow(positive_fitted)
  positive_fitted$y <- positive_fitted$positive
  positive_fitted$residuals <- (positive_fitted$y - positive_fitted$y_hat)  # Corrected residual calculation
  
  pos_r2 <- calculate_r2(positive_fitted)
  print(pos_r2)
  
  ggplot(positive_fitted, aes(x=id)) + geom_line(aes(y=y_hat), color="red") + 
    geom_line(aes(y=df$positive), color="blue")
  
  #calculate the feature and individual importance of the x variables
  X_data = df_for_model[, -1]
  Y_data = df_for_model$positive
  
  predictor <- Predictor$new(positive_rf1, data = X_data, y = Y_data)
  
  featureImportance <- FeatureImp$new(predictor = predictor, loss="mae")
  plot(featureImportance)
  
  featureImportanceMSE <- FeatureImp$new(predictor = predictor, loss="mse")
  plot(featureImportanceMSE)
  
  shapley <- Shapley$new(predictor, x.interest = X_data[70,], sample.size=50)
  plot(shapley)
}

get_random_forest_positive(df_positive)


#-------------------------------------------------
#build model to predict popularity 
df_for_model <- df[,c("positive", "popularity",
                      "day", "day_of_the_week", "state",
                     "nth_speech_in_state", "neg_positive", 
                     "neg_negative", "CTTR", "scale", "tfidf1", 
                     "tfidf2", "tfidf3", "tfidf4", "tfidf5")]



popularity_rf1 <- randomForest(scale ~ ., df_for_model, 
                             ntree=700, 
                             mtry = floor((ncol(df_for_model)-1)/3)
)


pop_fitted <- df_for_model[, "scale", drop = FALSE]  
pop_fitted$y_hat <- popularity_rf1$predicted
pop_fitted$id <- 1:nrow(pop_fitted)
pop_fitted$y <- pop_fitted$scale
pop_fitted$residuals <- (pop_fitted$y - pop_fitted$y_hat)  # Corrected residual calculation

pop_r2 <- calculate_r2(pop_fitted)

ggplot(pop_fitted, aes(x=id)) + geom_line(aes(y=y_hat), color="red") + 
  geom_line(aes(y=y), color="blue")

#calculate the feature and individual importance of the x variables
X_data = df_for_model[, -2]
Y_data = df_for_model$popularity

predictor <- Predictor$new(popularity_rf1, data = X_data, y = Y_data)

featureImportance <- FeatureImp$new(predictor = predictor, loss="mae")
plot(featureImportance)

featureImportanceMSE <- FeatureImp$new(predictor = predictor, loss="mse")
plot(featureImportanceMSE)

shapley <- Shapley$new(predictor, x.interest = X_data[70,], sample.size=50)
plot(shapley)


df_for_corr <- df[,c("positive", "popularity",
                      "day", "day_of_the_week",
                      "nth_speech_in_state", "neg_positive", 
                      "neg_negative", "CTTR", "scale", "tfidf1", 
                      "tfidf2", "tfidf3", "tfidf4", "tfidf5")]
cor(df_for_corr)

