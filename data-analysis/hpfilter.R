setwd("~/University/TDK/trump_speech_analysis/trump-speech-analysis")

library(mFilter)
df <- read.csv("df_for_model_final_hpfilter.csv")
str(df)
hp_model <- hpfilter(df$popularity, freq = 	129600, type="lambda")
df$popularity_hpfiltered <- hp_model$trend[,1]














