setwd("~/University/TDK/trump_speech_analysis")
install.packages("quanteda.textmodels")
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textstats)

text1 <- readLines('/Users/barnabasepres/University/TDK/trump_speech_analysis/text_files/speech_1_"2024-06-06"_AZ.txt')

tokens <- text1 %>% 
  stringr::str_to_lower() %>% 
  tokens(remove_punct = TRUE)
tokens

text_dfm <- c(
  t1 = readLines('/Users/barnabasepres/University/TDK/trump_speech_analysis/text_files/speech_1_"2024-06-06"_AZ.txt'),
  t2 = readLines('/Users/barnabasepres/University/TDK/trump_speech_analysis/text_files/speech_2_"2024-06-09"_NV.txt')
)

dfm <- text_dfm %>% 
  tokens(
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = TRUE
  ) %>% 
  tokens_remove(pattern = stopwords("english")) %>% 
  tokens_tolower() %>% 
  tokens_wordstem(language = "english") %>% 
  dfm()

dfm2