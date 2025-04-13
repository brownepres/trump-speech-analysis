setwd("~/University/TDK/trump_speech_analysis/extra")
library(dplyr)
library(quanteda)
library(quanteda.textstats)


text_for_dtm <- c()
text_for_dtm <- c(
  paste(readLines('/Users/barnabasepres/University/TDK/trump_speech_analysis/extra/biden_speech.txt'), collapse=" "),
  paste(readLines('/Users/barnabasepres/University/TDK/trump_speech_analysis/extra/obama_speech.txt'), collapse = " ")
  )

dtm <- text_for_dtm %>% 
  tokens(
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = TRUE
  ) %>% 
  tokens_remove(pattern = stopwords("english")) %>% 
  tokens_tolower() %>% 
  tokens_wordstem(language = "english") %>% 
  dfm()

dtm

#calculate lexical diversity
lexical_diversity <- dtm %>% 
  quanteda.textstats::textstat_lexdiv(measure = "all") %>% 
  dplyr::arrange(dplyr::desc(CTTR))

lexical_diversity




