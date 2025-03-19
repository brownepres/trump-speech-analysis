setwd("~/University/TDK/trump_speech_analysis")
install.packages("text")
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textstats)
library(readxl)
library(ggdendro)
library(word2vec)
library(text)
library(dplyr)


meta_data <- read_excel("speeches.xlsx")
text_for_dtm <- c()
for (i in 1:74){
  if(i == 8){
    next
  }
  text_for_dtm <- c(text_for_dtm,
                    readLines(sprintf('/Users/barnabasepres/University/TDK/trump_speech_analysis/text_files/speech_%s_%s_%s.txt', 
                                      i, meta_data$date[i], meta_data$state[i])))
}

dtm_to_train <- text_for_dtm %>% 
  tokens(
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = TRUE
  ) %>% 
  tokens_remove(pattern = stopwords("english")) %>% 
  tokens_tolower() %>% 
  tokens_wordstem(language = "english")

token_train <- lapply(dtm_to_train, as.character)


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

dtm_tfidf <- quanteda::dfm_tfidf(dtm)
dtm_tfidf_matrix <- as.matrix(dtm_tfidf)

get_top_words <- function(tfidf_row, top_n = 5) {
  sorted_indices <- order(tfidf_row, decreasing = TRUE)[1:top_n]  # Get indices of top scores
  top_words <- colnames(dtm_tfidf_matrix)[sorted_indices]  # Get words
  top_scores <- tfidf_row[sorted_indices]  # Get TF-IDF scores
  
  return(data.frame(word = top_words, score = top_scores))  # Return both
}

# Extract top 5 words for each document
top_words_list <- apply(dtm_tfidf_matrix, 1, get_top_words)
top_words_df <- do.call(rbind, lapply(seq_along(top_words_list), function(i) {
  cbind(textid = i, top_words_list[[i]])
}))

#train word2vec model
word2vec_model <- word2vec(text_for_dtm, type = "skip-gram", dim = 300, iter = 20)

output_dir <- "~/University/TDK/trump_speech_analysis"

# Train the Word2Vec model and specify the output directory
word2vec_model <- word2vec(
  token_train,
  type = "skip-gram",
  dim = 300,
  iter = 20,
  output_dir = output_dir
)

saveRDS(word2vec_model, file = file.path(output_dir, "word2vec_model.rds"))
loaded_model <- readRDS(file.path(output_dir, "word2vec_model.rds"))
word_vectors <- loaded_model$vectors

# Load the word vectors from the specified directory
word_vectors_path <- file.path(output_dir, "word_vectors.txt") 

get_word_vector <- function(word, model) {
  if (word %in% rownames(model$vectors)) {
    return(model$vectors[word, ])
  } else {
    return(NA)  # or handle missing words as needed
  }
}

top_words_df$word_vectors <- apply(top_words_df, 1,
                                   function(row) get_word_vector(row['word'], word2vec_model))


words_in_model <- rownames(word2vec_model$vectors)
words_in_df <- top_words_df$word
missing_words <- setdiff(words_in_df, words_in_model)
print(missing_words)

write.csv(top_words_df, "top_word_df.csv")


write.csv(text_for_dtm, "text_for_dtm.csv")
