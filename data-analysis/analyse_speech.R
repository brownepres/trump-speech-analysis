setwd("~/University/TDK/trump_speech_analysis")
install.packages("quanteda.textstats")
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textstats)
library(readxl)
library(ggdendro)

#create document term matrix
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
#text_for_dtm[31]

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
top_words <- quanteda::topfeatures(dtm, 20)
write.csv(top_words, "top_words.csv")


dtm_tfidf <- quanteda::dfm_tfidf(dtm)
dtm_tfidf
dtm_tfidf_matrix <- as.matrix(dtm_tfidf)

get_top_words <- function(tfidf_row, top_n = 5) {
  sorted_indices <- order(tfidf_row, decreasing = TRUE)[1:top_n]
  return((tfidf_row)[sorted_indices])
}

# Extract top 5 words for each document
top_words <- apply(dtm_tfidf_matrix, 1, get_top_words)

# Print top words
top_word_matrix <- as.matrix(top_words)
as.vector(top_word_matrix[1:5,1])

tfidf_feature_vectors <- data.frame(docid = integer(0), feature_vector = list())
  
for (i in 1:73){
  new_row <- data.frame(docid = paste("text", i), 
                        feature_vector = I(list(as.vector(top_word_matrix[1:5, i]))))
  
  # Append the row to the data frame
  tfidf_feature_vectors <- rbind(tfidf_feature_vectors, new_row)
}
str(tfidf_feature_vectors$feature_vector)
write.csv(tfidf_feature_vectors, "tfidf_feature_vector.csv")

#descriptive statistics #################
#scale with wordscores model

text_for_dtm_scale <- c(read_lines('/Users/barnabasepres/University/TDK/trump_speech_analysis/kennedy_speech.txt'),
                        read_lines('/Users/barnabasepres/University/TDK/trump_speech_analysis/bush_speech.txt'))
text_for_dtm_scale <- c(text_for_dtm, text_for_dtm_scale)
#kennedy: left, -1
#bush: right, 1

dtm_scale <- text_for_dtm_scale %>% 
  tokens(
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = TRUE
  ) %>% 
  tokens_remove(pattern = stopwords("english")) %>% 
  tokens_tolower() %>% 
  tokens_wordstem(language = "english") %>% 
  dfm()

docvars(dtm_scale, "reference") <- NA
docvars(dtm_scale, "reference")[length(dtm_scale$reference)] <- 1
docvars(dtm_scale, "reference")[length(dtm_scale$reference)-1] <- -1

wordscores <- textmodel_wordscores(
  x=dtm_scale, 
  y=docvars(dtm_scale, "reference"),
  scale="linear",
  smooth=0
)

summary(wordscores, 10)
wordscores_predict <- predict(
  wordscores, 
  newdata = dtm_scale,
  interval = "confidence")

wordscores_predict <- as.data.frame(wordscores_predict$fit)
wordscores_predict
write.csv(wordscores_predict, "wordscores.csv")


#lexical diversity
lexical_diversity <- dtm %>% 
  quanteda.textstats::textstat_lexdiv(measure = "all") %>% 
  dplyr::arrange(dplyr::desc(CTTR))

write.csv(lexical_diversity, "lexical_diversity.csv")

#jaccard similarity score
jaccard_similarity <- dtm %>% 
  dfm_weight("prop") %>% 
  quanteda.textstats::textstat_simil(margin = "documents", method="jaccard")

write.csv(jaccard_similarity, "jaccard_similarity.csv")

jaccard_similarity

#distances between documents
dist <- dtm %>% 
  textstat_dist(margin = "documents", method = "euclidean")
hierarchical_cluster <- hclust(as.dist(dist))

ggdendro::ggdendrogram(hierarchical_cluster)


#context of the most used words
text_for_dtm %>% 
  tokens() %>% 
  quanteda::kwic(
    pattern = "glori*", 
    valuetype = "glob",
    window = 3, 
    case_insensitive = TRUE
  ) %>%
  head(5)

#using lexicoder for political sentiment analysis
tokens <- text_for_dtm %>% 
  tokens(
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = TRUE
  )

#stopwords are not removed for senitment scores
sentiment_scores <- tokens_lookup(tokens, dictionary = data_dictionary_LSD2015) %>%
  dfm()

sentiment_scores

sentiment_scores_ratio <- convert(sentiment_scores, to = "data.frame")
sentiment_scores_ratio$sum <- sentiment_scores_ratio$negative + sentiment_scores_ratio$positive +
  sentiment_scores_ratio$neg_positive + sentiment_scores_ratio$neg_negative

sentiment_scores_ratio$negative <- sentiment_scores_ratio$negative / sentiment_scores_ratio$sum
sentiment_scores_ratio$positive <- sentiment_scores_ratio$positive / sentiment_scores_ratio$sum
sentiment_scores_ratio$neg_positive <- sentiment_scores_ratio$neg_positive / sentiment_scores_ratio$sum
sentiment_scores_ratio$neg_negative <- sentiment_scores_ratio$neg_negative / sentiment_scores_ratio$sum

write.csv(sentiment_scores_ratio, "sentiment_scores_ratio.csv")
<<<<<<< HEAD
=======



100000/sqrt(200000)



>>>>>>> 19a6f92 (update modelling)
