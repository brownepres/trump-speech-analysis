setwd("~/University/TDK/trump_speech_analysis")

df <- read_csv("jaccard_similarity.csv")
str(df)

df$id <- 1:nrow(df)
ggplot(df, aes(x=id)) + geom_histogram(aes(y=jaccard))
ggplot(df, aes(x = jaccard)) +
  geom_histogram(bins = 20, fill = "white", color = "black") +
  theme_bw() +
  labs(
    x = "Jaccard-similarity score",
    y = "Number of speech-pairs"
  ) +
  theme(
    text = element_text(size = 12, family = "serif"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
