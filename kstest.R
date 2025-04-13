setwd("~/University/TDK/trump_speech_analysis/trump-speech-analysis/")

df <- read.csv("df_for_model_final.csv")
str(df)

df$proba <- 1:nrow(df)

ks.test(df$CTTR, "punif")

ks.test(df$CTTR, "punif", min(df$CTTR), max(df$CTTR))

# For plotting
p <- ggplot(aes(x), data = dd) +
  stat_ecdf() +
  stat_function(fun = punif, args = list(min(x), max(x))) +
  theme_bw() +
  labs(title = "ECDF and theoretical CDF") +
  geom_vline(xintercept = maxdiffat, lty = 2)

p
