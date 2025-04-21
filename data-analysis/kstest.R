setwd("~/University/TDK/trump_speech_analysis/trump-speech-analysis/")

df <- read.csv("df_for_model_final.csv")
str(df)

df$proba <- 1:nrow(df)

ks.test(df$CTTR, "punif")

ks.test(df$CTTR, "punif", min(df$CTTR), max(df$CTTR))

# For plotting
p <- ggplot(aes(x), data = df) +
  stat_ecdf() +
  stat_function(fun = punif, args = list(min(x), max(x))) +
  theme_bw() +
  labs(title = "ECDF and theoretical CDF") +
  geom_vline(xintercept = maxdiffat, lty = 2)

p
x <- df$CTTR

# Calculate the point of maximum difference (optional)
ecdf_vals <- ecdf(x)(x)
uniform_vals <- punif(x, min(x), max(x))
maxdiffat <- x[which.max(abs(ecdf_vals - uniform_vals))]

# Plot
p <- ggplot(data = df, aes(x = CTTR)) +
  stat_ecdf(geom = "step", color = "black") +
  stat_function(fun = punif, args = list(min = min(x), max = max(x)), 
                color = "grey50", linetype = "dashed") +
  geom_vline(xintercept = maxdiffat, linetype = 2) +
  theme_bw() +
  labs(
    title = "ECDF vs Uniform CDF",
    x = "Corrected lexical diversity",
    y = "Cumulative Probability"
  )

p

hist(x)
shapiro.test(x)

ggplot(df, aes(x = CTTR)) +
  geom_histogram(binwidth = 0.3, fill = "white", color = "black") +
  theme_bw(base_size = 12) +
  labs(x = "CTTR (Corrected Type-Token Ratio)", y = "Frequency") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


t.test(df$CTTR, mu = 13.2)


