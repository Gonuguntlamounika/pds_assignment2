
library("ggplot2")

# load the diabetes dataset
diabetes <- read.csv("diabetes.csv")

# set a seed for reproducibility
set.seed(91)

# create a function to calculate mean, standard deviation, and percentile of BloodPressure
bp_stats <- function(diabetesdata) {
  mean_bp <- mean(diabetesdata$BloodPressure)
  sd_bp <- sd(diabetesdata$BloodPressure)
  percentile_bp <- quantile(diabetesdata$BloodPressure, probs = c(0.25, 0.5, 0.75))
  return(c(mean_bp, sd_bp, percentile_bp))
}

# create 500 bootstrap samples of 150 observations each
bootstrap_samples <- lapply(1:500, function(x) {
  sample <- diabetes[sample.int(nrow(diabetes), size = 150, replace = TRUE), ]
  return(bp_stats(sample))
})

# calculate the mean, standard deviation, and percentile of BloodPressure for the population
population_stats <- bp_stats(diabetes)

# calculate the average mean, standard deviation, and percentile of BloodPressure across the bootstrap samples
bootstrap_mean <- colMeans(do.call(rbind, bootstrap_samples))
bootstrap_sd <- apply(do.call(rbind, bootstrap_samples), 2, sd)
bootstrap_percentile <- t(apply(do.call(rbind, bootstrap_samples), 2, quantile, probs = c(0.25, 0.5, 0.75)))

# create a bar chart to compare the mean BloodPressure values
mean_data <- data.frame(
  type = c("Bootstrap Mean", "Population Mean"),
  bp = c(bootstrap_mean[1], population_stats[1])
)
dev.off()
ggplot(mean_data, aes(x = type, y = bp, fill = type)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8) +
  theme_classic() +
  ggtitle("Comparison of Mean BloodPressure Values") +
  xlab("") +
  ylab("BloodPressure")

# create a bar chart to compare the standard deviation of BloodPressure
sd_data <- data.frame(
  type = c("Bootstrap SD", "Population SD"),
  bp = c(bootstrap_sd[2], population_stats[2])
)
dev.off()
ggplot(sd_data, aes(x = type, y = bp, fill = type)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8) +
  theme_classic() +
  ggtitle("Comparison of Standard Deviation of BloodPressure") +
  xlab("") +
  ylab("BloodPressure")

# create a box plot to compare the percentile of BloodPressure
percentile_data <- data.frame(
  type = rep(c("Bootstrap", "Population"), each = 3),
  percentile = c(bootstrap_percentile[,1], population_stats[3][1],
                 bootstrap_percentile[,2], population_stats[3][2],
                 bootstrap_percentile[,3], population_stats[3][3])
)
dev.off()
ggplot(percentile_data, aes(x = type, y = percentile, fill = type)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0) +
  theme_classic() +
  ggtitle("Comparison of Percentile of BloodPressure") +
  xlab("") +
  ylab("BloodPressure")