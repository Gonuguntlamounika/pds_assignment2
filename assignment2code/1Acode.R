
library("ggplot2")
diabetes<-read.csv("diabetes.csv")

set.seed(91)

sample <- diabetes[sample(nrow(diabetes), 25, replace = FALSE), ]

sample_mean <- mean(sample$Glucose)
sample_max <- max(sample$Glucose)
population_mean <- mean(diabetes$Glucose)
population_max <- max(diabetes$Glucose)

glucose_values <- c(sample_mean, sample_max, population_mean, population_max)
x_labels <- c("Sample Mean", "Sample Max", "Population Mean", "Population Max")
glucose_df <- data.frame(x_labels,glucose_values )
dev.off()
ggplot(glucose_df, aes(x = x_labels, y = glucose_values, fill = x_labels)) + 
  geom_bar(stat = "identity") +
  labs(title = "Comparison of Glucose Statistics between Sample and Population", y = "Glucose Value") +
  theme_bw()


