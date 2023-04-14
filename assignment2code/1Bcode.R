
library("ggplot2")
diabetes<-read.csv("diabetes.csv")

set.seed(91)

sample <- diabetes[sample(nrow(diabetes), 25, replace = FALSE), ]
sample_bmi_98_percentile <- quantile(sample$BMI, 0.98)
population_bmi_98_percentile <- quantile(df$BMI, 0.98)
bmi_values <- c(sample_bmi_98_percentile, population_bmi_98_percentile)
x_labels <- c("Sample 98th Percentile", "Population 98th Percentile")
bmi_df <- data.frame(bmi_values, x_labels)
dev.off()
ggplot(bmi_df, aes(x = x_labels, y = bmi_values, fill = x_labels)) +
  geom_bar(stat = "identity") +
  labs(title = "Comparison of BMI 98th Percentile between Sample and Population", y = "BMI Value") +
  theme_bw()