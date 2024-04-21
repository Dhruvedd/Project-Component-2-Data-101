# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load your data
data <- read.csv("onlinefoods.csv")

# Convert Feedback to binary if it's not already
data$FeedbackBinary <- ifelse(data$Feedback == "Positive", 1, 0)

# Function to calculate and plot the ratio of positive to negative feedbacks for a given category
plot_feedback_ratio <- function(data, category) {
  data <- data %>%
    group_by(!!sym(category)) %>%
    mutate(PositiveNegativeRatio = sum(FeedbackBinary == 1) / sum(FeedbackBinary == 0)) %>%
    ungroup()
  
  ggplot(data, aes_string(x = category, y = "PositiveNegativeRatio")) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(x = category, y = "Positive vs. Negative Feedback Ratio") +
    theme_minimal()
}

# List of categories to plot
categories <- c("Age", "Gender", "Marital.Status", "Occupation", "Monthly.Income", "Educational.Qualifications", "Family.size", "latitude", "longitude", "Pin.code", "Output")

# Plot for each category

plot_feedback_ratio(data, categories[11])
plot_feedback_ratio(data, categories[1])
plot_feedback_ratio(data, categories[2])
plot_feedback_ratio(data, categories[3])
plot_feedback_ratio(data, categories[4])
plot_feedback_ratio(data, categories[4])
plot_feedback_ratio(data, categories[5])
plot_feedback_ratio(data, categories[6])
plot_feedback_ratio(data, categories[7])


