# Load necessary libraries
library(dplyr)
library(ggplot2)
library(knitr)

data <- read.csv("C:/Users/ADMIN/Desktop/Projects/amazon2/supermarket.csv")

data

names(data)

# Checking for missing values
missing_values <- sum(is.na(data))
print(missing_values)

# Statistics
summary_stats <- data %>%
  summarise(
    Total_Sales = sum(Total),
    Average_Rating = mean(Rating, na.rm = TRUE),
    Total_Quantity = sum(Quantity)
  )

print(summary_stats)

# Sales Across Branches (ANOVA)

anova_results <- aov(Total ~ Branch, data = data)
summary(anova_results)

# Customer Type Impact on Sales (t-test)

t_test_results <- t.test(Total ~ Customer.type, data = data)
print(t_test_results)

# Ratings by Gender (t-test)

t_test_gender <- t.test(Rating ~ Gender, data = data)
print(t_test_gender)

# Data Visualization

# Sales by branch visualization
ggplot(data, aes(x = Branch, y = Total, fill = Branch)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Sales by Branch", x = "Branch", y = "Total Sales") +
  theme_minimal()

# Average rating by gender visualization
ggplot(data, aes(x = Gender, y = Rating, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Average Rating by Gender", x = "Gender", y = "Rating") +
  theme_minimal()

