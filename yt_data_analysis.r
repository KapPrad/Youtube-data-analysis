# Install necessary packages if not already installed
if (!require("tidyr")) install.packages("tidyr")
if (!require("corrplot")) install.packages("corrplot")
if (!require("moments")) install.packages("moments")

# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(moments)

# Load the dataset
data <- read.csv("path-to-your-excel.csv")

# Map category_id to descriptive category names
category_mapping <- c("1"="Film & Animation", "2"="Autos & Vehicles", "10"="Music", 
                      "15"="Pets & Animals", "17"="Sports", "18"="Short Movies", 
                      "19"="Travel & Events", "20"="Gaming", "21"="Videoblogging", 
                      "22"="People & Blogs", "23"="Comedy", "24"="Entertainment", 
                      "25"="News & Politics", "26"="Howto & Style", "27"="Education", 
                      "28"="Science & Technology", "30"="Movies", "31"="Anime/Animation", 
                      "32"="Action/Adventure", "33"="Classics", "34"="Comedy", 
                      "35"="Documentary", "36"="Drama", "37"="Family", "38"="Foreign", 
                      "39"="Horror", "40"="Sci-Fi/Fantasy", "41"="Thriller", "42"="Shorts", 
                      "43"="Shows", "44"="Trailers")

# Add category_name column
data$category_name <- category_mapping[as.character(data$category_id)]

# Drop rows with missing values in views, likes, dislikes, or comment_count
clean_data <- data %>% drop_na(views, likes, dislikes, comment_count)

# Basic statistics for views, likes, dislikes, and comment count
summary_stats <- clean_data %>%
  summarise(across(c(views, likes, dislikes, comment_count), list(mean=mean, median=median, sd=sd)))
print(summary_stats)

# Skewness and Kurtosis (using moments library)
skewness_vals <- sapply(clean_data[c("views", "likes", "dislikes", "comment_count")], skewness)
kurtosis_vals <- sapply(clean_data[c("views", "likes", "dislikes", "comment_count")], kurtosis)

print(skewness_vals)
print(kurtosis_vals)

# Correlation matrix for views, likes, dislikes, and comment count
cor_matrix <- cor(clean_data[c("views", "likes", "dislikes", "comment_count")])

# Plot the correlation matrix
corrplot(cor_matrix, method = "color", type = "upper", addCoef.col = "black")

# Simple linear regression of views on likes
model <- lm(views ~ likes, data = clean_data)
summary(model)

# Plotting the regression
ggplot(clean_data, aes(x = likes, y = views)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, color = "blue") +
  labs(title = "Views vs. Likes Regression", x = "Likes", y = "Views")

# Multiple regression: views as a function of likes, dislikes, and comment_count
multi_model <- lm(views ~ likes + dislikes + comment_count, data = clean_data)
summary(multi_model)

# Diagnostics for multiple regression
par(mfrow = c(2, 2))
plot(multi_model)

# Histogram for views
ggplot(clean_data, aes(x = views)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Views", x = "Views", y = "Frequency")

# Boxplot comparing views across categories
ggplot(clean_data, aes(x = category_name, y = views)) +
  geom_boxplot() +
  labs(title = "Views Distribution Across Categories", x = "Category", y = "Views") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Scatterplot for views vs likes by category
ggplot(clean_data, aes(x = likes, y = views, color = category_name)) +
  geom_point(alpha = 0.7) +
  labs(title = "Views vs Likes by Category", x = "Likes", y = "Views") +
  theme(legend.position = "bottom")

# One-way ANOVA to compare mean views across different categories
anova_model <- aov(views ~ category_name, data = clean_data)
summary(anova_model)

# Density Plot of Views by Category
ggplot(clean_data, aes(x = views, fill = factor(category_id))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Views by Category", x = "Views", fill = "Category ID") +
  theme_minimal()

# Scatter Plot of Views vs Dislikes
ggplot(clean_data, aes(x = views, y = dislikes, color = factor(category_id))) +
  geom_point(alpha = 0.6) +
  labs(title = "Scatter Plot of Views vs Dislikes", x = "Views", y = "Dislikes", color = "Category ID") +
  theme_minimal()
