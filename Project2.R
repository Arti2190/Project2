# Project 2
#Get the data
#install.packages("ggside")
#library(ggside)
#install.packages("ggpubr")
#install.packages("viridis")
library(viridis)
library(ggpubr)
library(dplyr)
library(tidyverse)
library(readxl)
library(tidyr)
library(ggplot2)

# Load the Data 
super_data <- read_excel("US_Superstore_data.xls")
head(super_data)
View(super_data)
names(super_data) <- make.names(names(super_data))
head(super_data)

# check is there any missing data
sum_na <- function(...) {
  sum(is.na(...))
}
na_counts <- super_data %>%
  summarize(across(everything(), sum_na))
print(na_counts)
#colSums(is.na(super_data)) # there is no missing data


# Produce numerical and graphical summaries to investigate the data
#  One-way 
# Frequency of each segment
#----table(super_data$Segment)
#Frequency of each category
#------table(super_data$Category)

super_data |>
  group_by(Segment) |>
  summarize(count = n())

#Two-way contingency tables
# cross tabulation of segment and category
#-----table(super_data$Segment,super_data$Category)

super_data |>
  group_by(Segment, Category) |>
  summarize(count = n()) |>
  pivot_wider(names_from = Category, values_from = count)



# Numerical summaries (means, medians, sds, etc.) for quantitative variables at levels of categorical variables
# summarize the center and spread of Sales and Profit
summary_stats <- super_data |>
  group_by(State,Category) |>
  summarize(
    across(c(Sales, Profit), .fns = list("mean" = mean,
                             "median" = median,
                             "var" = var,
                             "sd" = sd,
                             "IQR" = IQR), .names = "{.fn}_{.col}")
           )
print(summary_stats)

    
# Create at least six plots.    
    
# check total sales per category for each segment
# 1. Bar plot
g_bar <- ggplot(
  super_data, aes(x = Category, fill = Segment)) +
  geom_bar(position = "dodge") +
  labs(title = "Total sales by Category and Segment",
       x = "Category",
       y = "Total Sales") +
  theme_minimal()
print(g_bar)

#2. Box Plot
# Distribution of Profit by category and segment 
g_box <- ggplot(
  super_data, aes(x = Category, y = Profit, fill = Segment)) +
  geom_boxplot() +
  labs(title = "Profit Distribution by Category and Segment",
       x = "Category",
       y = "Profit") 
print(g_box) +
  theme_minimal()

# 3. Scatter Plot
# scatter Plot of sales versus points,:- points colored by segment and smoothing line

g_scatter <- ggplot(super_data, aes(x = Sales, y = Profit, color = Segment)) +
  geom_jitter(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~Segment)
  labs(title = "Sales vs. Profit ",
       x = "Sales",
       y = "Profit") + 
  theme_minimal()

print(g_scatter) 

# 4. Faceted Scatter Plot
# Faceted scatter Plot of sales vs. Profit, faceted by segment

ggplot(super_data, aes(x = Sales, y = Profit)) +
  geom_point(aes(color = Category), alpha = 0.6) +
  facet_wrap(~ Segment) +
  labs(title = "Sales vs. Profit by Segment",
       x = "Sales",
       y = "Profit") +
  theme_minimal()


# 5. Create a 

# Create a bivariate density plot
# Create a bivariate density plot
ggplot(super_data, aes(x = Sales, y = Profit)) +
  geom_density_2d_filled(aes(fill = after_stat(level)), alpha = 0.7) +  # Use after_stat(level)
  scale_fill_viridis_c(option = "A") +  # Use continuous color scale
  labs(title = "Bivariate Density Plot of Sales and Profit",
       x = "Sales",
       y = "Profit",
       fill = "Density Level") +
  theme_pubr() +  # Apply a publication-ready theme
  theme(legend.position = "right")  # Position the legend
