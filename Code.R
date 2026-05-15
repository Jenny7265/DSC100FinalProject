# load packages
library(tidyverse)      # data wrangling
library(janitor)        # clean names
library(gt)             # pretty tables
library(gtsummary)      # summary stats
library(ggplot2)        # visualizations
library(corrplot)       # correlations
library(scales)         # formatting

# import dataset
food <- read_csv("branded_food.csv")

# clean columns
food <- food %>%
  clean_names()

# remove missing
food_clean <- food %>%
  filter(
    !is.na(calories),
    !is.na(serving_size),
    serving_size > 0
  )

# normalize calories
food_clean <- food_clean %>%
  mutate(
    calories_per_100g = (calories / serving_size) * 100
  )

# serving ratio
food_clean <- food_clean %>%
  mutate(
    serving_ratio = serving_size / 100
  )

# calorie gap
food_clean <- food_clean %>%
  mutate(
    calorie_difference = calories_per_100g - calories
  )

# sugar density
food_clean <- food_clean %>%
  mutate(
    sugar_density = (added_sugars_g / serving_size) * 100
  )

# protein density
food_clean <- food_clean %>%
  mutate(
    protein_density = (protein_g / serving_size) * 100
  )

# fat density
food_clean <- food_clean %>%
  mutate(
    fat_density = (total_fat_g / serving_size) * 100
  )

# remove outliers
food_filtered <- food_clean %>%
  filter(
    calories_per_100g < 3000,
    serving_size < 1000,
    added_sugars_g < 300
  )

# summary stats
summary(food_filtered)

# grouped summaries
category_summary <- food_filtered %>%
  group_by(branded_food_category) %>%
  summarise(
    avg_calories = mean(calories, na.rm = TRUE),
    avg_density = mean(calories_per_100g, na.rm = TRUE),
    avg_serving = mean(serving_size, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(desc(avg_density))

# top calorie dense
top_dense <- food_filtered %>%
  arrange(desc(calories_per_100g)) %>%
  select(description, branded_food_category, calories_per_100g) %>%
  slice_head(n = 10)

# low serving high density
misleading_foods <- food_filtered %>%
  arrange(desc(calorie_difference)) %>%
  select(description, serving_size, calories, calories_per_100g) %>%
  slice_head(n = 10)

# weird foods
weird_foods <- food_filtered %>%
  mutate(
    weird_score = calories_per_100g / serving_size
  ) %>%
  arrange(desc(weird_score)) %>%
  slice_head(n = 10)

# correlation test
cor.test(
  food_filtered$serving_size,
  food_filtered$calories_per_100g
)

# sugar correlation
cor.test(
  food_filtered$added_sugars_g,
  food_filtered$calories_per_100g
)

# scatter plot
ggplot(food_filtered,
       aes(x = serving_size,
           y = calories_per_100g)) +
  geom_point(alpha = 0.3) +
  labs(
    title = "Serving Size vs Density",
    x = "Serving Size",
    y = "Calories per 100g"
  )

# histogram plot
ggplot(food_filtered,
       aes(x = calories_per_100g)) +
  geom_histogram(bins = 50) +
  labs(
    title = "Density Distribution",
    x = "Calories per 100g"
  )

# boxplot categories
ggplot(food_filtered,
       aes(x = branded_food_category,
           y = calories_per_100g)) +
  geom_boxplot() +
  coord_flip()

# density plot
ggplot(food_filtered,
       aes(x = calories_per_100g)) +
  geom_density(fill = "lightblue") +
  labs(title = "Density Curve")

# correlation matrix
numeric_data <- food_filtered %>%
  select(
    calories,
    calories_per_100g,
    serving_size,
    added_sugars_g,
    protein_g,
    total_fat_g
  )

corrplot(cor(numeric_data,
             use = "complete.obs"))

# formatted summary
food_filtered %>%
  select(
    calories,
    calories_per_100g,
    serving_size,
    added_sugars_g
  ) %>%
  tbl_summary()

# gt summary table
category_summary %>%
  gt()

# export csv
write_csv(category_summary,
          "category_summary.csv")

# export cleaned
write_csv(food_filtered,
          "cleaned_food_data.csv")
