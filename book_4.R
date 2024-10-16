# Load the necessary libraries
#install.packages('tidyverse')
library(tidyverse)

# Step 1: Read the CSV files with explicit encoding
books <- read_csv("books (1).csv", locale = locale(encoding = "UTF-8"))
ratings_pga <- read_csv("RatingPGA (1).csv", locale = locale(encoding = "UTF-8")) %>%
  select(-1)
ratings_pgb <- read_csv("RatingPGB (1).csv", locale = locale(encoding = "UTF-8")) %>%
  select(-1)
users <- read_csv("users (1).csv", locale = locale(encoding = "UTF-8"))

# Remove rows where Age is NA in users dataset
users <- users %>%
  filter(!is.na(Age))

# Step 2: Filter books published after 2000
books_filtered <- books %>%
  filter(`Year-Of-Publication` > 2000)


# Step 3: Join the PGA dataset with books and users
book_ratings_pga <- ratings_pga %>%
  inner_join(books_filtered, by = "ISBN") %>%
  inner_join(users, by = c("User" = "User-ID"))

# Step 4: Join the PGB dataset with books and users
book_ratings_pgb <- ratings_pgb %>%
  inner_join(books_filtered, by = "ISBN") %>%
  inner_join(users, by = c("User" = "User-ID"))

# Step 5: Create more detailed age groups for PGA dataset
book_ratings_pga <- book_ratings_pga %>%
  mutate(Age_Group = case_when(
    Age < 13 ~ "Under 13",
    Age >= 13 & Age <= 17 ~ "13-17",
    Age >= 18 & Age <= 24 ~ "18-24",
    Age >= 25 & Age <= 30 ~ "25-30",
    Age >= 31 & Age <= 40 ~ "31-40",
    Age >= 41 & Age <= 50 ~ "41-50",
    Age >= 51 & Age <= 60 ~ "51-60",
    Age > 60 ~ "Above 60",
    TRUE ~ "Unknown"
  ))


# Step 6: Create more detailed age groups for PGB dataset
book_ratings_pgb <- book_ratings_pgb %>%
  mutate(Age_Group = case_when(
    Age < 13 ~ "Under 13",
    Age >= 13 & Age <= 17 ~ "13-17",
    Age >= 18 & Age <= 24 ~ "18-24",
    Age >= 25 & Age <= 30 ~ "25-30",
    Age >= 31 & Age <= 40 ~ "31-40",
    Age >= 41 & Age <= 50 ~ "41-50",
    Age >= 51 & Age <= 60 ~ "51-60",
    Age > 60 ~ "Above 60",
    TRUE ~ "Unknown"
  ))


# Step 7: Calculate average rating per age group for PGA dataset
age_group_ratings_pga <- book_ratings_pga %>%
  group_by(Age_Group) %>%
  summarise(Average_Rating_PGA = mean(Book.Rating, na.rm = TRUE))

# Step 8: Calculate average rating per age group for PGB dataset
age_group_ratings_pgb <- book_ratings_pgb %>%
  group_by(Age_Group) %>%
  summarise(Average_Rating_PGB = mean(Book.Rating, na.rm = TRUE))

# Step 9: Combine the two results for comparison
comparison_ratings <- age_group_ratings_pga %>%
  inner_join(age_group_ratings_pgb, by = "Age_Group")

# Step 10: Plot the comparison of PGA and PGB ratings
comparison_ratings_long <- comparison_ratings %>%
  pivot_longer(cols = c(Average_Rating_PGA, Average_Rating_PGB), 
               names_to = "Dataset", 
               values_to = "Average_Rating")

ggplot(comparison_ratings_long, aes(x = Age_Group, y = Average_Rating, fill = Dataset)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(
    title = "Comparison of Average Book Ratings by Age Group (PGA vs PGB)",
    x = "Age Group",
    y = "Average Rating"
  )







