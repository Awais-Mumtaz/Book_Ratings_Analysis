# Load necessary libraries
library(dplyr)
library(readr)

# Read CSV files with explicit encoding
books <- read_csv("books (1).csv", locale = locale(encoding = "UTF-8"))
ratings_pga <- read_csv("RatingPGA (1).csv", locale = locale(encoding = "UTF-8")) %>%
  select(-1)
ratings_pgb <- read_csv("RatingPGB (1).csv", locale = locale(encoding = "UTF-8")) %>%
  select(-1)
users <- read_csv("users (1).csv", locale = locale(encoding = "UTF-8"))

# Combine the two rating files
ratings <- bind_rows(ratings_pga, ratings_pgb)

# Merge the ratings data with books data using ISBN as the common key
ratings_books <- ratings %>%
  inner_join(books, by = "ISBN")



# Check for unique publisher names before applying iconv
unique_publishers_before <- unique(ratings_books$Publisher)
print(unique_publishers_before)






# Group by Publisher and calculate the average rating
publisher_avg_ratings <- ratings_books %>%
  group_by(Publisher) %>%
  summarise(Average_Rating = mean(Book.Rating, na.rm = TRUE)) %>%
  arrange(desc(Average_Rating))

# Display the result
print(publisher_avg_ratings)


# Select the top 20 publishers by average rating
top_20_publishers <- publisher_avg_ratings %>%
  slice_max(Average_Rating, n = 20, with_ties = FALSE)

# Display the top 20 publishers in a neat table using kableExtra
top_20_publishers %>%
  kable("html", col.names = c("Publisher", "Average Rating")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)





# Load necessary libraries
library(dplyr)
library(ggplot2)

# Count the number of ratings for each publisher and calculate the average rating
top_publishers_with_most_ratings <- ratings_books %>%
  group_by(Publisher) %>%
  summarise(
    Average_Rating = mean(Book.Rating, na.rm = TRUE),
    Rating_Count = n()
  ) %>%
  arrange(desc(Rating_Count)) %>%
  slice_max(Rating_Count, n = 10)

# Plot the average rating for the top 10 publishers with the most ratings
ggplot(top_publishers_with_most_ratings, aes(x = reorder(Publisher, Average_Rating), y = Average_Rating)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(
    title = "Top 10 Publishers with Most Ratings - Average Rating",
    x = "Publisher",
    y = "Average Rating"
  ) +
  theme_minimal()
