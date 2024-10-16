# Read CSV files with explicit encoding
books <- read_csv("books (1).csv", locale = locale(encoding = "UTF-8"))
ratings_pga <- read_csv("RatingPGA (1).csv", locale = locale(encoding = "UTF-8")) %>%
  select(-1)
ratings_pgb <- read_csv("RatingPGB (1).csv", locale = locale(encoding = "UTF-8")) %>%
  select(-1)
users <- read_csv("users (1).csv", locale = locale(encoding = "UTF-8"))


# Combine the rating data (if you want to use both PGA and PGB)
ratings <- rbind(ratings_pga, ratings_pgb)

# Merge ratings with users data to add user information
merged_data <- merge(ratings, users, by.x = "User", by.y = "User-ID")


# Remove users with missing age values
merged_data <- merged_data[!is.na(merged_data$Age), ]

# Categorize age into groups: 0-18, 19-30, 31-45, 46-60, 61+
age_groups <- cut(merged_data$Age, breaks = c(-Inf, 18, 30, 45, 60, Inf), 
                  labels = c("0-18", "19-30", "31-45", "46-60", "61+"), right = FALSE)

# Add age groups to the merged data
merged_data$Age.Group <- age_groups

# Plot boxplot for Age Group vs Book Rating
boxplot(merged_data$Book.Rating ~ merged_data$Age.Group,
        xlab = "Age Group", ylab = "Book Rating",
        main = "Distribution of Book Ratings by Age Groups")
