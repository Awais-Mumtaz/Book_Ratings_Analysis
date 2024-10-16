# Read CSV files with explicit encoding
books <- read_csv("books (1).csv", locale = locale(encoding = "UTF-8"))
ratings_pga <- read_csv("RatingPGA (1).csv", locale = locale(encoding = "UTF-8")) %>%
  select(-1)
ratings_pgb <- read_csv("RatingPGB (1).csv", locale = locale(encoding = "UTF-8")) %>%
  select(-1)
users <- read_csv("users (1).csv", locale = locale(encoding = "UTF-8"))

# Load necessary library
library(tidyr)
# Load necessary library
library(dplyr)
library(stringr)  # This is the missing library

# Remove consecutive commas in the 'Location' column
users$Location <- gsub(",{2,}", ",", users$Location)



# Function to count commas in each cell
count_commas <- function(location) {
  return(str_count(location, ","))
}

# Apply the comma count function to the 'location' column
users <- users %>%
  mutate(comma_count = count_commas(Location))

# Function to clean 'Location' by removing extra part if more than two commas
clean_location <- function(location) {
  if (str_count(location, ",") > 2) {
    # Split the location into parts
    parts <- str_split(location, ",")[[1]]
    # Return only the first, second, and the last part (skip extra parts)
    return(paste(parts[1], parts[length(parts) - 1], parts[length(parts)], sep = ", "))
  }
  return(location)  # Return unchanged if there are not more than 2 commas
}

# Apply the clean_location function to the 'Location' column
users <- users %>%
  mutate(Location = sapply(Location, clean_location))

# Remove the comma count column after processing
users <- users %>%
  select(-comma_count)






















# Assuming the 'users' dataframe has a 'location' column, apply the separate function
users <- users %>%
  separate(Location, into = c("city", "state", "country"), sep = ",", fill = "right", extra = "drop")

# Trim any leading or trailing spaces from the new columns
users <- users %>%
  mutate(across(c(city, state, country), ~ trimws(.)))



# Identify rows where the Country is empty or NA
missing_country_indices <- which(is.na(users$country) | users$country == "")

# Fill in missing Country values
for (i in missing_country_indices) {
  city <- users$city[i]
  
  # Find rows in the users data where the Location contains the city
  matching_rows <- users[users$city == city & !is.na(users$country) & users$country != "", ]
  
  # If matches are found, get the first matching country
  if (nrow(matching_rows) > 0) {
    users$country[i] <- matching_rows$country[1]  # Fill with the first matching country
  }
}



# Check for empty or NA values in the Country column
empty_countries <- users[is.na(users$country) | users$country == "", ]

# Count the number of empty or NA entries
num_empty_countries <- nrow(empty_countries)



# Drop rows where Country is NA or empty
users<- users[!is.na(users$country) & users$country != "", ]


# Drop the columns 'City' and 'State'
users <- users %>%
  select(-city, -state)


# Print unique countries from users dataframe
unique_countries <- unique(users$country)
print(unique_countries)

# Remove rows where the country is "everywhere and anywhere"
users <- users %>%
  filter(country != "everywhere and anywhere")

# Remove rows where the country is "unknown"
users <- users %>%
  filter(country != "unknown")







# Count occurrences of each country
country_counts <- users %>%
  group_by(country) %>%
  summarise(count = n()) %>%
  arrange(count)

# Get the most rare countries (countries with the lowest count)
rare_countries <- country_counts %>%
  filter(count == min(count))

# Print the rare countries
print(rare_countries)


# Count occurrences of each country
country_counts <- users %>%
  group_by(country) %>%
  summarise(count = n()) %>%
  arrange(count)

# Get the most rare countries (countries with the lowest count)
rare_countries <- country_counts %>%
  filter(count == min(count)) %>%
  pull(country)  # Extract the list of rare countries

# Remove rows from users where country is one of the rare countries
users <- users %>%
  filter(!country %in% rare_countries)




# Remove special characters from the country column
users$country <- gsub("[^A-Za-z ]", "", users$country)


# Remove rows where country is NA or empty
users <- users %>%
  filter(!is.na(country) & country != "")





##################################
####FOR A


# Join the ratings_pga data with users data
ratings_pga_with_country <- ratings_pga %>%
  rename(User.ID = "User") %>%  # Aligning the column name with users data
  left_join(users, by = c("User.ID" = "User-ID"))  # Joining based on User-ID



# Print unique countries from ratings_pga_with_country dataframe
unique_countries <- unique(ratings_pga_with_country$country)
print(unique_countries)




# Count occurrences of each country
country_counts <- ratings_pga_with_country %>%
  group_by(country) %>%
  summarise(count = n()) %>%
  arrange(count)

# Get the most rare countries (countries with the lowest count)
rare_countries <- country_counts %>%
  filter(count == min(count))

# Print the rare countries
print(rare_countries)


# Count occurrences of each country
country_counts <- ratings_pga_with_country %>%
  group_by(country) %>%
  summarise(count = n()) %>%
  arrange(count)

# Get the most rare countries (countries with the lowest count)
rare_countries <- country_counts %>%
  filter(count == min(count)) %>%
  pull(country)  # Extract the list of rare countries

# Remove rows from users where country is one of the rare countries
ratings_pga_with_country <- ratings_pga_with_country %>%
  filter(!country %in% rare_countries)


ratings_pga_with_country <- ratings_pga_with_country %>%
  filter(!is.na(country))
########################################################################
####For B

# Join the ratings_pgb data with users data
ratings_pgb_with_country <- ratings_pgb %>%
  rename(User.ID = "User") %>%
  left_join(users, by = c("User.ID" = "User-ID"))






# Count occurrences of each country
country_counts <- ratings_pgb_with_country %>%
  group_by(country) %>%
  summarise(count = n()) %>%
  arrange(count)

# Get the most rare countries (countries with the lowest count)
rare_countries <- country_counts %>%
  filter(count == min(count))

# Print the rare countries
print(rare_countries)


# Count occurrences of each country
country_counts <- ratings_pgb_with_country %>%
  group_by(country) %>%
  summarise(count = n()) %>%
  arrange(count)

# Get the most rare countries (countries with the lowest count)
rare_countries <- country_counts %>%
  filter(count == min(count)) %>%
  pull(country)  # Extract the list of rare countries

# Remove rows from users where country is one of the rare countries
ratings_pgb_with_country <- ratings_pgb_with_country %>%
  filter(!country %in% rare_countries)






# Print unique countries from ratings_pga_with_country dataframe
unique_countries <- unique(ratings_pgb_with_country$country)
print(unique_countries)







# Remove rows where the country is "everywhere and anywhere"
ratings_pgb_with_country <- ratings_pgb_with_country %>%
  filter(country != "catalonia")

# Remove rows where the country is "everywhere and anywhere"
ratings_pgb_with_country <- ratings_pgb_with_country %>%
  filter(country != "quit")



ratings_pgb_with_country <- ratings_pgb_with_country %>%
  filter(!is.na(country))
########################################################################




# Calculate average rating by country for RatingPGA
avg_rating_pga <- ratings_pga_with_country %>%
  group_by(country) %>%
  summarise(avg_rating = mean(Book.Rating, na.rm = TRUE)) %>%
  arrange(desc(avg_rating))



# Calculate average rating by country for RatingPGB
avg_rating_pgb <- ratings_pgb_with_country %>%
  group_by(country) %>%
  summarise(avg_rating = mean(Book.Rating, na.rm = TRUE)) %>%
  arrange(desc(avg_rating))




# Top 5 countries for RatingPGA
top_5_countries_pga <- avg_rating_pga %>%
  slice_max(avg_rating, n = 5)



# Top 5 countries for RatingPGB
top_5_countries_pgb <- avg_rating_pgb %>%
  slice_max(avg_rating, n = 5)




library(ggplot2)

# Bar chart for top 5 countries in RatingPGA
ggplot(top_5_countries_pga, aes(x = reorder(country, avg_rating), y = avg_rating)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +  # Flip the coordinates to make it horizontal
  labs(title = "Top 5 Countries with Highest Average Ratings (RatingPGA)",
       x = "Country", y = "Average Rating") +
  theme_minimal()






# Bar chart for top 5 countries in RatingPGB
ggplot(top_5_countries_pgb, aes(x = reorder(country, avg_rating), y = avg_rating)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  coord_flip() +  # Flip the coordinates to make it horizontal
  labs(title = "Top 5 Countries with Highest Average Ratings (RatingPGB)",
       x = "Country", y = "Average Rating") +
  theme_minimal()



# Display the average ratings by country
cat("Average Ratings by Country (RatingPGA):\n")
print(avg_rating_pga)

cat("\nAverage Ratings by Country (RatingPGB):\n")
print(avg_rating_pgb)



