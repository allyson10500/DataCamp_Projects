# We've loaded the necessary packages for you in the first cell. Please feel free to add as many cells as you like!
suppressMessages(library(dplyr)) # This line is required to check your answer correctly
options(readr.show_types = FALSE) # This line is required to check your answer correctly
library(readr)
library(readxl)
library(stringr)

# Begin coding here ...

# Load CSV file for prices
airbnb_price <- read_csv('data/airbnb_price.csv', show_col_types=FALSE)

# Load Excel file for room types
airbnb_room_type <- read_excel('data/airbnb_room_type.xlsx')

# Load TSV file for review dates
airbnb_last_review <- read_tsv('data/airbnb_last_review.tsv', show_col_types=FALSE)

# Merge the three data frames together into one
listings <- airbnb_price %>%
  inner_join(airbnb_room_type, by = "listing_id") %>%
  inner_join(airbnb_last_review, by = "listing_id")

# What are the dates of the earliest and most recent reviews? 
# In order to use a function like max()/min() on last_review date column, format needs to be converted
review_dates <- listings %>%
  # Convert to date using the format 'Month DD YYYY'
  mutate(last_review_date = as.Date(last_review, format = "%B %d %Y")) %>%
  # Use max() and min() to take the latest and earliest dates
  summarize(first_reviewed = min(last_review_date),
            last_reviewed = max(last_review_date))

# How many of the listings are private rooms? 
# Since there are differences in capitalization, make capitalization consistent
private_room_count <- listings %>%
  mutate(room_type = str_to_lower(room_type)) %>%
  # Then count the number of each room_type
  count(room_type) %>%
  # Get row containing count for private rooms only
  filter(room_type == "private room") 

# Extract number of rooms
nb_private_rooms <- private_room_count$n


# What is the average listing price? 
# To convert price to numeric, remove "dollars" from each value
avg_price <- listings %>%
  mutate(price_clean = str_remove(price, " dollars") %>%
        as.numeric()) %>%
  # Take the mean of price_clean
  summarize(avg_price = mean(price_clean)) %>%
  # Convert from a tibble to a single number
  as.numeric()

# Load solution values into solution tibble:
# Note first_reviewed and last_reviewed columns in 
# review_dates were created earlier
review_dates$nb_private_rooms = nb_private_rooms
review_dates$avg_price = round(avg_price, 2)

print(review_dates)
