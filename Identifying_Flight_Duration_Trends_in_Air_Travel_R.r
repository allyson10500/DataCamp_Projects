# Import required packages
library(dplyr)
library(readr)

# Load the data
flights <- read_csv("flights2022-h2.csv")
airlines <- read_csv("airlines.csv")
airports <- read_csv("airports.csv")

# Join the flights, airlines, and airports data frames together
complex_join <- flights %>%
  left_join(airlines, by = "carrier") %>%
  rename(airline_name = name) %>% 
  left_join(airports, by = c("dest" = "faa")) %>% 
  rename(airport_name = name)

# Find flight duration in hours
transformed_data <- complex_join %>%
  mutate(flight_duration = air_time / 60)

# Determine the average flight duration and number of flights for each airline and airport combination
analysis_result <- transformed_data %>%
  group_by(airline_name, airport_name) %>%
  summarize(avg_flight_duration = mean(flight_duration, na.rm = TRUE),
            count = n()) %>%
  ungroup()

# From which airline and to which city do the most flights from NYC go to?
frequent <- analysis_result %>% arrange(desc(count)) %>% head(1)

# Which airline and to which airport has the longest average flight duration (in hours) from NYC?
longest <- analysis_result %>% arrange(desc(avg_flight_duration)) %>% head(1)

# What was the least common destination airport departing from JFK?
transformed_data %>% 
  filter(origin == "JFK") %>% 
  group_by(airport_name) %>% 
  summarize(count = n()) %>% 
  arrange(count)

least <- "Eagle County Regional Airport"