# Imported libraries
library(tidyverse)

# Load and inspect the data
yearly <- read_csv('data/yearly_deaths_by_clinic.csv')
yearly

monthly <- read_csv("data/monthly_deaths.csv")
monthly

# Add proportion_deaths to both data frames
yearly <- yearly %>% 
  mutate(proportion_deaths = deaths / births)

monthly <- monthly %>% 
  mutate(proportion_deaths = deaths / births)

# Plot the data
ggplot(yearly, aes(x = year, y = proportion_deaths, color = clinic)) +
  geom_line()

ggplot(monthly, aes(date, proportion_deaths)) +
  geom_line() +
  labs(x = "Year", y = "Proportion Deaths")

# Add the threshold and flag and plot again
handwashing_start = as.Date('1847-06-01')

monthly <- monthly %>%
  mutate(handwashing_started = date >= handwashing_start)

ggplot(monthly, aes(x = date, y = proportion_deaths, color = handwashing_started)) +
  geom_line()

# Find the mean
monthly_summary <- monthly %>% 
  group_by(handwashing_started) %>%
  summarize(mean_proportion_deaths = mean(proportion_deaths))

monthly_summary