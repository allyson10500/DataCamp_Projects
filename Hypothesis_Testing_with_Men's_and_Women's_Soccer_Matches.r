library(tidyverse)
library(gridExtra)

# Load men's and women's datasets
women <- read_csv("women_results.csv")
men <- read_csv("men_results.csv")

# Filtering the matches and creating the test values
men <- men %>%
          filter(tournament == "FIFA World Cup", date > "2002-01-01") %>%
          mutate(goals_scored = home_score + away_score)

women <- women %>%
          filter(tournament == "FIFA World Cup", date > "2002-01-01") %>%
          mutate(goals_scored = home_score + away_score)

# Determine normality using histograms
men_plot <- ggplot(men, aes(x = goals_scored)) +
  geom_histogram(color = "red", bins = 30) +
  ggtitle("Goals Scored (Men's)") +
  xlab("Goals Scored") +
  ylab("Frequency")

women_plot <- ggplot(women, aes(x = goals_scored)) +
  geom_histogram(color = "blue", bins = 30) +
  ggtitle("Goals Scored (Women's)") +
  xlab("Goals Scored") +
  ylab("Frequency")

# Goals scored is not normally distributed, so use Wilcoxon-Mann-Whitney test of two groups
grid.arrange(men_plot, women_plot, nrow = 1)

# Run a Wilcoxon-Mann-Whitney test on goals_scored vs. group
test_results <- wilcox.test(
  x = women$goals_scored,
  y = men$goals_scored,
  alternative = "greater"
)

# Determine hypothesis test result using sig. level
p_val <- round(test_results$p.value, 4)
result <- ifelse(p_val <= 0.01, "reject", "fail to reject")

# Create the result data frame
result_df <- data.frame(p_val, result)