# Start your code here
# Import packages
if (!requireNamespace("survminer", quietly = TRUE)) install.packages("survminer")
if (!requireNamespace("GPArotation", quietly = TRUE)) install.packages("GPArotation")
library(psych)
library(survival)
library(survminer)
library(readr)
library(broom)
library(GPArotation)

# Load the factor_data.csv
factor_data <- read_csv("factor_data.csv")

# Load the survival_data.csv
survival_data <- read_csv("survival_data.csv")

# Identify which variable has the strongest correlation to SpeciesDiversity
cor_factor_data <- cor(factor_data, use = "pairwise.complete.obs")

most_impactful_factor <- "DeforestationRate"

# Perform a scree plot to determine the number of factors
scree(cor_factor_data, factors = FALSE)

# From observing the scree plot choose 2 factors
num_factors <- 2

# Perform Exploratory Factor Analysis
EFA_model <- fa(factor_data, nfactors = num_factors)
EFA_model

# Perform Survival Analysis
survival_fit <- survfit(Surv(Survival_Time, Censoring_Status) ~ Habitat, 
                        data = survival_data)

# Tidy the survival fit results using the broom package
survival_fit_df <- tidy(survival_fit)

# Plot survival curves
ggsurvplot(survival_fit, data = survival_data)

# Which habitat drops to the lowest survival probability?
low_surv_habitat <- "Savanna"