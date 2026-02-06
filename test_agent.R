# Test script for the financial analysis agent

# Load required libraries
library(conflicted)

# Resolve conflicts
conflicts_prefer(
  dplyr::filter
)

# Load required libraries
library(dplyr)
library(magrittr)
library(lubridate)
library(tidyquant)
library(ecb)
library(emayili)
library(httr)

# Source the data fetching script
source("fetch_data.R")

# Display the loaded data
cat("Loaded financial data:\n")
print(fin_data)

# Test the calculation function from monday_agent.R
source("monday_agent.R")

# Test calculate_variations function
cat("\nTesting variation calculations:\n")
test_variations <- calculate_variations(fin_data)
print(test_variations)

# Test prepare_ai_prompt function
cat("\nTesting AI prompt preparation:\n")
test_prompt <- prepare_ai_prompt(test_variations)
cat("Prepared prompt summary:\n")
cat(substr(test_prompt, 1, 300), "...\n")

cat("\nTest completed successfully!\n")
cat("To test the full workflow, set the required environment variables and run:\n")
cat("Rscript monday_agent.R\n")