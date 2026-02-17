# Setup -------------------------------------------------------------------
# Handle packages conflicts
library(conflicted)

conflicts_prefer(
  dplyr::filter,
  emayili::html
)

# Load packages
library(dplyr)
library(magrittr)
library(lubridate)
library(tidyquant)
library(ecb)
library(ellmer)
# library(emayili)
library(blastula)
library(gt)

source("./fun.R")

# Get the date for the Monday of the previous complete week
date_monday_complweek <-
  floor_date(today(), unit = "week", week_start = 7) - days(6)

# Alternative: go for the last 7 days, whatever they are
# date_start_last_7d <- today() %m-% weeks(1)

# Analysis ----------------------------------------------------------------
cat("Starting Monday Financial Analysis...\n")

cat("Get last week financial data...\n")
fin_data <- fetch_fin_data(date_monday_complweek)
# fin_data |> print(n = Inf, width = Inf)

cat("Calculating variations...\n")
variations <- calculate_variations(fin_data)
# variations |> print(n = Inf, width = Inf)

cat("Perform AI analysis...\n")
# Prepare AI prompt
variation_summary <- prepare_ai_prompt(variations)

# Call Gemini
ai_analysis <- perform_ai_analysis(variation_summary)
# cat(ai_analysis)

# Reporting ---------------------------------------------------------------
cat("Sending email report...\n")
# curl::curl_version()
# curl_fetch_memory("https://www.google.com")

email_success <- send_email_report(ai_analysis, variations)

if (email_success) {
  cat("Analysis complete! Email sent successfully.\n")
} else {
  cat("Analysis complete! Email sending failed.\n")
}
