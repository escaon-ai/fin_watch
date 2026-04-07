# Setup -------------------------------------------------------------------
# Handle packages conflicts
library(conflicted)

conflicts_prefer(
  dplyr::filter,
  dplyr::first,
  dplyr::last,
  emayili::html,
  lubridate::isoweek
)

# Load packages
library(dplyr)
library(magrittr)
library(purrr)
library(lubridate)
library(tidyquant)
library(ecb)
library(ellmer)
library(emayili)
library(quarto)
library(gt)
library(gtExtras)
library(svglite)

source("./fun.R")

# Get the date for the Monday of the previous complete week
date_monday_complweek <-
  floor_date(today(), unit = "week", week_start = 7) - days(6)
date_weeknum <- sprintf("%02d", isoweek(date_monday_complweek))
date_year <- year(date_monday_complweek)
doc_title <- paste0(
  "année ", date_year, " - semaine ", date_weeknum
  )

# Alternative: go for the last 7 days, whatever they are
# date_start_last_7d <- today() %m-% weeks(1)

# Analysis ----------------------------------------------------------------
cat("Starting Monday Financial Analysis...\n")

cat("Get last week financial raw data...\n")
fin_data <- fetch_fin_data(date_monday_complweek)

cat("Wrangle financial data...\n")
fin_data_wrangled <- wrangle_fin_data(fin_data)
# fin_data_wrangled |> print(n = Inf, width = Inf)

cat("Perform AI analysis...\n")
ai_analysis <- perform_ai_analysis(fin_data_wrangled, date_monday_complweek)
# cat(ai_analysis)

if (is.null(ai_analysis)) {
  cat("AI analysis failed. Aborting: no email sent, no PDF archived.\n")
  quit(status = 1)
}

# Reporting ---------------------------------------------------------------
cat("Sending email report...\n")
# curl::curl_version()
# curl_fetch_memory("https://www.google.com")

email_success <- send_email_report(
  doc_title,
  fin_data_wrangled |> select(-summary),
  ai_analysis
  )

if (email_success) {
  cat("Analysis complete! Email sent successfully.\n")
} else {
  cat("Analysis complete! Email sending failed.\n")
}

# Cleanup -----------------------------------------------------------------
if (file.exists("table.png")) {
  file.remove("table.png")
}
