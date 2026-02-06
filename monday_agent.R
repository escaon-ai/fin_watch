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
library(httr)  # Alternative for API calls if ellmer is not available

# Source the data fetching script
source("fetch_data.R")

# Calculate weekly variations based on your rules
calculate_variations <- function(data) {
  # Group by index to calculate variations
  variations <- data %>%
    group_by(index) %>%
    arrange(date) %>%
    mutate(
      # Determine window based on index type
      window_days = case_when(
        index %in% c("DCAM", "PCEU", "&#8364;STER") ~ 5,  # Mon-Fri
        index %in% c("BTC") ~ 7,                          # Mon-Sun
        TRUE ~ 5
      ),
      # Calculate percentage change over the appropriate window
      variation_pct = case_when(
        n() >= window_days ~ (value / lag(value, window_days - 1) - 1) * 100,
        TRUE ~ NA_real_
      )
    ) %>%
    filter(!is.na(variation_pct)) %>%
    select(index, date, value, variation_pct)

  return(variations)
}

# Prepare data for AI analysis
prepare_ai_prompt <- function(variations) {
  # Create a summary of variations for the AI prompt
  summary_text <- variations %>%
    group_by(index) %>%
    summarize(
      latest_date = max(date),
      latest_value = last(value),
      latest_variation = last(variation_pct),
      .groups = 'drop'
    ) %>%
    mutate(
      summary = paste0(
        index, ": ",
        round(latest_variation, 2), "% change (",
        round(latest_value, 2), " ",
        ifelse(latest_variation > 0, "&#8593;", "&#8595;"),
        ")\n"
      )
    ) %>%
    pull(summary) %>%
    paste(collapse = "")

  return(summary_text)
}

# Perform AI analysis using Gemini API directly
perform_ai_analysis <- function(variation_summary) {
  # Get API key from environment
  gemini_key <- Sys.getenv("GEMINI_API_KEY")
  if (gemini_key == "") {
    warning("GEMINI_API_KEY environment variable not set, skipping AI analysis")
    return("AI analysis skipped - API key not configured.")
  }

  # Create prompt for Gemini
  system_prompt <- "
  You are a financial analyst providing weekly market insights.
  Investment Strategy Context:
  - DCAM & PCEU: Long-term holds. Focus on weekly dynamics.
  - BTC: 'Buy the dip' monitoring. Focus on daily lows and volatility reasons.
  - &#8364;STER: Cash parking. Focus on rate stability/risk of drop.

  Analyze the following weekly variations and provide insights on what might
  have caused these market movements. Search for relevant news/events from
  this specific week that could explain these trends.
  "

  user_prompt <- paste0(
    "Weekly Financial Summary:\n",
    variation_summary,
    "\n\nPlease provide a concise analysis of why these markets moved this way this week."
  )

  # Call Gemini API directly using httr
  tryCatch({
    # Prepare the request body
    request_body <- list(
      contents = list(
        list(
          parts = list(
            list(text = paste(system_prompt, user_prompt, sep = "\n\n"))
          )
        )
      )
    )

    # Make API request
    response <- POST(
      url = "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash:generateContent",
      query = list(key = gemini_key),
      body = request_body,
      encode = "json",
      timeout = 60
    )

    # Check if request was successful
    if (response$status_code == 200) {
      response_data <- content(response, "parsed")
      if (!is.null(response_data$candidates) && length(response_data$candidates) > 0) {
        return(response_data$candidates[[1]]$content$parts[[1]]$text)
      } else {
        return("No response from AI model.")
      }
    } else {
      warning(paste("Error calling Gemini API:", response$status_code, "-", content(response)$error$message))
      return(paste("AI analysis unavailable. Error:", response$status_code))
    }
  }, error = function(e) {
    warning(paste("Error calling Gemini API:", e$message))
    return(paste("AI analysis unavailable due to technical issues:", e$message))
  })
}

# Send email report
send_email_report <- function(ai_analysis, variations) {
  # Get email credentials from environment
  email_user <- Sys.getenv("EMAIL_USER")
  email_password <- Sys.getenv("EMAIL_PASSWORD")  # You'll need to add this to your secrets

  if (email_user == "" || email_password == "") {
    stop("Email credentials not set in environment variables")
  }

  # Create email body
  email_body <- paste0(
    "<h2>Weekly Financial Analysis Report</h2>\n\n",
    "<h3>Market Movements:</h3>\n<pre>",
    paste(capture.output(print(variations)), collapse = "\n"),
    "</pre>\n\n<h3>AI Analysis:</h3>\n<p>",
    gsub("\n", "<br>", ai_analysis),
    "</p>\n\n<p>Report generated on ",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "</p>"
  )

  # Create and send email
  email <- envelope(
    from = email_user,
    to = email_user,  # Send to yourself
    subject = paste("Weekly Financial Report -", format(Sys.Date(), "%Y-%m-%d"))
  ) %>%
    html(email_body)

  # Configure SMTP
  smtp <- server(
    host = "smtp.gmail.com",
    port = 587,
    username = email_user,
    password = email_password,
    tls = TRUE
  )

  # Send email
  tryCatch({
    smtp(email)
    cat("Email sent successfully!\n")
    return(TRUE)
  }, error = function(e) {
    cat("Failed to send email:", e$message, "\n")
    return(FALSE)
  })
}

# Main execution
main <- function() {
  cat("Starting Monday Financial Analysis...\n")

  # Calculate variations
  cat("Calculating variations...\n")
  variations <- calculate_variations(fin_data)
  print(variations)

  # Prepare AI prompt
  cat("Preparing AI analysis...\n")
  variation_summary <- prepare_ai_prompt(variations)

  # Perform AI analysis
  cat("Calling Gemini for analysis...\n")
  ai_analysis <- perform_ai_analysis(variation_summary)
  cat("AI Analysis:\n", ai_analysis, "\n")

  # Send email report
  cat("Sending email report...\n")
  email_success <- send_email_report(ai_analysis, variations)

  if (email_success) {
    cat("Analysis complete! Email sent successfully.\n")
  } else {
    cat("Analysis complete! Email sending failed.\n")
  }
}

# Run the main function
if (interactive()) {
  main()
}