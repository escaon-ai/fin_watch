fetch_fin_data <- function(start_date) {
  # Use Yahoo finance for DCAM, PCEU and BTC
  # Nb: BTC the only one with 7 days of data, other will have 5 (workweek)
  yahoo <- tq_get(
    c("DCAM.PA", "PCEU.PA", "BTC-EUR"),
    get = "stock.prices",
    from = date_monday_complweek,
    to = date_monday_complweek + days(6)
  ) %>%
    as_tibble() %>%
    select(
      symbol, date, adjusted
    ) %>%
    rename(
      index = symbol,
      value = adjusted
    ) %>%
    mutate(
      index = recode(
        index, 
        "DCAM.PA" = "DCAM", 
        "PCEU.PA" = "PCEU", 
        "BTC-EUR" = "BTC"
      )
    )
  
  # Use ECB for €STER
  ecb <- get_data(
    key = "EST.B.EU000A2X2A25.WT",
    filter = list(
      startPeriod = as.character(date_monday_complweek),
      endPeriod = as.character(date_monday_complweek + days(6))
    ) # lastNObservations = 7
  ) %>%
    as_tibble() %>%
    select(
      benchmark_item, obstime, obsvalue
    ) %>%
    rename(
      index = benchmark_item,
      date = obstime,
      value = obsvalue
    ) %>%
    mutate(
      index = recode(
        index, 
        "EU000A2X2A25" = "€STER"
      ),
      date = as_date(date)
    )
  
  return(bind_rows(yahoo, ecb))
}

# Calculate weekly variations based on your rules
calculate_variations <- function(data) {
  # Calculate variations for each index separately
  variations_list <- lapply(split(data, data$index), function(index_data) {
    # Sort by date
    index_data <- index_data[order(index_data$date), ]

    # Determine window based on index type
    window_days <- if (index_data$index[1] %in% c("DCAM", "PCEU", "&#8364;STER")) {
      5  # Mon-Fri
    } else if (index_data$index[1] %in% c("BTC")) {
      7  # Mon-Sun
    } else {
      5  # Default
    }

    # Initialize variation column
    index_data$variation_pct <- NA_real_

    # Calculate the percentage change for each row where we have enough data
    for (i in seq_len(nrow(index_data))) {
      if (i >= window_days) {
        # Calculate percentage change from window_days ago
        prev_value <- index_data$value[i - window_days + 1]
        current_value <- index_data$value[i]
        index_data$variation_pct[i] <- (current_value / prev_value - 1) * 100
      }
    }

    return(index_data)
  })

  # Combine all results
  variations <- do.call(rbind, variations_list)

  # Filter out rows where we don't have enough data for the calculation
  variations <- variations[!is.na(variations$variation_pct), ]

  # Select only the columns we need
  variations <- variations[, c("index", "date", "value", "variation_pct")]

  return(variations)
}

# Prepare data for AI analysis
prepare_ai_prompt <- function(variations) {
  # Create a summary of variations for the AI prompt
  summary_text <- variations %>%
    group_by(index) %>%
    summarize(
      latest_date = max(date),
      latest_value = dplyr::last(value),
      latest_variation = dplyr::last(variation_pct),
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

# Perform AI analysis using Gemini via ellmer
perform_ai_analysis <- function(variation_summary) {
  
  # 1. Vérification de la clé API
  if (Sys.getenv("GEMINI_API_KEY") == "") {
    warning("GEMINI_API_KEY environment variable not set, skipping AI analysis")
    return("AI analysis skipped - API key not configured.")
  }
  
  # 2. Configuration du "Cerveau" (System Prompt)
  system_prompt <- "
  You are a financial analyst providing weekly market insights.
  Investment Strategy Context:
  - DCAM (Amundi PEA Monde (MSCI World) UCITS ETF Acc) & PCEU (Amundi PEA MSCI Europe UCITS ETF Acc): Long-term holds. Focus on weekly dynamics.
  - BTC (Bitcoin) : 'Buy the dip' monitoring. Focus on daily lows and volatility reasons.
  - &#8364;STER (ESTER stands for Euro Short-Term Rate, for euro zone): I use this for cash waiting investment. Focus on rate stability/risk of drop.

  Analyze the following weekly variations and provide insights on what might
  have caused these market movements. Search for relevant news/events from
  this specific week that could explain these trends.
  "
  
  # 3. Initialisation du chat avec ellmer
  # Let him pick model
  chat <- chat_google_gemini(
    # model = "gemini-1.5-flash",
    system_prompt = system_prompt
  )
  
  # 4. Préparation du message utilisateur
  user_prompt <- paste0(
    "Weekly Financial Summary:\n",
    variation_summary,
    "\n\nPlease provide a concise analysis of why these markets moved this way this week."
  )
  
  # 5. Appel à l'API via tryCatch
  tryCatch({
    
    # Avec ellmer, on envoie juste le user_prompt via la méthode $reply()
    response <- chat$chat(user_prompt, echo = FALSE)
    
    if (is.null(response) || response == "") {
      return("No response from AI model.")
    }
    
    return(response)
    
  }, error = function(e) {
    # Gestion des erreurs (Quota 429, timeout, etc.)
    err_msg <- e$message
    warning(paste("Error calling Gemini API via ellmer:", err_msg))
    
    if (grepl("429", err_msg)) {
      return("AI analysis unavailable: Quota exceeded (Error 429). Please check your Google AI Studio limits.")
    }
    
    return(paste("AI analysis unavailable due to technical issues:", err_msg))
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
    port = 465,
    username = email_user,
    password = email_password
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
