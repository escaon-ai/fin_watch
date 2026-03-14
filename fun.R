# Get raw financial data (consolidated value for each weekday)
fetch_fin_data <- function(start_date) {
  # Use Yahoo finance for DCAM, PCEU and BTC
  # Nb: BTC the only one with 7 days of data, other will have 5 (workweek)
  yahoo <- tq_get(
    c("DCAM.PA", "PCEU.PA", "BTC-EUR"),
    get = "stock.prices",
    from = start_date,
    to = start_date + days(6)
  ) |>
    as_tibble() |>
    select(
      symbol, date, adjusted
    ) |>
    rename(
      index = symbol,
      value = adjusted
    ) |>
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
      startPeriod = as.character(start_date),
      endPeriod = as.character(start_date + days(6))
    )
  ) |>
    as_tibble() |>
    select(
      benchmark_item, obstime, obsvalue
    ) |>
    rename(
      index = benchmark_item,
      date = obstime,
      value = obsvalue
    ) |>
    mutate(
      index = recode(
        index,
        "EU000A2X2A25" = "€STER"
      ),
      date = as_date(date)
    )

  fin_data <- bind_rows(yahoo, ecb)
  
  if (!is.null(fin_data) & !any(is.na(fin_data))) {
    return(fin_data)
  } else {
    cat("Issue with raw data")
  }
}

# Compute variations, list for sparklines & summary for AI
wrangle_fin_data <- function(fin_data) {
  fin_data_wrangled <-
    fin_data |>
    group_by(index) |>
    summarize(
      start_value   = first(value),
      end_value     = last(value),
      variation_pct = (last(value) / first(value) - 1) * 100,
      index_data = list(value),
      .groups       = "drop"
    ) |>
    mutate(
      last_value = map_dbl(index_data, ~as.numeric(last(.x))),
      summary = paste0(
        index, ": ",
        round(variation_pct, 2), "% (",
        round(last_value, 2), " ",
        case_when(
          variation_pct > 0 ~ "\u2191",
          variation_pct < 0 ~ "\u2193",
          .default          = "\u2192"
        ),
        ")\n"
      )
    ) |>
    select(-last_value)
  
  if (!is.null(fin_data_wrangled) & !any(is.na(fin_data_wrangled))) {
    return(fin_data_wrangled)
  } else {
    cat("Issue with wrangled data")
  }
  
  
}

# Fetch financial news headlines via NewsAPI for a given query
fetch_market_news <- function(query, language = "en") {
  api_key <- Sys.getenv("NEWS_API_KEY")
  if (api_key == "") {
    return("NEWS_API_KEY non configurée — actualités indisponibles.")
  }

  tryCatch({
    resp <- httr2::request("https://newsapi.org/v2/everything") |>
      httr2::req_url_query(
        q        = query,
        language = language,
        sortBy   = "publishedAt",
        pageSize = 5,
        apiKey   = api_key
      ) |>
      httr2::req_perform() |>
      httr2::resp_body_json()

    if (length(resp$articles) == 0) {
      return(paste0("Aucun article trouvé pour : '", query, "'"))
    }

    resp$articles |>
      purrr::map_chr(~ paste0(
        .x$title,
        if (!is.null(.x$description) && nchar(.x$description) > 0)
          paste0(" — ", .x$description)
        else "",
        " [", .x$source$name, ", ", substr(.x$publishedAt, 1, 10), "]"
      )) |>
      paste(collapse = "\n")
  }, error = function(e) {
    paste("Erreur fetch_market_news:", e$message)
  })
}

# ellmer tool wrapping fetch_market_news
news_tool <- ellmer::tool(
  fetch_market_news,
  "Recherche et retourne les 5 derniers titres d'actualité financière pour une requête donnée.
   Utilise cet outil pour obtenir des informations récentes sur les marchés, les banques centrales,
   les cryptomonnaies ou les événements macro-économiques avant de rédiger ton analyse.",
  arguments = list(
    query    = ellmer::type_string(
      "Requête de recherche en anglais (ex: 'ECB interest rate decision', 'Bitcoin ETF', 'MSCI World weekly')"
    ),
    language = ellmer::type_enum(
      c("en", "fr"),
      "Langue des articles : 'en' (défaut) pour l'anglais, 'fr' pour le français",
      required = FALSE
    )
  )
)

# Perform AI analysis using Gemini via ellmer
perform_ai_analysis <- function(fin_data_wrangled, week_start) {

  # 1. Vérification de la clé API
  if (Sys.getenv("GEMINI_API_KEY") == "") {
    warning("GEMINI_API_KEY environment variable not set, skipping AI analysis")
    return("AI analysis skipped - API key not configured.")
  }

  # 2. System prompt en français avec sources financières de référence
  system_prompt <- "
  Tu es un analyste financier senior chargé de produire des insights hebdomadaires sur les marchés.

  Contexte d'investissement :
  - DCAM (Amundi PEA Monde MSCI World UCITS ETF Acc) & PCEU (Amundi PEA MSCI Europe UCITS ETF Acc) :
    Positions long terme diversifiées. Analyser les dynamiques macro et sectorielles de la semaine.
  - BTC (Bitcoin en EUR) : Surveillance 'Buy the Dip'. Analyser les points bas intra-semaine et les
    facteurs de volatilité (actualité crypto, réglementation, sentiment de marché).
  - €STER (Euro Short-Term Rate) : Placement de trésorerie en attente d'investissement. Analyser
    la stabilité du taux directeur et les signaux de la BCE sur la politique monétaire.

  Tu as accès à un outil 'fetch_market_news' pour récupérer les dernières actualités financières.
  Utilise-le AVANT de rédiger ton analyse pour chaque sujet pertinent : politique BCE, marchés actions,
  Bitcoin, indicateurs macro. Effectue plusieurs appels avec des requêtes ciblées en anglais.

  Sources à croiser dans ton analyse :
  - Banque Centrale Européenne (BCE) : décisions de taux, comptes-rendus, discours du Président
  - Bloomberg et Reuters : flux d'informations macro-économiques de la semaine
  - Financial Times : analyses de fond sur les marchés
  - Indicateurs macro : inflation (CPI/HICP), emploi (NFP), PMI, croissance (PIB)

  Rédige en français. Sois concis (3 à 5 paragraphes), factuel, et oriente l'analyse vers les
  implications pratiques pour ces actifs spécifiques.
  N'explicite jamais les acronymes dans le texte : utilise uniquement DCAM, PCEU, BTC et €STER,
  sans jamais les développer entre parenthèses.
  
  Avant d'analyser les indices spécifiques du rapport, identifie systématiquement les trois principaux
  moteurs géopolitiques mondiaux de la semaine et leur impact sur les matières premières et les actions.
  "

  # 3. Initialisation du chat avec ellmer + enregistrement du tool news
  chat <- chat_google_gemini(
    system_prompt = system_prompt
  )
  chat$register_tool(news_tool)
  
  # Some data derived summary for final IA output
  variation_summary <-
    fin_data_wrangled |>
    pull(summary) |>
    paste(collapse = "")

  # 4. Message utilisateur en français
  week_end <- week_start + lubridate::days(4)
  week_label <- paste0(
    format(week_start, "%d %B %Y"), " au ", format(week_end, "%d %B %Y")
  )

  user_prompt <- paste0(
    "Semaine analysée : du ", week_label, ".\n\n",
    "Résumé financier de la semaine :\n",
    variation_summary,
    "\n\nAnalyse en français les raisons probables de ces mouvements de marché. ",
    "Rappel : Identifie les événements macro-économiques, les décisions de banques centrales ",
    "ou les facteurs géopolitiques de cette semaine (", week_label, ") ",
    "susceptibles d'expliquer ces variations."
  )

  # 5. Appel à l'API via tryCatch
  tryCatch({

    response <- chat$chat(user_prompt, echo = FALSE)

    if (is.null(response) || response == "") {
      return("Aucune réponse du modèle IA.")
    }

    return(response)

  }, error = function(e) {
    err_msg <- e$message
    warning(paste("Erreur lors de l'appel à l'API Gemini via ellmer:", err_msg))

    if (grepl("429", err_msg)) {
      return("Analyse IA indisponible : quota dépassé (Erreur 429). Vérifiez vos limites Google AI Studio.")
    }

    return(paste("Analyse IA indisponible :", err_msg))
  })
}

# Send email report
send_email_report <- function(doc_title, fin_data_wrangled, ai_analysis) {
  email_user <- Sys.getenv("EMAIL_USER")
  email_password <- Sys.getenv("EMAIL_PASSWORD")

  if (email_user == "" || email_password == "") {
    stop("Email credentials not set in environment variables")
  }

  # QUarto + Typst PDF generation
  pdf_file <- "financial_report.pdf"
  
  tryCatch({
    quarto::quarto_render(
      input = "email_report.qmd",
      output_format = "typst",
      output_file = pdf_file,
      execute_params = list(
        doc_title = doc_title,
        fin_data_wrangled = fin_data_wrangled,
        ai_analysis = ai_analysis
      ),
      quiet = FALSE
    )
    cat("PDF generated successfully via Typst.\n")
  }, error = function(e) {
    cat("PDF generation failed:", conditionMessage(e), "\n")
    return(FALSE)
  })

  # SMTP config
  smtp <- server(
    host = "smtp.gmail.com",
    port = 465,
    username = email_user,
    password = email_password,
    protocol = "smtps",
    reuse = FALSE,
    insecure = TRUE,
    use_ssl = TRUE
  )
  
  # Email content
  email_body_html <- glue::glue(
    "
          <h2
            style = '
              font-size:20px;
              color: #405D8B;
            '
          >
           Données et analyse sur les indices suivis
          </h2>

          <p
            style = '
              font-size:16px;
            '
          >
            Rapport en pièce-jointe.<br>
          </p>

          <p
            style = '
              font-size:14px;
              color: #0178b3;
              border-left: 1px solid #008645;
              padding-left: 12px;
            '
          >
            <a href = 'https://www.linkedin.com/in/erwann-scaon-data-analyst'>e_scaon</a>
          </p>
        "
  )
  

  mail_html <- envelope(
    from = email_user,
    to = email_user,
    subject = paste("Rapport financier hebdomadaire -", format(Sys.Date(), "%Y-%m-%d"))
  ) |>
    html(email_body_html) |>
    attachment(pdf_file)

  # Send email
  tryCatch({
    smtp(mail_html)
    cat("Email sent successfully!\n")
    return(TRUE)
  }, error = function(e) {
    cat("Failed to send email:", e$message, "\n")
    return(FALSE)
  })
}
