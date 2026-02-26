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

  return(bind_rows(yahoo, ecb))
}

# Calculate weekly variations based on your rules
calculate_variations <- function(data) {
  calc_group <- function(index_data) {
    w <- if (index_data$index[1] == "BTC") 7L else 5L
    index_data |>
      arrange(date) |>
      mutate(
        variation_pct = if_else(
          row_number() >= w,
          (value / dplyr::lag(value, w - 1L) - 1) * 100,
          NA_real_
        )
      ) |>
      filter(!is.na(variation_pct)) |>
      select(index, date, value, variation_pct)
  }

  data |>
    split(data$index) |>
    lapply(calc_group) |>
    bind_rows()
}

# Prepare data for AI analysis
prepare_ai_prompt <- function(variations) {
  summary_text <- variations |>
    group_by(index) |>
    summarize(
      latest_date = max(date),
      latest_value = dplyr::last(value),
      latest_variation = dplyr::last(variation_pct),
      .groups = "drop"
    ) |>
    mutate(
      summary = paste0(
        index, ": ",
        round(latest_variation, 2), "% (",
        round(latest_value, 2), " ",
        ifelse(latest_variation > 0, "\u2191", "\u2193"),
        ")\n"
      )
    ) |>
    pull(summary) |>
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

  Pour ton analyse, croise les données avec les publications des sources suivantes quand c'est pertinent :
  - Banque Centrale Européenne (BCE) : décisions de taux, comptes-rendus, discours du Président
  - Bloomberg et Reuters : flux d'informations macro-économiques de la semaine
  - Financial Times : analyses de fond sur les marchés
  - Indicateurs macro : inflation (CPI/HICP), emploi (NFP), PMI, croissance (PIB)

  Rédige en français. Sois concis (3 à 5 paragraphes), factuel, et oriente l'analyse vers les
  implications pratiques pour ces actifs spécifiques.
  N'explicite jamais les acronymes dans le texte : utilise uniquement DCAM, PCEU, BTC et €STER,
  sans jamais les développer entre parenthèses.
  "

  # 3. Initialisation du chat avec ellmer
  chat <- chat_google_gemini(
    system_prompt = system_prompt
  )

  # 4. Message utilisateur en français
  user_prompt <- paste0(
    "Résumé financier de la semaine :\n",
    variation_summary,
    "\n\nAnalyse en français les raisons probables de ces mouvements de marché. ",
    "Identifie les événements macro-économiques, les décisions de banques centrales ",
    "ou les facteurs géopolitiques de cette semaine susceptibles d'expliquer ces variations."
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
send_email_report <- function(
    doc_title, fin_data, variations, ai_analysis
    ) {
  email_user <- Sys.getenv("EMAIL_USER")
  email_password <- Sys.getenv("EMAIL_PASSWORD")

  if (email_user == "" || email_password == "") {
    stop("Email credentials not set in environment variables")
  }

  # --- STEP 1: Generate PDF via Typst ---
  pdf_file <- "financial_report.pdf"

  tryCatch({
    quarto::quarto_render(
      input = "email_report.qmd",
      output_format = "typst",
      output_file = pdf_file,
      execute_params = list(
        doc_title = doc_title,
        fin_data = fin_data,
        variations = variations |> select(-date),
        ai_analysis = ai_analysis
      ),
      quiet = FALSE
    )
    cat("PDF generated successfully via Typst.\n")
  }, error = function(e) {
    cat("PDF generation failed:", conditionMessage(e), "\n")
    return(FALSE)
  })

  # --- STEP 2: Build gt table with sparklines for email body ---
  df_fin_clean <- as.data.frame(fin_data) |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::filter(!is.na(value))

  df_table <- df_fin_clean |>
    dplyr::arrange(index, date) |>
    dplyr::group_by(index) |>
    dplyr::summarize(
      start_value   = dplyr::first(value),
      end_value     = dplyr::last(value),
      variation_pct = (dplyr::last(value) / dplyr::first(value) - 1) * 100,
      index_data    = list(value),
      .groups       = "drop"
    )

  gt_table_html <- df_table |>
    gt::gt() |>
    gtExtras::gt_plt_sparkline(
      index_data,
      type = "shaded",
      palette = c("black", rep("transparent", 3), "lightgrey"),
      same_limit = FALSE
    ) |>
    gt::cols_label(
      index         = "Indice",
      start_value   = "Ouverture",
      end_value     = "Cl\u00f4ture",
      variation_pct = "Variation",
      index_data    = "Tendance"
    ) |>
    gt::fmt_number(columns = c("start_value", "end_value"), decimals = 2, sep_mark = "") |>
    gt::fmt_number(columns = "variation_pct", decimals = 2, pattern = "{x} %") |>
    gt::data_color(
      columns = "variation_pct",
      method  = "numeric",
      palette = c("#e74c3c", "white", "#27ae60"),
      domain  = c(-5, 5)
    ) |>
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#f4f6f8"),
        gt::cell_text(weight = "bold", color = "#2c3e50")
      ),
      locations = gt::cells_column_labels()
    ) |>
    gt::tab_style(
      style     = gt::cell_text(color = "#2c3e50"),
      locations = gt::cells_body()
    ) |>
    gt::cols_align(align = "left",   columns = "index") |>
    gt::cols_align(align = "right",  columns = c("start_value", "end_value", "variation_pct")) |>
    gt::cols_align(align = "center", columns = "index_data") |>
    gt::tab_options(
      table.width                       = gt::pct(100),
      table.border.top.style            = "solid",
      table.border.top.color            = "#2c3e50",
      table.border.top.width            = gt::px(2),
      table.border.bottom.style         = "solid",
      table.border.bottom.color         = "#2c3e50",
      table.border.bottom.width         = gt::px(2),
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.color = "#adb5bd",
      column_labels.border.bottom.width = gt::px(1),
      data_row.padding                  = gt::px(7),
      table.font.size                   = gt::px(11)
    ) |>
    gt::opt_table_font(
      font = list("Segoe UI", "Helvetica", "Arial", "sans-serif")
    ) |>
    gt::opt_row_striping() |>
    gt::as_raw_html()

  gt_table_html <- as.character(gt_table_html)

  # --- STEP 3: Build professional HTML email body ---
  email_body_html <- glue::glue(
    .open = "<<", .close = ">>",
    r"(
    <div style="font-family: 'Segoe UI', Helvetica, Arial, sans-serif; max-width: 640px; margin: 0 auto; background-color: #ffffff; border: 1px solid #dee2e6; border-radius: 6px; overflow: hidden;">

      <div style="background-color: #405D8B; padding: 20px 28px;">
        <h1 style="color: #ffffff; font-size: 17px; margin: 0 0 5px 0; font-weight: 600; letter-spacing: 0.2px;">
          Rapport Financier Hebdomadaire
        </h1>
        <p style="color: #c8d5e8; font-size: 12px; margin: 0;"><<doc_title>></p>
      </div>

      <div style="padding: 24px 28px;">

        <p style="color: #2c3e50; font-size: 12px; font-weight: 700; text-transform: uppercase; letter-spacing: 0.8px; margin: 0 0 14px 0;">
          Mouvements de march&eacute;
        </p>

        <<gt_table_html>>

        <p style="color: #6c757d; font-size: 13px; line-height: 1.6; margin: 18px 0 0 0;">
          L&rsquo;analyse macro compl&egrave;te, g&eacute;n&eacute;r&eacute;e par IA, est disponible en pi&egrave;ce jointe.
        </p>

        <hr style="border: none; border-top: 1px solid #e9ecef; margin: 18px 0;">

        <p style="font-size: 11px; color: #adb5bd; margin: 0;">
          <a href="https://www.linkedin.com/in/erwann-scaon-data-analyst" style="color: #405D8B; text-decoration: none; font-weight: 600;">e_scaon</a>
          &nbsp;&middot;&nbsp; G&eacute;n&eacute;r&eacute; automatiquement
        </p>

      </div>

    </div>
    )"
  )

  # Configuration SMTPS
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

  mail_html <- envelope(
    from = email_user,
    to = email_user,
    subject = paste("Weekly Financial Report -", format(Sys.Date(), "%Y-%m-%d"))
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
