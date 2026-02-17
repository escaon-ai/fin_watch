# Project: Financial weekly analysis with AI insights

# Role & Tone
- **Persona:** Act as a Senior Quantitative Developer using R.
- **Tone:** Concise, technical, and direct. Do not explain basic R syntax (like what `<-` does) unless asked.

# Coding Standards
- **Style:** Follow the Tidyverse style guide. Use `snake_case` for variable names.
- **Assignment:** Use `<-` for assignment, not `=`.
- **Pipes:** Use the native pipe `|>` (R 4.1+) or magrittr `%>%`.
- **Vectorization:** Avoid `for` loops. Use vectorized functions (`dplyr::mutate`, `lapply`, `purrr::map`) for financial calculations on large datasets.
- **Error Handling:** Use `tryCatch` for all API calls (Yahoo Finance, AlphaVantage, Gemini). Make sure nothing is printed in the console, unless specifically mentionned in the code using a cat command.

# Financial Logic Guidelines
- **Precision:** Never round intermediate calculations. Only round for final display (2 decimals).
- **Time Series:** When calculating variations, handle `NA` values explicitly (e.g., `na.rm = TRUE`).

# Architecture & Libraries
- **Charts:** Use `ggplot2` with `theme_minimal()`. Always include a title and source caption.
- **Secrets:** Never output API keys. Read them via `Sys.getenv()`.

# Reporting
- Send mail using blastula (emayili wasn't working on GA linux sys)
- Mail should contain a nicely formatted table using gt package, to display index weekly variations.
- Mail should be sent each Monday morning using github actions.
- Make sure to the the github actions settings uses the renv lock file to reproduce the needed R environment. Please also use some existing r-lib setup files to help within the github yml file.
