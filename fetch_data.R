library(conflicted)

conflicts_prefer(
  dplyr::filter
)

library(dplyr)
library(magrittr)
library(lubridate)
library(tidyquant)
library(ecb)

# start_last_7d <- today() %m-% weeks(1)
start_last_complweek <- floor_date(today(), unit = "week", week_start = 7) - days(6)

yahoo <- tq_get(
  c("DCAM.PA", "PCEU.PA", "BTC-EUR"),
  get = "stock.prices",
  from = start_last_complweek, # start_last_7d
  to = start_last_complweek + days(6)
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

ecb <- get_data(
  key = "EST.B.EU000A2X2A25.WT",
  filter = list(
    startPeriod = as.character(start_last_complweek),
    endPeriod = as.character(start_last_complweek + days(6))
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

fin_data <- bind_rows(yahoo, ecb)
