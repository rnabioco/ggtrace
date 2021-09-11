library(tidyverse)

stocks <- as_tibble(EuStockMarkets) %>%
  mutate(day = as.numeric(rownames(.))) %>%
  pivot_longer(-day)

usethis::use_data(stocks)
