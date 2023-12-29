x <- sequence(137/2, 1, 2)
y <- hw_w[x,] %>% dplyr::select(-c(seasonYear, month))
saveRDS(y, "C:/Users/KeppelE/Documents/GitHub/cemore/tech_report/troubleshooting/hw_flatfile.rds")

saveRDS(hw1.5.hn.v, "C:/Users/KeppelE/Documents/GitHub/cemore/tech_report/troubleshooting/hw_df.rds")

dhtw_test <- dht2(
  ddf = hw1.5.hn.v, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = y,
  strat_formula = ~year,
  convert_units = 1,
  er_est = "O2",
  multipliers = NULL,
  sample_fraction = 1,
  ci_width = 0.95,
  innes = FALSE,
  stratification = "replicate",
  total_area = NULL,
  binomial_var = FALSE
)


hw1.5.hn.v    <-ds(obs_hw, key = "hn", truncation = 1.5, formula=~Clumped_Visibility)
