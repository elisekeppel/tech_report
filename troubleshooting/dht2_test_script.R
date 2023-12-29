

# Detection function call:
# df <-ds(dataset, key = "hn", truncation = 1.5, formula=~Clumped_Visibility)

# Example of error when using er_est = "O2".
# Expect this is due to grouping of "res" dataset going into er_var_f() ~s line 735,
# and then trying to create a new index colum ~ line 33.

dht2_test <- dht2(
  ddf = hw_df, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = flatfile,
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
# Error in `mutate()`:
#   i In argument: `.originalorder = 1:nrow(erdat)`.
# i In group 1: `year = 2021`.
# Caused by error:
#   ! `.originalorder` must be size 42 or 1, not 68.


############################################################################

# Example of error when one of the "replicates" (here, "SurveyID") contains no sightings.
# It seems to break at line 694 calling varNhat(), where at line 62 it tries to pass variance values
# from the deltamethod output to each of the SurveyID's in `vardat_str`.


dht2_test2 <- dht2(
  ddf = hw_df, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = flatfile,
  strat_formula = ~SurveyID,
  convert_units = 1,
  er_est = "R2",
  multipliers = NULL,
  sample_fraction = 1,
  ci_width = 0.95,
  innes = FALSE,
  stratification = "replicate",
  total_area = NULL,
  binomial_var = FALSE
)

# Error in `$<-`:
#   ! Assigned data `diag(dm$variance)` must be compatible with existing data.
# x Existing data has 5 rows.
# x Assigned data has 3 rows.
# i Only vectors of size 1 are recycled.
# Caused by error in `vectbl_recycle_rhs_rows()`:
#   ! Can't recycle input of size 3 to size 5.
