# Dec 5 start exploring Innes

# From Innes et al (2002):
# Instead of the second term of the Borchers et al.
# (1998b) variance the variance estimator of
# Buckland et al. (2001; equation 3.78) was used
# applied to the estimated abundance rather than
# the observed number of sightings. Thus, it includes
# variation in both encounter rate and
# group size.
# innes - default is true in dht(), estimated abundance/effort for each transect
# false - default is False in dht2(), number of obs/effort for each transect
#

sp <-  "harbour"
df.hp <- all_ap_sf %>% filter(Species %like% sp) %>% data.frame()

hp <- df.hp %>% dplyr::select(
  SurveyID,
  Sample.Label=order,
  object,
  distance,
  Species,
  size = Group_Size,
  Clumped_Beaufort,
  Observer,
  season,
  year) %>% mutate(Region.Label="cemore") %>% arrange(Sample.Label)

# find transects with no sightings and create NA distance rows
effort <- all_effort_lines %>%
  data.frame() %>%
  mutate(
    seasonYear = factor(seasonYear, levels = unique(seasonYear))) %>%
  # unique segment ID's and length for each
  transmute(Sample.Label=order, # field 'order' = new transect numbers applied geographically with all transects in all surveys pooled (in 03_results.Rmd)
            SurveyID = SurveyID,
            year = year,
            month = month,
            season,
            Region.Label="cemore",
            seasonYear = seasonYear,
            Effort = st_length(geometry),
            Area=B) %>%
  # group_by(Region.Label,Area,Sample.Label) %>%
  group_by(Region.Label, Sample.Label, Area, year, SurveyID, season, seasonYear) %>% #, month
  summarise(Effort=as.numeric(sum(Effort))/1000) %>% ungroup()

hp_obs <- full_join(hp, effort)

hp.0.65.hn.bfc.obs <- ds(data=hp_obs, truncation=0.65, key="hn", formula=~Clumped_Beaufort+Observer)

duplicated(effort) %>% sum()

hp_w <- hp_obs %>% filter(season=="Winter")
hp_sp <- hp_obs %>% filter(season=="Spring")
hp_su <- hp_obs %>% filter(season=="Summer")
hp_f <- hp_obs %>% filter(season=="Fall")


###################################
# er_est = O2,
# strat_formula = SurveyID
# innes = T
###################################

# here, all data in a season (across years) is pooled, then stratn is by surveyid (stratification = "replicate", strat_formula = ~SurveyID)
# Currently, all transects are pooled within season, no grouping of surveys or years
# Analysing with Buckland method (innes = TRUE, er_var computed using method where encounter rate = number of observations divided by effort)
# er_est = R2
# w %<>% filter(SurveyID %in% c("cemore_2021jan", "cemore_2022feb", "cemore_2022jan"))
hp_O2_i_w <- dht2y(
  ddf = hp.0.65.hn.bfc.obs, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = hp_w,
  strat_formula = ~SurveyID,
  convert_units = 1,
  er_est = "O2",
  multipliers = NULL,
  sample_fraction = 1,
  ci_width = 0.95,
  innes = TRUE,
  stratification = "replicate",
  total_area = NULL,
  binomial_var = FALSE
)
hp_O2_i_w
# w %>% group_by(SurveyID) %>% summarise(sgts=sum(!is.na(size)))
# unique(w$SurveyID)
saveRDS(hp_O2_i_w, "variance/hp/surveyid/O2/hp_O2_i_w.rds")
###################################

hp_O2_i_sp <- dht2y(
  ddf = hp.0.65.hn.bfc.obs, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = hp_sp,
  strat_formula = ~SurveyID,
  convert_units = 1,
  er_est = "O2",
  multipliers = NULL,
  sample_fraction = 1,
  ci_width = 0.95,
  innes = TRUE,
  stratification = "replicate",
  total_area = NULL,
  binomial_var = FALSE
)
hp_O2_i_sp
# unique(all_effort_lines[all_effort_lines$season=="Spring",]$SurveyID) %>% length()
# sp %>% group_by(SurveyID) %>% summarise(sgts=n())

saveRDS(hp_O2_i_sp, "variance/hp/surveyid/O2/hp_O2_i_sp.rds")
#########################################################
hp_O2_i_su <- dht2y(
  ddf = hp.0.65.hn.bfc.obs, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = hp_su,
  strat_formula = ~SurveyID,
  convert_units = 1,
  er_est = "O2",
  multipliers = NULL,
  sample_fraction = 1,
  ci_width = 0.95,
  innes = TRUE,
  stratification = "replicate",
  total_area = NULL,
  binomial_var = FALSE
)
hp_O2_i_su
# unique(all_effort_lines[all_effort_lines$season=="Summer",]$SurveyID)
# su %>% group_by(SurveyID) %>% summarise(sgts=n())
# unique(su$SurveyID)
saveRDS(hp_O2_i_su, "variance/hp/surveyid/O2/hp_O2_i_su.rds")
#########################################################

hp_O2_i_f <- dht2y(
  ddf = hp.0.65.hn.bfc.obs, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = hp_f,
  strat_formula = ~SurveyID,
  convert_units = 1,
  er_est = "O2",
  multipliers = NULL,
  sample_fraction = 1,
  ci_width = 0.95,
  innes = TRUE,
  stratification = "replicate",
  total_area = NULL,
  binomial_var = FALSE
)
hp_O2_i_f
# unique(all_effort_lines[all_effort_lines$season=="Fall",]$SurveyID)
# f %>% group_by(SurveyID) %>% summarise(sgts=n())
# unique(f$SurveyID)
saveRDS(hp_O2_i_f, "variance/hp/surveyid/O2hp_O2_i_f.rds")
#########################################################


#########################################################

#########################################################
#########################################################
#   STRAT_FORM = year INSTEAD OF surveyid
########################################################
#########################################################

# here, all data in a season (across years) is pooled, then stratn is by surveyid (stratification = "replicate", strat_formula = ~SurveyID)
# Currently, all transects are pooled within season, no grouping of surveys or years
# Analysing with Buckland method (innes = TRUE, er_var computed using method where encounter rate = number of observations divided by effort)
# er_est = R2
# w %<>% filter(SurveyID %in% c("cemore_2021jan", "cemore_2022feb", "cemore_2022jan"))
hp_O2_y_i_w <- dht2y(
  ddf = hp.0.65.hn.bfc.obs, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = hp_w,
  strat_formula = ~year,
  convert_units = 1,
  er_est = "O2",
  multipliers = NULL,
  sample_fraction = 1,
  ci_width = 0.95,
  innes = TRUE,
  stratification = "replicate",
  total_area = NULL,
  binomial_var = FALSE
)
hp_O2_y_i_w
saveRDS(hp_O2_y_i_w, "variance/hp/year/O2/hp_O2_y_i_w.rds")
###################################

hp_O2_y_i_sp <- dht2y(
  ddf = hp.0.65.hn.bfc.obs, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = hp_sp,
  strat_formula = ~year,
  convert_units = 1,
  er_est = "O2",
  multipliers = NULL,
  sample_fraction = 1,
  ci_width = 0.95,
  innes = TRUE,
  stratification = "replicate",
  total_area = NULL,
  binomial_var = FALSE
)
hp_O2_y_i_sp
saveRDS(hp_O2_y_i_sp, "variance/hp/year/O2/hp_O2_y_i_sp.rds")
#########################################################
hp_O2_y_i_su <- dht2y(
  ddf = hp.0.65.hn.bfc.obs, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = hp_su,
  strat_formula = ~year,
  convert_units = 1,
  er_est = "O2",
  multipliers = NULL,
  sample_fraction = 1,
  ci_width = 0.95,
  innes = TRUE,
  stratification = "replicate",
  total_area = NULL,
  binomial_var = FALSE
)
hp_O2_y_i_su
# unique(all_effort_lines[all_effort_lines$season=="Summer",]$SurveyID)
# su %>% group_by(SurveyID) %>% summarise(sgts=n())
# unique(su$SurveyID)
saveRDS(hp_O2_y_i_su, "variance/hp/year/O2/hp_O2_y_i_su.rds")
#########################################################

hp_O2_y_i_f <- dht2y(
  ddf = hp.0.65.hn.bfc.obs, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = hp_f,
  strat_formula = ~year,
  convert_units = 1,
  er_est = "O2",
  multipliers = NULL,
  sample_fraction = 1,
  ci_width = 0.95,
  innes = TRUE,
  stratification = "replicate",
  total_area = NULL,
  binomial_var = FALSE
)
hp_O2_y_i_f
# unique(all_effort_lines[all_effort_lines$season=="Fall",]$SurveyID)
# f %>% group_by(SurveyID) %>% summarise(sgts=n())
# unique(f$SurveyID)
saveRDS(hp_O2_y_i_f, "variance/hp/year/O2/hp_O2_y_i_f.rds")
#########################################################

hp_O2_i_w
hp_O2_i_sp
hp_O2_i_su
hp_O2_i_f

hp_O2_y_i_w
hp_O2_y_i_sp
hp_O2_y_i_su
hp_O2_y_i_f


# For hp, all seasons had lower contribution of ER var with survey id than with year as strat var
# notably, all seasons other than summer had lower cv with year as strat var
#########################################################
O2_i_sid_vs_y <- list(hp_O2_i_f,
                      hp_O2_i_w,
                      hp_O2_i_sp,
                      hp_O2_i_su,
                      hp_O2_y_i_f,
                      hp_O2_y_i_w,
                      hp_O2_y_i_sp,
                      hp_O2_y_i_su)
saveRDS(O2_sid_vs_y, "variance/hp/O2_i_sid_vs_y.rds")
#NEXT  STEPS

#########################################################

# strat by surveyid
# strat by year (which therefore == seasonyear), on data for each season as above == interannual variabiility


# Innes vs not
# O2 vs R2

# save Rdata
# share summary in sharepoint for each output (for each sp)



