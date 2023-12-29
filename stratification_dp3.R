# Oct 3, 2023
#
#
# innes - default is true in dht(), estimated abundance/effort for each transect
# false - default is False in dht2(), number of obs/effort for each transect
#
# er_est is how we put them together (ie. er_var from ds())
#
# stratification = Thom's infographic (how to stratify)
sp <-  "Dall's"
df.dp <- all_ap_sf %>% filter(Species %like% sp) %>% data.frame()

dp <- df.dp %>% dplyr::select(
  SurveyID,
  Sample.Label=order,
  object,
  distance,
  Species,
  size = Group_Size,
  Clumped_Swell,
  Glare90,
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

dp_obs <- full_join(dp, effort)

dp0.8.hn.swc.g90c <- ds(data=dp_obs, truncation=0.8, key="hn", formula=~Clumped_Swell+Glare90) # swell y/n, glare mild/none or severe

duplicated(effort) %>% sum()

dp_w <- dp_obs %>% filter(season=="Winter")
dp_sp <- dp_obs %>% filter(season=="Spring")
dp_su <- dp_obs %>% filter(season=="Summer")
dp_f <- dp_obs %>% filter(season=="Fall")

################################################
# DATA SUMMARIES
################################################
dp.8 <- dp %>% filter(distance<=.8)
obs_dp.8 <- full_join(dp.8, effort)

obs_dp.8 %>% st_drop_geometry() %>%
  mutate(size=case_when(is.na(size) ~ 0, !is.na(size) ~ size)) %>%
  group_by(season) %>%
  summarise(surveys=length(unique(SurveyID)),
            transects=length(unique(Sample.Label)),
            distance_km=round(sum(unique(Effort)),0),
            sightings=sum(!is.na(object)),
            indivs = sum(size)) %>%
  write.csv("variance/dp/dp_eff_sgt_summary_season.csv",row.names=F)
obs_dp.8 %>% st_drop_geometry() %>%
  mutate(size=case_when(is.na(size) ~ 0, !is.na(size) ~ size)) %>%
  group_by(season,year) %>%
  summarise(surveys=length(unique(SurveyID)),
            transects=length(unique(Sample.Label)),
            distance_km=round(sum(unique(Effort)),0),
            sightings=sum(!is.na(object)),
            indivs = sum(size)) %>%
  write.csv("variance/dp/dp_eff_sgt_summary_seasonyear.csv",row.names=F)
obs_dp.8 %>% st_drop_geometry() %>%
  mutate(size=case_when(is.na(size) ~ 0, !is.na(size) ~ size)) %>%
  group_by(season,year,SurveyID) %>%
  summarise(transects=length(unique(Sample.Label)),
            distance_km=round(sum(unique(Effort)),0),
            sightings=sum(!is.na(object)),
            indivs = sum(size)) %>%
  write.csv("variance/dp/dp_eff_sgt_summary_surveyid.csv",row.names=F)

###################################
# er_est = R2,
# strat_formula = SurveyID
# innes = F
###################################

# here, all data in a season (across years) is pooled, then stratn is by surveyid (stratification = "replicate", strat_formula = ~SurveyID)
# Currently, all transects are pooled within season, no grouping of surveys or years
# Analysing with Buckland method (Innes = False, er_var computed using method where encounter rate = number of observations divided by effort)
# er_est = R2
# w %<>% filter(SurveyID %in% c("cemore_2021jan", "cemore_2022feb", "cemore_2022jan"))
dp_R2_w <- dht2y(
  ddf = dp0.8.hn.swc.g90c, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = dp_w,
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
dp_R2_w
# w %>% group_by(SurveyID) %>% summarise(sgts=sum(!is.na(size)))
# unique(w$SurveyID)
saveRDS(dp_R2_w, "variance/dp/surveyid/dp_R2_w.rds")
###################################

dp_R2_sp <- dht2y(
  ddf = dp0.8.hn.swc.g90c, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = dp_sp,
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
dp_R2_sp
# unique(all_effort_lines[all_effort_lines$season=="Spring",]$SurveyID) %>% length()
# sp %>% group_by(SurveyID) %>% summarise(sgts=n())

saveRDS(dp_R2_sp, "variance/dp/surveyid/dp_R2_sp.rds")
#########################################################
dp_R2_su <- dht2y(
  ddf = dp0.8.hn.swc.g90c, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = dp_su,
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
dp_R2_su
# unique(all_effort_lines[all_effort_lines$season=="Summer",]$SurveyID)
# su %>% group_by(SurveyID) %>% summarise(sgts=n())
# unique(su$SurveyID)
saveRDS(dp_R2_su, "variance/dp/surveyid/dp_R2_su.rds")
#########################################################

dp_R2_f <- dht2y(
  ddf = dp0.8.hn.swc.g90c, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = dp_f,
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
dp_R2_f
# unique(all_effort_lines[all_effort_lines$season=="Fall",]$SurveyID)
# f %>% group_by(SurveyID) %>% summarise(sgts=n())
# unique(f$SurveyID)
saveRDS(dp_R2_f, "variance/dp/surveyid/dp_R2_f.rds")
#########################################################


#########################################################

#########################################################
#########################################################
#   STRAT_FORM = year INSTEAD OF surveyid
########################################################
#########################################################

# here, all data in a season (across years) is pooled, then stratn is by surveyid (stratification = "replicate", strat_formula = ~SurveyID)
# Currently, all transects are pooled within season, no grouping of surveys or years
# Analysing with Buckland method (Innes = False, er_var computed using method where encounter rate = number of observations divided by effort)
# er_est = R2
# w %<>% filter(SurveyID %in% c("cemore_2021jan", "cemore_2022feb", "cemore_2022jan"))
dp_R2_y_w <- dht2y(
  ddf = dp0.8.hn.swc.g90c, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = dp_w,
  strat_formula = ~year,
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
dp_R2_y_w
saveRDS(dp_R2_y_w, "variance/dp/year/dp_R2_y_w.rds")
###################################

dp_R2_y_sp <- dht2y(
  ddf = dp0.8.hn.swc.g90c, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = dp_sp,
  strat_formula = ~year,
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
dp_R2_y_sp
saveRDS(dp_R2_y_sp, "variance/dp/year/dp_R2_y_sp.rds")
#########################################################
dp_R2_y_su <- dht2y(
  ddf = dp0.8.hn.swc.g90c, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = dp_su,
  strat_formula = ~year,
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
dp_R2_y_su
# unique(all_effort_lines[all_effort_lines$season=="Summer",]$SurveyID)
# su %>% group_by(SurveyID) %>% summarise(sgts=n())
# unique(su$SurveyID)
saveRDS(dp_R2_y_su, "variance/dp/year/dp_R2_y_su.rds")
#########################################################

dp_R2_y_f <- dht2y(
  ddf = dp0.8.hn.swc.g90c, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = dp_f,
  strat_formula = ~year,
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
dp_R2_y_f
# unique(all_effort_lines[all_effort_lines$season=="Fall",]$SurveyID)
# f %>% group_by(SurveyID) %>% summarise(sgts=n())
# unique(f$SurveyID)
saveRDS(dp_R2_y_f, "variance/dp/year/dp_R2_y_f.rds")
#########################################################
dp_R2_w
dp_R2_sp
# dp_R2_su
dp_R2_f

dp_R2_y_w
dp_R2_y_sp
dp_R2_y_su
dp_R2_y_f

# For dp, all seasons had lower contribution of ER var with survey id than with year as strat var
# notably, all seasons other than summer had lower cv with year as strat var
#########################################################
r2_sid_vs_y <- list(dp_R2_f,
                    dp_R2_w,
                    dp_R2_sp,
                    dp_R2_su,
                    dp_R2_y_f,
                    dp_R2_y_w,
                    dp_R2_y_sp,
                    dp_R2_y_su)
saveRDS(r2_sid_vs_y, "variance/dp/r2_sid_vs_y.rds")
#NEXT  STEPS

#########################################################

# strat by surveyid
# strat by year (which therefore == seasonyear), on data for each season as above == interannual variabiility


# Innes vs not
# O2 vs R2

# save Rdata
# share summary in sharepoint for each output (for each sp)



