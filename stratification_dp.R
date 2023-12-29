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

obs <- full_join(dp, effort)

dp0.8.hn.swc.g90c <- ds(data=obs, truncation=0.8, key="hn", formula=~Clumped_Swell+Glare90, er_var = "R2") # swell y/n, glare mild/none or severe

duplicated(effort) %>% sum()

w <- obs %>% filter(season=="Winter")
sp <- obs %>% filter(season=="Spring")
su <- obs %>% filter(season=="Summer")
f <- obs %>% filter(season=="Fall")

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
R_w <- dht2(
  ddf = dp0.8.hn.swc.g90c, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = w,
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
R_w
# w %>% group_by(SurveyID) %>% summarise(sgts=sum(!is.na(size)))
# unique(w$SurveyID)
saveRDS(R_w, "variance/dp/R2_w.rds")
###################################

R_sp <- dht2(
  ddf = dp0.8.hn.swc.g90c, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = sp,
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
R_sp
# unique(all_effort_lines[all_effort_lines$season=="Spring",]$SurveyID) %>% length()
# sp %>% group_by(SurveyID) %>% summarise(sgts=n())

saveRDS(R_sp, "variance/dp/R2_sp.rds")
#########################################################
R_su <- dht2(
  ddf = dp0.8.hn.swc.g90c, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = su,
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
R_su
# unique(all_effort_lines[all_effort_lines$season=="Summer",]$SurveyID)
# su %>% group_by(SurveyID) %>% summarise(sgts=n())
# unique(su$SurveyID)
saveRDS(R_su, "variance/dp/R2_su.rds")
#########################################################

R_f <- dht2(
  ddf = dp0.8.hn.swc.g90c, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = f,
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
R_f
# unique(all_effort_lines[all_effort_lines$season=="Fall",]$SurveyID)
# f %>% group_by(SurveyID) %>% summarise(sgts=n())
# unique(f$SurveyID)
saveRDS(R_f, "variance/dp/R2_f.rds")
#########################################################
R_w2 <- R_w %>% mutate(season="winter")
R_sp2 <- R_sp %>% mutate(season="spring")
R_su2 <- R_su %>% mutate(season="summer")
R_f2 <- R_f %>% mutate(season="fall")
R2_w <- rbind(R_w2, R_sp2, R_su2, R_f2)
write.csv(R2_w, "variance/dp/R2_dp.csv", row.names = F)
#########################################################
#NEXT  STEPS

#########################################################

# strat by surveyid
# strat by year (which therefore == seasonyear), on data for each season as above == interannual variabiility


# Innes vs not
# O2 vs R2

# save Rdata
# share summary in sharepoint for each output (for each sp)



