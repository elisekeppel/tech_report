# Oct 3, 2023
#
#
# innes - default is true in dht(), estimated abundance/effort for each transect
# false - default is False in dht2(), number of obs/effort for each transect
#
# er_est is how we put them together (ie. er_var from ds())
#
# stratification = Thom's infographic (how to stratify)
sp <-  "humpback"
df.hw <- all_ap_sf %>% filter(Species %like% sp) %>% data.frame()

hw <- df.hw %>% dplyr::select(Region.Label,
                              SurveyID,
                              Sample.Label=order,
                              object,
                              distance,
                              Species,
                              size = Group_Size,
                              Clumped_Visibility,
                              season,
                              year,
                              month
) %>%  arrange(Sample.Label) %>%
  mutate(
  # Region.Label="cemore",
         object = 1:nrow(df.hw))

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
            Region.Label=season,
            # Region.Label="cemore",
            seasonYear = seasonYear,
            Effort = st_length(geometry),
            Area=B) %>% # in hectares
  # group_by(Region.Label,Area,Sample.Label) %>%
  group_by(Region.Label, Sample.Label, Area, year, SurveyID, season, seasonYear) %>% #, month
  summarise(Effort=as.numeric(sum(Effort))/1000) %>% # to go from m to km
  ungroup()


obs_hw <- full_join(hw, effort) %>% mutate(Region.Label=as.character(Region.Label))

hw_w <- obs_hw %>% filter(season=="Winter") #doesn't work yet - we have some winter surveys without sightings and summer with only one transect with a sighting
hw_sp <- obs_hw %>% filter(season=="Spring")
hw_su <- obs_hw %>% filter(season=="Summer") #doesn't work yet - we have some winter surveys without sightings and summer with only one transect with a sighting (ie. no variability)
hw_f <- obs_hw %>% filter(season=="Fall")

conversion.factor <- convert_units("meter", "kilometer", "square kilometre")
hw1.5.hn.v    <-ds(obs_hw, key = "hn", truncation = 1.5, formula=~Clumped_Visibility,  convert_units=conversion.factor)

# **FIXED** why are there only 3 surveyid's showing up for hw in winter? the join above should bring in all transects of all surveys.
# unique(all_effort_lines[all_effort_lines$season=="Winter",]$SurveyID)
# all_ap_sf %>% st_drop_geometry() %>% group_by(season) %>% summarise(n=length(unique(SurveyID)))
# hw %>% st_drop_geometry() %>% group_by(season) %>% summarise(n=length(unique(SurveyID)))
# hw %>% st_drop_geometry() %>% group_by(season,year) %>% summarise(n=length(unique(object)))
# effort %>% st_drop_geometry() %>%   group_by(season,year) %>% summarise(surveys=length(unique(SurveyID)), transects=length(unique(Sample.Label)), distance_km=sum(Effort)) %>%
  # write.csv("variance/hw/effort_summary.csv", row.names = F)
hw1.5 <- hw %>% filter(distance<=1.5)
obs_hw1.5 <- full_join(hw1.5, effort)

obs_hw1.5 %>% st_drop_geometry() %>%
  mutate(size=case_when(is.na(size) ~ 0, !is.na(size) ~ size)) %>%
  group_by(season) %>%
  summarise(surveys=length(unique(SurveyID)),
            transects=length(unique(Sample.Label)),
            distance_km=round(sum(unique(Effort)),0),
            sightings=sum(!is.na(object)),
            indivs = sum(size)) %>%
  write.csv("variance/hw/hw_eff_sgt_summary_season.csv",row.names=F)
obs_hw1.5 %>% st_drop_geometry() %>%
  mutate(size=case_when(is.na(size) ~ 0, !is.na(size) ~ size)) %>%
  group_by(season,year) %>%
  summarise(surveys=length(unique(SurveyID)),
            transects=length(unique(Sample.Label)),
            distance_km=round(sum(unique(Effort)),0),
            sightings=sum(!is.na(object)),
            indivs = sum(size)) %>%
  write.csv("variance/hw/hw_eff_sgt_summary_seasonyear.csv",row.names=F)
obs_hw1.5 %>% st_drop_geometry() %>%
  mutate(size=case_when(is.na(size) ~ 0, !is.na(size) ~ size)) %>%
  group_by(season,year,SurveyID) %>%
  summarise(transects=length(unique(Sample.Label)),
            distance_km=round(sum(unique(Effort)),0),
            sightings=sum(!is.na(object)),
            indivs = sum(size)) %>%
  write.csv("variance/hw/hw_eff_sgt_summary_surveyid.csv",row.names=F)

# dht2() ERROR: hw, winter, R2, innes=F
# Error in `$<-`:
# ! Assigned data `diag(dm$variance)` must be compatible with existing data.
# x Existing data has 5 rows.
# x Assigned data has 3 rows.
# i Only vectors of size 1 are recycled.
# Caused by error in `vectbl_recycle_rhs_rows()`:
#   ! Can't recycle input of size 3 to size 5.

# TEMP FIx: filtering out the surveys with no sightings allows dht2() to run to glimpse the analysis, though the er_var can't be calculated
# dht2() can't analyze survey's with no sightings in any transect.
# When there is only one sighting in a transect, it cannot compute er_var
# because it uses er = abund (or count) per effort for each transect

#########################################################
# TRYING TO FIX INTERNALLY WITHIN DHT2()
# Breaks around line 480:
#     df_unc <- Distance:::varNhat(model = ddf[[1]], data=res) ######### BREAKS HERE
# within varNhat(), breaks when samples (transects) without a sighting are removed
# ~ line 40 :   # data<- data[!is.na(data$object), ]
# because some surveys (feb 2021 and mar 2021) have no sightings at all so all
# existance of them in the data (the zero's) is removed from variance calcs
## I stepped through dht2 to line 480, and then through varNhat() to line ~40 and
# skipped removal of no-sighting transects - the variance output was the same
# except held places for the other surveys with zeros!
# Now to step through more of dht2 to see how it carries through...
#########################################################

#########################################################
#########################################################
###################################
# er_est = R2,
# strat_formula = SurveyID
# innes = F
###################################
#########################################################
#########################################################

# here, all data in a season (across years) is pooled, then stratn is by surveyid (stratification = "replicate", strat_formula = ~SurveyID)
# Currently, all transects are pooled within season, no grouping of surveys or years
# Analysing with Buckland method (Innes = False, er_var computed using method where encounter rate = number of observations divided by effort)
# er_est = R2
# hw_w_filtered <- hw_w %>% filter(SurveyID %in% c("cemore_2021jan", "cemore_2022feb", "cemore_2022jan"))

# removed issue in dht2() where surveys with no sightings weren't being carried through to summary tables :)
hw_R2_w <- dht2y(
  ddf = hw1.5.hn.v, # ds output
  observations = NULL,
  transects = NULL,
  geo_strat = NULL,
  flatfile = hw_w,
  # strat_formula = ~month,
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

###############################
# can step through without those surveys with zero hw sightings - perhaps the function can be adjusted
# as it seems the issue is just calculating variance, so maybe variance can be zero ???
###############################
hw_R2_w
# w %>% group_by(SurveyID) %>% summarise(sgts=sum(!is.na(size)))
# unique(w$SurveyID)
# saveRDS(hw_R2_w, "variance/hw/surveyid/hw_R2_w_____filtered.rds")
saveRDS(hw_R2_w, "variance/hw/surveyid/hw_R2_w_fixed.rds")

### see y <- readRDS("variance/hw/hw_R2_comparision.rds") for comparison of
# w = dht2run_with_only3surveys
# w2 = step_through
# w_ALL = step_through_with_ALL_surveys
# OVERALL, overestimate abundance when exclude surveys with no hw sightings,
# ER var for sightless surveys = 0, cv=0, not sure how to add these into the variance...
###################################

hw_R2_sp <- dht2y(
  ddf = hw1.5.hn.v, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = hw_sp,
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
hw_R2_sp
# unique(all_effort_lines[all_effort_lines$season=="Spring",]$SurveyID) %>% length()
# sp %>% group_by(SurveyID) %>% summarise(sgts=n())

saveRDS(hw_R2_sp, "variance/hw/surveyid/new/hw_R2_sp.rds")
#########################################################
hw_R2_su <- dht2y(
  ddf = hw1.5.hn.v, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = hw_su,
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
hw_R2_su
# unique(all_effort_lines[all_effort_lines$season=="Summer",]$SurveyID)
# su %>% group_by(SurveyID) %>% summarise(sgts=n())
# unique(su$SurveyID)
saveRDS(hw_R2_su, "variance/hw/surveyid/new/hw_R2_su.rds")
#########################################################

hw_R2_f <- dht2y(
  ddf = hw1.5.hn.v, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = hw_f,
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
hw_R2_f
# unique(all_effort_lines[all_effort_lines$season=="Fall",]$SurveyID)
# f %>% group_by(SurveyID) %>% summarise(sgts=n())
# unique(f$SurveyID)
saveRDS(hw_R2_f, "variance/hw/surveyid/new/hw_R2_f.rds")
#########################################################
hw_R2_w2 <-  hw_R2_w %>% mutate(season="winter")
hw_R2_sp2 <- hw_R2_sp %>% mutate(season="spring")
hw_R2_su2 <- hw_R2_su %>% mutate(season="summer")
hw_R2_f2 <-  hw_R2_f %>% mutate(season="fall")
hw_R2_surveyid <- rbind(hw_R2_w2, hw_R2_sp2, hw_R2_su2, hw_R2_f2)
write.csv(hw_R2_surveyid, "variance/hw/surveyid/hw_R2_surveyid.csv", row.names = F)
# create output summary using winter, all survey, step-through analysis
hw_R2_w2_all <-  hw_R2_w_ALL_SurveyIDs %>% mutate(season="winter")
hw_R2_surveyid_all <- rbind(hw_R2_w2_all, hw_R2_sp2, hw_R2_su2, hw_R2_f2)
write.csv(hw_R2_surveyid_all, "variance/hw/surveyid/hw_R2_surveyid_all.csv", row.names = F)

#########################################################

#########################################################
#########################################################
#   STRAT_FORM = YEAR INSTEAD OF surveyID
# er_est = R2,
# strat_formula = year
# innes = F
########################################################
#########################################################

# here, all data in a season (across years) is pooled, then stratn is by surveyid (stratification = "replicate", strat_formula = ~SurveyID)
# Currently, all transects are pooled within season, no grouping of surveys or years
# Analysing with Buckland method (Innes = False, er_var computed using method where encounter rate = number of observations divided by effort)
# er_est = R2
# w %<>% filter(SurveyID %in% c("cemore_2021jan", "cemore_2022feb", "cemore_2022jan"))
hw_R2_y_w <- dht2y(
  ddf = hw1.5.hn.v, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = hw_w,
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
hw_R2_y_w
# w %>% group_by(SurveyID) %>% summarise(sgts=sum(!is.na(size)))
# unique(w$SurveyID)
saveRDS(hw_R2_y_w, "variance/hw/year/R2/new/hw_R2_y_w.rds")
###################################

hw_R2_y_sp <- dht2y(
  ddf = hw1.5.hn.v, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = hw_sp,
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
hw_R2_y_sp
# unique(all_effort_lines[all_effort_lines$season=="Spring",]$SurveyID) %>% length()
# sp %>% group_by(SurveyID) %>% summarise(sgts=n())

saveRDS(hw_R2_y_sp, "variance/hw/year/R2/new/hw_R2_y_sp.rds")
#########################################################
hw_R2_y_su <- dht2y(
  ddf = hw1.5.hn.v, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = hw_su,
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
hw_R2_y_su
# unique(all_effort_lines[all_effort_lines$season=="Summer",]$SurveyID)
# su %>% group_by(SurveyID) %>% summarise(sgts=n())
# unique(su$SurveyID)
saveRDS(hw_R2_y_su, "variance/hw/year/R2/new/hw_R2_y_su.rds")
#########################################################

hw_R2_y_f <- dht2y(
  ddf = hw1.5.hn.v, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = hw_f,
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
hw_R2_y_f
# unique(all_effort_lines[all_effort_lines$season=="Fall",]$SurveyID)
# f %>% group_by(SurveyID) %>% summarise(sgts=n())
# unique(f$SurveyID)
saveRDS(hw_R2_y_f, "variance/hw/year/R2/new/hw_R2_y_f.rds")
#########################################################
hw_R2_y_w <-  hw_R2_y_w %>% mutate(season="winter")
hw_R2_y_sp <- hw_R2_y_sp %>% mutate(season="spring")
hw_R2_y_su <- hw_R2_y_su %>% mutate(season="summer")
hw_R2_y_f <-  hw_R2_y_f %>% mutate(season="fall")
hw_R2_y <- rbind(hw_R2_y_w, hw_R2_y_sp, hw_R2_y_su, hw_R2_y_f)
write.csv(hw_R2_y, "variance/hw/year/hw_R2_y.csv", row.names = F)
#########################################################
#NEXT  STEPS

#########################################################

# DONE # strat by surveyid
# DONE # strat by year (which therefore == seasonyear), on data for each season as above == interannual variabiility


# O2 vs R2
# Innes vs not

# save Rdata
# share summary in sharepoint for each output (for each sp)

r2 <- list(hw_R2_f,
        hw_R2_sp,
        hw_R2_su,
        hw_R2_y_f,
        hw_R2_y_w,
        hw_R2_y_sp,
        hw_R2_y_su)
saveRDS(r2, "variance/hw/R2.rds")
