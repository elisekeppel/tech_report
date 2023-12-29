# Dec 5 start exploring Innes

# From Innes et al (2002):
# Instead of the second term of the Borchers et al.
# (1998b) variance the variance estimator of
# Buckland et al. (2001; equation 3.78) was used
# applied to the estimated abundance rather than
# the observed number of sightings. Thus, it includes
# variation in both encounter rate and
# group size.

# EK : however, in dht2, don't we combine var from det func, ER, and group size?
# why might one method be better than the other?

## In all seasons, O2 performs best without Innes = F. R2 performs less well w/o Innes,
# then O2 with Innes, then R2 with Innes

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
) %>% mutate(Region.Label="cemore") %>% arrange(Sample.Label)
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


obs_hw <- full_join(hw, effort)

hw_w <- obs_hw %>% filter(season=="Winter") #doesn't work yet - we have some winter surveys without sightings and summer with only one transect with a sighting
hw_sp <- obs_hw %>% filter(season=="Spring")
hw_su <- obs_hw %>% filter(season=="Summer") #doesn't work yet - we have some winter surveys without sightings and summer with only one transect with a sighting (ie. no variability)
hw_f <- obs_hw %>% filter(season=="Fall")

hw1.5.hn.v    <-ds(obs_hw, key = "hn", truncation = 1.5, formula=~Clumped_Visibility)

#########################################################
#########################################################
#   ER_EST = O2 INSTEAD OF R2
# er_est = O2,
# strat_formula = year
# innes = F
########################################################
#########################################################

# here, all data in a season in a given year are pooled, and stratn is by year (stratification = "replicate", strat_formula = ~year)
# Analysing with Buckland method (Innes = False, er_var computed using method where encounter rate = number of observations divided by effort)
# er_est = O2
hw_O2_y_i_w <- dht2y(
  ddf = hw1.5.hn.v, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = hw_w,
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

# Still using dht2y :)
#####################################
# also, I made other edits : see EK edits in dht2y
#####################################

hw_O2_y_i_w
# w %>% group_by(SurveyID) %>% summarise(sgts=sum(!is.na(size)))
# unique(w$SurveyID)
saveRDS(hw_O2_y_i_w, "variance/hw/year/o2 innes/hw_O2_y_w.rds")
###################################

hw_R2_y_i_w <- dht2y(
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
  innes = TRUE,
  stratification = "replicate",
  total_area = NULL,
  binomial_var = FALSE
)
# Error in `mutate()`:
#   i In argument: `.originalorder = 1:nrow(erdat)`.
# i In group 1: `year = 2020`.
# Caused by error:
#   ! `.originalorder` must be size 34 or 1, not 124.

# ER_var_f() in dht2() breaks when trying to assign .originalorder to data by 1:nrow(res) since it's grouped)
# seems unneeded, so stepped through skipping that one line.
hw_R2_y_i_w
# unique(all_effort_lines[all_effort_lines$season=="Spring",]$SurveyID) %>% length()
# sp %>% group_by(SurveyID) %>% summarise(sgts=n())

saveRDS(hw_R2_y_i_w, "variance/hw/year/r2 innes/hw_R2_y_i_w.rds")
# hw_O2_y_sp <- readRDS("variance/hw/year/hw_O2_y_sp.rds")
#######################################################
hw_O2_y_i_su <- dht2y(
  ddf = hw1.5.hn.v, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = hw_su,
  strat_formula = ~year,
  convert_units = 1,
  er_est = "O2",
  multipliers = NULL,
  sample_fraction = 1,
  ci_width = 0.95,
  innes = T,
  stratification = "replicate",
  total_area = NULL,
  binomial_var = FALSE
)
# Error in `mutate()`:
#   i In argument: `.originalorder = 1:nrow(erdat)`.
# i In group 1: `year = 2020`.
# Caused by error:
#   ! `.originalorder` must be size 34 or 1, not 124.

# ER_var_f() in dht2() breaks when trying to assign .originalorder to data by 1:nrow(res) since it's grouped)
# seems unneeded, so stepped through skipping that one line.
hw_O2_y_i_su
# unique(all_effort_lines[all_effort_lines$season=="Summer",]$SurveyID)
# su %>% group_by(SurveyID) %>% summarise(sgts=n())
# unique(su$SurveyID)
saveRDS(hw_O2_y_i_su, "variance/hw/year/o2 innes/hw_O2_y_i_su.rds")
#########################################################

hw_R2_y_i_su <- dht2y(
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
  innes = T,
  stratification = "replicate",
  total_area = NULL,
  binomial_var = FALSE
)
# Error in `mutate()`:
#   i In argument: `.originalorder = 1:nrow(erdat)`.
# i In group 1: `year = 2020`.
# Caused by error:
#   ! `.originalorder` must be size 34 or 1, not 124.

# ER_var_f() in dht2() breaks when trying to assign .originalorder to data by 1:nrow(res) since it's grouped)
# seems unneeded, so stepped through skipping that one line.
hw_R2_y_i_su
# unique(all_effort_lines[all_effort_lines$season=="Summer",]$SurveyID)
# su %>% group_by(SurveyID) %>% summarise(sgts=n())
# unique(su$SurveyID)
saveRDS(hw_R2_y_i_su, "variance/hw/year/r2 innes/hw_R2_y_i_su.rds")
#########################################################

hw_O2_y_i_f <- dht2y(
  ddf = hw1.5.hn.v, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = hw_f,
  strat_formula = ~year,
  convert_units = 1,
  er_est = "O2",
  multipliers = NULL,
  sample_fraction = 1,
  ci_width = 0.95,
  innes = T,
  stratification = "replicate",
  total_area = NULL,
  binomial_var = FALSE
)
hw_O2_y_i_f
# unique(all_effort_lines[all_effort_lines$season=="Fall",]$SurveyID)
# f %>% group_by(SurveyID) %>% summarise(sgts=n())
# unique(f$SurveyID)
saveRDS(hw_O2_y_i_f, "variance/hw/year/o2 innes/hw_O2_y_i_f.rds")
#########################################################

hw_R2_y_i_f <- dht2y(
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
  innes = T,
  stratification = "replicate",
  total_area = NULL,
  binomial_var = FALSE
)
hw_R2_y_i_f
# unique(all_effort_lines[all_effort_lines$season=="Fall",]$SurveyID)
# f %>% group_by(SurveyID) %>% summarise(sgts=n())
# unique(f$SurveyID)
saveRDS(hw_R2_y_i_f, "variance/hw/year/r2 innes/hw_R2_y_i_f.rds")
#########################################################

hw_O2_y_i_sp <- dht2y(
  ddf = hw1.5.hn.v, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = hw_sp,
  strat_formula = ~year,
  convert_units = 1,
  er_est = "O2",
  multipliers = NULL,
  sample_fraction = 1,
  ci_width = 0.95,
  innes = T,
  stratification = "replicate",
  total_area = NULL,
  binomial_var = FALSE
)
hw_O2_y_i_sp
# unique(all_effort_lines[all_effort_lines$season=="Fall",]$SurveyID)
# f %>% group_by(SurveyID) %>% summarise(sgts=n())
# unique(f$SurveyID)
saveRDS(hw_O2_y_i_sp, "variance/hw/year/o2 innes/hw_O2_y_i_sp.rds")
#########################################################

hw_R2_y_i_sp <- dht2y(
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
  innes = T,
  stratification = "replicate",
  total_area = NULL,
  binomial_var = FALSE
)
hw_R2_y_i_sp
# unique(all_effort_lines[all_effort_lines$season=="Fall",]$SurveyID)
# f %>% group_by(SurveyID) %>% summarise(sgts=n())
# unique(f$SurveyID)
saveRDS(hw_R2_y_i_sp, "variance/hw/year/r2 innes/hw_R2_y_i_sp.rds")
#########################################################

# hw_O2_y_w2 <-  hw_O2_y_w %>% mutate(season="winter")
# hw_O2_y_sp2 <- hw_O2_y_sp %>% mutate(season="spring")
# hw_O2_y_su2 <- hw_O2_y_su %>% mutate(season="summer")
# hw_O2_y_f2 <-  hw_O2_y_f %>% mutate(season="fall")
# hw_O2_y <- rbind(hw_O2_y_w2 ,
#                  hw_O2_y_sp2,
#                  hw_O2_y_su2,
#                  hw_O2_y_f2 )
# write.csv(hw_O2_yhw_O2_y, "variance/hw/year/hw_O2_y.csv", row.names = F)
#########################################################
#NEXT  STEPS
i <- list(hw_O2_y_i_f,
           hw_O2_y_i_w,
           hw_O2_y_i_sp,
           hw_O2_y_i_su,
           hw_R2_y_i_f,
           hw_R2_y_i_w,
           hw_R2_y_i_sp,
           hw_R2_y_i_su)
saveRDS(i, "variance/hw/innes.rds")

#########################################################