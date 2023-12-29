# Oct 3, 2023
#
# currently, all transects are pooled within season, no grouping of surveys or years
#
#
# innes - default is true in dht(), estimated abundance/effort for each transect
# false - number of obs/effort for each transect
#
# er_est is how we put them together (ie. er_var from ds())
#
# stratification = Thom;s infographic (how to stratify)
sp <-  "humpback"
df.hw <- all_ap_sf2 %>% filter(Species %like% sp) %>% data.frame()

hw <- df.hw %>% dplyr::select(
                              Survey=SurveyID,
                              Sample.Label,
                              object,
                              distance,
                              Species,
                              size = Group_Size,
                              Clumped_Visibility,
                              season,
                              year
                              # Clumped_Beaufort,
                              # Glare90,
                              # swell,
                              # Observer
) %>% mutate(Survey=paste0("cemore_",Survey))
# find transects with no sightings and create NA distance rows
effort <- all_effort_lines2 %>%
  data.frame() %>%
  mutate(
    seasonYear = factor(seasonYear, levels = unique(seasonYear))) %>%
  # unique segment ID's and length for each
  transmute(Sample.Label=order, #TransectID,
            SurveyID = SurveyID,
            year = year,
            month = month,
            season,
            seasonYear = seasonYear,
            Effort = st_length(geometry),
            Area=B) %>%
  # group_by(Region.Label,Area,Sample.Label) %>%
  group_by(year, month,season,Area,Sample.Label,SurveyID, seasonYear) %>%
  summarise(Effort=as.numeric(sum(Effort))/1000) %>% ungroup()

obs <- full_join(hw, effort)# %>% mutate(Region.Label = "cemore")

hw1.5.hn.v    <-ds(obs, key = "hn", truncation = 1.5, formula=~Clumped_Visibility, er_var = "R2")

hw_w <- obs %>% filter(season=="Winter")
hw_spr <- obs %>% filter(season=="Spring")
hw_summ <- obs %>% filter(season=="Summer")
hw_f <- obs %>% filter(season=="Fall")

x <- dht2(
  ddf = hw1.5.hn.v, # ds output
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

###################################

w <- obs %>% filter(season=="Winter")

x <- dht2(
  ddf = hw1.5.hn.v, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = obs,
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
