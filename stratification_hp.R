# Oct 27, 2023
#  copied from hw version
#
# currently, all transects are pooled within season, no grouping of surveys or years
#
#
# innes - default is true in dht(), estimated abundance/effort for each transect
# false - number of obs/effort for each transect
#
# er_est is how we put them together (ie. er_var from ds())
#
# stratification = Thom's infographic (how to stratify)
sp <-  "harbour"
df.hp <- all_ap_sf %>% filter(Species %like% sp) %>% data.frame()

hp <- df.hp %>% dplyr::select(
  Survey=SurveyID,
  Sample.Label,
  object,
  distance,
  Species,
  size = Group_Size,
  Clumped_Beaufort,
  Observer,
  season,
  year)

# find transects with no sightings and create NA distance rows
effort <- all_effort_lines %>%
  data.frame() %>%
  mutate(
    seasonYear = factor(seasonYear, levels = unique(seasonYear))) %>%
  # unique segment ID's and length for each
  transmute(Sample.Label=order, #TransectID,
            SurveyID = SurveyID,
            year = year,
            month = month,
            Region.Label = season,
            seasonYear = seasonYear,
            Effort = st_length(geometry),
            Area=B) %>%
  # group_by(Region.Label,Area,Sample.Label) %>%
  group_by(Region.Label, Sample.Label, Area, year, month, SurveyID, seasonYear) %>%
  summarise(Effort=as.numeric(sum(Effort))/1000) %>% ungroup()

obs <- full_join(hp, effort)# %>% mutate(Region.Label = "cemore")

# hw1.5.hn.v    <-ds(obs, key = "hn", truncation = 1.5, formula=~Clumped_Visibility, er_var = "O2")
hp.0.65.hn.bfc.obs <- ds(data=obs, truncation=0.65, key="hn", formula=~Clumped_Beaufort+Observer, er_var = "R2")

duplicated(effort) %>% sum()

# hw_w <- obs %>% filter(season=="Winter") doesn't work yet - we have some winter surveys without sightings
spr <- obs %>% filter(season=="Spring")
summ <- obs %>% filter(season=="Summer")
f <- obs %>% filter(season=="Fall")

x <- dht2(
  ddf = hw1.5.hn.v, # ds output
  # observations = NULL,
  # transects = NULL,
  # geo_strat = NULL,
  flatfile = summ,
  strat_formula = ~SurveyID,
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
