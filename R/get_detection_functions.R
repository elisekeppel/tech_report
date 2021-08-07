# July 7, 2021
# Begin detection functions!


# get data - code originally from 'plot_survey.R'

# ----------------------------------------------------------------------
# ----------------- LOAD ALL PROCESSED EFFORT DATA FOR ALL SURVEYS -----------
# ----------------------------------------------------------------------
dir <- file.path("OUTPUT FILES","dataEffort table")
effort_files <- list.files(dir)
all_effort <- map_df(file.path(dir, effort_files), read.delim) %>% 
  filter(Vessel == "MB") %>% 
  mutate(CloudCover = gsub("<25%", "0%-25%", CloudCover),
         SurveyID = paste0("cemore_", year(GpsT), tolower(month.abb[month(GpsT)])))
all_effort_lines <- get_effort_lines(all_effort) %>% 
  mutate(month_abb = factor(month.abb[month(date)], levels = c("Aug", "Sep", "Oct", "Nov", "Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul")),
         ONSEQ_ID = paste(SurveyID, ONSEQ_ID, sep ="_"))
all_effort_lines$month_year <- factor(paste0(all_effort_lines$month_abb, " ", year(all_effort_lines$date)),
                          levels = c("Aug 2020", "Sep 2020", "Oct 2020", "Nov 2020",
                                     "Jan 2021", "Feb 2021", "Mar 2021", "Apr 2021", "May 2021", "Jun 2021", "Jul 2021")) # TO DO: add as needed"Jun", "Jul",, "Dec"
coord <- ggplot2::coord_sf(xlim = c(-125.5, -123), ylim = c(48.1, 49.5), crs = sf::st_crs(4326)) 
ggplot() +
  geom_sf(data = coast) +
  geom_sf(data = all_effort_lines, aes(colour = SurveyID)) +
  coord

# ----------------------------------------------------------------------
# -------------- IMPORT ALL SIGHTINGS DATA TO DATE---------
#---------------------------------------------------------------
setwd("OUTPUT FILES/dataSightings_True Positions")
files <- list.files(pattern = "\\.shp$")
AP <- purrr::map(files, rgdal::readOGR)
setwd("../../")
AP <- do.call(rbind, AP)
survey_title <- paste0("All CeMoRe surveys from Aug 2020 - ", survey_title)
#---------------------------------------------------------------
ap_sf <- AP %>% 
  st_as_sf() %>%
  filter(vessel == "MB") %>% 
  dplyr::mutate( 
                   year = year(time_index), 
                   month = month(time_index), 
                   SightingTime = time_index,
                   distance = PSD_nm * 1852,
                   object = row_number(),
                   Sample.Label = paste(SurveyID, onseq_id, sep = "_"),
                   size = BestNumber,
                   SightedBy,
                   Beaufort = beauf,
                   long = final_lon,
                   lat = final_lat) %>% 
  dplyr::mutate(season = as.factor(case_when(
    month %in% c(6:8) ~ "Summer",
    month %in% c(9:11)  ~ "Fall",
    month %in% c(12, 1,2) ~ "Winter",
    month %in% c(3,4,5) ~ "Spring"
  )),
  # observer = case_when(
  #   observer == "CMcMillan" ~ 1,
  #   observer == "LSpaven"  ~ 2,
  #   observer == "EKeppel" ~ 3,
  #   observer== "SHrushowy" ~ 4
  # )
  ) %>% 
  st_transform(crs = st_crs(bc_shp))
ap_sf$Species %<>% factor(c('Humpback', 
                            'Harbour Porpoise', 
                            'Dalls Porpoise',
                            'Unknown Porpoise',
                            'KW - Northern Resident',
                            'KW - Southern Resident',
                            'KW - Transient',
                            'KW - Unknown ecotype',  # added for special account in Apr 2021
                            'Sei Whale',
                            'Sperm Whale',
                            'Cuvier\'s Whale',
                            'Fin Whale',
                            "Grey Whale",
                            "H. Porpoise group 40-50"))

ap_sf$month_abb <- factor(month.abb[ap_sf$month], 
                          levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")) # TO DO: add as needed"Jun", "Jul",, "Dec" 
ap_sf$month_year <- factor(paste0(ap_sf$month_abb, " ", ap_sf$year),
                           levels = c("Jan 2021", "Feb 2021", "Mar 2021", "Apr 2021", "May 2021", "Jun 2021", "Jul 2021",
                                      "Aug 2020", "Sep 2020", "Oct 2020", "Nov 2020")) # TO DO: add as needed"Jun", "Jul",, "Dec" 


#-------------------------------------------------------------------------------
# ------------------- Try for HW data-------------------------------------------
#-------------------------------------------------------------------------------
x <- ap_sf %>% filter(Species == "Humpback") %>% 
  filter(!is.na(distance)) %>% 
  mutate(beauf = as.factor(beauf))
ggplot() +
  geom_sf(data = coast)+
  geom_sf(data = x, aes(colour = x$SurveyID)) +
  coord
#---------------------------------------------------------------
ggplot() + 
  geom_histogram(data = x, aes(distance, fill = beauf))

df_hn <- ds(data.frame(x), truncation = 2000)
summary(df_hn)
saveRDS(df_hn, "cemore_HW_ddf_hn_2021_jul.rds")

# Model : Half-normal key function 
# AIC   : 1313.127 
# 
# Detection function parameters
# Scale coefficient(s):  
#   estimate         se
# (Intercept) 6.103085 0.06760098
# 
# Estimate          SE         CV
# Average p             0.2801448  0.01893817 0.06760138
# N in covered region 342.6799827 37.64560437 0.10985644
plot(df_hn)
# savepdf("cemore_HW_ddf_hn_2021_jul.pdf")
# savePlot("cemore_HW_ddf_hn_2021_jul", type = "pdf")
df_hr <- ds(data.frame(x), key = "hr", truncation = 2000)
summary(df_hr)

# Model : Hazard-rate key function 
# AIC   : 1314.428 
# 
# Detection function parameters
# Scale coefficient(s):  
#   estimate       se
# (Intercept) 6.182397 0.123572
# 
# Shape coefficient(s):  
#   estimate        se
# (Intercept) 1.296273 0.1977732
# 
# Estimate          SE         CV
# Average p             0.30244  0.02659071 0.08792062
# N in covered region 317.41834 38.87083339 0.12245932
plot(df_hr)

df_un <- ds(x, key = "unif", truncation = 2000)
summary(df_un)
plot(df_un)

#-------------------------------------------------------------------------------
# ------------------- Try for HP data-------------------------------------------
#-------------------------------------------------------------------------------
x <- ap_sf %>% filter(Species == "Harbour Porpoise") %>% 
  filter(!is.na(distance)) %>% 
  mutate(beauf = as.factor(beauf))
ggplot() +
  geom_sf(data = coast)+
  geom_sf(data = x, aes(colour = x$SurveyID)) +
  coord
#---------------------------------------------------------------
ggplot() + 
  geom_histogram(data = x, aes(distance, fill = beauf))

df_hn <- ds(data.frame(x), truncation = 550)
summary(df_hn)

# Summary for distance analysis 
# Number of observations :  482 
# Distance range         :  0  -  550 
# 
# Model : Half-normal key function with cosine adjustment term of order 2 
# 
# Strict monotonicity constraints were enforced.
# AIC   : 5750.149 
# 
# Detection function parameters
# Scale coefficient(s):  
#   estimate         se
# (Intercept) 5.292108 0.03922016
# 
# Adjustment term coefficient(s):  
#   estimate         se
# cos, order 2 0.1155652 0.07052024
# 
# Estimate          SE         CV
# Average p              0.4070949  0.02198946 0.05401557
# N in covered region 1183.9992009 76.25336710 0.06440323

plot(df_hn)

df_hr <- ds(data.frame(x), key = "hr", truncation = 550)
summary(df_hr)

# Summary for distance analysis 
# Number of observations :  482 
# Distance range         :  0  -  550 
# 
# Model : Hazard-rate key function with cosine adjustment term of order 2 
# 
# Strict monotonicity constraints were enforced.
# AIC   : 5749.312 
# 
# Detection function parameters
# Scale coefficient(s):  
#   estimate         se
# (Intercept) 5.659653 0.07699011
# 
# Shape coefficient(s):  
#   estimate        se
# (Intercept) 1.679825 0.1962855
# 
# Adjustment term coefficient(s):  
#   estimate         se
# cos, order 2 0.3936251 0.09285786
# 
# Estimate          SE         CV
# Average p              0.4119268  0.02244257 0.05448194
# N in covered region 1170.1108546 75.72660992 0.06471747
plot(df_hr)


