# bootdht to estimate er variance instead of delta method which assumes independence of
# surveys

# So for humpback, would use Innes to account for effect of covariates on ER variance,
# would use O2 to account for systematic study design and non-independence of
# geographically close transects, and would use year as strata as surveyid
# does not cover the entire study area so would be misleading to apply estimates from
# partial study area to entire study area.

# one goal of the analysis was
# to estimate average density over season (and year?) which
# this involves averaging the season-specific estimates,
# which are not independent since they share common detection
# function parameters (as well as having the same mean school size and trackline detection probability)

# Does bootstrap uncertainty estimation account for both ER and df var? and group size var?

# group_mean is the mean group size over the entire stratum but divvied up by season. So should be able to look at variance of group size by season from summary.
# BUT, is it accounted for in the bootstrap uncertainty estimate?

# And df var?

##################################################################
# So, can't run seasons separately in ds() because sample size too small.
# And can't run in dht2() or dht() after running ds() for just det func as
# need class ds model for bootdht().
# So, could run ds() using Region.Label as season, then bootdht()
# for variance.
##################################################################
library(Distance)
# data(wren_lt)
# names(wren_lt)
# conversion.factor <- convert_units("meter", "kilometer", "hectare")
# wren.unif.cos <- ds(wren_lt, key="unif", adjustment="cos",
#                     convert_units=conversion.factor)
# print(wren.unif.cos$Nht$individuals$N)
sp <-  "humpback"
df.hw <- all_ap_sf %>% filter(Species %like% sp) %>% data.frame()

hw <- df.hw %>% dplyr::select(Region.Label,
                              SurveyID,
                              Sample.Label=order,
                              object,
                              distance,
                              Species,
                              size = Group_Size,
                              Clumped_Beaufort,
                              Clumped_Group_Size2,
                              # Clumped_Group_Size,
                              # Observer,
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

# convert_units(distance_units, effort_units, area_units)
conversion.factor <- convert_units("kilometer", "kilometer", "square kilometer")
hw1.75.hn.bfc.szc2  <-ds(obs_hw, truncation=1.75, key = "hn", formula=~Clumped_Beaufort+Clumped_Group_Size2,  convert_units=conversion.factor)

print(hw1.75.hn.bfc.szc2$Nht)

obs_hw_nonstrat <- obs_hw %>% mutate(
  Region.Label= "cemore"
)
hw1.75.hn.bfc.szc2_nonstrat  <-ds(obs_hw_nonstrat, truncation=1.75, key = "hn", formula=~Clumped_Beaufort+Clumped_Group_Size2,  convert_units=conversion.factor)

####################################################################################
# Bootstrap estimates of precision
####################################################################################
# Rather than relying upon the delta-method approximation that assumes independence
# between uncertainty in the detection function and variability in encounter rate, a
# bootstrap procedure can be employed. Resampling with replacement of the transects
# produces replicate samples with which a sampling distribution of D^
#   is approximated. From that sampling distribution, the percentile method is used
# to produce confidence interval bounds respecting the shape of the sampling distribution
# (S. Buckland et al., 2015, sec. 6.3.1.2).


bootdht_Nhat_summarize <- function(ests, fit) {
  return(data.frame(N=ests$individuals$N$Estimate))
}

est.boot <- bootdht(model=hw1.75.hn.bfc.szc2, flatfile=obs_hw,
                    summary_fun=bootdht_Nhat_summarize,
                    # resample_strata = T,
                    convert_units=conversion.factor, nboot=100, cores=3)


est.boot_nonstrat <- bootdht(model=hw1.75.hn.bfc.szc2_nonstrat, flatfile=obs_hw_nonstrat,
                    summary_fun=bootdht_Nhat_summarize,
                    convert_units=conversion.factor, nboot=100, cores=3)
alpha <- 0.05
(bootci <- quantile(est.boot$N, probs = c(alpha/2, 1-alpha/2),
                    na.rm=TRUE))

hist(est.boot$N, nc=30,
     main="Distribution of bootstrap estimates\nwithout model uncertainty",
     xlab="Estimated density")
abline(v=bootci, lwd=2, lty=2)


#################### SPRING STRAT DS ##############
est.boot_sp <- bootdht(model=hw1.75.hn.bfc.szc2, flatfile=hw_sp,
                       summary_fun=bootdht_Nhat_summarize,
                       convert_units=conversion.factor, nboot=100, cores=3)
mean(est.boot_sp$N)
(bootci_sp <- quantile(est.boot_sp$N, probs = c(alpha/2, 1-alpha/2),na.rm=TRUE))

hist(est.boot_sp$N, nc=30,
     main="Distribution of bootstrap SPRING estimates\nstratified ds()",
     xlab="Estimated density")
abline(v=bootci_sp, lwd=2, lty=2)

#################### SPRING STRAT YEAR ##############
obs_hw_year <- obs_hw %>% mutate(
  Region.Label= year
)
hw_sp_year <- obs_hw_year %>% filter(season=="Spring")
hw1.75.hn.bfc.szc2_year  <-ds(obs_hw_year, truncation=1.75, key = "hn", formula=~Clumped_Beaufort+Clumped_Group_Size2,  convert_units=conversion.factor)

est.boot_sp_y <- bootdht(model=hw1.75.hn.bfc.szc2_year, flatfile=hw_sp_year,
                       summary_fun=bootdht_Nhat_summarize,
                       resample_strata = T,
                       convert_units=conversion.factor, nboot=25, cores=3)
mean(est.boot_sp$N)
(bootci_sp <- quantile(est.boot_sp$N, probs = c(alpha/2, 1-alpha/2),na.rm=TRUE))

hist(est.boot_sp$N, nc=30,
     main="Distribution of bootstrap SPRING estimates\nstratified ds()",
     xlab="Estimated density")
abline(v=bootci_sp, lwd=2, lty=2)

######################## SPRING NONSTRAT DS #####################################################

hw_sp_nonstrat <- obs_hw %>% filter(season=="Spring")

est.boot_sp2 <- bootdht(model=hw1.75.hn.bfc.szc2_nonstrat, flatfile=hw_sp_nonstrat,
                       summary_fun=bootdht_Nhat_summarize,
                       convert_units=conversion.factor, nboot=100, cores=3)
mean(est.boot_sp2$N)
(bootci_sp2 <- quantile(est.boot_sp2$N, probs = c(alpha/2, 1-alpha/2),
                    na.rm=TRUE))

hist(est.boot_sp2$N, nc=30,
     main="Distribution of spring bootstrap estimates\n nonstratified ds()",
     xlab="Estimated density")
abline(v=bootci_sp2, lwd=2, lty=2)
######################## SUMMER STRAT DS ######################################################

est.boot_su <- bootdht(model=hw1.75.hn.bfc.szc2, flatfile=hw_su,
                       summary_fun=bootdht_Nhat_summarize,
                       convert_units=conversion.factor, nboot=100, cores=3)
mean(est.boot_su$N)
(bootci_su <- quantile(est.boot_su$N, probs = c(alpha/2, 1-alpha/2),
                    na.rm=TRUE))

hist(est.boot_su$N, nc=30,
     main="Distribution of bootstrap SUMMER estimates\nstratified ds()",
     xlab="Estimated density")
abline(v=bootci_su, lwd=2, lty=2)


######################## SUMMER NONSTRAT DS #####################################################


hw_su_nonstrat <- obs_hw %>% filter(season=="Summer")

est.boot_su2 <- bootdht(model=hw1.75.hn.bfc.szc2_nonstrat, flatfile=hw_su_nonstrat,
                        summary_fun=bootdht_Nhat_summarize,
                        convert_units=conversion.factor, nboot=100, cores=3)
mean(est.boot_su2$N)
(bootci_su2 <- quantile(est.boot_su2$N, probs = c(alpha/2, 1-alpha/2),
                    na.rm=TRUE))

hist(est.boot_su2$N, nc=30,
     main="Distribution of SUMMER bootstrap estimates\n nonstratified ds()",
     xlab="Estimated density")
abline(v=bootci_su2, lwd=2, lty=2)

######################## FALL STRAT DS ######################################################

est.boot_f <- bootdht(model=hw1.75.hn.bfc.szc2, flatfile=hw_f,
                       summary_fun=bootdht_Nhat_summarize,
                       convert_units=conversion.factor, nboot=100, cores=3)
mean(est.boot_f$N)
(bootci_f <- quantile(est.boot_f$N, probs = c(alpha/2, 1-alpha/2),
                    na.rm=TRUE))

hist(est.boot_f$N, nc=30,
     main="Distribution of bootstrap FALL estimates\nstratified ds()",
     xlab="Estimated density")
abline(v=bootci_f, lwd=2, lty=2)


######################## FALL NONSTRAT DS #####################################################

hw_f_nonstrat <- obs_hw %>% filter(season=="Fall")

est.boot_f2 <- bootdht(model=hw1.75.hn.bfc.szc2_nonstrat, flatfile=hw_f_nonstrat,
                        summary_fun=bootdht_Nhat_summarize,
                        convert_units=conversion.factor, nboot=100, cores=3)
mean(est.boot_f2$N)
(bootci_f2 <- quantile(est.boot_f2$N, probs = c(alpha/2, 1-alpha/2),
                    na.rm=TRUE))

hist(est.boot_f2$N, nc=30,
     main="Distribution of FALL bootstrap estimates\n nonstratified ds()",
     xlab="Estimated density")
abline(v=bootci_f2, lwd=2, lty=2)

##################### WINTER STRAT DS#########################################################

est.boot_w <- bootdht(model=hw1.75.hn.bfc.szc2, flatfile=hw_w,
                       summary_fun=bootdht_Nhat_summarize,
                       convert_units=conversion.factor, nboot=100, cores=3)
mean(est.boot_w$N)
(bootci_w <- quantile(est.boot_w$N, probs = c(alpha/2, 1-alpha/2),
                    na.rm=TRUE))

hist(est.boot_w$N, nc=30,
     main="Distribution of bootstrap WINTER estimates\nstratified ds()",
     xlab="Estimated density")
abline(v=bootci_w, lwd=2, lty=2)


######################## WINTER NONSTRAT DS #####################################################

hw_w_nonstrat <- obs_hw %>% filter(season=="Winter")

est.boot_w2 <- bootdht(model=hw1.75.hn.bfc.szc2_nonstrat, flatfile=hw_w_nonstrat,
                        summary_fun=bootdht_Nhat_summarize,
                        convert_units=conversion.factor, nboot=100, cores=3)
mean(est.boot_w2$N)
(bootci_w2 <- quantile(est.boot_w2$N, probs = c(alpha/2, 1-alpha/2),
                    na.rm=TRUE))

hist(est.boot_w2$N, nc=30,
     main="Distribution of WINTER bootstrap estimates\n nonstratified ds()",
     xlab="Estimated density")
abline(v=bootci_w2, lwd=2, lty=2)

################################################################
# how to compare seasons??
# how to check if overlap??

###############################################################
###############################################################
###############################################################
# SUMMARY
data.frame(season=c("spring","spring", "summer","summer","fall","fall","winter","winter"),
           stratified=rep(c("strat","non"),4),
           mean=c(mean(est.boot_sp$N), mean(est.boot_sp2$N),
             mean(est.boot_su$N), mean(est.boot_su2$N),
             mean(est.boot_f$N),  mean(est.boot_f2$N),
             mean(est.boot_w$N),  mean(est.boot_w2$N))           ,
           lowerCI=c(bootci_sp[1],
             bootci_sp2[1],
             bootci_su[1],
             bootci_su2[1],
             bootci_f[1],
             bootci_f2[1],
             bootci_w[1],
             bootci_w2[1]),
upperCI=c(bootci_sp[2],
  bootci_sp2[2],
  bootci_su[2],
  bootci_su2[2],
  bootci_f[2],
  bootci_f2[2],
  bootci_w[2],
  bootci_w2[2]))
,
c(bootci_sp, bootci_sp2, bootci_su, bootci_su2,
  bootci_f, bootci_f2, bootci_w, bootci_w2)
           )
###############################################################
###############################################################
# HP
{sp <-  "harbour"
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


hp_obs <- full_join(hp, effort)
conversion.factor <- convert_units("meter", "kilometer", "square kilometre")
hp.0.65.hn.bfc.obs <- ds(data=hp_obs, truncation=0.65, key="hn", formula=~Clumped_Beaufort+Observer, convert_units = conversion.factor)

summary(hp.0.65.hn.bfc.obs)
print(hp.0.65.hn.bfc.obs$Nht$individuals$N)


est.boot3 <- bootdht(model=hp.0.65.hn.bfc.obs, flatfile=hp_obs,
                    summary_fun=bootdht_Nhat_summarize,
                    # resample_strata = T,
                    convert_units=conversion.factor, nboot=100, cores=3)
}
est.boot3[which(is.na(est.boot3$N)),]$bootstrap_ID %>% unique() %>% length()

alpha <- 0.05
(bootci <- quantile(est.boot$N, probs = c(alpha/2, 1-alpha/2),
                    na.rm=TRUE))

hist(est.boot$N, nc=30,
     main="Distribution of bootstrap estimates\nwithout model uncertainty",
     xlab="Estimated density")
abline(v=bootci, lwd=2, lty=2)


# Isn't dht2() telling the df var calculation to do so by year? ie. is it stratified/gropued by year/stratum
# when going into the varNhat() piece