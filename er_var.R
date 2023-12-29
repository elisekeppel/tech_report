# ER variance
# ds() (specify argument er_var = ie. "O2", which becomes ervar in list dht_options) calls
# dht (er_var becomes ervar in dht_options in ds which goes to argument options in dht) which calls
# varn() where options feeds into argument type

#-----------------------------------------------
# first check transects that are all in sequential, spatial order
#-----------------------------------------------

View(effort)
unique(all_effort_lines$SurveyID)
# x <- all_effort_lines[which(all_effort_lines$SurveyID == "cemore_2020sep"),]

# x <- all_effort_lines[which(all_effort_lines$SurveyID == "cemore_2020oct"),]

x1 <- all_effort_lines[which(all_effort_lines$SurveyID == "cemore_2021jan"),]
# x <- all_effort_lines[which(all_effort_lines$SurveyID == "cemore_2021feb"),]
x2 <- all_effort_lines[which(all_effort_lines$SurveyID == "cemore_2021mar"),]

# x <- all_effort_lines[which(all_effort_lines$SurveyID == "cemore_2021apr"),]
x3 <- all_effort_lines[which(all_effort_lines$SurveyID == "cemore_2021may"),]
x4 <- all_effort_lines[which(all_effort_lines$SurveyID == "cemore_2021jun"),]

x5 <- all_effort_lines[which(all_effort_lines$SurveyID == "cemore_2021jul"),]
x6 <- all_effort_lines[which(all_effort_lines$SurveyID == "cemore_2021aug"),]

x7 <- all_effort_lines[which(all_effort_lines$SurveyID == "cemore_2021oct"),]
x8 <- all_effort_lines[which(all_effort_lines$SurveyID == "cemore_2021nov"),]

x9 <- all_effort_lines[which(all_effort_lines$SurveyID == "cemore_2022jan"),]
x10 <- all_effort_lines[which(all_effort_lines$SurveyID == "cemore_2022feb"),]

x11 <- all_effort_lines[which(all_effort_lines$SurveyID == "cemore_2022apr"),]
x12 <- all_effort_lines[which(all_effort_lines$SurveyID == "cemore_2022jun"),]

x13 <- all_effort_lines[which(all_effort_lines$SurveyID == "cemore_2022jul"),]
# x <- all_effort_lines[which(all_effort_lines$SurveyID == "cemore_2022aug"),]
x14 <- all_effort_lines[which(all_effort_lines$SurveyID == "cemore_2022sep"),]

# x <- all_effort_lines[which(all_effort_lines$SurveyID == "cemore_2022oct"),]
# x <- all_effort_lines[which(all_effort_lines$SurveyID == "cemore_2022dec"),]

ggplot(x) + geom_sf(aes(colour=transect_no)) +
  geom_sf_label(aes(label = transect_no))

# Run models for the seasons which have properly ordered transect id's for all surveys within a seasonYear
# (for each species, and for each potential er variance method)
x <- all_ap_sf %>%
  filter(SurveyID %in% c("2021jul",
                         "2021aug",
                         "2021oct",
                         "2021nov",
                         "2022jan",
                         "2022feb",
                         "2022apr",
                         "2022jun"))

# worked for hp and dp, but no models could be fitted for hw
# so, create new transect id's in order of NE to SW
# (ie. increasing distance from a point near Vancouver N of transects)

sid_list <- unique(all_effort_lines$SurveyID)
# ggplot() + geom_sf(data=all_effort_lines[which(all_effort_lines$SurveyID == sid_list[1]),]) +
#   geom_sf(data=van, colour="red")
order_transects <- function(sid){
  x <- all_effort_lines %>%
    filter(SurveyID==sid) %>%
    st_centroid(geometry) %>%
    dplyr::select(SurveyID, TransectID)

  van <- data.frame(x = -123.3, y = 49.4) %>%
    sf::st_as_sf(coords = c("x","y"), crs=4326)
  d <- as.vector(round(st_distance(van, x$geometry)/1000,2))

y <- x %>% dplyr::mutate(dist = d) %>%
  # mutate(order = 1:nrow(x)) %>%
  st_drop_geometry() %>%
  group_by(TransectID, SurveyID) %>%
  summarise(dist=min(dist)) %>%
  ungroup() %>%
  arrange(dist) %>%
  mutate(order = 1:length(unique(TransectID)),
         new_TID = paste0(SurveyID, order),
         new_TID = factor(new_TID, levels = new_TID))
return(y)
}

new_tids <- map_df(sid_list, order_transects) %>% dplyr::select(-c(dist, order, SurveyID))

all_effort_lines <- all_effort_lines %>% full_join(new_tids) %>% mutate(TransectID=new_TID, new_TID = NULL) %>% arrange(TransectID)
all_ap_sf <- all_ap_sf %>% left_join(new_tids) %>% mutate(TransectID=new_TID, new_TID = NULL)
all_ap_sf %<>%   mutate(Sample.Label=TransectID) #update Sample.Label w new tids
# GO BACK to 03_resulsts.Rmd to run effort <- ...

#---------------------------------------------------------------
#---------------------------------------------------------------
# HUMPBACK
#---------------------------------------------------------------
#---------------------------------------------------------------

sp <-  "humpback"
df.hw <- all_ap_sf %>% filter(Species %like% sp) %>% data.frame()

hw <- df.hw %>% dplyr::select(Region.Label,
                              # Survey=SurveyID,
                              Sample.Label,
                              distance,
                              Species,
                              size = Group_Size,
                              Clumped_Visibility#,
                              # Clumped_Beaufort,
                              # Glare90,
                              # swell,
                              # Observer
)
# find transects with no sightings and create NA distance rows
obs <- full_join(hw, effort) #%>% #left_join(reg) %>%

# un.2 <- ds(data=obs, truncation=2, key="un")
hw1.5.hn.v    <-ds(obs, key = "hn", truncation = 1.5, formula=~Clumped_Visibility)

er <- hw1.5.hn.v$dht$individuals$summary[c(1:4),c(1,5,7,9,10)] %>%
  transmute(Season=Region,
            n     =round(n,     digits=0),
            ER    =round(ER   , digits=2),
            cv.ER =round(cv.ER, digits=2),
            GS=round(mean.size, digits=2)
  )
# abundance
Nhat_t <- hw1.5.hn.v$dht$individuals$N[c(1:4),c(1,2,4,5,6)] %>%
  transmute(Season=Label,
            N=    round(Estimate,digits=0),
            cv.N= round(cv,      digits=2),
            L95.N=round(lcl,     digits=0),
            U95.N=round(ucl,     digits=0))
# density
d <- hw1.5.hn.v$dht$individuals$D[c(1:4),c(1,2,5,6)]  %>%
  transmute(Season=Label,
            D =    round(Estimate*25,digits=2),
            # cv.D = round(cv,         digits=2),
            L95.D= round(lcl*25,     digits=2),
            U95.D= round(ucl*25,     digits=2))

ab_hw <- full_join(er, Nhat_t) %>% full_join(d) %>% tibble::as_tibble() %>% dplyr::select(1:6,8:12,7) %>% mutate(er.var="R2")
ab_hw

#---------------------------------------------------------------
# S1
#---------------------------------------------------------------
hw1.5.hn.v    <-ds(obs, key = "hn", truncation = 1.5, formula=~Clumped_Visibility, er_var = "S1")

er <- hw1.5.hn.v$dht$individuals$summary[c(1:4),c(1,5,7,9,10)] %>%
  transmute(Season=Region,
            n     =round(n,     digits=0),
            ER    =round(ER   , digits=2),
            cv.ER =round(cv.ER, digits=2),
            GS=round(mean.size, digits=2)
  )
# abundance
Nhat_t <- hw1.5.hn.v$dht$individuals$N[c(1:4),c(1,2,4,5,6)] %>%
  transmute(Season=Label,
            N=    round(Estimate,digits=0),
            cv.N= round(cv,      digits=2),
            L95.N=round(lcl,     digits=0),
            U95.N=round(ucl,     digits=0))
# density
d <- hw1.5.hn.v$dht$individuals$D[c(1:4),c(1,2,5,6)]  %>%
  transmute(Season=Label,
            D =    round(Estimate*25,digits=2),
            # cv.D = round(cv,         digits=2),
            L95.D= round(lcl*25,     digits=2),
            U95.D= round(ucl*25,     digits=2))

ab_hw_S1 <- full_join(er, Nhat_t) %>% full_join(d) %>% tibble::as_tibble() %>% dplyr::select(1:6,8:12,7) %>% mutate(er.var="S1")
ab_hw_S1

#---------------------------------------------------------------
# S2
#---------------------------------------------------------------
hw1.5.hn.v    <-ds(obs, key = "hn", truncation = 1.5, formula=~Clumped_Visibility, er_var = "S2")

er <- hw1.5.hn.v$dht$individuals$summary[c(1:4),c(1,5,7,9,10)] %>%
  transmute(Season=Region,
            n     =round(n,     digits=0),
            ER    =round(ER   , digits=2),
            cv.ER =round(cv.ER, digits=2),
            GS=round(mean.size, digits=2)
  )
# abundance
Nhat_t <- hw1.5.hn.v$dht$individuals$N[c(1:4),c(1,2,4,5,6)] %>%
  transmute(Season=Label,
            N=    round(Estimate,digits=0),
            cv.N= round(cv,      digits=2),
            L95.N=round(lcl,     digits=0),
            U95.N=round(ucl,     digits=0))
# density
d <- hw1.5.hn.v$dht$individuals$D[c(1:4),c(1,2,5,6)]  %>%
  transmute(Season=Label,
            D =    round(Estimate*25,digits=2),
            # cv.D = round(cv,         digits=2),
            L95.D= round(lcl*25,     digits=2),
            U95.D= round(ucl*25,     digits=2))

ab_hw_S2 <- full_join(er, Nhat_t) %>% full_join(d) %>% tibble::as_tibble() %>% dplyr::select(1:6,8:12,7) %>% mutate(er.var="S2")
ab_hw_S2

#---------------------------------------------------------------
# O1
#---------------------------------------------------------------
hw1.5.hn.v    <-ds(obs, key = "hn", truncation = 1.5, formula=~Clumped_Visibility, er_var = "O1")

er <- hw1.5.hn.v$dht$individuals$summary[c(1:4),c(1,5,7,9,10)] %>%
  transmute(Season=Region,
            n     =round(n,     digits=0),
            ER    =round(ER   , digits=2),
            cv.ER =round(cv.ER, digits=2),
            GS=round(mean.size, digits=2)
  )
# abundance
Nhat_t <- hw1.5.hn.v$dht$individuals$N[c(1:4),c(1,2,4,5,6)] %>%
  transmute(Season=Label,
            N=    round(Estimate,digits=0),
            cv.N= round(cv,      digits=2),
            L95.N=round(lcl,     digits=0),
            U95.N=round(ucl,     digits=0))
# density
d <- hw1.5.hn.v$dht$individuals$D[c(1:4),c(1,2,5,6)]  %>%
  transmute(Season=Label,
            D =    round(Estimate*25,digits=2),
            # cv.D = round(cv,         digits=2),
            L95.D= round(lcl*25,     digits=2),
            U95.D= round(ucl*25,     digits=2))

ab_hw_O1 <- full_join(er, Nhat_t) %>% full_join(d) %>% tibble::as_tibble() %>% dplyr::select(1:6,8:12,7) %>% mutate(er.var="O1")
ab_hw_O1

#---------------------------------------------------------------
# O2
#---------------------------------------------------------------
hw1.5.hn.v    <-ds(obs, key = "hn", truncation = 1.5, formula=~Clumped_Visibility, er_var = "O2")

er <- hw1.5.hn.v$dht$individuals$summary[c(1:4),c(1,5,7,9,10)] %>%
  transmute(Season=Region,
            n     =round(n,     digits=0),
            ER    =round(ER   , digits=2),
            cv.ER =round(cv.ER, digits=2),
            GS=round(mean.size, digits=2)
  )
# abundance
Nhat_t <- hw1.5.hn.v$dht$individuals$N[c(1:4),c(1,2,4,5,6)] %>%
  transmute(Season=Label,
            N=    round(Estimate,digits=0),
            cv.N= round(cv,      digits=2),
            L95.N=round(lcl,     digits=0),
            U95.N=round(ucl,     digits=0))
# density
d <- hw1.5.hn.v$dht$individuals$D[c(1:4),c(1,2,5,6)]  %>%
  transmute(Season=Label,
            D =    round(Estimate*25,digits=2),
            # cv.D = round(cv,         digits=2),
            L95.D= round(lcl*25,     digits=2),
            U95.D= round(ucl*25,     digits=2))

ab_hw_O2 <- full_join(er, Nhat_t) %>% full_join(d) %>% tibble::as_tibble() %>% dplyr::select(1:6,8:12,7) %>% mutate(er.var="O2")
ab_hw_O2

tab_hw <- rbind(ab_hw,
                ab_hw_S1,
                ab_hw_S2,
                ab_hw_O1,
                ab_hw_O2)
write.csv(tab_hw, "C:/users/keppele/documents/cemore/analysis/cemore_analysis/er/er_var_hw_abund.csv", row.names=F)
tab_hw <- read.csv("C:/users/keppele/documents/cemore/analysis/cemore_analysis/er/er_var_hw_abund.csv")

names(tab_hw) <- c("Season","n","ER","cv.ER","GS","N","L95.N","U95.N","D","L95.D","U95.D","cv","er_var")
plot_ab_hw <- tab_hw %>% dplyr::select(Season, "Estimated Abundance"=N, L95.N, U95.N, er_var) %>% mutate(Species = "Humpback whale")



{hw <- brewer.pal(8, "Reds")[8]
  hp <- brewer.pal(8, "Greens")[8]
  dp <- brewer.pal(8, "Purples")[8]

  cols <- c('Humpback whale' = hw,
            'Harbour porpoise' = hp,
            'Dall\'s porpoise' = dp)
  shape <- c('Humpback whale' = 23,
             'Harbour porpoise' = 24,
             'Dall\'s porpoise' = 22)}

plot_comp_er_var <- function(plot_ab, sp){
  for(i in unique(plot_ab$Season)){
    x <- plot_ab %>% filter(Season==i)

ggplot(x, aes(er_var, `Estimated Abundance`), colour="black") +
  geom_errorbar(colour = "grey70", aes(ymin = L95.N, ymax = U95.N), width=.3, position=position_dodge(.3)) +
  geom_point(aes(fill=Species, shape=Species), size=3.5, position=position_dodge(.3)) +
  scale_fill_manual(values=cols)+
  scale_shape_manual(values=shape)+
  theme_classic()+
  theme(legend.text = element_text(size=fig_legend_size),
        legend.title=element_blank(),
        axis.text= element_text(size=fig_axis_size),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size=14),
        axis.text.x = element_text(angle=90))+
  ggtitle(paste0(first_up(sp), " ", i))
ggsave(paste0("C:/users/keppele/documents/cemore/analysis/cemore_analysis/er/comp_er_var_", sp, "_",i,".png"))
  }
}
plot_comp_er_var(plot_ab_hw, "humpback")
# plot_ab_hp <- ab_hp[1:4,] %>% dplyr::select(Season, "Estimated Abundance"=N, L95.N, U95.N) %>% mutate(Species = "Harbour porpoise")
# plot_ab_dp <- ab_dp[1:4,] %>% dplyr::select(Season, "Estimated Abundance"=N, L95.N, U95.N) %>% mutate(Species = "Dall's porpoise")

#---------------------------------------------------------------
#---------------------------------------------------------------
# HARBOUR PORPOISE
#---------------------------------------------------------------
#---------------------------------------------------------------

sp <-  "harbour porpoise"
df.hp <- all_ap_sf %>% filter(Species %like% sp) %>% data.frame()

hp <- df.hp %>% dplyr::select(Region.Label,
                              Sample.Label,
                              distance,
                              Species,
                              size = Group_Size,
                              Clumped_Beaufort,
                              Observer)
# find transects with no sightings and create NA distance rows
obs <- full_join(hp, effort) #%>% #left_join(reg) %>%

hp.0.65.hn.bfc.obs <- ds(data=obs, truncation=0.65, key="hn", formula=~Clumped_Beaufort+Observer)
# summary(hn.bfc.obs0.7)

# encounter rate and group size
er <- hp.0.65.hn.bfc.obs$dht$individuals$summary[c(1:4),c(1,5,7,9,10)] %>%
  transmute(Season=Region,
            n     =round(n,         digits=0),
            ER    =round(ER,        digits=2),
            cv.ER =round(cv.ER,     digits=2),
            GS=    round(mean.size, digits=2))
# abundance
Nhat_t <- hp.0.65.hn.bfc.obs$dht$individuals$N[c(1:4),c(1,2,4,5,6)] %>%
  transmute(Season=Label,
            N=    round(Estimate,digits=0),
            cv.N= round(cv,      digits=2),
            L95.N=round(lcl,     digits=0),
            U95.N=round(ucl,     digits=0))

# density
d <- hp.0.65.hn.bfc.obs$dht$individuals$D[c(1:4),c(1,2,5,6)] %>%
  transmute(Season=Label,
            D =    round(Estimate*25,digits=2),
            # cv.D = round(cv,         digits=2),
            L95.D= round(lcl*25,     digits=2),
            U95.D= round(ucl*25,     digits=2))

ab_hp <- full_join(er, Nhat_t) %>% full_join(d) %>% tibble::as_tibble()  %>% dplyr::select(1:6,8:12,7) %>% mutate(er.var="R2")
ab_hp

#---------------------------------------------------------------
# S1
#---------------------------------------------------------------
hp.0.65.hn.bfc.obs <- ds(data=obs, truncation=0.65, key="hn", formula=~Clumped_Beaufort+Observer, er_var = "S1")
# summary(hn.bfc.obs0.7)

# encounter rate and group size
er <- hp.0.65.hn.bfc.obs$dht$individuals$summary[c(1:4),c(1,5,7,9,10)] %>%
  transmute(Season=Region,
            n     =round(n,         digits=0),
            ER    =round(ER,        digits=2),
            cv.ER =round(cv.ER,     digits=2),
            GS=    round(mean.size, digits=2))
# abundance
Nhat_t <- hp.0.65.hn.bfc.obs$dht$individuals$N[c(1:4),c(1,2,4,5,6)] %>%
  transmute(Season=Label,
            N=    round(Estimate,digits=0),
            cv.N= round(cv,      digits=2),
            L95.N=round(lcl,     digits=0),
            U95.N=round(ucl,     digits=0))

# density
d <- hp.0.65.hn.bfc.obs$dht$individuals$D[c(1:4),c(1,2,5,6)] %>%
  transmute(Season=Label,
            D =    round(Estimate*25,digits=2),
            # cv.D = round(cv,         digits=2),
            L95.D= round(lcl*25,     digits=2),
            U95.D= round(ucl*25,     digits=2))

ab_hp_S1 <- full_join(er, Nhat_t) %>% full_join(d) %>% tibble::as_tibble()  %>% dplyr::select(1:6,8:12,7) %>% mutate(er.var="S1")
ab_hp_S1

#---------------------------------------------------------------
# S2
#---------------------------------------------------------------
hp.0.65.hn.bfc.obs <- ds(data=obs, truncation=0.65, key="hn", formula=~Clumped_Beaufort+Observer, er_var = "S2")
# summary(hn.bfc.obs0.7)

# encounter rate and group size
er <- hp.0.65.hn.bfc.obs$dht$individuals$summary[c(1:4),c(1,5,7,9,10)] %>%
  transmute(Season=Region,
            n     =round(n,         digits=0),
            ER    =round(ER,        digits=2),
            cv.ER =round(cv.ER,     digits=2),
            GS=    round(mean.size, digits=2))
# abundance
Nhat_t <- hp.0.65.hn.bfc.obs$dht$individuals$N[c(1:4),c(1,2,4,5,6)] %>%
  transmute(Season=Label,
            N=    round(Estimate,digits=0),
            cv.N= round(cv,      digits=2),
            L95.N=round(lcl,     digits=0),
            U95.N=round(ucl,     digits=0))

# density
d <- hp.0.65.hn.bfc.obs$dht$individuals$D[c(1:4),c(1,2,5,6)] %>%
  transmute(Season=Label,
            D =    round(Estimate*25,digits=2),
            # cv.D = round(cv,         digits=2),
            L95.D= round(lcl*25,     digits=2),
            U95.D= round(ucl*25,     digits=2))

ab_hp_S2 <- full_join(er, Nhat_t) %>% full_join(d) %>% tibble::as_tibble()  %>% dplyr::select(1:6,8:12,7) %>% mutate(er.var="S2")
ab_hp_S2

#---------------------------------------------------------------
# O1
#---------------------------------------------------------------
hp.0.65.hn.bfc.obs <- ds(data=obs, truncation=0.65, key="hn", formula=~Clumped_Beaufort+Observer, er_var = "O1")
# summary(hn.bfc.obs0.7)

# encounter rate and group size
er <- hp.0.65.hn.bfc.obs$dht$individuals$summary[c(1:4),c(1,5,7,9,10)] %>%
  transmute(Season=Region,
            n     =round(n,         digits=0),
            ER    =round(ER,        digits=2),
            cv.ER =round(cv.ER,     digits=2),
            GS=    round(mean.size, digits=2))
# abundance
Nhat_t <- hp.0.65.hn.bfc.obs$dht$individuals$N[c(1:4),c(1,2,4,5,6)] %>%
  transmute(Season=Label,
            N=    round(Estimate,digits=0),
            cv.N= round(cv,      digits=2),
            L95.N=round(lcl,     digits=0),
            U95.N=round(ucl,     digits=0))

# density
d <- hp.0.65.hn.bfc.obs$dht$individuals$D[c(1:4),c(1,2,5,6)] %>%
  transmute(Season=Label,
            D =    round(Estimate*25,digits=2),
            # cv.D = round(cv,         digits=2),
            L95.D= round(lcl*25,     digits=2),
            U95.D= round(ucl*25,     digits=2))

ab_hp_O1 <- full_join(er, Nhat_t) %>% full_join(d) %>% tibble::as_tibble()  %>% dplyr::select(1:6,8:12,7) %>% mutate(er.var="O1")
ab_hp_O1

#---------------------------------------------------------------
# O2
#---------------------------------------------------------------
hp.0.65.hn.bfc.obs <- ds(data=obs, truncation=0.65, key="hn", formula=~Clumped_Beaufort+Observer, er_var = "O2")
# summary(hn.bfc.obs0.7)

# encounter rate and group size
er <- hp.0.65.hn.bfc.obs$dht$individuals$summary[c(1:4),c(1,5,7,9,10)] %>%
  transmute(Season=Region,
            n     =round(n,         digits=0),
            ER    =round(ER,        digits=2),
            cv.ER =round(cv.ER,     digits=2),
            GS=    round(mean.size, digits=2))
# abundance
Nhat_t <- hp.0.65.hn.bfc.obs$dht$individuals$N[c(1:4),c(1,2,4,5,6)] %>%
  transmute(Season=Label,
            N=    round(Estimate,digits=0),
            cv.N= round(cv,      digits=2),
            L95.N=round(lcl,     digits=0),
            U95.N=round(ucl,     digits=0))

# density
d <- hp.0.65.hn.bfc.obs$dht$individuals$D[c(1:4),c(1,2,5,6)] %>%
  transmute(Season=Label,
            D =    round(Estimate*25,digits=2),
            # cv.D = round(cv,         digits=2),
            L95.D= round(lcl*25,     digits=2),
            U95.D= round(ucl*25,     digits=2))

ab_hp_O2 <- full_join(er, Nhat_t) %>% full_join(d) %>% tibble::as_tibble()  %>% dplyr::select(1:6,8:12,7) %>% mutate(er.var="O2")

tab_hp <- rbind(ab_hp,
                ab_hp_S1,
                ab_hp_S2,
                ab_hp_O1,
                ab_hp_O2)

write.csv(tab_hp, "C:/users/keppele/documents/cemore/analysis/cemore_analysis/er/er_var_hp_abund.csv")
tab_hp <- read.csv("C:/users/keppele/documents/cemore/analysis/cemore_analysis/er/er_var_hp_abund.csv")[1:13]
names(tab_hp) <- c("Season","n","ER","cv.ER","GS","N","L95.N","U95.N","D","L95.D","U95.D","cv","er_var")

plot_ab_hp <- tab_hp %>% dplyr::select(Season, "Estimated Abundance"=N, L95.N, U95.N, er_var) %>% mutate(Species = "Harbour porpoise")
plot_comp_er_var(plot_ab_hp, "harbour porpoise")

#---------------------------------------------------------------
#---------------------------------------------------------------
# DALL'S PORPOISE
#---------------------------------------------------------------
#---------------------------------------------------------------

sp <-  "Dall's porpoise"
df.dp <- all_ap_sf %>% filter(Species %like% sp) %>% data.frame()

dp <- df.dp %>% dplyr::select(Region.Label,
                              Sample.Label,
                              distance,
                              Species,
                              size = Group_Size,
                              Glare90,
                              Clumped_Swell)
# find transects with no sightings and create NA distance rows
obs <- full_join(dp, effort) #%>% #left_join(reg) %>%

dp0.8.hn.swc.g90c <- ds(data=obs, truncation=0.8, key="hn", formula=~Clumped_Swell+Glare90) # swell y/n, glare mild/none or severe
# summary(hn.sw.gl0.9)

# encounter rate and group size
er <- dp0.8.hn.swc.g90c$dht$individuals$summary[c(1:4),c(1,5,7,9,10)]  %>%
  transmute(Season=Region,
            n     =round(n,     digits=0),
            ER    =round(ER,    digits=2),
            cv.ER =round(cv.ER, digits=2),
            GS=round(mean.size, digits=2))
# abundance
Nhat_t <- dp0.8.hn.swc.g90c$dht$individuals$N[c(1:4),c(1,2,4,5,6)] %>%
  transmute(Season=Label,
            N=    round(Estimate,digits=0),
            cv.N= round(cv,      digits=2),
            L95.N=round(lcl,     digits=0),
            U95.N=round(ucl,     digits=0))
# density
d <- dp0.8.hn.swc.g90c$dht$individuals$D[c(1:4),c(1,2,5,6)] %>%
  transmute(Season=Label,
            D =    round(Estimate*25,digits=2), # *25 cuz density=indivs/25km^2
            # cv.D = round(cv,         digits=2),
            L95.D= round(lcl*25,     digits=2),
            U95.D= round(ucl*25,     digits=2))

ab_dp <- full_join(er, Nhat_t) %>% full_join(d) %>% tibble::as_tibble() %>% dplyr::select(1:6,8:12,7) %>% mutate(er.var="R2")
# ab_dp

#---------------------------------------------------------------
# S1
#---------------------------------------------------------------
dp0.8.hn.swc.g90c <- ds(data=obs, truncation=0.8, key="hn", formula=~Clumped_Swell+Glare90, er_var = "S1") # swell y/n, glare mild/none or severe
# summary(hn.sw.gl0.9)

# encounter rate and group size
er <- dp0.8.hn.swc.g90c$dht$individuals$summary[c(1:4),c(1,5,7,9,10)]  %>%
  transmute(Season=Region,
            n     =round(n,     digits=0),
            ER    =round(ER,    digits=2),
            cv.ER =round(cv.ER, digits=2),
            GS=round(mean.size, digits=2))
# abundance
Nhat_t <- dp0.8.hn.swc.g90c$dht$individuals$N[c(1:4),c(1,2,4,5,6)] %>%
  transmute(Season=Label,
            N=    round(Estimate,digits=0),
            cv.N= round(cv,      digits=2),
            L95.N=round(lcl,     digits=0),
            U95.N=round(ucl,     digits=0))
# density
d <- dp0.8.hn.swc.g90c$dht$individuals$D[c(1:4),c(1,2,5,6)] %>%
  transmute(Season=Label,
            D =    round(Estimate*25,digits=2), # *25 cuz density=indivs/25km^2
            # cv.D = round(cv,         digits=2),
            L95.D= round(lcl*25,     digits=2),
            U95.D= round(ucl*25,     digits=2))

ab_dp_S1 <- full_join(er, Nhat_t) %>% full_join(d) %>% tibble::as_tibble()  %>% dplyr::select(1:6,8:12,7) %>% mutate(er.var="S1")
# ab_dp_S1

#---------------------------------------------------------------
# S2
#---------------------------------------------------------------
dp0.8.hn.swc.g90c <- ds(data=obs, truncation=0.8, key="hn", formula=~Clumped_Swell+Glare90, er_var = "S2") # swell y/n, glare mild/none or severe
# summary(hn.sw.gl0.9)

# encounter rate and group size
er <- dp0.8.hn.swc.g90c$dht$individuals$summary[c(1:4),c(1,5,7,9,10)]  %>%
  transmute(Season=Region,
            n     =round(n,     digits=0),
            ER    =round(ER,    digits=2),
            cv.ER =round(cv.ER, digits=2),
            GS=round(mean.size, digits=2))
# abundance
Nhat_t <- dp0.8.hn.swc.g90c$dht$individuals$N[c(1:4),c(1,2,4,5,6)] %>%
  transmute(Season=Label,
            N=    round(Estimate,digits=0),
            cv.N= round(cv,      digits=2),
            L95.N=round(lcl,     digits=0),
            U95.N=round(ucl,     digits=0))
# density
d <- dp0.8.hn.swc.g90c$dht$individuals$D[c(1:4),c(1,2,5,6)] %>%
  transmute(Season=Label,
            D =    round(Estimate*25,digits=2), # *25 cuz density=indivs/25km^2
            # cv.D = round(cv,         digits=2),
            L95.D= round(lcl*25,     digits=2),
            U95.D= round(ucl*25,     digits=2))

ab_dp_S2 <- full_join(er, Nhat_t) %>% full_join(d) %>% tibble::as_tibble()  %>% dplyr::select(1:6,8:12,7) %>% mutate(er.var="S2")
# ab_dp_S2
#---------------------------------------------------------------
# O1
#---------------------------------------------------------------
dp0.8.hn.swc.g90c <- ds(data=obs, truncation=0.8, key="hn", formula=~Clumped_Swell+Glare90, er_var = "O1") # swell y/n, glare mild/none or severe
# summary(hn.sw.gl0.9)

# encounter rate and group size
er <- dp0.8.hn.swc.g90c$dht$individuals$summary[c(1:4),c(1,5,7,9,10)]  %>%
  transmute(Season=Region,
            n     =round(n,     digits=0),
            ER    =round(ER,    digits=2),
            cv.ER =round(cv.ER, digits=2),
            GS=round(mean.size, digits=2))
# abundance
Nhat_t <- dp0.8.hn.swc.g90c$dht$individuals$N[c(1:4),c(1,2,4,5,6)] %>%
  transmute(Season=Label,
            N=    round(Estimate,digits=0),
            cv.N= round(cv,      digits=2),
            L95.N=round(lcl,     digits=0),
            U95.N=round(ucl,     digits=0))
# density
d <- dp0.8.hn.swc.g90c$dht$individuals$D[c(1:4),c(1,2,5,6)] %>%
  transmute(Season=Label,
            D =    round(Estimate*25,digits=2), # *25 cuz density=indivs/25km^2
            # cv.D = round(cv,         digits=2),
            L95.D= round(lcl*25,     digits=2),
            U95.D= round(ucl*25,     digits=2))

ab_dp_O1 <- full_join(er, Nhat_t) %>% full_join(d) %>% tibble::as_tibble()  %>% dplyr::select(1:6,8:12,7) %>% mutate(er.var="O1")
# ab_dp_O1
#---------------------------------------------------------------
# O2
#---------------------------------------------------------------
dp0.8.hn.swc.g90c <- ds(data=obs, truncation=0.8, key="hn", formula=~Clumped_Swell+Glare90, er_var = "O2") # swell y/n, glare mild/none or severe
# summary(hn.sw.gl0.9)

# encounter rate and group size
er <- dp0.8.hn.swc.g90c$dht$individuals$summary[c(1:4),c(1,5,7,9,10)]  %>%
  transmute(Season=Region,
            n     =round(n,     digits=0),
            ER    =round(ER,    digits=2),
            cv.ER =round(cv.ER, digits=2),
            GS=round(mean.size, digits=2))
# abundance
Nhat_t <- dp0.8.hn.swc.g90c$dht$individuals$N[c(1:4),c(1,2,4,5,6)] %>%
  transmute(Season=Label,
            N=    round(Estimate,digits=0),
            cv.N= round(cv,      digits=2),
            L95.N=round(lcl,     digits=0),
            U95.N=round(ucl,     digits=0))
# density
d <- dp0.8.hn.swc.g90c$dht$individuals$D[c(1:4),c(1,2,5,6)] %>%
  transmute(Season=Label,
            D =    round(Estimate*25,digits=2), # *25 cuz density=indivs/25km^2
            # cv.D = round(cv,         digits=2),
            L95.D= round(lcl*25,     digits=2),
            U95.D= round(ucl*25,     digits=2))

ab_dp_O2 <- full_join(er, Nhat_t) %>% full_join(d) %>% tibble::as_tibble()  %>% dplyr::select(1:6,8:12,7) %>% mutate(er.var="O2")

tab_dp <- rbind(ab_dp,
                ab_dp_S1,
                ab_dp_S2,
                ab_dp_O1,
                ab_dp_O2)

write.csv(tab_dp, "C:/users/keppele/documents/cemore/analysis/cemore_analysis/er/er_var_dp_abund.csv")
tab_dp <- read.csv("C:/users/keppele/documents/cemore/analysis/cemore_analysis/er/er_var_dp_abund.csv")[1:13]
names(tab_dp) <- c("Season","n","ER","cv.ER","GS","N","L95.N","U95.N","D","L95.D","U95.D","cv","er_var")

plot_ab_dp <- tab_dp %>% dplyr::select(Season, "Estimated Abundance"=N, L95.N, U95.N, er_var) %>% mutate(Species = "Dall's porpoise")
plot_comp_er_var(plot_ab_dp, "Dall's porpoise")

# purrr::map(c(plot_ab_hw, plot_ab_hp, plot_ab_hw), plot_comp_er_var, c("humpback", "harbour porpoise", "Dall's porpoise"))
