# Looking at seasonal results for dp for publication in primary literature.

# first run data loading in 03_results.Rmd

# filter out each season from each of sightings and effort data
{fall   <- all_ap_sf %>% filter(season == "Fall")
winter <- all_ap_sf %>% filter(season == "Winter")
spring <- all_ap_sf %>% filter(season == "Spring")
summer <- all_ap_sf %>% filter(season == "Summer")

effort <- all_effort_lines %>%
  data.frame() %>%
  # unique segment ID's and length for each
  transmute(Sample.Label=TransectID,
            Region.Label = year,
            Effort = st_length(geometry),
            Area=B,
            season) %>%
  group_by(Region.Label,Area,Sample.Label,season) %>%
  summarise(Effort=as.numeric(sum(Effort))/1000) %>% ungroup()

effort_fall   <- effort %>% filter(season == "Fall")
effort_winter <- effort %>% filter(season == "Winter")
effort_spring <- effort %>% filter(season == "Spring")
effort_summer <- effort %>% filter(season == "Summer")
}
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#    Dall's Porpoise
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# the following code is from 07_tables_and_figures.Rmd where the models are run
# and abundance estimated

#-------------------------------------------------------------------------------
#    FALL
#-------------------------------------------------------------------------------
sp <-  "Dall"
dp_fall <- fall %>% filter(Species %like% sp) %>% data.frame() # n = 29
# table(dp_fall$)

dp_fall0.8.un       <-ds(dp_fall, key = "un", truncation = 0.8)
dp_fall0.8.hn       <-ds(dp_fall, key = "hn", truncation = 0.8)
dp_fall0.8.hr       <-ds(dp_fall, key = "hr", truncation = 0.8)

dp_fall0.8.hn.g90c        <-ds(dp_fall, 0.8, key="hn", formula=~Glare90)
dp_fall0.8.hn.swc         <-ds(dp_fall, 0.8, key="hn", formula=~Clumped_Swell)
dp_fall0.8.hn.swc.g90c    <-ds(dp_fall, 0.8, key="hn", formula=~Clumped_Swell+Glare90)
dp_fall0.8.hn.bfc         <-ds(dp_fall, 0.8, key="hn", formula=~Clumped_Beaufort)
dp_fall0.8.hn.szc         <-ds(dp_fall, 0.8, key="hn", formula=~Clumped_Group_Size)
dp_fall0.8.hn.swc.g90c.bfc<-ds(dp_fall, 0.8, key="hn", formula=~Clumped_Swell+Glare90+Clumped_Beaufort)
dp_fall0.8.hn.swc2        <-ds(dp_fall, 0.8, key="hn", formula=~Clumped_Swell2)
dp_fall0.8.hn.ob          <-ds(dp_fall, 0.8, key="hn", formula=~Observer)

model.table.dp_fall <- summarize_ds_models(
  dp_fall0.8.un,
  dp_fall0.8.hn,
  dp_fall0.8.hr,

  dp_fall0.8.hn.g90c        ,
  dp_fall0.8.hn.swc         ,
  dp_fall0.8.hn.swc.g90c    ,
  dp_fall0.8.hn.bfc         ,
  dp_fall0.8.hn.szc         ,
  dp_fall0.8.hn.swc.g90c.bfc,
  dp_fall0.8.hn.swc2        ,
  dp_fall0.8.hn.ob          ,
  output="plain")
model.table.dp_fall

#-------------------------------------------------------------------------------
# WINTER
#-------------------------------------------------------------------------------
dp_winter <- winter %>% filter(Species %like% sp) %>% data.frame() # n = 50

dp_winter0.8.un              <-ds(dp_winter, key = "un", truncation = 0.8)
dp_winter0.8.hn              <-ds(dp_winter, key = "hn", truncation = 0.8)
dp_winter0.8.hr              <-ds(dp_winter, key = "hr", truncation = 0.8)

dp_winter0.8.hn.g90c          <-ds(dp_winter, 0.8, key="hn", formula=~Glare90)
dp_winter0.8.hn.swc           <-ds(dp_winter, 0.8, key="hn", formula=~Clumped_Swell)
dp_winter0.8.hn.swc.g90c      <-ds(dp_winter, 0.8, key="hn", formula=~Clumped_Swell+Glare90)

dp_winter0.8.hn.bfc           <-ds(dp_winter, 0.8, key="hn", formula=~Clumped_Beaufort)
dp_winter0.8.hn.szc           <-ds(dp_winter, 0.8, key="hn", formula=~Clumped_Group_Size)
dp_winter0.8.hn.g90           <-ds(dp_winter, 0.8, key="hn", formula=~Glare90)
dp_winter0.8.hn.g45c          <-ds(dp_winter, 0.8, key="hn", formula=~Glare45)
dp_winter0.8.hn.sw            <-ds(dp_winter, 0.8, key="hn", formula=~swell)
dp_winter0.8.hn.swc..g90c     <-ds(dp_winter, 0.8, key="hn", formula=~Clumped_Swell*Glare90)
dp_winter0.8.hn.swc.g90c.bfc  <-ds(dp_winter, 0.8, key="hn", formula=~Clumped_Swell+Glare90+Clumped_Beaufort)
dp_winter0.8.hn.swc.g45c.szc  <-ds(dp_winter, 0.8, key="hn", formula=~Clumped_Swell+Glare45+Clumped_Group_Size)
dp_winter0.8.hn.swc..g45c     <-ds(dp_winter, 0.8, key="hn", formula=~Clumped_Swell*Glare45)
# dp_winter0.8.hn.swc..obs      <-ds(dp_winter, 0.8, key="hn", formula=~Clumped_Swell*Observer)
dp_winter0.8.hn.swc2          <-ds(dp_winter, 0.8, key="hn", formula=~Clumped_Swell2)
dp_winter0.8.hn.ob            <-ds(dp_winter, 0.8, key="hn", formula=~Observer)


model.table.dp_winter <- summarize_ds_models(
  dp_winter0.8.un            ,
  dp_winter0.8.hn            ,
  dp_winter0.8.hr            ,

  dp_winter0.8.hn.g90c        ,
  dp_winter0.8.hn.swc         ,
  dp_winter0.8.hn.swc.g90c    ,

  dp_winter0.8.hn.bfc         ,
  dp_winter0.8.hn.szc         ,
  dp_winter0.8.hn.g90         ,
  dp_winter0.8.hn.g45c        ,
  dp_winter0.8.hn.sw          ,
  dp_winter0.8.hn.swc..g90c   ,
  dp_winter0.8.hn.swc.g90c.bfc,
  dp_winter0.8.hn.swc.g45c.szc,
  dp_winter0.8.hn.swc..g45c   ,
  # dp_winter0.8.hn.swc..obs    ,
  dp_winter0.8.hn.swc2        ,
  dp_winter0.8.hn.ob          ,
  output="plain")

model.table.dp_winter

#-------------------------------------------------------------------------------
# SPRING
#-------------------------------------------------------------------------------
dp_spr <- spring %>% filter(Species %like% sp) %>% data.frame() # n = 26

dp_spr0.8.un               <-ds(dp_spr, key = "un", truncation = 0.8)
dp_spr0.8.hn               <-ds(dp_spr, key = "hn", truncation = 0.8)
dp_spr0.8.hr               <-ds(dp_spr, key = "hr", truncation = 0.8)

# dp_spr0.8.hn.g90c          <-ds(dp_spr, 0.8, key="hn", formula=~Glare90)
dp_spr0.8.hn.swc           <-ds(dp_spr, 0.8, key="hn", formula=~Clumped_Swell)
# dp_spr0.8.hn.swc.g90c      <-ds(dp_spr, 0.8, key="hn", formula=~Clumped_Swell+Glare90)

# dp_spr0.8.hn.bfc           <-ds(dp_spr, 0.8, key="hn", formula=~Clumped_Beaufort) # too few samples in some categories - bf 4 only one sample, bf 3 no samples
dp_spr0.8.hn.szc           <-ds(dp_spr, 0.8, key="hn", formula=~Clumped_Group_Size)
# dp_spr0.8.hn.g90           <-ds(dp_spr, 0.8, key="hn", formula=~Glare90)
# dp_spr0.8.hn.g45c          <-ds(dp_spr, 0.8, key="hn", formula=~Glare45)
dp_spr0.8.hn.sw            <-ds(dp_spr, 0.8, key="hn", formula=~swell)
# dp_spr0.8.hn.swc..g90c     <-ds(dp_spr, 0.8, key="hn", formula=~Clumped_Swell*Glare90)
# dp_spr0.8.hn.swc.g90c.bfc  <-ds(dp_spr, 0.8, key="hn", formula=~Clumped_Swell+Glare90+Clumped_Beaufort)
# dp_spr0.8.hn.swc.g45c.szc  <-ds(dp_spr, 0.8, key="hn", formula=~Clumped_Swell+Glare45+Clumped_Group_Size)
# dp_spr0.8.hn.swc..g45c     <-ds(dp_spr, 0.8, key="hn", formula=~Clumped_Swell*Glare45)
dp_spr0.8.hn.swc..obs      <-ds(dp_spr, 0.8, key="hn", formula=~Clumped_Swell*Observer)
dp_spr0.8.hn.swc2          <-ds(dp_spr, 0.8, key="hn", formula=~Clumped_Swell2)
dp_spr0.8.hn.ob            <-ds(dp_spr, 0.8, key="hn", formula=~Observer)


model.table.dp_spr <- summarize_ds_models(
  dp_spr0.8.un            ,
  dp_spr0.8.hn            ,
  dp_spr0.8.hr            ,

  # dp_spr0.8.hn.g90c        ,
  dp_spr0.8.hn.swc         ,
  # dp_spr0.8.hn.swc.g90c    ,

  # dp_spr0.8.hn.bfc         ,
  dp_spr0.8.hn.szc         ,
  # dp_spr0.8.hn.g90         ,
  # dp_spr0.8.hn.g45c        ,
  dp_spr0.8.hn.sw          ,
  # dp_spr0.8.hn.swc..g90c   ,
  # dp_spr0.8.hn.swc.g90c.bfc,
  # dp_spr0.8.hn.swc.g45c.szc,
  # dp_spr0.8.hn.swc..g45c   ,
  dp_spr0.8.hn.swc..obs    ,
  dp_spr0.8.hn.swc2        ,
  dp_spr0.8.hn.ob          ,
  output="plain")

model.table.dp_spr

#-------------------------------------------------------------------------------
# SUMMER
#-------------------------------------------------------------------------------
dp_summ <- summer %>% filter(Species %like% sp) %>% data.frame() # n = 11

dp_summ0.8.un               <-ds(dp_summ, key = "un", truncation = 0.8)
dp_summ0.8.hn               <-ds(dp_summ, key = "hn", truncation = 0.8)
# dp_summ0.8.hr               <-ds(dp_summ, key = "hr", truncation = 0.8)

# dp_summ0.8.hn.g90c          <-ds(dp_summ, 0.8, key="hn", formula=~Glare90)
dp_summ0.8.hn.swc           <-ds(dp_summ, 0.8, key="hn", formula=~Clumped_Swell)
# dp_summ0.8.hn.swc.g90c      <-ds(dp_summ, 0.8, key="hn", formula=~Clumped_Swell+Glare90)

dp_summ0.8.hn.bfc           <-ds(dp_summ, 0.8, key="hn", formula=~Clumped_Beaufort)
dp_summ0.8.hn.szc           <-ds(dp_summ, 0.8, key="hn", formula=~Clumped_Group_Size)
# dp_summ0.8.hn.g90           <-ds(dp_summ, 0.8, key="hn", formula=~Glare90)
# dp_summ0.8.hn.g45c          <-ds(dp_summ, 0.8, key="hn", formula=~Glare45)
# dp_summ0.8.hn.sw            <-ds(dp_summ, 0.8, key="hn", formula=~swell)
# dp_summ0.8.hn.swc..g90c     <-ds(dp_summ, 0.8, key="hn", formula=~Clumped_Swell*Glare90)
# dp_summ0.8.hn.swc.g90c.bfc  <-ds(dp_summ, 0.8, key="hn", formula=~Clumped_Swell+Glare90+Clumped_Beaufort)
# dp_summ0.8.hn.swc.g45c.szc  <-ds(dp_summ, 0.8, key="hn", formula=~Clumped_Swell+Glare45+Clumped_Group_Size)
# dp_summ0.8.hn.swc..g45c     <-ds(dp_summ, 0.8, key="hn", formula=~Clumped_Swell*Glare45)
dp_summ0.8.hn.swc..obs      <-ds(dp_summ, 0.8, key="hn", formula=~Clumped_Swell*Observer)
# dp_summ0.8.hn.swc2          <-ds(dp_summ, 0.8, key="hn", formula=~Clumped_Swell2)
dp_summ0.8.hn.ob            <-ds(dp_summ, 0.8, key="hn", formula=~Observer)


model.table.dp_summ <- summarize_ds_models(
  dp_summ0.8.un            ,
  dp_summ0.8.hn            ,
  # dp_summ0.8.hr            ,

  # dp_summ0.8.hn.g90c        ,
  dp_summ0.8.hn.swc         ,
  # dp_summ0.8.hn.swc.g90c    ,

  dp_summ0.8.hn.bfc         ,
  dp_summ0.8.hn.szc         ,
  # dp_summ0.8.hn.g90         ,
  # dp_summ0.8.hn.g45c        ,
  # dp_summ0.8.hn.sw          ,
  # dp_summ0.8.hn.swc..g90c   ,
  # dp_summ0.8.hn.swc.g90c.bfc,
  # dp_summ0.8.hn.swc.g45c.szc,
  # dp_summ0.8.hn.swc..g45c   ,
  dp_summ0.8.hn.swc..obs    ,
  # dp_summ0.8.hn.swc2        ,
  dp_summ0.8.hn.ob          ,
  output="plain")
model.table.dp_summ

dp <- all_ap_sf %>% filter(Species %like% "Dall")
table(dp$swell)
table(dp$Clumped_Swell)
table(dp$Clumped_Swell2)
