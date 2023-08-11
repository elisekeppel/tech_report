# Looking at seasonal results for publication in primary literature.

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
#    HW
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# the following code is from 07_tables_and_figures.Rmd where the models are run
# and abundance estimated

#-------------------------------------------------------------------------------
#    FALL
#-------------------------------------------------------------------------------
sp <-  "humpback"
hw_fall <- fall %>% filter(Species %like% sp) %>% data.frame() # n = 173

hw_fall_1.5.un.cos       <-ds(hw_fall, key = "un", truncation = 1.5)
hw_fall_1.5.hn           <-ds(hw_fall, key = "hn", truncation = 1.5)
hw_fall_1.5.hr           <-ds(hw_fall, key = "hr", truncation = 1.5)

hw_fall_1.5.hn.v         <-ds(hw_fall, 1.5, key="hn", formula=~Clumped_Visibility)
hw_fall_1.5.hn.g90c      <-ds(hw_fall, 1.5, key="hn", formula=~Glare90)
hw_fall_1.5.hn.bf        <-ds(hw_fall, 1.5, key="hn", formula=~Beaufort)
hw_fall_1.5.hn.v.g90c    <-ds(hw_fall, key = "hn", truncation = 1.5, formula=~Clumped_Visibility+Glare90)
hw_fall_1.5.hn.v.bf      <-ds(hw_fall, key = "hn", truncation = 1.5, formula=~Clumped_Visibility+Beaufort)

model.table.hw_fall <- summarize_ds_models(
  hw_fall_1.5.un.cos   ,
  hw_fall_1.5.hr,
  hw_fall_1.5.hn       ,
  hw_fall_1.5.hn.v,
  hw_fall_1.5.hn.g90c  ,
  hw_fall_1.5.hn.bf    ,
  hw_fall_1.5.hn.v.g90c ,
  hw_fall_1.5.hn.v.bf   ,
  output="plain")
model.table.hw_fall

#-------------------------------------------------------------------------------
# WINTER
#-------------------------------------------------------------------------------
hw_winter <- winter %>% filter(Species %like% sp) %>% data.frame() # n = 13

hw_winter_1.5.un           <-ds(hw_winter, key = "un", truncation = 1.5)
hw_winter_1.5.hn           <-ds(hw_winter, key = "hn", truncation = 1.5)
hw_winter_1.5.hr           <-ds(hw_winter, key = "hr", truncation = 1.5)

# hw_winter_1.5.hn.v         <-ds(hw_winter, 1.5, key="hn", formula=~Clumped_Visibility)
hw_winter_1.5.hn.g90c      <-ds(hw_winter, 1.5, key="hn", formula=~Glare90)
hw_winter_1.5.hn.bf        <-ds(hw_winter, 1.5, key="hn", formula=~Beaufort)
# hw_winter_1.5.hn.v.g90c    <-ds(hw_winter, key = "hn", truncation = 1.5, formula=~Clumped_Visibility+Glare90)
# hw_winter_1.5.hn.v.bf      <-ds(hw_winter, key = "hn", truncation = 1.5, formula=~Clumped_Visibility+Beaufort)

model.table.hw_winter <- summarize_ds_models(
  hw_winter_1.5.un   ,
  hw_winter_1.5.hr,
  hw_winter_1.5.hn,
  # hw_winter_1.5.hn.v,
  hw_winter_1.5.hn.g90c  ,
  hw_winter_1.5.hn.bf  ,
  # hw_winter_1.5.hn.v.g90c ,
  # hw_winter_1.5.hn.v.bf   ,
# hw_winter_1.5.un,
  output="plain"
  )
model.table.hw_winter

#-------------------------------------------------------------------------------
# SPRING
#-------------------------------------------------------------------------------
hw_spr <- spring %>% filter(Species %like% sp) %>% data.frame() # n = 57

hw_spr_1.5.un.cos       <-ds(hw_spr, key = "un", truncation = 1.5)
hw_spr_1.5.hn           <-ds(hw_spr, key = "hn", truncation = 1.5)
hw_spr_1.5.hr           <-ds(hw_spr, key = "hr", truncation = 1.5)

hw_spr_1.5.hn.v         <-ds(hw_spr, 1.5, key="hn", formula=~Clumped_Visibility)
hw_spr_1.5.hn.g90c      <-ds(hw_spr, 1.5, key="hn", formula=~Glare90)
hw_spr_1.5.hn.bf        <-ds(hw_spr, 1.5, key="hn", formula=~Beaufort)
hw_spr_1.5.hn.v.g90c    <-ds(hw_spr, key = "hn", truncation = 1.5, formula=~Clumped_Visibility+Glare90)
hw_spr_1.5.hn.v.bf      <-ds(hw_spr, key = "hn", truncation = 1.5, formula=~Clumped_Visibility+Beaufort)

model.table.hw_spring <- summarize_ds_models(
  hw_spr_1.5.un.cos   ,
  hw_spr_1.5.hr,
  hw_spr_1.5.hn       ,
  hw_spr_1.5.hn.v,
  hw_spr_1.5.hn.g90c  ,
  hw_spr_1.5.hn.bf    ,
  hw_spr_1.5.hn.v.g90c ,
  hw_spr_1.5.hn.v.bf   ,
  output="plain")
model.table.hw_spring
#-------------------------------------------------------------------------------
# SUMMER
#-------------------------------------------------------------------------------
hw_summ <- summer %>% filter(Species %like% sp) %>% data.frame() # n = 129

hw_summ_1.5.un.cos       <-ds(hw_summ, key = "un", truncation = 1.5)
hw_summ_1.5.hn           <-ds(hw_summ, key = "hn", truncation = 1.5)
hw_summ_1.5.hr           <-ds(hw_summ, key = "hr", truncation = 1.5)

hw_summ_1.5.hn.v         <-ds(hw_summ, 1.5, key="hn", formula=~Clumped_Visibility)
hw_summ_1.5.hn.g90c      <-ds(hw_summ, 1.5, key="hn", formula=~Glare90)
hw_summ_1.5.hn.bf        <-ds(hw_summ, 1.5, key="hn", formula=~Beaufort)
hw_summ_1.5.hn.v.g90c    <-ds(hw_summ, key = "hn", truncation = 1.5, formula=~Clumped_Visibility+Glare90)
hw_summ_1.5.hn.v.bf      <-ds(hw_summ, key = "hn", truncation = 1.5, formula=~Clumped_Visibility+Beaufort)

model.table.hw_summ <- summarize_ds_models(
  hw_summ_1.5.un.cos   ,
  hw_summ_1.5.hr,
  hw_summ_1.5.hn       ,
  hw_summ_1.5.hn.v,
  hw_summ_1.5.hn.g90c  ,
  hw_summ_1.5.hn.bf    ,
  hw_summ_1.5.hn.v.g90c ,
  hw_summ_1.5.hn.v.bf   ,
  output="plain")
model.table.hw_summ
