# Looking at seasonal results for hp for publication in primary literature.

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
#    Harbour Porpoise
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# the following code is from 07_tables_and_figures.Rmd where the models are run
# and abundance estimated

#-------------------------------------------------------------------------------
#    FALL
#-------------------------------------------------------------------------------
sp <-  "harbour"
hp_fall <- fall %>% filter(Species %like% sp) %>% data.frame() # n = 204

un.fall0.65        <- ds(data=hp_fall, truncation=0.65, key="un")
hn.fall0.65        <- ds(data=hp_fall, truncation=0.65, key="hn")
hr.fall0.65        <- ds(data=hp_fall, truncation=0.65, key="hr")

hn.fall.bfc0.65        <- ds(data=hp_fall, truncation=0.65, key="hn", formula=~Clumped_Beaufort)
hn.fall.szc0.65        <- ds(data=hp_fall, truncation=0.65, key="hn", formula=~Clumped_Group_Size)
hn.fall.obs0.65        <- ds(data=hp_fall, truncation=0.65, key="hn", formula=~Observer)
hn.fall.bfc.obs0.65    <- ds(data=hp_fall, truncation=0.65, key="hn", formula=~Clumped_Beaufort+Observer)
hn.fall.bfc.szc0.65    <- ds(data=hp_fall, truncation=0.65, key="hn", formula=~Clumped_Beaufort+Clumped_Group_Size)
hn.fall.bfc.obs.szc0.65<- ds(data=hp_fall, truncation=0.65, key="hn", formula=~Clumped_Beaufort+Observer+Clumped_Group_Size)
hn.fall.bfc..obs0.65   <- ds(data=hp_fall, truncation=0.65, key="hn", formula=~Clumped_Beaufort*Observer)


model.table.hp_fall <- summarize_ds_models(
  un.fall0.65,
  hn.fall0.65,
  hr.fall0.65,
  hn.fall.bfc0.65        ,
  hn.fall.szc0.65        ,
  hn.fall.obs0.65        ,
  hn.fall.bfc.obs0.65    ,
  hn.fall.bfc.szc0.65    ,
  hn.fall.bfc.obs.szc0.65,
  # hn.fall.bfc..obs0.65   ,
  output="plain")
model.table.hp_fall

#-------------------------------------------------------------------------------
# WINTER
#-------------------------------------------------------------------------------
hp_winter <- winter %>% filter(Species %like% sp) %>% data.frame() # n = 118

un.winter0.65        <- ds(data=hp_winter, truncation=0.65, key="un")
hn.winter0.65        <- ds(data=hp_winter, truncation=0.65, key="hn")
hr.winter0.65        <- ds(data=hp_winter, truncation=0.65, key="hr")

hn.winter.bfc0.65        <- ds(data=hp_winter, truncation=0.65, key="hn", formula=~Clumped_Beaufort)
hn.winter.szc0.65        <- ds(data=hp_winter, truncation=0.65, key="hn", formula=~Clumped_Group_Size)
hn.winter.obs0.65        <- ds(data=hp_winter, truncation=0.65, key="hn", formula=~Observer)
hn.winter.bfc.obs0.65    <- ds(data=hp_winter, truncation=0.65, key="hn", formula=~Clumped_Beaufort+Observer)
hn.winter.bfc.szc0.65    <- ds(data=hp_winter, truncation=0.65, key="hn", formula=~Clumped_Beaufort+Clumped_Group_Size)
hn.winter.bfc.obs.szc0.65<- ds(data=hp_winter, truncation=0.65, key="hn", formula=~Clumped_Beaufort+Observer+Clumped_Group_Size)
hn.winter.bfc..obs0.65   <- ds(data=hp_winter, truncation=0.65, key="hn", formula=~Clumped_Beaufort*Observer)


model.table.hp_winter <- summarize_ds_models(
  un.winter0.65,
  hn.winter0.65,
  hr.winter0.65,
  hn.winter.bfc0.65        ,
  hn.winter.szc0.65        ,
  hn.winter.obs0.65        ,
  hn.winter.bfc.obs0.65    ,
  hn.winter.bfc.szc0.65    ,
  hn.winter.bfc.obs.szc0.65,
  hn.winter.bfc..obs0.65   ,
  output="plain")
model.table.hp_winter

#-------------------------------------------------------------------------------
# SPRING
#-------------------------------------------------------------------------------
hp_spr <- spring %>% filter(Species %like% sp) %>% data.frame() # n = 57

un.spr0.65        <- ds(data=hp_spr, truncation=0.65, key="un")
hn.spr0.65        <- ds(data=hp_spr, truncation=0.65, key="hn")
hr.spr0.65        <- ds(data=hp_spr, truncation=0.65, key="hr")

hn.spr.bfc0.65        <- ds(data=hp_spr, truncation=0.65, key="hn", formula=~Clumped_Beaufort)
hn.spr.szc0.65        <- ds(data=hp_spr, truncation=0.65, key="hn", formula=~Clumped_Group_Size)
hn.spr.obs0.65        <- ds(data=hp_spr, truncation=0.65, key="hn", formula=~Observer)
hn.spr.bfc.obs0.65    <- ds(data=hp_spr, truncation=0.65, key="hn", formula=~Clumped_Beaufort+Observer)
hn.spr.bfc.szc0.65    <- ds(data=hp_spr, truncation=0.65, key="hn", formula=~Clumped_Beaufort+Clumped_Group_Size)
hn.spr.bfc.obs.szc0.65<- ds(data=hp_spr, truncation=0.65, key="hn", formula=~Clumped_Beaufort+Observer+Clumped_Group_Size)
hn.spr.bfc..obs0.65   <- ds(data=hp_spr, truncation=0.65, key="hn", formula=~Clumped_Beaufort*Observer)


model.table.hp_spr <- summarize_ds_models(
  un.spr0.65,
  hn.spr0.65,
  hr.spr0.65,
  hn.spr.bfc0.65        ,
  hn.spr.szc0.65        ,
  hn.spr.obs0.65        ,
  hn.spr.bfc.obs0.65    ,
  hn.spr.bfc.szc0.65    ,
  hn.spr.bfc.obs.szc0.65,
  hn.spr.bfc..obs0.65   ,
  output="plain")
model.table.hp_spr

#-------------------------------------------------------------------------------
# SUMMER
#-------------------------------------------------------------------------------
hp_summ <- summer %>% filter(Species %like% sp) %>% data.frame() # n = 129

un.summ0.65        <- ds(data=hp_summ, truncation=0.65, key="un")
hn.summ0.65        <- ds(data=hp_summ, truncation=0.65, key="hn")
hr.summ0.65        <- ds(data=hp_summ, truncation=0.65, key="hr")

hn.summ.bfc0.65        <- ds(data=hp_summ, truncation=0.65, key="hn", formula=~Clumped_Beaufort)
hn.summ.szc0.65        <- ds(data=hp_summ, truncation=0.65, key="hn", formula=~Clumped_Group_Size)
hn.summ.obs0.65        <- ds(data=hp_summ, truncation=0.65, key="hn", formula=~Observer)
hn.summ.bfc.obs0.65    <- ds(data=hp_summ, truncation=0.65, key="hn", formula=~Clumped_Beaufort+Observer)
hn.summ.bfc.szc0.65    <- ds(data=hp_summ, truncation=0.65, key="hn", formula=~Clumped_Beaufort+Clumped_Group_Size)
hn.summ.bfc.obs.szc0.65<- ds(data=hp_summ, truncation=0.65, key="hn", formula=~Clumped_Beaufort+Observer+Clumped_Group_Size)
hn.summ.bfc..obs0.65   <- ds(data=hp_summ, truncation=0.65, key="hn", formula=~Clumped_Beaufort*Observer)


model.table.hp_summ <- summarize_ds_models(
  un.summ0.65,
  hn.summ0.65,
  hr.summ0.65,
  hn.summ.bfc0.65        ,
  hn.summ.szc0.65        ,
  hn.summ.obs0.65        ,
  hn.summ.bfc.obs0.65    ,
  hn.summ.bfc.szc0.65    ,
  hn.summ.bfc.obs.szc0.65,
  hn.summ.bfc..obs0.65   ,
  output="plain")
model.table.hp_summ
