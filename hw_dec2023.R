sp <-  "humpback"
df.hw <- all_ap_sf %>% filter(Species %like% sp) %>% data.frame()

# if(file.exists("data/hw-models.Rdata")){
#   load("data/hw-models.Rdata")
# }else{

hw.un       <-ds(df.hw, key = "un")
plot_df("humpback whale", hw.un)


hw2.un          <-ds(df.hw, key = "un", truncation=2)
gof_ds(hw2.un)
hw1.75.un       <-ds(df.hw, key = "un", truncation=1.75)
gof_ds(hw1.75.un)
hw1.5.un        <-ds(df.hw, key = "un", truncation=1.5)
gof_ds(hw1.5.un)
plot_df("humpback whale", hw2.un)
plot_df("humpback whale", hw1.75.un)
plot_df("humpback whale", hw1.5.un)

hw1.75 <- df.hw %>% filter(distance<1.75)

hw1.75.un       <-ds(obs_hw, key = "un", truncation=1.75)
hw1.75.hn       <-ds(obs_hw, key = "hn", truncation = 1.75)
hw1.75.hr       <-ds(obs_hw, key = "hr", truncation = 1.75)

hw1.75 %>% group_by(season, Visibility) %>% summarise(n())
hw1.75 %>% group_by(season, Beaufort) %>% summarise(n())
hw1.75 %>% group_by(season, Clumped_Beaufort) %>% summarise(n())
hw1.75 %>% group_by(season, Group_Size) %>% summarise(n())
hw1.75 %>% group_by(Group_Size) %>% summarise(n())
hw1.75 %>% group_by(season, Clumped_Group_Size) %>% summarise(n())
hw1.75 %>% group_by(season, Clumped_Group_Size2) %>% summarise(n())
hw1.75 %>% group_by(Clumped_Group_Size2) %>% summarise(n())
hw1.75 %>% group_by(season, Observer) %>% summarise(n())

# hw1.75.hn.v      <-ds(obs_hw, 1.75, key = "hn", formula=~Visibility)
# hw1.75.hn.vc     <-ds(obs_hw, 1.75, key = "hn", formula=~Clumped_Visibility)
# hw1.75.hn.bf     <-ds(obs_hw, 1.75, key = "hn", formula=~Beaufort)
hw1.75.hn.bfc      <-ds(obs_hw, 1.75, key = "hn", formula=~Clumped_Beaufort)
# hw1.75.hn.sz       <-ds(obs_hw, 1.75, key = "hn",  formula=~Group_Size)
hw1.75.hn.szc      <-ds(obs_hw, 1.75, key = "hn",  formula=~Clumped_Group_Size)
hw1.75.hn.szc2     <-ds(obs_hw, 1.75, key = "hn",  formula=~Clumped_Group_Size2)
hw1.75.hn.obs      <-ds(obs_hw, 1.75, key = "hn",  formula=~Observer)
hw1.75.hn.bfc.szc  <-ds(obs_hw, 1.75, key = "hn", formula=~Clumped_Beaufort+Clumped_Group_Size)
hw1.75.hn.bfc.szc2 <-ds(obs_hw, 1.75, key = "hn", formula=~Clumped_Beaufort+Clumped_Group_Size2)
hw1.75.hn.bfc.obs  <-ds(obs_hw, 1.75, key = "hn", formula=~Clumped_Beaufort+Observer)

table(hw1.75$Group_Size)
table(hw1.75$Clumped_Group_Size)
table(hw1.75$Clumped_Group_Size2)

hw_sum_sz <- hw1.75 %>% data.frame() %>% count(Clumped_Group_Size2)

ggplot(data = hw1.75, aes(x=Clumped_Group_Size2, y=distance)) +
  geom_violin()+
  # stat_summary(fun=mean, geom="pointrange", shape=23, size=2)
  geom_boxplot(width=0.1)+
  geom_text(data=hw_sum_sz,
            aes(Clumped_Group_Size2,max(hw1.75$distance)+(0.05*max(hw1.75$distance)),label=n,size=0.1),size=3,vjust = -0.5)+
  # stat_summary(fun.data=mean_sdl, geom="pointrange")+
  ylab("Perpendicular sighting distance (km)")+
  xlab("Clumped Group Size")
#------------------------------------------------------------

hw_sum_bfc <- hw1.75 %>% data.frame() %>% count(Clumped_Beaufort)

ggplot(data = hw1.75, aes(x=Clumped_Beaufort, y=distance)) +
  geom_violin()+
  # stat_summary(fun=mean, geom="pointrange", shape=23, size=2)
  geom_boxplot(width=0.1)+
  geom_text(data=hw_sum_bfc,
            aes(Clumped_Beaufort,max(hw1.5$distance)+(0.05*max(hw1.5$distance)),label=n,size=0.1),size=3,vjust = -0.5)+
  # stat_summary(fun.data=mean_sdl, geom="pointrange")+
  ylab("Perpendicular sighting distance (km)")+
  xlab("Clumped_Beaufort")
#------------------------------------------------------------

hw1.75 %<>% mutate(bf_sz=paste(Clumped_Beaufort, Clumped_Group_Size2, sep=", "))

hw_sum_bf_sz <- hw1.75 %>% data.frame() %>% count(bf_sz)

ggplot(data = hw1.75, aes(x=bf_sz, y=distance)) +
  geom_violin()+
  # stat_summary(fun=mean, geom="pointrange", shape=23, size=2)
  geom_boxplot(width=0.1)+
  geom_text(data=hw_sum_bf_sz,
            aes(bf_sz,max(hw1.5$distance)+(0.05*max(hw1.5$distance)),label=n,size=0.1),size=3,vjust = -0.5)+
  # stat_summary(fun.data=mean_sdl, geom="pointrange")+
  ylab("Perpendicular sighting distance (km)")+
  xlab("Clumped_Beaufort and Clumped_Group_Size")

model.table.hw <- summarize_ds_models(
  hw1.75.un  ,
  hw1.75.hn      ,
  hw1.75.hr      ,
  # hw1.75.hn.bf   ,
  hw1.75.hn.bfc  ,
  # hw1.75.hn.sz,
  hw1.75.hn.szc  ,
  hw1.75.hn.szc2  ,
  hw1.75.hn.obs  ,
  hw1.75.hn.bfc.szc ,
  hw1.75.hn.bfc.szc2 ,
  hw1.75.hn.bfc.obs ,
  # hw1.75.hn.v ,
  # hw1.75.hn.vc,
  output="plain") %>% tibble::as_tibble()
model.table.hw
