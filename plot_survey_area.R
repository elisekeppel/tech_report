library(terra)
z <- rast("data/ss1.tif") %>% as.data.frame(xy=TRUE) %>% fortify() %>% reclassify()

ss <- rast("C:/users/keppele/documents/cemore/analysis/cemore_analysis/required shapefiles/SS_Bathymetry.tif")# %>%
  as.data.frame(xy=TRUE)%>% fortify()

m <- c(-800,-700,-800,
       -700,-600,-700,
       -600,-500,-600,
       -500,-400,-500,
       -400,-300,-400,
       -300,-200,-300,
       -200,-100,-200,
       -100,191,-100)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc <- reclassify(ss, rclmat)%>% as.data.frame(xy=TRUE) %>% fortify()
library(terra)


# for values >= 0 (instead of > 0), do
rc <- reclassify(r, rclmat, include.lowest=TRUE)

pe <- as.polygons(ext(ss))
pr <- as.polygons(ss > -Inf)

plot(ss)
plot(pe, lwd=5, border='red', add=TRUE)
plot(pr, lwd=3, border='blue', add=TRUE)

p_sf <- st_as_sf(pr) %>% st_transform(crs=3156)
write_sf(p_sf, dsn="data/SS_polygon.shp", driver="ESRI shapefile")
#---------------------
bathy_r <- marmap::as.raster(bathy)
ss_bathy <- st_intersection(bathy_sgdf,pr)
bathy_sgdf <- as.SpatialGridDataFrame(bathy)
bathy_sf <- st_as_sf(bathy)

ss_bathy <- crop(bathy_r, pe)
install.packages("ragg")
library(ragg)

bl <- RColorBrewer::brewer.pal(11L, "RdBu")[8]

# agg_png("study_area_map.png", width = 500, height = 500, res = 144)
ggplot() +
  # geom_raster(aes(x=x,y=y,fill = z), data = bathy) +  #labs(fill = "Depth (m)") +
  # scale_fill_gradientn(colours = col_ramp(20), guide = "none") +
  # ggnewscale::new_scale("fill") +
  theme(panel.background = element_rect(colour="grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  geom_sf(aes(alpha = "Salish Sea"), stroke=0.05,data = p_sf,fill=bl,colour=bl) +
  scale_alpha_manual(values=1)+

  geom_sf(data = tss, aes(fill = "Traffic Separation Scheme"), colour=RColorBrewer::brewer.pal(9,"Oranges")[4]) +
  geom_sf(data = survey_area, aes(colour = "Full Study Area"),
          fill=NA,alpha=0.3)+
  geom_sf_pattern(data = survey_can, fill = NA, aes(pattern_fill = "Cdn Study Area"),
                  pattern_spacing = 0.02, pattern = "stripe", pattern_size = 0.05)+
  coord_fixed(ratio = 1/2)+
  geom_sf(data = canada_shp, aes(linetype = "Can-US border"), fill=NA,colour="red") +

  scale_colour_manual(values = 'grey40') +
  scale_fill_manual(values = RColorBrewer::brewer.pal(9,"Oranges")[4]) +
  scale_pattern_fill_manual(values = 'grey60') +
  scale_linetype_manual(values = c(5)) +
  labs(fill='',colour = '',pattern_fill='',linetype='',shape='')+
  guides(fill=guide_legend(order=1),
         colour = guide_legend(order =2),
         pattern_fill =guide_legend(order=3),
         linetype = guide_legend(title = NULL, order =4),
         shape=guide_legend(title=NULL, order=5),
         alpha=guide_legend(title=NULL, order=6)) +
  theme(legend.position = "bottom",
        legend.key = element_blank(),
        legend.spacing = unit(c(-0.4,-0.4,-0.3,-0.4,0), "lines"),
        plot.margin = unit(c(-0.2,-1,0,-1,0), "cm")
  ) +
  geom_sf(data = coast_file, stroke = 0.01, fill = "light yellow", colour = " ") +
  geom_sf(data=w,aes(shape=TextString))+
  geom_sf(data = survey_area, aes(colour = "Full Study Area"),
          fill=NA,alpha=0.3)+

  geom_sf_text(data = locations, aes(label = TextString,
                                     angle = Angle),
               nudge_x = locations$XOffset,
               nudge_y = locations$YOffset,
               size = 3,#locations$FontSize*0.3,
               colour="black")+
  coord+
  ylab("")+xlab("")
# invisible(dev.off())
#--- trying to make legend at bottom to make image larger
# theme(legend.position = "bottom",
#           legend.margin = margin(0),
#           legend.key.size = unit(0.1,"cm")) +
#     theme(plot.margin = unit(c(-1,-1,-1,-1), "cm"))+
