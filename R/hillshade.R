# USEFUL ONLY IF YOU ALREADY HAVE A HILLSHADE LAYER
# https://gist.github.com/simonohanlon101/2972862

# url1 <- "http://dl.dropbox.com/s/xp4xsrjn3vb5mn5/GHA_HS.asc"
# url2 <- "http://dl.dropbox.com/s/gh7gzou9711n5q7/GHA_DEM.asc"
# f1 <- paste(getwd(),"\\GHA_HS.asc",sep="")
# f2 <- paste(getwd(),"\\GHA_DEM.asc",sep="")
# download.file(url1,f1)	#File is ~ 5,655Kb
# download.file(url2,f2)	#File is ~ 2,645Kb
#
# #	Create rasters from downloaded files
# hs <- raster("GHA_HS.asc")
# dem <- raster("GHA_DEM.asc")
#
#
# #	Plot with base graphics to show desired output
# plot(hs,col=grey(1:100/100),legend=F)
# plot(dem,col=rainbow(100),alpha=0.4,add=T,legend=F)
#
#
# #	Convert rasters TO dataframes for plotting with ggplot
# hdf <- rasterToPoints(hs); hdf <- data.frame(hdf)
# colnames(hdf) <- c("X","Y","Hill")
# ddf <- rasterToPoints(dem); ddf <- data.frame(ddf)
# colnames(ddf) <- c("X","Y","DEM")
#
#
# #	Create vectors for colour breaks
# b.hs <- seq(min(hdf$Hill),max(hdf$Hill),length.out=100)
# b.dem <- seq(min(ddf$DEM),max(ddf$DEM),length.out=100)

#--------------------------------------------------------
#--------------------------------------------------------
x <- rast("../dem_full_stud") # from arcgis/cemore folder
plot(x)

# TERRA
# https://rdrr.io/cran/terra/man/shade.html
# f <- system.file("ex/elev.tif", package="terra")
# r <- rast(f)
alt <- disagg(x, 10, method="bilinear")
slope <- terrain(alt, "slope", unit="radians")
aspect <- terrain(alt, "aspect", unit="radians")
hill <- shade(slope, aspect, 40, 270)
plot(hill, col=grey(0:100/100), legend=FALSE, mar=c(2,2,1,4))
plot(alt, col=rainbow(25, alpha=0.35), add=TRUE)

# A better hill shade may be achieved by combining
# different angles and directions. For example

h <- shade(slope, aspect, angle = c(45, 45, 45, 80), direction = c(225, 270, 315, 135))
h <- Reduce(mean, h)

# # RASTER
# # https://rdrr.io/cran/raster/man/hillShade.html
# # alt <- getData('alt', country='CHE')
# slope2 <- terrain(alt, 'slope')
# aspect2 <- terrain(alt, 'aspect')
# hill2 <- hillShade(slope2, aspect2, 40, 270)
# plot(hill2, col=grey(0:100/100), legend=FALSE, main='TSS6_km_buffer')
# plot(alt, col=rainbow(25, alpha=0.35), add=TRUE)

# FROM ABOVE

#	Convert rasters TO dataframes for plotting with ggplot
hdf <- rasterToPoints(raster(hill))
hdf <- data.frame(hdf) # 89487924 obs or 3 var
# hdf2 <- as.data.frame(hill)
colnames(hdf) <- c("X","Y","Hill")

ddf <- rasterToPoints(raster(alt))
ddf <- data.frame(ddf)
colnames(ddf) <- c("X","Y","DEM")


#	Create vectors for colour breaks
b.hs <- seq(min(hdf$Hill),max(hdf$Hill),length.out=100)
b.dem <- seq(min(ddf$DEM),max(ddf$DEM),length.out=100)

#	Plot DEM layer with ggplot()
p1 <- ggplot()+
  layer(geom="raster", stat="identity", position="identity",data=ddf,mapping=aes(X,Y,fill=DEM))+
  scale_fill_gradientn(name="Altitude",colours = rainbow(100),breaks=b.dem)+
  scale_x_continuous(name=expression(paste("Longitude (",degree,")")),limits=c(-4,2),expand=c(0,0))+
  scale_y_continuous(name=expression(paste("Latitude (",degree,")")),limits=c(4,12),expand=c(0,0))+
  coord_equal()
print(p1)


#	Plot hillShade layer with ggplot()
p2 <- ggplot()+
  layer(geom="raster", stat="identity", position="identity",data=hdf,mapping=aes(X,Y,fill=Hill))+
  scale_fill_gradientn(colours=grey(1:100/100),breaks=b.hs,guide="none")+
  scale_x_continuous(name=expression(paste("Longitude (",degree,")")),limits=c(-4,2),expand=c(0,0))+
  scale_y_continuous(name=expression(paste("Latitude (",degree,")")),limits=c(4,12),expand=c(0,0))+
  coord_equal()
print(p2)


#	Try to plot both together with transparency on the DEM layer
p3 <- ggplot(hdf)+
  geom_raster(aes(X,Y,fill=Hill))+
  scale_fill_gradientn(colours=grey(1:100/100),breaks=b.hs,guide="none")+
  scale_x_continuous(name=expression(paste("Longitude (",degree,")")),limits=c(-4,2),expand=c(0,0))+
  scale_y_continuous(name=expression(paste("Latitude (",degree,")")),limits=c(4,12),expand=c(0,0))+
  geom_raster(data=ddf,aes(X,Y,fill=DEM),alpha=I(0.4))+
  scale_fill_gradientn(name="Altitude",colours = rainbow(100),breaks=b.dem)+
  coord_equal()
print(p3)

#	This won't work because ggplot2 won't work with multiple continuous colour scales (deliberate design convention to avoid users making complicated plots which cloud data meaning)
#	However there are a few use cases where overplotting of data may be useful
#	We can do this using the gridExtra package...

grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 1)))
print(p1 , vp = vplayout( 1 , 1 ) )
pushViewport(viewport(layout = grid.layout(1, 1)))
print(p2 , vp = vplayout( 1 , 1 ) )


#	Cleanup downloaded files and return to previous wd
unlink(tmp,recursive=T)
setwd(oldwd)
