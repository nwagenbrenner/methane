library(sp)
library(plotGoogleMaps)

library(maps)
library(maptools)

#=======================================================
#  read data into spatialPointsDataFrame
#=======================================================
fileName<-"/home/natalie/methane/MasterLatLongCH42014_09_04.csv"

methane<-as.data.frame(read.table(fileName, sep=",", header=TRUE, as.is=TRUE))
colnames(methane)<-c("lat", "lon", "ppb", "dlat_m", "obs_dlon_m", "dist_m")
#convert to spatialPointsDataFrame
coordinates(methane) = c("lon", "lat")
#add projection
proj4string(methane)<-CRS("+proj=longlat +datum=WGS84")

#optional filtering
#methane_filtered<-subset(methane, subset=(ppb < 300))
#methane_filtered<-subset(methane_filtered, subset=(ppb > 200))

#=======================================================
#  plot raw data with county lines
#=======================================================
domain<-map("county", "indiana", plot = FALSE, fill = TRUE)
IDs<-sub("^indiana,", "", domain$names)
domain_sp<-map2SpatialPolygons(domain, IDs, CRS("+proj=longlat"))
#plot(domain_sp, axes = TRUE)

spl <- list("sp.lines", as(domain_sp, "SpatialLines"))

spplot(methane_filtered, "ppb", key.space='right', main="Methane Concentrations (ppb)", sp.layout=spl)
bubble(methane_filtered, "ppb", key.space='right', main="Methane Concentrations (ppb)", sp.layout=spl)


#=======================================================
#  bubble plot in google maps
#=======================================================
m<-bubbleGoogleMaps(methane_filtered, zcol="ppb",
                    max.radius=1000, 
                    do.sqrt=FALSE, 
                    strokeOpacity=0,
                    filename="methane_lessthan500ppb.htm")

#=======================================================
#  kml with points
#=======================================================
#kmlPoints(obj=methane, kmlfile="methane.kml", kmlname="methane")

#=======================================================
#  rasterize points
#=======================================================
library(raster)
library(gstat)

#subset for < 500 ppb
methane<-subset(methane, subset=(ppb < 500))

#set projection to utm
crs<-CRS("+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
methane<-spTransform(methane, crs)

#generate a raster to interpolate spatial points onto
r <- raster(xmn=bbox(methane)[1], xmx=bbox(methane)[3], ymn=bbox(methane)[2], ymx=bbox(methane)[4])
projection(r) <- projection(methane)
res(r) <- 100

#rasterize and plot the raw data, averaging values if more than one obs per cell
library(fields)
rr<-rasterize(methane, r, field="ppb", fun=mean)

brk<-c(0,30,60,100,200,300,400,500) 
rgb.palette <- colorRampPalette(c("red", "orange", "green", "blue"))
plot(rr, breaks = brk, col=rev(rgb.palette(7)), main="Methane Concentrations (ppb)")
#plot(rr, col=rev(rgb.palette(255)), horizontal = TRUE, main="Methane Concentrations (ppb)")

#write a kml of the raster
rr.kml<-projectRaster(rr, crs="+proj=longlat +datum=WGS84")
KML(rr.kml, "methane_100m.kml", col=rev(rgb.palette(255)))

#=======================================================
#  interpolate to raster
#=======================================================
#generate a raster to interpolate spatial points onto
r <- raster(xmn=bbox(methane)[1], xmx=bbox(methane)[3], ymn=bbox(methane)[2], ymx=bbox(methane)[4])
projection(r) <- projection(methane)
res(r) <- 100

#set up an inverse distance weighting model for interpolation
mg <- gstat(id = "ppb", formula = ppb~1, data=methane)
#interpolate to raster points
z <- interpolate(r, mg)

#write the raster
writeRaster(z, "methane_idw_interp.asc")

#write a kml of the raster
z.kml<-projectRaster(z, crs="+proj=longlat +datum=WGS84")
KML(z.kml, "methane.kml")





