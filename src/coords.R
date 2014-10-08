#=======================================================
#  read indy coords with ogr
#=======================================================
dsn<-"/home/natalie/methane/coords/"
layer<-'coords'
methane<-readOGR(dsn, layer)


#=======================================================
#  read indy coords as spatialPoints
#=======================================================
fileName<-"/home/natalie/methane/Indy_Coordinates_new_sorted_2010.txt"
#fileName<-"/home/natalie/methane/MasterLatLongCH42014_09_04.csv"

methane<-as.data.frame(read.table(fileName, header=TRUE, as.is=TRUE))
colnames(methane)<-c("lat", "lon")
methane<-as.data.frame(cbind(methane$lon, methane$lat))
colnames(methane)<-c("lon", "lat")

#write.table(methane, file = "/home/natalie/methane/coords.csv", append = FALSE, quote = TRUE, sep = ",",
#                 eol = "\n", na = "NA", dec = ".", row.names = FALSE,
#                 col.names = TRUE, qmethod = c("escape", "double"),
#                 fileEncoding = "")



#convert to spatialPointsDataFrame
coordinates(methane) = c("lon", "lat")
#add projection
proj4string(methane)<-CRS("+proj=longlat +datum=WGS84")


library(rgeos)
#set projection to utm

crs<-CRS("+proj=utm +zone=16 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
methane<-spTransform(methane, crs)

Sr1 <- Polygon(methane)
Srs1 <- Polygons(list(Sr1), "s1")
SpP <- SpatialPolygons(list(Srs1), 1:1)

gArea(SpP)








