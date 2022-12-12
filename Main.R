library(sp)
library(maptools)
library(foreign)
library(shapefiles)
library(sf)
library(terra)
library(classInt)
library(RColorBrewer)

fdc <- readShapePoly("Tunisie_snuts4")
donnees <- read.csv(file="tunisie_data_del_2014.csv", header = TRUE, sep = ",")
dim(donnees)
summary(fdc)
pt <- cbind(fdc@data[,"id"], as.data.frame(coordinates(fdc)))

colnames(pt) <- c("id", "x", "y")

i=match(fdc@data[, "id"], donnees [, "del"])
fdc@data <- data.frame(fdc@data, donnees[i,])

fdc@data$var <- fdc@data$Jabha_2014
var <- as.vector(na.omit(fdc@data$var))

nbclass <- 8
distr <- classIntervals(var,nbclass,style= "quantile")$brks

colours<- brewer.pal(nbclass,"YlOrRd")
colMap<- colours[(findInterval(fdc$var,distr,all.inside = TRUE))]

plot(fdc, col= colMap, border= "black", lwd= 1)
#pt <- data.frame(pt, donnees[i, ])
#pt$var <- pt$Jabha_2014
#pt$var <- pt$Nahdha_2014
#pt$var <- pt$Nidaa_2014
#pt$var <- pt$Takatol_2014
#pt$var <- pt$Afek_2014
#pt$var <- pt$Tayar_2014
#pt$var <- pt$Jomhouri_2014



#x1 <- bbox(fdc)[1] 
#y1 <- bbox(fdc) [2] 
#x2 <- bbox(fdc) [3] 
#y2 <- bbox(fdc)[4] 
