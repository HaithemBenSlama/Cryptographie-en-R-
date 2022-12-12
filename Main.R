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

###################### Nahdha
fdc@data$var <- fdc@data$Nahdha_2014
var <- as.vector(na.omit(fdc@data$var))

nbclass <- 8
distr <- classIntervals(var,nbclass,style= "quantile")$brks

colours<- brewer.pal(nbclass,"YlOrRd")
colMap<- colours[(findInterval(fdc$var,distr,all.inside = TRUE))]

plot(fdc, col= colMap, border= "black", lwd= 1)

legend(x="bottomleft", legend=leglabs(round(distr,2), over = ">", under="<"), fill=colours, bty="n",pt.cex =1,cex=0.7,title="indice 0-1")

title(main="Nombre d'élus du Nahdha 2014")

######################## Jabha_2014
fdc@data$var <- fdc@data$Jabha_2014
var <- as.vector(na.omit(fdc@data$var))

nbclass <- 8
distr <- classIntervals(var,nbclass,style= "quantile")$brks

colours<- brewer.pal(nbclass,"YlOrRd")
colMap<- colours[(findInterval(fdc$var,distr,all.inside = TRUE))]

plot(fdc, col= colMap, border= "black", lwd= 1)

legend(x="bottomleft", legend=leglabs(round(distr,2), over = ">", under="<"), fill=colours, bty="n",pt.cex =1,cex=0.7,title="indice 0-1")

title(main="Nombre d'élus du Jabha 2014")

######################## Nidaa_2014
fdc@data$var <- fdc@data$Nidaa_2014
var <- as.vector(na.omit(fdc@data$var))

nbclass <- 8
distr <- classIntervals(var,nbclass,style= "quantile")$brks

colours<- brewer.pal(nbclass,"YlOrRd")
colMap<- colours[(findInterval(fdc$var,distr,all.inside = TRUE))]

plot(fdc, col= colMap, border= "black", lwd= 1)

legend(x="bottomleft", legend=leglabs(round(distr,2), over = ">", under="<"), fill=colours, bty="n",pt.cex =1,cex=0.7,title="indice 0-1")

title(main="Nombre d'élus du Nidaa 2014")

######################## Takatol_2014
fdc@data$var <- fdc@data$Takatol_2014
var <- as.vector(na.omit(fdc@data$var))

nbclass <- 8
distr <- classIntervals(var,nbclass,style= "quantile")$brks

colours<- brewer.pal(nbclass,"YlOrRd")
colMap<- colours[(findInterval(fdc$var,distr,all.inside = TRUE))]

plot(fdc, col= colMap, border= "black", lwd= 1)

legend(x="bottomleft", legend=leglabs(round(distr,2), over = ">", under="<"), fill=colours, bty="n",pt.cex =1,cex=0.7,title="indice 0-1")

title(main="Nombre d'élus du Takatol 2014")

######################## Afek_2014
fdc@data$var <- fdc@data$Afek_2014
var <- as.vector(na.omit(fdc@data$var))

nbclass <- 8
distr <- classIntervals(var,nbclass,style= "quantile")$brks

colours<- brewer.pal(nbclass,"YlOrRd")
colMap<- colours[(findInterval(fdc$var,distr,all.inside = TRUE))]

plot(fdc, col= colMap, border= "black", lwd= 1)

legend(x="bottomleft", legend=leglabs(round(distr,2), over = ">", under="<"), fill=colours, bty="n",pt.cex =1,cex=0.7,title="indice 0-1")

title(main="Nombre d'élus du Afek 2014")

######################## Tayar_2014
fdc@data$var <- fdc@data$Tayar_2014
var <- as.vector(na.omit(fdc@data$var))

nbclass <- 8
distr <- classIntervals(var,nbclass,style= "quantile")$brks

colours<- brewer.pal(nbclass,"YlOrRd")
colMap<- colours[(findInterval(fdc$var,distr,all.inside = TRUE))]

plot(fdc, col= colMap, border= "black", lwd= 1)

legend(x="bottomleft", legend=leglabs(round(distr,2), over = ">", under="<"), fill=colours, bty="n",pt.cex =1,cex=0.7,title="indice 0-1")

title(main="Nombre d'élus du Tayar 2014")

######################## Jomhouri_2014
fdc@data$var <- fdc@data$Jomhouri_2014
var <- as.vector(na.omit(fdc@data$var))

nbclass <- 8
distr <- classIntervals(var,nbclass,style= "quantile")$brks

colours<- brewer.pal(nbclass,"YlOrRd")
colMap<- colours[(findInterval(fdc$var,distr,all.inside = TRUE))]

plot(fdc, col= colMap, border= "black", lwd= 1)

legend(x="bottomleft", legend=leglabs(round(distr,2), over = ">", under="<"), fill=colours, bty="n",pt.cex =1,cex=0.7,title="indice 0-1")

title(main="Nombre d'élus du Jomhouri 2014")

