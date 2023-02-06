###########################################################################################
# Reduce Sampling Complexity of Species Coordinates for species distribution modeling
# Author: Jared Streich
# Version 0.1.0
# Email: while at ORNL: ju0@ornl.gov, beyond ORNL: streich.jared@gmail.com
###########################################################################################


###########################################################################################
############################### Load Libraries and Packages ###############################
###########################################################################################

library(raster)
library(rworldmap)


###########################################################################################
##################################### Start Script ########################################
###########################################################################################

setwd("~/Desktop/")

##### Read in file and adjust format and encoding for maxent  
coords <- read.table("Theobroma_cocao_SpecLatLong.csv", sep = ",", header = T)
head(coords)

### Digits to round to
dg <- 1

# Adjust Coords
coords.round <- cbind(round(coords[,2], digits = dg), round(coords[,3], digits = dg))
colnames(coords.round) <- c("Latitude", "Longitude")
coords.round <- as.data.frame(coords.round)
coords <- cbind(coords, paste(coords.round[,1], coords.round[,2], sep = "_"))
coords <- coords[!duplicated(coords[,4]), ]
coords <- coords[,1:3]

head(coords)
dim(coords)

##### Check that the format actually plots on a map correctly
newmap <- getMap(resolution = "high", projection = "NA")
plot(newmap, border = "grey60", col = "grey80")
points(as.numeric(as.character(coords[,3])), as.numeric(as.character(coords[,2])), col = "dodgerblue3", pch = 16, cex = 0.4)

##### Limit the upper and lower bounds of lat and long
### Adjust Latitude max and min
# !!!!!!!!! Adjust for each species, these are not default values !!!!!!!!
coords <- coords[coords[,2] < 30, ]
coords <- coords[coords[,2] > -30, ]

# Adjust Long max and min
# coords <- coords[coords[,3] > n, ]
# coords <- coords[coords[,3] < n, ]

##### Check new map after filtering points that are likely indoor collections
plot(newmap, border = "grey60", col = "grey80")
points(as.numeric(as.character(coords[,3])), as.numeric(as.character(coords[,2])), col = "dodgerblue3", pch = 16, cex = 0.4)


##### Write out final file
write.table(coords, file = "Theobroma_cocoa_Reduced_SpecLatLong.csv", sep = ",", quote = F, col.names = T, row.names = F)


##### Get dimensions of current clim
coords.mxx <- max(coords[,2]) + 10
coords.mnx <- min(coords[,2]) - 10

coords.mxx
coords.mnx

theo.coords.mxx
theo.coords.mnx

coff.coords.mxx
coff.coords.mnx


##############################################################################################
#################### Crop biolayers to species for climate projections #######################
##############################################################################################



##### Load packages
library(raster)

##### Set working directory
setwd("/Volumes/Lisa/culex_tarsalis_sdm/projection_files/")

##### Read in current climate and one projection climate for analysis
cur.clm <- raster("c.tarsalis_avg.asc")
prj.clm <- raster("c.tarsalis_fc_2081-2100_585_avg.asc")

##### Resize to projection layers
# cur.clm <- resample(cur.clm, prj.clm, method='bilinear')

##### Set current clim threshold
cur.clm[cur.clm >= 0.5] <- 1
cur.clm[cur.clm < 0.5] <- 0

par(mfrow = c(3,1))
plot(cur.clm)
plot(prj.clm)

loop.clm <- cur.clm + prj.clm
##### Set color regime
cols <- c("grey30", "maroon3", "goldenrod2", "springgreen4")


plot(loop.clm, col = cols)
##### Add rasters to parse geography by range expansion/contraction
### Four is future and current are same
### Three is future climate only
### One is current climate only
### Zero is never amenable

##### get list of all outputs to read, calculate, and plot
# files <- list.files(pattern = "*asc")
files <- list.files(pattern = "C.tarsalis_fc_*")


##### Loop through files
i <- 16
for(i in 1:length(files)){
  prj.clm <- raster(files[i])
  # prj.clm <- resample(prj.clm, cur.clm)
  ##### Set projection clim threshold
  prj.clm[prj.clm >= 0.5] <- 3
  prj.clm[prj.clm < 0.5] <- 0
  ##### Add rasters to parse geography by range expansion/contraction
  loop.clm <- cur.clm + prj.clm
  ##### Create Output Image
  png.nam <- substr(files[i],15,27)
  pdf(paste("C.tarsalis_Projection_Current_",png.nam, ".pdf", sep = ""))
  plot(loop.clm, col = cols, main = png.nam)
  dev.off()
}


