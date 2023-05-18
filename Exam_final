# In order to start working, as in every r project, I had to first install and load packages
# having to work with a Species Distribution Modelling, the packages needed are the followins:
library(raster)
library(rgdal)
library(sdm)
library(sp)
library(sf)
# these latter two are needed to create the shapefile of occurrences starting from the paper additional data

# set the working directory 
# it is where R will look for files I want to load and where it will put any files I save
setwd("C:/monitoring_exam")

# I recall the file I want from my wd, which is a csv 
wolf_data<-read.csv(file="mexican_wolf.csv")

# Now, of the whole data, I need to select only Latitudes and Longitudes (named Lat and Long)
nLat <-wolf_data$Lat
nLong <-wolf_data$Long
# and then I save the dataframe in a csv
write.csv(wolf_data, "C:/MonitoringExam/mexican_wolf.csv")

# Creation of the spatial dataframe
wolf.spacial=wolf_data[c("Long","Lat")]
coordinates(wolf.spacial)<-~Long+Lat

# With st_as_sf function I can create a simple feature object starting from a spatial point object
wolf.sf <-st_as_sf(wolf.spacial)
# and so convert it into a Shapefile (shp)
st_write(wolf.sf, "C:/MonitoringExam/wolf_shp.shp")

# I am missing occurrences, so I have modified the shp with qgis, adding the occurrence field
# at this point it is in the wd ready to be used

# Now that I have all the instruments for my study I can load the predictors
# the necessary files are in the wd, and are the .tif files
lst<-list.files(pattern = 'tif', full.names = T)
predictors <- stack(lst)

# The downloaded predictors are covering all the world
# the study I am interested in, however, focuses only on the Mexican area, so i crop the rasters
mex.extent<- extent(-125, -90, 14, 40)
preds.crop<- crop(x = predictors, y = mex.extent)
plot(preds.crop) #to see the image 
cl <- colorRampPalette(c('light green', 'dark green', ' yellow')) (100) #to create my own color palette of the image

# Let's start with the sdm creation
# in order to create a sdm I need presences shapefile and predictors
species <- shapefile("wolf_shp.shp")
datasdm <- sdmData(train = species, predictors = preds.crop)

# Next stop is to "model" with the method of bioclim
m1 <- sdm(Occurence~wc2.1_30s_AnnualP+wc2.1_30s_elev+wc2.1_30s_MaxT+wc2.1_30s_MinT, data=datasdm, methods="bioclim")

# Map probability of the  model
p1 <- predict(m1, preds.crop)

# I want to extract the raster and save it
writeRaster(p1, "Mexican_Wolf.grd")

# I use again the function of stack adding to the predictors the probability map
stack_1<- stack(preds.crop, p1) 
names(stack_1)<- c('Annual precipitation', 'Elevation', 'Maximum temperature of warmest month', 'Minimum temperature of coldest month', 'Probability')
plot(stack_1, col = cl) #to visualize the final result

# Final operations and the creation of a PDF file
pdf("Mexican_Wolf_SDM.pdf")
par(mfrow=c(3,2))
plot(stack_1$Probability, col = cl)
points(species, pch=19)
plot(preds.crop, col = cl)
dev.off()




