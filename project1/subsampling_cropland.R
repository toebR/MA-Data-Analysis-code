library(sf)
library(tidyverse)
require(rgdal)
require(GGally)
require(gridExtra)


#set working directory
dir  = "C:\\Users\\tobia\\OneDrive\\Desktop\\MA_local\\geoprocessing\\maps\\dispo2"
setwd(dir)

#import dispo iteration 2 cropland geodata
cropland = st_read(dsn = "C:\\Users\\tobia\\OneDrive\\Desktop\\MA_local\\geoprocessing\\maps\\dispo2\\shapes_new\\cropland_all.shp")
st_crs(cropland)

#quick glimpse on data
head(cropland)

ggplot(cropland) +
  geom_sf(aes(color = land_use))

#select the preselected cropland sites that were defined out of the ellipse
cropland_preselection2 = cropland %>%
  filter(preselec_2 == 1)
nrow(cropland_preselection2)

#the preselection contains 22 sites that are within the calculated ellipse. 
#lets narrow that down to 15 sites with a random subsampling without replacement:


cropland_subsets <- data.frame(sample(cropland_preselection2$No_,15, replace = FALSE))
colnames(cropland_subsets) <- c("Site Nr")
cropland_selection <- arrange(cropland_subsets, cropland_subsets$`Site Nr`)
print(cropland_selection)
write.table(cropland_selection, file = "cropland_selection_dispo2.txt")

#the subsampled cropland site selection will be taken into account with a join in QGIS later.