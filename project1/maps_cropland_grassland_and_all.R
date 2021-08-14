library(stars)
library(sf)
library(tidyverse)
library(raster)
library(progress)
library(ggthemes)
library(ggnewscale)
library(ggsn)
library(tmaptools)
library(shinyjs)

palette_explorer()
#setwd
dir = "C:\\Users\\tobia\\OneDrive\\Desktop\\MA_local\\geoprocessing\\maps\\dispo2"
setwd(dir)

#load geodata
# hillshade = raster::raster("C:\\Users\\tobia\\OneDrive\\Desktop\\MA_local\\geoprocessing\\maps\\dispo2\\shapes_new\\hillshade_CH.tif")
hillshade2 = read_stars("C:\\Users\\tobia\\OneDrive\\Desktop\\MA_local\\geoprocessing\\maps\\dispo2\\shapes_new\\hillshade_CH.tif")
CH = st_read(dsn = "C:\\Users\\tobia\\OneDrive\\Desktop\\MA_local\\geoprocessing\\maps\\dispo2\\shapes_new\\CH.shp")
landcover = st_read(dsn = "C:\\Users\\tobia\\OneDrive\\Desktop\\MA_local\\geoprocessing\\maps\\dispo2\\shapes_new\\CORINE18\\clc_2018_all.shp")
sites_all = st_read(dsn = "C:\\Users\\tobia\\OneDrive\\Desktop\\MA_local\\geoprocessing\\maps\\dispo2\\shapes_new\\sites_crop_grass_all.shp")
ellipses = st_read(dsn = "C:\\Users\\tobia\\OneDrive\\Desktop\\MA_local\\geoprocessing\\maps\\dispo2\\shapes_new\\ellipses_all.shp")

#transform landcover crs to lv03
landcover = st_transform(landcover, 21781)

#convert raster object hillshade to dataframe with fixed coordinates
hill_df = as.data.frame(hillshade2, xy = TRUE)

#filter NAs out of raster
hill_df = hill_df %>%
  filter(is.na(hillshade_CH.tif) == FALSE)

#add category to ellipse geodata
ellipses$category <- c("crop rotation ellipse", "grassland ellipse")

#rename colnames of sites
colnames(sites_all) <- c("OBJECTID","No_","municipali","canton","koord_y","koord_x","altitude","NABO land use",  
 "land_use_d","first_samp","soil_type","sub_type" , "humus_char", "F__organic", "pH","clay" ,     
 "silt","skelett", "Field18","GlobalID","GDB_ID","sewage_sit","preselecti","selection_",
 "additions","preselec_2", "geometry")
#rename site variable description
sites_all$`NABO land use` = gsub("crop rotation", "NABO crop rotation sites", sites_all$`NABO land use`)
sites_all$`NABO land use` = gsub("grassland", "NABO grassland sites", sites_all$`NABO land use`)


#goal are 3 maps: grassland and cropland sites with ellipses and then the final map with all selected sites.


#plot cropland sites (1)-------------------------------------------------------------------------------------------------------------
#prepare geodata:
  #filter for cropland sites in selection:

crop_all = sites_all %>%
  filter(`NABO land use` == "NABO crop rotation sites")
    #check if right amount of sites (15)
ifelse(nrow(crop_sel) == 15, print("OK"), print("WHOOPS"))


  #get agricutltural land cover % waterbodies
agriculture = landcover %>%
  filter(LABEL1 %in% c("Agricultural areas" , "Water bodies"))

  #???chose ellipse
ellipse_crop = ellipses %>%
  filter(category %in% "crop rotation ellipse")

#colors for elevation data (increase contrast)
pal1 = c("white", "white", "lightgrey", "grey","black", "black")
print(get_brewer_pal("Greys", n = 9, contrast = c(0, 1)))
pal2 = c("#FFFFFF", "#FFFFFF", "#D9D9D9", "#BBBBBB", "#969696",
         "#747474", "#232323", "#232323", "#232323")
#plot crop rotation sites
ggplot() +
  geom_sf(data = CH, aes(group = NAME), fill = "transparent", color = "grey", size = 1.2) +
  geom_raster(data = hill_df, aes(x = x, y = y, fill = hillshade_CH.tif), alpha = 0.5, show.legend = FALSE) +
  scale_fill_gradientn(colours = pal2) +
  new_scale_fill() +
  geom_sf(data = agriculture, aes(fill = LABEL1), color = "transparent", alpha = .7) +
  scale_fill_manual(values = c("orange", "lightblue")) +
  geom_sf(data = ellipse_crop, aes(color = category), fill = "transparent", size = 1, linetype = "dashed") +
  scale_color_manual(values = "brown") +
  new_scale_color() +
  geom_sf(data = crop_all, aes(color = `NABO land use`), color = "white", size = 4) +
  geom_sf(data = crop_all, aes(color = `NABO land use`), size = 2.5) +
  scale_color_manual(values = "brown") +
  # geom_sf_label(data = crop_all, aes(label = No_, vjust = -0.5, hjust = 0.2), size = 3.5, fontface = 2, fill = "transparent") +
  theme_map() +
  theme(legend.position = c(0.8, 0.7),
  legend.title = element_blank(),
  legend.text = element_text(size = 10)) +
  scalebar(CH, dist_unit = "km", transform = FALSE, dist = 30, location = "bottomright", st.size = 3) +
  guides(
    color = guide_legend(order = 2),
    fill = guide_legend(order = 1))-> crop_plot
crop_plot
ggsave(crop_plot, filename = "crop_selection_plot.png", width = 30, height = 20, units = "cm")


#maps (2): grassland selected sites-------------------------------------------------------------------------------
#prepare geodata:
#filter for cropland sites in selection:

grass_all = sites_all %>%
  filter(`NABO land use` == "NABO grassland sites")
#check if right amount of sites (15)
ifelse(nrow(crop_sel) == 15, print("OK"), print("WHOOPS"))


#get agricutltural land cover % waterbodies
grassland = landcover %>%
  filter(LABEL1 %in% c("Water bodies") | tos_label %in% c("Pastures", "Natural grasslands"))

#???chose ellipse
ellipse_grass = ellipses %>%
  filter(category %in% "grassland ellipse")
ellipse_grass

#set
print(get_brewer_pal("YlGn", n = 20, contrast = c(0.22,1)))

min(hill_df$hillshade_CH.tif)
#grassland map
ggplot() +
  geom_sf(data = CH, aes(group = NAME), fill = "transparent", color = "grey", size = 1.2) +
  geom_raster(data = hill_df, aes(x = x, y = y, fill = hillshade_CH.tif), alpha = 0.5, show.legend = FALSE) +
  scale_fill_gradientn(colours = pal2) +
  new_scale_fill() +
  geom_sf(data = grassland, aes(fill = tos_label), color = "transparent", alpha = .7) +
  scale_fill_manual(values = c("#1fc310", "#128507", "lightblue")) +
  geom_sf(data = ellipse_grass, aes(color = category), fill = "transparent", size = 1, linetype = "dashed") +
  scale_color_manual(values = "darkgreen") +
  new_scale_color() +
  geom_sf(data = grass_all, aes(color = `NABO land use`), color = "white", size = 4) +
  geom_sf(data = grass_all, aes(color = `NABO land use`), size = 2.5) +
  scale_color_manual(values = "darkgreen") +
  # geom_sf_label(data = grass_all, aes(label = No_, vjust = -0.5, hjust = 0.2), size = 3.5, fontface = 2, fill = "transparent") +
  theme_map() +
  theme(legend.position = c(0.8, 0.7),
        legend.title = element_blank(),
        legend.text = element_text(size = 10)) +
  scalebar(CH, dist_unit = "km", transform = FALSE, dist = 30, location = "bottomright", st.size = 3) +
  guides(
    color = guide_legend(order = 2),
    fill = guide_legend(order = 1)
  )-> grass_plot
grass_plot
ggsave(grass_plot, filename = "grass_selection_plot.png", width = 30, height = 20, units = "cm")

#map (3)---------------------------------------all NABO sites--------------------------------------
#prepare geodata:
#create new column for NABO site ID cat identifier
sites_all$glob = "NABO sites"

ggplot() +
  geom_sf(data = CH, aes(group = NAME), fill = "transparent", color = "grey", size = 1.2) +
  geom_raster(data = hill_df, aes(x = x, y = y, fill = hillshade_CH.tif), alpha = 0.5, show.legend = FALSE) +
  scale_fill_gradientn(colours = pal2) +
  new_scale_fill() +
  geom_sf(data = landcover, aes(fill = tos_label), color = "transparent", alpha = .7) +
  scale_fill_manual(values = c("orange", "#1fc310", "#128507", "lightblue")) +
  new_scale_color() +
  geom_sf(data = sites_all, aes(color = glob), color = "white", size = 4) +
  geom_sf(data = sites_all, aes(color = glob), size = 2.5) +
  scale_color_manual(values = "darkblue") +
  # geom_sf_label(data = grass_all, aes(label = No_, vjust = -0.5, hjust = 0.2), size = 3.5, fontface = 2, fill = "transparent") +
  theme_map() +
  theme(legend.position = c(0.8, 0.7),
        legend.title = element_blank(),
        legend.text = element_text(size = 10)) +
  scalebar(CH, dist_unit = "km", transform = FALSE, dist = 30, location = "bottomright", st.size = 3)
  # guides(
  #   color = guide_legend(order = 2),
  #   fill = guide_legend(order = 1)) -> all_sites_plot
all_sites_plot
ggsave(all_sites_plot, filename = "all_sites.png", width = 30, height = 20, units = "cm")


#last map: selected grassland & cropland sites
#prepare geodata:

  #filter for selected cropland & grassland sites in selection:
selec = sites_all %>%
  filter(selection_ == 1)
view(selec)



  #get agricutltural land cover % waterbodies
grassland = landcover %>%
  filter(LABEL1 %in% c("Water bodies") | tos_label %in% c("Pastures", "Natural grasslands"))

#???chose ellipse
ellipse_grass = ellipses %>%
  filter(category %in% "grassland ellipse")
ellipse_grass

#set
print(get_brewer_pal("YlGn", n = 20, contrast = c(0.22,1)))

min(hill_df$hillshade_CH.tif)
#overall selection map
ggplot() +
  geom_sf(data = CH, aes(group = NAME), fill = "transparent", color = "grey", size = 1.2) +
  geom_raster(data = hill_df, aes(x = x, y = y, fill = hillshade_CH.tif), alpha = 0.5, show.legend = FALSE) +
  scale_fill_gradientn(colours = pal2)
  new_scale_fill() +
  geom_sf(data = landcover, aes(fill = tos_label), color = "transparent", alpha = .7) +
  scale_fill_manual(values = c("orange", "#1fc310", "#128507", "lightblue")) +
  new_scale_color() +
  geom_sf(data = selec, aes(color = `NABO land use`), color = "white", size = 4) +
  geom_sf(data = selec, aes(color = `NABO land use`), size = 2.5) +
  scale_color_manual(values = c("brown", "darkgreen")) +
  geom_sf_label(data = selec, aes(label = No_, vjust = -0.4, hjust = 0.1), size = 3.5, fontface = 2, fill = "transparent") +
  theme_map() +
  theme(legend.position = c(0.8, 0.7),
        legend.title = element_blank(),
        legend.text = element_text(size = 10)) +
  scalebar(CH, dist_unit = "km", transform = FALSE, dist = 30, location = "bottomright", st.size = 3
           )  -> selec_plot
  # guides(
  #   color = guide_legend(order = 2),
  #   fill = guide_legend(order = 1)
selec_plot
ggsave(selec_plot, filename = "selected_plot.png", width = 30, height = 20, units = "cm")
