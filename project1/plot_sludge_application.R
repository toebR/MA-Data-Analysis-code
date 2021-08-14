library(tidyverse)
library(readxl)
library(RColorBrewer)

sewdatlong <- data.frame(read_excel("C:/Users/tobia/OneDrive/Desktop/MA_local/geoprocessing/plots/sewage_sludge/sewage_sludge_dat_long.xlsx"))
sewdatwide <- data.frame(read_excel("C:/Users/tobia/OneDrive/Desktop/MA_local/geoprocessing/plots/sewage_sludge/sewage_sludge_dat_wide.xlsx"))

sewdatlong$GDB_ID<- as.character(sewdatlong$GDB_ID)

# sew_plot <- ggplot(sewdatlong) +
#   geom_bar(aes(x = sewdatlong$Time_Period, y = sewdatlong$Abs_appl, fill = sewdatlong$GDB_ID),stat = "identity") +
#   xlab("Time Period") +
#   ylab("Sewage Sludge Applications")
 
tile_dat <- sewdatlong
tile_dat$Abs_appl <- as.character(tile_dat$Abs_appl)

sew_tile_plot <- ggplot(tile_dat) +
  geom_tile(aes(x = tile_dat$Time_Period, y = tile_dat$GDB_ID, fill = tile_dat$Abs_appl),stat = "identity") +
  scale_fill_manual(name = "Sewage Sludge \nApplications", values = c("#fee6ce", "#fdae6b", "#e6550d")) +
  geom_text(aes(x = tile_dat$Time_Period, y = tile_dat$GDB_ID, label = tile_dat$Abs_appl)) +
  xlab("Time Period") +
  ylab("NABO Cropland Sites\n with Sewage Sludge Applications") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
sew_tile_plot


file_name = paste("Application_plot.png")
png(file_name, width = 16, height = 14, units = "cm", res = 300)
print(last_plot())
dev.off()

#make the tile plot more beatiful!!
  

  
