
# meta --------------------------------------------------------------------

# Tobias Stalder
# 30.04.2021
# Analysis of Feo Spectral experiment data of inner-filter measurements along border area
# Goal: Plot Spectra and apply uncertainties



# load libraries ----------------------------------------------------------
library(OPUSdata)
library(tidyverse)
library(here)
library(ggforce)
library(prismatic)


# 1) Analyse single-filter measurements -----------------------------------


## load data ---------------------------------------------------------------

### Load border particle spectra --------------------------------------------

folder_path <-  paste0(here(), r"(/data/28_04_2021/28_04_2021/FeO/Blank_1_26_FeO_border_variance/spectra/dpt/area_border/)")
mypath = folder_path
setwd(folder_path)
# Create list of dpt files
txt_files_ls = list.files(path=mypath, pattern="*.dpt") 

# Read the files in, assuming tabular separator
txt_files_df <- lapply(txt_files_ls, function(x) {read.table(file = x, header = F, sep ="\t")})
# Combine them to 1 df
df_border <- do.call("cbind", lapply(txt_files_df, as.data.frame)) 

df_border[-c(3, 5, 7, 9)] -> df_border

rename_cols <- function(df) {
  df %>%
  rename(Wavelength_cm = V1,
         P1 = V2,
         P2 = V2.1,
         P3 = V2.2,
         P4 = V2.3,
         P5 = V2.4)
}

df_border %>% rename_cols() %>%
  pivot_longer(cols = -Wavelength_cm) %>%
  mutate(Area = "Border",
         group = paste0(name, "_", Area))-> df_border_long

### Load clean area particle spectra --------------------------------------------

folder_path <-  paste0(here(), r"(/data/28_04_2021/28_04_2021/FeO/Blank_1_26_FeO_border_variance/spectra/dpt/area_clean/)")
mypath = folder_path
setwd(folder_path)
# Create list of dpt files
txt_files_ls = list.files(path=mypath, pattern="*.dpt") 

txt_files_df <- lapply(txt_files_ls, function(x) {read.table(file = x, header = F, sep ="\t")})
# Combine them to 1 df
df_clean_area <- do.call("cbind", lapply(txt_files_df, as.data.frame)) 

df_clean_area[-c(3, 5, 7, 9)] -> df_clean_area


df_clean_area %>% rename_cols() %>%
  pivot_longer(cols = -Wavelength_cm) %>%
  mutate(Area = "Clean",
         group = paste0(name, "_", Area))-> df_clean_area_long

### Load FeO area particle spectra --------------------------------------------
folder_path <-  paste0(here(), r"(/data/28_04_2021/28_04_2021/FeO/Blank_1_26_FeO_border_variance/spectra/dpt/area_FeO/)")
mypath = folder_path
setwd(folder_path)
# Create list of dpt files
txt_files_ls = list.files(path=mypath, pattern="*.dpt") 

txt_files_df <- lapply(txt_files_ls, function(x) {read.table(file = x, header = F, sep ="\t")})
# Combine them to 1 df
df_FeO_area <- do.call("cbind", lapply(txt_files_df, as.data.frame)) 

df_FeO_area[-c(3, 5, 7, 9)] -> df_FeO_area


df_FeO_area %>% rename_cols() %>%
  pivot_longer(cols = -Wavelength_cm) %>%
  mutate(Area = "FeO",
         group = paste0(name, "_", Area))-> df_FeO_area_long

data.frame(rbind(df_FeO_area_long, df_border_long, df_clean_area_long)) -> df_all


# spectra plot function ---------------------------------------------------

plot_spectra <- function(df, Wavelength_cm, value, group, color) {
  ggplot(df) +
    geom_line(aes(x =Wavelength_cm, y = value, group = group, color = Area),alpha = 0.5)+
    scale_x_reverse() +
    scale_color_manual(values = c("#ff5722", "#0097a7","#e91e63"))+
    xlab("Wavenumber [cm^-1]")+
    ylab("Absorbance [Au]")+
    theme_bw()
}  

  
# plot --------------------------------------------------------------------

plot_spectra(df = df_all, Wavelength_cm = Wavelength_cm , value = value,
             group = group, color = Area) +
    scale_y_continuous(expand = c(0,0), limits = c(-0.01, 0.2)) + 
    ggtitle("Single-Filter Spectral Comparison")+
    labs(subtitle = "Clean, Border, and FeO-Areas") -> p_raw


  
plot_spectra(df = df_all, Wavelength_cm = Wavelength_cm , value = value,
               group = group, color = Area) +
    ggtitle("Single-Filter Spectral Comparison: WL 1400-1480")+
    labs(subtitle = "Clean, Border, and FeO-Areas")+
    scale_y_continuous(expand = c(0,0)) +
    facet_zoom(xlim = c(-1480, -1400), ylim = c(0.0, 0.135),
               horizontal = TRUE, zoom.size = 1) +
    theme_bw() -> p_1480_1400
  p_1480_1400

  
plot_spectra(df = df_all, Wavelength_cm = Wavelength_cm , value = value,
             group = group, color = Area) +
  ggtitle("Single-Filter Spectral Comparison: WL 1670-1760")+
  labs(subtitle = "Clean, Border, and FeO-Areas")+
  scale_y_continuous(expand = c(0,0)) +
  facet_zoom(xlim = c(-1760, -1670), ylim = c(-0.001, 0.055),
             horizontal = TRUE, zoom.size = 1) +
  theme_bw() -> p_1760_1670
p_1760_1670



plot_spectra(df = df_all, Wavelength_cm = Wavelength_cm , value = value,
             group = group, color = Area) +
  ggtitle("Single-Filter Spectral Comparison: WL 1740-1800")+
  labs(subtitle = "Clean, Border, and FeO-Areas")+
  scale_y_continuous(expand = c(0,0)) +
  facet_zoom(xlim = c(-1800, -1740), ylim = c(-0.001, 0.02),
             horizontal = TRUE, zoom.size = 1) +
  theme_bw() -> p_1800_1740
p_1800_1740


plot_spectra(df = df_all, Wavelength_cm = Wavelength_cm , value = value,
             group = group, color = Area) +
  ggtitle("Single-Filter Spectral Comparison: WL 2780-2980")+
  labs(subtitle = "Clean, Border, and FeO-Areas")+
  scale_y_continuous(expand = c(0,0)) +
  facet_zoom(xlim = c(-2980, -2780), ylim = c(-0.002, 0.01),
             horizontal = TRUE, zoom.size = 1) +
  theme_bw() -> p_2980_2780
p_2980_2780





# export plots ------------------------------------------------------------
ggsave(p_raw, path = paste0(here(), "/plots_single_filter"), filename = "Spectra_all.png")
ggsave(p_1480_1400, path = paste0(here(), "/plots_single_filter"), filename = "Spectra_zoom_1400_1480.png")
ggsave(p_1760_1670, path = paste0(here(), "/plots_single_filter"), filename = "Spectra_zoom_1760_1670.png")
ggsave(p_1800_1740, path = paste0(here(), "/plots_single_filter"), filename = "Spectra_zoom_1740_1800.png")
ggsave(p_2980_2780, path = paste0(here(), "/plots_single_filter"), filename = "Spectra_zooom_2780_2980.png")



