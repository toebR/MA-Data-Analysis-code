
# meta --------------------------------------------------------------------

# Tobias Stalder
# 30.04.2021
# Analysis of Feo Spectral experiment data of 3 filters
# Goal: Plot Spectra and apply uncertainties



# load libraries ----------------------------------------------------------
library(OPUSdata)
library(tidyverse)
library(here)
library(ggforce)
library(prismatic)

# 1) Analyse multi- measurements -----------------------------------


# load data ---------------------------------------------------------------

## load data ---------------------------------------------------------------

### Load FeO particle spectra --------------------------------------------

folder_path <-  paste0(here(), r"(\data\28_04_2021\28_04_2021\FeO\Blank_1_26_dirty\spectra)")
mypath = folder_path
setwd(folder_path)
# Create list of dpt files
txt_files_ls = list.files(path=mypath, pattern="*.dpt") 

# Read the files in, assuming tabular separator
txt_files_df <- lapply(txt_files_ls, function(x) {read.table(file = x, header = F, sep ="\t")})
# Combine them to 1 df
df_FeO <- do.call("cbind", lapply(txt_files_df, as.data.frame)) 

df_FeO[-c(3, 5, 7, 9, 11, 13, 15, 17, 19)] -> df_FeO

rename_cols <- function(df) {
  df %>%
    rename(Wavelength_cm = V1,
           P1 = V2,
           P2 = V2.1,
           P3 = V2.2,
           P4 = V2.3,
           P5 = V2.4,
           P6 = V2.5,
           P7 = V2.6,
           P8 = V2.7,
           P9 = V2.8,
           P10 = V2.9)
}

df_FeO %>% rename_cols() %>%
  pivot_longer(cols = -Wavelength_cm) %>%
  mutate(Area = "FeO-filter",
         group = paste0(name, "_", Area))-> df_FeO_long

### Load anodisc spectra --------------------------------------------

folder_path <-  paste0(here(), r"(\data\28_04_2021\28_04_2021\FeO\anodisc_empty\spectra)")
mypath = folder_path
setwd(folder_path)
# Create list of dpt files
txt_files_ls = list.files(path=mypath, pattern="*.dpt") 

# Read the files in, assuming tabular separator
txt_files_df <- lapply(txt_files_ls, function(x) {read.table(file = x, header = F, sep ="\t")})
# Combine them to 1 df
df_anodisc <- do.call("cbind", lapply(txt_files_df, as.data.frame)) 

df_anodisc[-c(3, 5, 7, 9, 11, 13, 15, 17, 19)] -> df_anodisc

rename_cols <- function(df) {
  df %>%
    rename(Wavelength_cm = V1,
           P1 = V2,
           P2 = V2.1,
           P3 = V2.2,
           P4 = V2.3,
           P5 = V2.4,
           P6 = V2.5,
           P7 = V2.6,
           P8 = V2.7,
           P9 = V2.8,
           P10 = V2.9)
}

df_anodisc %>% rename_cols() %>%
  pivot_longer(cols = -Wavelength_cm) %>%
  mutate(Area = "anodisc",
         group = paste0(name, "_", Area))-> df_anodisc_long




### Load clean blank particle spectra --------------------------------------------

folder_path <-  paste0(here(), r"(\data\28_04_2021\28_04_2021\FeO\Blank2_45_clean\spectra)")
mypath = folder_path
setwd(folder_path)
# Create list of dpt files
txt_files_ls = list.files(path=mypath, pattern="*.dpt") 

# Read the files in, assuming tabular separator
txt_files_df <- lapply(txt_files_ls, function(x) {read.table(file = x, header = F, sep ="\t")})
# Combine them to 1 df
df_clean <- do.call("cbind", lapply(txt_files_df, as.data.frame)) 

df_clean[-c(3, 5, 7, 9, 11, 13, 15, 17, 19)] -> df_clean

rename_cols <- function(df) {
  df %>%
    rename(Wavelength_cm = V1,
           P1 = V2,
           P2 = V2.1,
           P3 = V2.2,
           P4 = V2.3,
           P5 = V2.4,
           P6 = V2.5,
           P7 = V2.6,
           P8 = V2.7,
           P9 = V2.8,
           P10 = V2.9)
}

df_clean %>% rename_cols() %>%
  pivot_longer(cols = -Wavelength_cm) %>%
  mutate(Area = "clean filter",
         group = paste0(name, "_", Area))-> df_clean_long


## merge df together -------------------------------------------------------
data.frame(rbind(df_clean_long,df_anodisc_long,df_FeO_long)) -> df_all
ggplot(df_all) +
  geom_line(aes(x =Wavelength_cm, y = value, group = group, color = Area)) +
  scale_x_reverse()


plot_spectra <- function(df, Wavelength_cm, value, group, color) {
  ggplot(df) +
    geom_line(aes(x =Wavelength_cm, y = value, group = group, color = Area),alpha = 0.5)+
    scale_x_reverse() +
    scale_color_manual(values = c( "#0097a7", "#ff5722","#e91e63"))+
    xlab("Wavenumber [cm^-1]")+
    ylab("Absorbance [Au]")+
    theme_bw()
}  


# plot --------------------------------------------------------------------


plot_all <-plot_spectra(df = df_all, Wavelength_cm = Wavelength_cm, group = group, color = area) +
  scale_y_continuous(expand = c(0,0), limits = c(-0.01, 0.2)) + 
  ggtitle("Multi-Filter Spectral Comparison")+
  labs(subtitle = "Feo-loaded, FeO-free and Anodisc")


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

# subset 2780-2980 wavenumbers for regression model
df_all %>%
  filter(Wavelength_cm <= 2980 & Wavelength_cm >= 2780) -> df_2780_2980

plot_reg_2980_2780 <-  ggplot(df_2780_2980, aes(x =Wavelength_cm, y = value, color = Area)) +
  geom_line(aes(group = group), alpha = 0.2)+
  scale_x_reverse(expand = c(0,0)) +
  scale_color_manual(values = c( "#0097a7", "#ff5722","#e91e63"))+
  scale_fill_manual(values = c( "#0097a7", "#ff5722","#e91e63"))+
  xlab("Wavenumber [cm^-1]")+
  ylab("Absorbance [Au]")+
  theme_bw() +
  geom_smooth(method = "loess", span = 0.15,
              aes(fill = Area))+
  scale_y_continuous(expand = c(0,0), limits = c(-0.002,0.01)) + 
  ggtitle("Multi-Filter Spectral Comparison")+
  labs(subtitle = "WL 2780-2980: Loessian regression")
plot_reg_2980_2780

ggsave(plot_all, path = paste0(here(), "/plots_multi_filter"), filename = "Spectra_all.png")
ggsave(p_1480_1400, path = paste0(here(), "/plots_multi_filter"), filename = "Spectra_zoom_1400_1480.png")
ggsave(p_1760_1670, path = paste0(here(), "/plots_multi_filter"), filename = "Spectra_zoom_1760_1670.png")
ggsave(p_1800_1740, path = paste0(here(), "/plots_multi_filter"), filename = "Spectra_zoom_1740_1800.png")
ggsave(p_2980_2780, path = paste0(here(), "/plots_multi_filter"), filename = "Spectra_zooom_2780_2980.png")
ggsave(plot_reg_2980_2780, path = paste0(here(), "/plots_multi_filter"), filename = "Spectra_zooom_2780_2980_regression.png")


