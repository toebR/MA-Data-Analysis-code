
# meta --------------------------------------------------------------------

# Tobias Stalder
# 12.05.2021
# Calculate deviations from piek-high particles with the 3 different spectral areas 2780_2980wavenumber
# Goal: .dpt files to feed back to opus to perform a library search and see if it differs, and plots of the differences!


#as correction we use the mean + STDEV model of the different filter comparisons (multi-filter measurement data)
# we cannot use loessian regression because the wavenumber would not be the same in the model and in the piek-high .dpt!



# load libraries ----------------------------------------------------------
library(OPUSdata)
library(tidyverse)
library(here)
library(ggforce)
library(prismatic)
library(Cairo)

options(scipen = 999)


# load data ---------------------------------------------------------------
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


## plot function -----------------------------------------------------------


plot_spectra <- function(df, Wavelength_cm, value, group, color) {
  ggplot(df) +
    geom_line(aes(x =Wavelength_cm, y = value, group = group, color = Area),alpha = 0.5)+
    scale_x_reverse() +
    scale_color_manual(values = c( "#0097a7", "#ff5722","#e91e63"))+
    xlab("Wavenumber [cm^-1]")+
    ylab("Absorbance [Au]")+
    theme_bw()
}  


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


# calculate mean + standart deviation ----------------------------------------------------
df_2780_2980 %>%
  group_by(Wavelength_cm, Area) %>%
  summarise(mean_areas = mean(value),
            stdev_areas = sd(value)) -> df_2780_2980_av



#split according to area to get 3 indiviudal dfs
ano_reg <- df_2780_2980_av %>%
  filter(Area == "anodisc") %>%
  rename(mean_ano = mean_areas,
         stdev_ano = stdev_areas) %>%
  mutate(mean_ano = ifelse(mean_ano < 0, 0, mean_ano))
clean_reg <- df_2780_2980_av %>%
  filter(Area == "clean filter") %>%
  rename(mean_clean = mean_areas,
         stdev_clean = stdev_areas) %>%
  mutate(mean_clean = ifelse(mean_clean < 0, 0, mean_clean))
FeO_reg <- df_2780_2980_av %>%
  filter(Area == "FeO-filter")  %>%
  rename(mean_FeO = mean_areas,
         stdev_FeO = stdev_areas) %>%
  mutate(mean_FeO = ifelse(mean_FeO < 0, 0, mean_FeO))


# load individual data on piek high polymers ------------------------------
Att2_PP <- read.table(file = r"(C:\Users\tobia\OneDrive\Desktop\MA2_local\experiment_results\4_experiment_FeO\data\12_05_2021\Att2.0_000016_PP_768.dpt)", header = F, sep ="\t")
Att2_PP %>%
  rename(Wavelength_cm = V1,
         PP = V2) %>%
  filter(Wavelength_cm <= 2980 & Wavelength_cm >= 2780) -> Att2_PP

Att2_CoPolyamide <- read.table(file = r"(C:\Users\tobia\OneDrive\Desktop\MA2_local\experiment_results\4_experiment_FeO\data\12_05_2021\Att2.0_000594_Copolyamide_682.dpt)", header = F, sep ="\t")
Att2_CoPolyamide %>%
  rename(Wavelength_cm = V1,
         CoPA = V2) %>%
  filter(Wavelength_cm <= 2980 & Wavelength_cm >= 2780) -> Att2_CoPolyamide

Blank2_PE_LD <- read.table(file = r"(C:\Users\tobia\OneDrive\Desktop\MA2_local\experiment_results\4_experiment_FeO\data\12_05_2021\Blank_2.0_000150_PE-LD_670.dpt)", header = F, sep ="\t")
Blank2_PE_LD %>%
  rename(Wavelength_cm = V1,
         PE_LD = V2) %>%
  filter(Wavelength_cm <= 2980 & Wavelength_cm >= 2780) -> Blank2_PE_LD

Blank2_Pulyuerathane <- read.table(file = r"(C:\Users\tobia\OneDrive\Desktop\MA2_local\experiment_results\4_experiment_FeO\data\12_05_2021\Blank_2.0_000198_pulyuerathane polyermer dispersion_666.dpt)", header = F, sep ="\t")
Blank2_Pulyuerathane %>%
  rename(Wavelength_cm = V1,
         Pulyurethane = V2) %>%
  filter(Wavelength_cm <= 2980 & Wavelength_cm >= 2780) -> Blank2_Pulyuerathane

DAL_PP <- read.table(file = r"(C:\Users\tobia\OneDrive\Desktop\MA2_local\experiment_results\4_experiment_FeO\data\12_05_2021\DAL_45_000883_PP_654.dpt)", header = F, sep ="\t")
DAL_PP %>%
  rename(Wavelength_cm = V1,
         DAL_PP = V2) %>%
  filter(Wavelength_cm <= 2980 & Wavelength_cm >= 2780) -> DAL_PP


# testplot
# ggplot(Blank2_PE_LD, aes(x = Wavelength_cm, y = P1))+
#   geom_line() + scale_x_reverse()

# for all piek highs, substract regression model for each BG area ---------
#workflow: merge dfs from the piek polymers with the area dfs and calculate the parameters


# Att2_PP -----------------------------------------------------------------



left_join(Att2_PP, ano_reg, by = c("Wavelength_cm" = "Wavelength_cm")) %>%
  left_join(clean_reg, by = c("Wavelength_cm" = "Wavelength_cm")) %>%
  left_join(FeO_reg, by = c("Wavelength_cm" = "Wavelength_cm")) %>%
  select(-c(Area.x, Area.y, Area)) %>%
  mutate(PP_ano = PP - mean_ano,
         PP_ano_upper = PP_ano + stdev_ano,
         PP_ano_lower = PP_ano - stdev_ano,
         PP_clean = PP - mean_clean,
         PP_clean_upper = PP_clean + stdev_clean,
         PP_clean_lower = PP_clean - stdev_clean,
         PP_FeO = PP - mean_FeO,
         PP_FeO_upper = PP_FeO + stdev_FeO,
         PP_FeO_lower = PP_FeO - stdev_FeO)  -> Att2_PP_corr

#make subsets and convert to long format

Att2_PP_ano <- Att2_PP_corr %>% select(Wavelength_cm,PP, PP_ano,PP_ano_upper,PP_ano_lower) %>%
  pivot_longer(cols = -c(Wavelength_cm, PP_ano_upper, PP_ano_lower)) %>% rename(Correction = name)
Att2_PP_clean  <- Att2_PP_corr %>% select(Wavelength_cm,PP, PP_clean,PP_clean_upper,PP_clean_lower) %>%
  pivot_longer(cols = -c(Wavelength_cm, PP_clean_upper, PP_clean_lower)) %>% rename(Correction = name)
Att2_PP_FeO <- Att2_PP_corr %>% select(Wavelength_cm,PP, PP_FeO,PP_FeO_upper,PP_FeO_lower) %>%
  pivot_longer(cols = -c(Wavelength_cm, PP_FeO_upper, PP_FeO_lower)) %>% rename(Correction = name)

#plot
prismatic::color(c( "#0097a7", "#ff5722","#e91e63"))

ggplot(Att2_PP_ano) +
  geom_line(aes(x = Wavelength_cm, y = PP_ano_upper), color = "#0097A7FF", linetype = "dotted") +
  geom_line(aes(x = Wavelength_cm, y = PP_ano_lower), color = "#0097A7FF", linetype = "dotted") +
  geom_line(aes(x = Wavelength_cm, y = value, color = Correction), show.legend = FALSE) +
  xlab("Wavenumber [cm^-1]")+
  ylab("Absorbance [Au]")+
  scale_x_reverse()+
  scale_color_manual(values = c("black", "#0097A7FF" ))+
  theme_bw() +
  ggsave(filename = paste0(here(),"/plots_area_correction/Att2_PP_anodisc.png"), width = 12, height = 6, type = "cairo-png", dpi = 300)


ggplot(Att2_PP_clean) +
  geom_line(aes(x = Wavelength_cm, y = PP_clean_upper), color = "#FF5722FF", linetype = "dotted") +
  geom_line(aes(x = Wavelength_cm, y = PP_clean_lower), color = "#FF5722FF", linetype = "dotted") +
  geom_line(aes(x = Wavelength_cm, y = value, color = Correction), show.legend = FALSE) +
  scale_x_reverse()+
  xlab("Wavenumber [cm^-1]")+
  ylab("Absorbance [Au]")+
  scale_color_manual(values = c("black", "#FF5722FF" ))+
  theme_bw() +
  ggsave(filename = paste0(here(),"/plots_area_correction/Att2_PP_clean.png"), width = 12, height = 6, type = "cairo-png", dpi = 300)

ggplot(Att2_PP_FeO) +
  geom_line(aes(x = Wavelength_cm, y = PP_FeO_upper), color = "#E91E63FF", linetype = "dotted") +
  geom_line(aes(x = Wavelength_cm, y = PP_FeO_lower), color = "#E91E63FF", linetype = "dotted") +
  geom_line(aes(x = Wavelength_cm, y = value, color = Correction), show.legend = FALSE) +
  scale_x_reverse()+
  xlab("Wavenumber [cm^-1]")+
  ylab("Absorbance [Au]")+
  scale_color_manual(values = c("black", "#E91E63FF" ))+
  theme_bw() +
  ggsave(filename = paste0(here(),"/plots_area_correction/Att2_PP_FeO.png"), width = 12, height = 6, type = "cairo-png", dpi = 300)

#export textfile with mean corrected spectra, call columns V1 and V2 again.
Att2_PP_corr_export_ano <- Att2_PP_corr %>%
  select(Wavelength_cm, PP_ano) %>%
  rename(V1 = Wavelength_cm, V2 = PP_ano)
# TO DO BEFORE EXPORT: substitute in-read file .dpt values of the selected wavebands with the corrected waveband values according to area spectral correction!!
Att2_PP_orig <- read.table(file = r"(C:\Users\tobia\OneDrive\Desktop\MA2_local\experiment_results\4_experiment_FeO\data\12_05_2021\Att2.0_000016_PP_768.dpt)", header = F, sep ="\t")
left_join(Att2_PP_orig, Att2_PP_corr_export_ano, by = "V1") %>%
  mutate(V3 =ifelse(is.na(V2.y), V2.x, V2.y)) %>%
  select(V1, V3) -> Att2_PP_corr_export_ano
names(Att2_PP_corr_export_ano) <- NULL #drop header
write.table(Att2_PP_corr_export_ano, sep ="\t",
            file = paste0(here(),"Att2_PP_corr_ano.txt"), row.names = FALSE)

Att2_PP_corr_export_clean <- Att2_PP_corr %>%
  select(Wavelength_cm, PP_clean) %>%
  rename(V1 = Wavelength_cm, V2 = PP_clean)
# TO DO BEFORE EXPORT: substitute in-read file .dpt values of the selected wavebands with the corrected waveband values according to area spectral correction!!
left_join(Att2_PP_orig, Att2_PP_corr_export_clean, by = "V1") %>%
  mutate(V3 =ifelse(is.na(V2.y), V2.x, V2.y))  %>%
  select(V1, V3) -> Att2_PP_corr_export_clean
names(Att2_PP_corr_export_clean) <- NULL #drop header
write.table(Att2_PP_corr_export_clean, sep ="\t",
            file = paste0(here(),"Att2_PP_corr_clean.txt"), row.names = FALSE)

Att2_PP_corr_export_FeO <- Att2_PP_corr %>%
  select(Wavelength_cm, PP_FeO) %>%
  rename(V1 = Wavelength_cm, V2 = PP_FeO)
# TO DO BEFORE EXPORT: substitute in-read file .dpt values of the selected wavebands with the corrected waveband values according to area spectral correction!!
left_join(Att2_PP_orig, Att2_PP_corr_export_FeO, by = "V1") %>%
  mutate(V3 =ifelse(is.na(V2.y), V2.x, V2.y)) %>%
  select(V1, V3) -> Att2_PP_corr_export_FeO
names(Att2_PP_corr_export_FeO) <- NULL #drop header
write.table(Att2_PP_corr_export_FeO, sep ="\t",
            file = paste0(here(),"Att2_PP_corr_FeO.txt"), row.names = FALSE)



# Att2_CoPolyamide --------------------------------------------------------
left_join(Att2_CoPolyamide, ano_reg, by = c("Wavelength_cm" = "Wavelength_cm")) %>%
  left_join(clean_reg, by = c("Wavelength_cm" = "Wavelength_cm")) %>%
  left_join(FeO_reg, by = c("Wavelength_cm" = "Wavelength_cm")) %>%
  select(-c(Area.x, Area.y, Area)) %>%
  mutate(CoPo_ano = CoPA - mean_ano,
         CoPo_ano_upper = CoPo_ano + stdev_ano,
         CoPo_ano_lower = CoPo_ano - stdev_ano,
         CoPo_clean = CoPA - mean_clean,
         CoPo_clean_upper = CoPo_clean + stdev_clean,
         CoPo_clean_lower = CoPo_clean - stdev_clean,
         CoPo_FeO = CoPA - mean_FeO,
         CoPo_FeO_upper = CoPo_FeO + stdev_FeO,
         CoPo_FeO_lower = CoPo_FeO - stdev_FeO)  -> Att2_CoPolyamide_corr

#make subsets and convert to long format

Att2_CoPA_ano <- Att2_CoPolyamide_corr %>% select(Wavelength_cm,CoPA, CoPo_ano,CoPo_ano_upper,CoPo_ano_lower) %>%
  pivot_longer(cols = -c(Wavelength_cm, CoPo_ano_upper, CoPo_ano_lower)) %>% rename(Correction = name)

Att2_CoPA_clean  <- Att2_CoPolyamide_corr %>% select(Wavelength_cm,CoPA, CoPo_clean,CoPo_clean_upper,CoPo_clean_lower) %>%
  pivot_longer(cols = -c(Wavelength_cm, CoPo_clean_upper, CoPo_clean_lower)) %>% rename(Correction = name)

Att2_CoPA_FeO <- Att2_CoPolyamide_corr %>% select(Wavelength_cm,CoPA, CoPo_FeO,CoPo_FeO_upper,CoPo_FeO_lower) %>%
  pivot_longer(cols = -c(Wavelength_cm, CoPo_FeO_upper, CoPo_FeO_lower)) %>% rename(Correction = name)

#plot
prismatic::color(c( "#0097a7", "#ff5722","#e91e63"))

ggplot(Att2_CoPA_ano) +
  geom_line(aes(x = Wavelength_cm, y = CoPo_ano_upper), color = "#0097A7FF", linetype = "dotted") +
  geom_line(aes(x = Wavelength_cm, y = CoPo_ano_lower), color = "#0097A7FF", linetype = "dotted") +
  geom_line(aes(x = Wavelength_cm, y = value, color = Correction), show.legend = FALSE) +
  scale_x_reverse()+
  xlab("Wavenumber [cm^-1]")+
  ylab("Absorbance [Au]")+
  scale_color_manual(values = c("black", "#0097A7FF" ))+
  theme_bw() +
  ggsave(filename = paste0(here(),"/plots_area_correction/Att2_CoPA_anodisc.png"), width = 12, height = 6, type = "cairo-png", dpi = 300)

ggplot(Att2_CoPA_clean) +
  geom_line(aes(x = Wavelength_cm, y = CoPo_clean_upper), color = "#FF5722FF", linetype = "dotted") +
  geom_line(aes(x = Wavelength_cm, y = CoPo_clean_lower), color = "#FF5722FF", linetype = "dotted") +
  geom_line(aes(x = Wavelength_cm, y = value, color = Correction), show.legend = FALSE) +
  scale_x_reverse()+
  xlab("Wavenumber [cm^-1]")+
  ylab("Absorbance [Au]")+
  scale_color_manual(values = c("black", "#FF5722FF" ))+
  theme_bw() +
  ggsave(filename = paste0(here(),"/plots_area_correction/Att2_CoPA_clean.png"), width = 12, height = 6, type = "cairo-png", dpi = 300)

ggplot(Att2_CoPA_FeO) +
  geom_line(aes(x = Wavelength_cm, y = CoPo_FeO_upper), color = "#E91E63FF", linetype = "dotted") +
  geom_line(aes(x = Wavelength_cm, y = CoPo_FeO_lower), color = "#E91E63FF", linetype = "dotted") +
  geom_line(aes(x = Wavelength_cm, y = value, color = Correction), show.legend = FALSE) +
  scale_x_reverse()+
  xlab("Wavenumber [cm^-1]")+
  ylab("Absorbance [Au]")+
  scale_color_manual(values = c("black", "#E91E63FF" ))+
  theme_bw() +
  ggsave(filename = paste0(here(),"/plots_area_correction/Att2_CoPA_FeO.png"), width = 12, height = 6, type = "cairo-png", dpi = 300)

#export textfile with mean corrected spectra, call columns V1 and V2 again.
Att2_CoPA_corr_export_ano <- Att2_CoPolyamide_corr %>%
  select(Wavelength_cm, CoPo_ano) %>%
  rename(V1 = Wavelength_cm, V2 = CoPo_ano)
# TO DO BEFORE EXPORT: substitute in-read file .dpt values of the selected wavebands with the corrected waveband values according to area spectral correction!!
Att2_CoPA_orig <- read.table(file = r"(C:\Users\tobia\OneDrive\Desktop\MA2_local\experiment_results\4_experiment_FeO\data\12_05_2021\Att2.0_000594_Copolyamide_682.dpt)", header = F, sep ="\t")
left_join(Att2_CoPA_orig, Att2_CoPA_corr_export_ano, by = "V1") %>%
  mutate(V3 =ifelse(is.na(V2.y), V2.x, V2.y)) %>%
  select(V1, V3) -> Att2_CoPA_corr_export_ano
names(Att2_CoPA_corr_export_ano) <- NULL #drop header
write.table(Att2_CoPA_corr_export_ano, sep ="\t",
            file = paste0(here(),"Att2_CoPA_corr_ano.txt"), row.names = FALSE)

Att2_CoPA_corr_export_clean <- Att2_CoPolyamide_corr %>%
  select(Wavelength_cm, CoPo_clean) %>%
  rename(V1 = Wavelength_cm, V2 = CoPo_clean)
# TO DO BEFORE EXPORT: substitute in-read file .dpt values of the selected wavebands with the corrected waveband values according to area spectral correction!!
left_join(Att2_CoPA_orig, Att2_CoPA_corr_export_clean, by = "V1") %>%
  mutate(V3 =ifelse(is.na(V2.y), V2.x, V2.y)) %>%
select(V1, V3) -> Att2_CoPA_corr_export_clean
names(Att2_CoPA_corr_export_clean) <- NULL #drop header
write.table(Att2_CoPA_corr_export_clean, sep ="\t",
            file = paste0(here(),"Att2_CoPA_corr_clean.txt"), row.names = FALSE)

Att2_CoPA_corr_export_FeO <- Att2_CoPolyamide_corr %>%
  select(Wavelength_cm, CoPo_FeO) %>%
  rename(V1 = Wavelength_cm, V2 = CoPo_FeO)
# TO DO BEFORE EXPORT: substitute in-read file .dpt values of the selected wavebands with the corrected waveband values according to area spectral correction!!
left_join(Att2_CoPA_orig, Att2_CoPA_corr_export_FeO, by = "V1") %>%
  mutate(V3 =ifelse(is.na(V2.y), V2.x, V2.y)) %>%
  select(V1, V3) -> Att2_CoPA_corr_export_FeO
names(Att2_CoPA_corr_export_FeO) <- NULL #drop header
write.table(Att2_CoPA_corr_export_FeO, sep ="\t",
            file = paste0(here(),"Att2_CoPA_corr_FeO.txt"), row.names = FALSE)

# Blank2_PE_LD ------------------------------------------------------------


left_join(Blank2_PE_LD, ano_reg, by = c("Wavelength_cm" = "Wavelength_cm")) %>%
  left_join(clean_reg, by = c("Wavelength_cm" = "Wavelength_cm")) %>%
  left_join(FeO_reg, by = c("Wavelength_cm" = "Wavelength_cm")) %>%
  select(-c(Area.x, Area.y, Area)) %>%
  mutate(PE_LD_ano = PE_LD - mean_ano,
         PE_LD_ano_upper = PE_LD_ano + stdev_ano,
         PE_LD_ano_lower = PE_LD_ano - stdev_ano,
         PE_LD_clean = PE_LD - mean_clean,
         PE_LD_clean_upper = PE_LD_clean + stdev_clean,
         PE_LD_clean_lower = PE_LD_clean - stdev_clean,
         PE_LD_FeO = PE_LD - mean_FeO,
         PE_LD_FeO_upper = PE_LD_FeO + stdev_FeO,
         PE_LD_FeO_lower = PE_LD_FeO - stdev_FeO)  -> Blank2_PE_LD_corr

#make subsets and convert to long format

Blank2_PE_LD_ano <- Blank2_PE_LD_corr %>% select(Wavelength_cm,PE_LD, PE_LD_ano,PE_LD_ano_upper,PE_LD_ano_lower) %>%
  pivot_longer(cols = -c(Wavelength_cm, PE_LD_ano_upper, PE_LD_ano_lower)) %>% rename(Correction = name)
Blank2_PE_LD_clean  <- Blank2_PE_LD_corr %>% select(Wavelength_cm,PE_LD, PE_LD_clean,PE_LD_clean_upper,PE_LD_clean_lower) %>%
  pivot_longer(cols = -c(Wavelength_cm, PE_LD_clean_upper, PE_LD_clean_lower)) %>% rename(Correction = name)
Blank2_PE_LD_FeO <- Blank2_PE_LD_corr %>% select(Wavelength_cm,PE_LD,PE_LD_FeO,PE_LD_FeO_upper,PE_LD_FeO_lower) %>%
  pivot_longer(cols = -c(Wavelength_cm, PE_LD_FeO_upper, PE_LD_FeO_lower)) %>% rename(Correction = name)

#plot
prismatic::color(c( "#0097a7", "#ff5722","#e91e63"))

ggplot(Blank2_PE_LD_ano) +
  geom_line(aes(x = Wavelength_cm, y = PE_LD_ano_upper), color = "#0097A7FF", linetype = "dotted", size = 0.2)+
  geom_line(aes(x = Wavelength_cm, y = PE_LD_ano_lower), color = "#0097A7FF", linetype = "dotted", size = 0.2) +
  geom_line(aes(x = Wavelength_cm, y = value, color = Correction), show.legend = FALSE, size = 0.5) +
  scale_x_reverse()+
  xlab("Wavenumber [cm^-1]")+
  ylab("Absorbance [Au]")+
  scale_color_manual(values = c("black", "#0097A7FF" ))+
  theme_bw() +
  ggsave(filename = paste0(here(),"/plots_area_correction/Blank2_PE_LD_anodisc.svg"), width =3.5, height = 3.5, dpi = 300)

ggplot(Blank2_PE_LD_clean) +
  geom_line(aes(x = Wavelength_cm, y = PE_LD_clean_upper), color = "#FF5722FF", linetype = "dotted") +
  geom_line(aes(x = Wavelength_cm, y = PE_LD_clean_lower), color = "#FF5722FF", linetype = "dotted") +
  geom_line(aes(x = Wavelength_cm, y = value, color = Correction), show.legend = FALSE) +
  scale_x_reverse()+
  xlab("Wavenumber [cm^-1]")+
  ylab("Absorbance [Au]")+
  scale_color_manual(values = c("black", "#FF5722FF" ))+
  theme_bw()+
  ggsave(filename = paste0(here(),"/plots_area_correction/Blank2_PE_LD_clean.svg"), width = 3.5, height = 3.5, dpi = 300)

ggplot(Blank2_PE_LD_FeO) +
  geom_line(aes(x = Wavelength_cm, y = PE_LD_FeO_upper), color = "#E91E63FF", linetype = "dotted") +
  geom_line(aes(x = Wavelength_cm, y = PE_LD_FeO_lower), color = "#E91E63FF", linetype = "dotted") +
  geom_line(aes(x = Wavelength_cm, y = value, color = Correction), show.legend = FALSE) +
  scale_x_reverse()+
  xlab("Wavenumber [cm^-1]")+
  ylab("Absorbance [Au]")+
  scale_color_manual(values = c("black", "#E91E63FF" ))+
  theme_bw() +
  ggsave(filename = paste0(here(),"/plots_area_correction/Blank2_PE_LD_FeO.svg"), width = 3.5, height = 3.5, dpi = 300)

#export textfile with mean corrected spectra, call columns V1 and V2 again.
Blank2_PE_LD_corr_export_ano <- Blank2_PE_LD_corr %>%
  select(Wavelength_cm, PE_LD_ano) %>%
  rename(V1 = Wavelength_cm, V2 = PE_LD_ano)
# TO DO BEFORE EXPORT: substitute in-read file .dpt values of the selected wavebands with the corrected waveband values according to area spectral correction!!
Blank2_PE_LD_orig <- read.table(file = r"(C:\Users\tobia\OneDrive\Desktop\MA2_local\experiment_results\4_experiment_FeO\data\12_05_2021\Blank_2.0_000150_PE-LD_670.dpt)", header = F, sep ="\t")
left_join(Blank2_PE_LD_orig, Blank2_PE_LD_corr_export_ano, by = "V1") %>%
  mutate(V3 =ifelse(is.na(V2.y), V2.x, V2.y)) %>%
  select(V1, V3) -> Blank2_PE_LD_corr_export_ano
names(Blank2_PE_LD_corr_export_ano) <- NULL #drop header
write.table(Blank2_PE_LD_corr_export_ano, sep ="\t",
            file = paste0(here(),"Blank2_PE_LD_corr_ano.txt"), row.names = FALSE)

Blank2_PE_LD_corr_export_clean <- Blank2_PE_LD_corr %>%
  select(Wavelength_cm, PE_LD_clean) %>%
  rename(V1 = Wavelength_cm, V2 = PE_LD_clean)
# TO DO BEFORE EXPORT: substitute in-read file .dpt values of the selected wavebands with the corrected waveband values according to area spectral correction!!
left_join(Blank2_PE_LD_orig, Blank2_PE_LD_corr_export_clean, by = "V1") %>%
  mutate(V3 =ifelse(is.na(V2.y), V2.x, V2.y))  %>%
  select(V1, V3) -> Blank2_PE_LD_corr_export_clean
names(Blank2_PE_LD_corr_export_clean) <- NULL #drop header
write.table(Blank2_PE_LD_corr_export_clean, sep ="\t",
            file = paste0(here(),"Blank2_PE_LD_corr_clean.txt"), row.names = FALSE)

Blank2_PE_LD_corr_export_FeO <- Blank2_PE_LD_corr %>%
  select(Wavelength_cm, PE_LD_FeO) %>%
  rename(V1 = Wavelength_cm, V2 = PE_LD_FeO)
# TO DO BEFORE EXPORT: substitute in-read file .dpt values of the selected wavebands with the corrected waveband values according to area spectral correction!!
left_join(Blank2_PE_LD_orig, Blank2_PE_LD_corr_export_FeO, by = "V1") %>%
  mutate(V3 =ifelse(is.na(V2.y), V2.x, V2.y)) %>%
  select(V1, V3) -> Blank2_PE_LD_corr_export_FeO
names(Blank2_PE_LD_corr_export_FeO) <- NULL #drop header
write.table(Blank2_PE_LD_corr_export_FeO, sep ="\t",
            file = paste0(here(),"Blank2_PE_LD_corr_FeO.txt"), row.names = FALSE)


# Blank2_Pulyuerathane ----------------------------------------------------
left_join(Blank2_Pulyuerathane, ano_reg, by = c("Wavelength_cm" = "Wavelength_cm")) %>%
  left_join(clean_reg, by = c("Wavelength_cm" = "Wavelength_cm")) %>%
  left_join(FeO_reg, by = c("Wavelength_cm" = "Wavelength_cm")) %>%
  select(-c(Area.x, Area.y, Area)) %>%
  mutate(PuEr_ano = Pulyurethane - mean_ano,
         PuEr_ano_upper = PuEr_ano + stdev_ano,
         PuEr_ano_lower = PuEr_ano - stdev_ano,
         PuEr_clean = Pulyurethane - mean_clean,
         PuEr_clean_upper = PuEr_clean + stdev_clean,
         PuEr_clean_lower = PuEr_clean - stdev_clean,
         PuEr_FeO = Pulyurethane - mean_FeO,
         PuEr_FeO_upper = PuEr_FeO + stdev_FeO,
         PuEr_FeO_lower = PuEr_FeO - stdev_FeO)  -> Blank2_PuEr_corr

#make subsets and convert to long format

Blank2_PuEr_ano <- Blank2_PuEr_corr %>% select(Wavelength_cm,Pulyurethane, PuEr_ano,PuEr_ano_upper,PuEr_ano_lower) %>%
  pivot_longer(cols = -c(Wavelength_cm, PuEr_ano_upper, PuEr_ano_lower)) %>% rename(Correction = name)
Blank2_PuEr_clean  <- Blank2_PuEr_corr %>% select(Wavelength_cm,Pulyurethane, PuEr_clean,PuEr_clean_upper,PuEr_clean_lower) %>%
  pivot_longer(cols = -c(Wavelength_cm, PuEr_clean_upper, PuEr_clean_lower)) %>% rename(Correction = name)
Blank2_PuEr_FeO <- Blank2_PuEr_corr %>% select(Wavelength_cm,Pulyurethane,PuEr_FeO,PuEr_FeO_upper,PuEr_FeO_lower) %>%
  pivot_longer(cols = -c(Wavelength_cm, PuEr_FeO_upper, PuEr_FeO_lower)) %>% rename(Correction = name)

#plot
prismatic::color(c( "#0097a7", "#ff5722","#e91e63"))

ggplot(Blank2_PuEr_ano) +
  geom_line(aes(x = Wavelength_cm, y = PuEr_ano_upper), color = "#0097A7FF", linetype = "dotted") +
  geom_line(aes(x = Wavelength_cm, y = PuEr_ano_lower), color = "#0097A7FF", linetype = "dotted") +
  geom_line(aes(x = Wavelength_cm, y = value, color = Correction), show.legend = FALSE) +
  scale_x_reverse()+
  xlab("Wavenumber [cm^-1]")+
  ylab("Absorbance [Au]")+
  scale_color_manual(values = c("#0097A7FF", "black"))+
  theme_bw()+
  ggsave(filename = paste0(here(),"/plots_area_correction/Blank2_puer_anodisc.png"), width = 12, height = 6, type = "cairo-png", dpi = 300)

ggplot(Blank2_PuEr_clean) +
  geom_line(aes(x = Wavelength_cm, y = PuEr_clean_upper), color = "#FF5722FF", linetype = "dotted") +
  geom_line(aes(x = Wavelength_cm, y = PuEr_clean_lower), color = "#FF5722FF", linetype = "dotted") +
  geom_line(aes(x = Wavelength_cm, y = value, color = Correction), show.legend = FALSE) +
  scale_x_reverse()+
  xlab("Wavenumber [cm^-1]")+
  ylab("Absorbance [Au]")+
  scale_color_manual(values = c("#FF5722FF", "black" ))+
  theme_bw()+
  ggsave(filename = paste0(here(),"/plots_area_correction/Blank2_puer_clean.png"), width = 12, height = 6, type = "cairo-png", dpi = 300)

ggplot(Blank2_PuEr_FeO) +
  geom_line(aes(x = Wavelength_cm, y = PuEr_FeO_upper), color = "#E91E63FF", linetype = "dotted") +
  geom_line(aes(x = Wavelength_cm, y = PuEr_FeO_lower), color = "#E91E63FF", linetype = "dotted") +
  geom_line(aes(x = Wavelength_cm, y = value, color = Correction), show.legend = FALSE) +
  scale_x_reverse()+
  xlab("Wavenumber [cm^-1]")+
  ylab("Absorbance [Au]")+
  scale_color_manual(values = c("#E91E63FF", "black" ))+
  theme_bw()+
  ggsave(filename = paste0(here(),"/plots_area_correction/Blank2_puer_FeO.png"), width = 12, height = 6, type = "cairo-png", dpi = 300)

#export textfile with mean corrected spectra, call columns V1 and V2 again.
Blank2_PuEr_corr_export_ano <- Blank2_PuEr_corr %>%
  select(Wavelength_cm, PuEr_ano) %>%
  rename(V1 = Wavelength_cm, V2 = PuEr_ano)
# TO DO BEFORE EXPORT: substitute in-read file .dpt values of the selected wavebands with the corrected waveband values according to area spectral correction!!
Blank2_PuEr_orig <-  read.table(file = r"(C:\Users\tobia\OneDrive\Desktop\MA2_local\experiment_results\4_experiment_FeO\data\12_05_2021\Blank_2.0_000198_pulyuerathane polyermer dispersion_666.dpt)", header = F, sep ="\t")
left_join(Blank2_PuEr_orig, Blank2_PuEr_corr_export_ano, by = "V1") %>%
  mutate(V3 =ifelse(is.na(V2.y), V2.x, V2.y)) %>%
  select(V1, V3) -> Blank2_PuEr_corr_export_ano
names(Blank2_PuEr_corr_export_ano) <- NULL #drop header
write.table(Blank2_PuEr_corr_export_ano, sep ="\t",
            file = paste0(here(),"Blank2_PuEr_corr_ano.txt"), row.names = FALSE)

Blank2_PuEr_corr_export_clean <- Blank2_PuEr_corr %>%
  select(Wavelength_cm, PuEr_clean) %>%
  rename(V1 = Wavelength_cm, V2 = PuEr_clean)
# TO DO BEFORE EXPORT: substitute in-read file .dpt values of the selected wavebands with the corrected waveband values according to area spectral correction!!
left_join(Blank2_PuEr_orig, Blank2_PuEr_corr_export_clean, by = "V1") %>%
  mutate(V3 =ifelse(is.na(V2.y), V2.x, V2.y))  %>%
  select(V1, V3) -> Blank2_PuEr_corr_export_clean
names(Blank2_PuEr_corr_export_clean) <- NULL #drop header
write.table(Blank2_PuEr_corr_export_clean, sep ="\t",
            file = paste0(here(),"Blank2_PuEr_corr_clean.txt"), row.names = FALSE)

Blank2_PuEr_corr_export_FeO <- Blank2_PuEr_corr %>%
  select(Wavelength_cm, PuEr_FeO) %>%
  rename(V1 = Wavelength_cm, V2 = PuEr_FeO)
# TO DO BEFORE EXPORT: substitute in-read file .dpt values of the selected wavebands with the corrected waveband values according to area spectral correction!!
left_join(Blank2_PuEr_orig, Blank2_PuEr_corr_export_FeO, by = "V1") %>%
  mutate(V3 =ifelse(is.na(V2.y), V2.x, V2.y)) %>%
  select(V1, V3) -> Blank2_PuEr_corr_export_FeO
names(Blank2_PuEr_corr_export_FeO) <- NULL #drop header
write.table(Blank2_PuEr_corr_export_FeO, sep ="\t",
            file = paste0(here(),"Blank2_PuEr_corr_FeO.txt"), row.names = FALSE)

# Dal2_PP -----------------------------------------------------------------
left_join(DAL_PP, ano_reg, by = c("Wavelength_cm" = "Wavelength_cm")) %>%
  left_join(clean_reg, by = c("Wavelength_cm" = "Wavelength_cm")) %>%
  left_join(FeO_reg, by = c("Wavelength_cm" = "Wavelength_cm")) %>%
  select(-c(Area.x, Area.y, Area)) %>%
  mutate(PP_ano = DAL_PP - mean_ano,
         PP_ano_upper = PP_ano + stdev_ano,
         PP_ano_lower = PP_ano - stdev_ano,
         PP_clean = DAL_PP - mean_clean,
         PP_clean_upper = PP_clean + stdev_clean,
         PP_clean_lower = PP_clean - stdev_clean,
         PP_FeO = DAL_PP - mean_FeO,
         PP_FeO_upper = PP_FeO + stdev_FeO,
         PP_FeO_lower = PP_FeO - stdev_FeO)  -> Dal2_PP_corr


Dal2_PP_ano <- Dal2_PP_corr %>% select(Wavelength_cm,DAL_PP  , PP_ano,PP_ano_upper,PP_ano_lower) %>%
  pivot_longer(cols = -c(Wavelength_cm, PP_ano_upper, PP_ano_lower)) %>% rename(Correction = name)
Dal2_PP_clean  <- Dal2_PP_corr %>% select(Wavelength_cm,DAL_PP  , PP_clean,PP_clean_upper,PP_clean_lower) %>%
  pivot_longer(cols = -c(Wavelength_cm, PP_clean_upper, PP_clean_lower)) %>% rename(Correction = name)
Dal2_PP_FeO <- Dal2_PP_corr %>% select(Wavelength_cm,DAL_PP  ,PP_FeO,PP_FeO_upper,PP_FeO_lower) %>%
  pivot_longer(cols = -c(Wavelength_cm, PP_FeO_upper, PP_FeO_lower)) %>% rename(Correction = name)

#plot
prismatic::color(c( "#0097a7", "#ff5722","#e91e63"))

ggplot(Dal2_PP_ano) +
  geom_line(aes(x = Wavelength_cm, y = PP_ano_upper), color = "#0097A7FF", linetype = "dotted") +
  geom_line(aes(x = Wavelength_cm, y = PP_ano_lower), color = "#0097A7FF", linetype = "dotted") +
  geom_line(aes(x = Wavelength_cm, y = value, color = Correction), show.legend = FALSE) +
  scale_x_reverse()+
  xlab("Wavenumber [cm^-1]")+
  ylab("Absorbance [Au]")+
  scale_color_manual(values = c("black", "#0097A7FF" ))+
  theme_bw()+
  ggsave(filename = paste0(here(),"/plots_area_correction/Dal2_PP_anodisc.png"), width = 12, height = 6, type = "cairo-png", dpi = 300)

ggplot(Dal2_PP_clean) +
  geom_line(aes(x = Wavelength_cm, y = PP_clean_upper), color = "#FF5722FF", linetype = "dotted") +
  geom_line(aes(x = Wavelength_cm, y = PP_clean_lower), color = "#FF5722FF", linetype = "dotted") +
  geom_line(aes(x = Wavelength_cm, y = value, color = Correction), show.legend = FALSE) +
  scale_x_reverse()+
  xlab("Wavenumber [cm^-1]")+
  ylab("Absorbance [Au]")+
  scale_color_manual(values = c("black", "#FF5722FF" ))+
  theme_bw()+
  ggsave(filename = paste0(here(),"/plots_area_correction/Dal2_PP_clean.png"), width = 12, height = 6, type = "cairo-png", dpi = 300)

ggplot(Dal2_PP_FeO) +
  geom_line(aes(x = Wavelength_cm, y = PP_FeO_upper), color = "#E91E63FF", linetype = "dotted") +
  geom_line(aes(x = Wavelength_cm, y = PP_FeO_lower), color = "#E91E63FF", linetype = "dotted") +
  geom_line(aes(x = Wavelength_cm, y = value, color = Correction), show.legend = FALSE) +
  scale_x_reverse()+
  xlab("Wavenumber [cm^-1]")+
  ylab("Absorbance [Au]")+
  scale_color_manual(values = c("black", "#E91E63FF" ))+
  theme_bw()+
  ggsave(filename = paste0(here(),"/plots_area_correction/Dal2_PP_FeO.png"), width = 12, height = 6, type = "cairo-png", dpi = 300)

#export textfile with mean corrected spectra, call columns V1 and V2 again.
Dal2_PP_corr_export_ano <- Dal2_PP_corr %>%
  select(Wavelength_cm, PP_ano) %>%
  rename(V1 = Wavelength_cm, V2 = PP_ano)
# TO DO BEFORE EXPORT: substitute in-read file .dpt values of the selected wavebands with the corrected waveband values according to area spectral correction!!
Dal2_PP_orig <-  read.table(file = r"(C:\Users\tobia\OneDrive\Desktop\MA2_local\experiment_results\4_experiment_FeO\data\12_05_2021\DAL_45_000883_PP_654.dpt)", header = F, sep ="\t")
left_join(Dal2_PP_orig, Dal2_PP_corr_export_ano, by = "V1") %>%
  mutate(V3 =ifelse(is.na(V2.y), V2.x, V2.y)) %>%
  select(V1, V3) -> Dal2_PP_corr_export_ano
names(Dal2_PP_corr_export_ano) <- NULL #drop header
write.table(Dal2_PP_corr_export_ano, sep ="\t",
            file = paste0(here(),"Dal2_PP_corr_ano.txt"), row.names = FALSE)

Dal2_PP_corr_export_clean <- Dal2_PP_corr %>%
  select(Wavelength_cm, PP_clean) %>%
  rename(V1 = Wavelength_cm, V2 = PP_clean)
# TO DO BEFORE EXPORT: substitute in-read file .dpt values of the selected wavebands with the corrected waveband values according to area spectral correction!!
left_join(Dal2_PP_orig, Dal2_PP_corr_export_clean, by = "V1") %>%
  mutate(V3 =ifelse(is.na(V2.y), V2.x, V2.y))  %>%
  select(V1, V3) -> Dal2_PP_corr_export_clean
names(Dal2_PP_corr_export_clean) <- NULL #drop header
write.table(Dal2_PP_corr_export_clean, sep ="\t",
            file = paste0(here(),"Dal2_PP_corr_clean.txt"), row.names = FALSE)

Dal2_PP_corr_export_FeO <- Dal2_PP_corr %>%
  select(Wavelength_cm, PP_FeO) %>%
  rename(V1 = Wavelength_cm, V2 = PP_FeO)
# TO DO BEFORE EXPORT: substitute in-read file .dpt values of the selected wavebands with the corrected waveband values according to area spectral correction!!
left_join(Dal2_PP_orig, Dal2_PP_corr_export_FeO, by = "V1") %>%
  mutate(V3 =ifelse(is.na(V2.y), V2.x, V2.y)) %>%
  select(V1, V3) -> Dal2_PP_corr_export_FeO
names(Dal2_PP_corr_export_FeO) <- NULL #drop header
write.table(Dal2_PP_corr_export_FeO, sep ="\t",
            file = paste0(here(),"Dal2_PP_corr_FeO.txt"), row.names = FALSE)

