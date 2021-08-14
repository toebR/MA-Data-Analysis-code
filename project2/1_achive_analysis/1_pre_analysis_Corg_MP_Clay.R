#10.2.2021
# This script will analyse and plot the already available data on the samples.
# This is conducted to have very first archive results on the samples from Gaby Witschi and Michael Scheurer.
# SO THIS SCRIPT PRODUCES PRELIMINARY RESULTS!! 
# THE PLOTS FROM THIS SCRIPT ARE SEPARATED FOR GABY AND MICHAELS DATA, THE LATER SCRIPT WILL INTEGRATE BOTH TO PLOT ON ONE PANEL!

# Next step will be to measure LOI on MS samples to leverage data on Corg(%) (script 2)
# 
# Then the classification and sample list will be calculated (script 3)

# load libraries ----------------------------------------------------------

library(readxl)
library(tidyverse)
library(here)
library(patchwork)
here()


# load archive data -------------------------------------------------------

dat <- read_xlsx(path = paste0(here(),"/archive_data.xlsx"))




# plots gaby --------------------------------------------------------------
dat_gab <- dat %>%
  filter(Person == "Gaby Witschi") %>%
  select(c(Site, `Soil Type`,`% organic C`, `% clay`, `MP N/kg < 5mm`)) %>%
  pivot_longer(cols = c(`% organic C`, `% clay`, `MP N/kg < 5mm`))

dat_gab_Corg <- dat_gab %>%
  filter(name %in% c("% organic C"))

dat_mic <- dat %>%
  filter(Person == "Michael Scheurer") %>%
  select(c(Site, `Soil Type`,`Total C %`, `% clay`, `MP N/kg < 5mm`)) %>%
  pivot_longer(cols = c(`Total C %`, `% clay`, `MP N/kg < 5mm`))

dat_mic_Ctot <- dat_mic %>%
  filter(name == c("Total C %"))

dat_both_clay_MP <- dat %>%
  select(Person, Site, `MP N/kg < 5mm`, `% clay`)

# 1. plot with Corg, clay and MP N < 5mm (label = soil type and caption that states measurement range from 2011 KABO to 2019 NABO)
#make 1 separate plot for both, 1 for Corg, 1 for clay

ggplot() +
  geom_col(data = dat_gab_Corg, aes(y = Site, x = value)) +
  geom_text(data = dat_gab_Corg, aes(y = Site, x = value, label = `Soil Type`), hjust = -.15) +
  scale_x_continuous(limits = c(0,22), expand = c(0,0)) +
  xlab("Corg [%]") +
  ylab("") +
  ggtitle("Gaby Witschi: Corg [%] and Soil Type") +
  labs(subtitle = "Data from NABO 2015-2019 and KABO 2011-2016") +
  theme_minimal()+
  theme(legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8))-> p_gab_Corg
p_gab_Corg



# plots Michael Scheurer (preliminary) ------------------------------------
#make 1 plot for total C
# 1. plot with Ctot %

ggplot() +
  geom_col(data = dat_mic_Ctot, aes(y = Site, x = value)) +
  xlab("Ctot [%]") +
  ylab("") +
  scale_x_continuous(limits = c(0,22), expand = c(0,0)) +
  ggtitle("Michael Scheurer: Ctot [%]") +
  labs(subtitle = "Carbonates were found in all samples") +
  theme_minimal()+
  theme(legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8)) -> p_mic_Ctot
p_mic_Ctot


# MP < 5mm plot for both with clay ----------------------------------------------
ggplot(dat_both_clay_MP) +
  geom_point(aes(y = Site, x = `MP N/kg < 5mm`, color = `% clay`)) +
  geom_segment(aes(y = Site, yend = Site, x = 0, xend = `MP N/kg < 5mm`, color =`% clay` )) +
  facet_wrap(Person~., ncol = 1, scales = "free_y") +
  scale_color_viridis_c(begin = 0, end = .9, option = "inferno") +
  scale_x_continuous(expand = c(0,0)) +
  labs(title = "MP and clay content in archive samples",
       subtitle = "Clay data Witschi retrieved from NABO 2015-2019 & KABO 2011-2016 \nClay data Scheurer retrieved MA 2017")+
  theme_minimal()+
  theme(legend.position = "right",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) + 
  guides(color = guide_colorsteps(barwidth =.5, barheight = 10)) -> P_both_clay_mp

P_both_clay_mp


# patchwork ---------------------------------------------------------------

P_both_clay_mp / (p_gab_Corg +p_mic_Ctot) -> p_fin

ggsave(p_fin, path = paste0(here(),"/1_pre_analysis"), filename = "Overview_pre_analysis.pdf", width = 20, height = 28, units = "cm")

