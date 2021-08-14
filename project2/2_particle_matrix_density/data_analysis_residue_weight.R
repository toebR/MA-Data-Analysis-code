# Author: Tobias Stalder
# Date: 16.03.2021
# Script Purpose: Data Analysis of Experiment 1
# Outputs:  Plots on weight loss between treatments, and t-test between groups
#



# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(readxl)
library(rstatix)
library(prismatic)


# discrete cols

c("#ff5722", "#ff9800", "#3f51b5", "#673ab7", "#9c27b0", "#e91e63", "#0097a7", "#1976d2")
color(c("#ff5722", "#ff9800", "#3f51b5", "#673ab7", "#9c27b0", "#e91e63", "#0097a7", "#1976d2"))
# Data Import --------------------------------------------------------------------

exp_dat <- read_excel(path =paste0(here(),"/data_cleaned.xlsx"))

arch_dat <- read_excel(path = paste0(here(),"/archive_data_classified.xls.xlsx"))




# Data wrangling ----------------------------------------------------------

# 1: calculate mg from g

exp_dat %>%
  mutate(mg_use = g_use *1000,
         dev_use_mg = dev_use_g * 1000) -> exp_dat

gsub(" um", "µm", exp_dat$batch) -> exp_dat$batch

# 2: join OM and clay % data from sample archive

dat <- left_join(exp_dat, arch_dat, by = c("sample" = "Site"))



# Plot 1: weight loss between treatments ----------------------------------

#t-test without assuming equal variance: insignificant
dat %>%
  group_by(batch) %>%
  get_summary_stats(mg_use, type = "mean_sd") -> sum_dat

stat_test <- dat %>%
  t_test(mg_use ~ batch, detailed = TRUE, conf.level = 0.95) %>%
  add_significance()
stat_test

stat_result <- paste0("T-test: ", "p=",stat_test$p, ", n=",stat_test$n1)

#plot
ggplot(dat) +
  geom_col(aes(x = sample, y = mg_use, fill = batch), position = position_dodge2(preserve = "single"), width = .5)+
  geom_errorbar(aes(x = sample, ymin = mg_use - dev_use_mg, ymax= mg_use + dev_use_mg, color = batch),
                position = position_dodge(width = .5), width = .1, size = .3) +
  annotate(geom = "text", label = stat_result, x = 1.2, y = 27, size = 3)+
  xlab("Site") +
  ylab("Weight [mg]")+
  scale_fill_manual("Filter",values = c("#85929e", "#7fb3d5"))+
  scale_color_manual(values = c("black", "black")) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0,25,5))+
  theme_classic()+
  theme(panel.grid.major.y = element_line(color = "lightgrey", size = .3),
        legend.position = "bottom")+
  guides(color = guide_none()) -> p1
p1

ggsave(p1, filename = "Plot_weight.png", path = paste0(here()),
       width = 12, height = 12, units = "cm", dpi = 300, type = "cairo-png")

#decide for other colors
#control plot with the data table on google sheets
#save plot and insert in word




