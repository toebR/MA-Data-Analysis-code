
# metadata ----------------------------------------------------------------

#Author: Tobias Stalder
# script: 2_classification

# Purpose: (1) Classifying archive data in quantile classes
          # (2) Plot archive data for sample selection out of (1)
       

#Outputs: (1) Updates archive datasheet with quantile classifier information
          # (2) Plots for sampling subchapter in master thesis
        

#To use for: Sample list to discuss with supervisor. Additional forest soil sample with high amounts of
# organic residues will be selected additionally.



# load libraries ----------------------------------------------------------

library(readxl)
library(tidyverse)
library(ggalt)
library(here)
library(Cairo)
here()


# load archive data -------------------------------------------------------

dat <- read_xlsx(path = paste0(here(),"/archive_data.xlsx"))

dat %>%
  select(-c(`Total C %`, Carbonates, pH)) %>%
  rename(soil_type = `Soil Type`,
         c_org = `% organic C`,
         clay = `% clay`,
         MP_N_Kg = `MP N/kg < 5mm`) -> dat

# 1_classifying archive with quantile ranges ------------------------------

quant_c_org <- data.frame(quantile(dat$c_org))
quant_c_org %>%
  rownames_to_column() %>%
  filter(rowname == "25%" | rowname == "75%" | rowname == "100%") -> quant_c_org_2

quant_clay <- data.frame(quantile(dat$clay))
quant_clay %>%
  rownames_to_column() %>%
  filter(rowname == "25%" | rowname == "75%" | rowname == "100%") -> quant_clay_2



#calculate quantile classes for clay and c_org in min-25%, 25%-75% and 75-100%
dat %>%
  mutate(quant_c_org = cut(dat$c_org,
                           breaks = c(-Inf,quant_c_org_2$quantile.dat.c_org.),
                           labels = c("C <= q0.25", "C q0.25-q0.75", "C >= q0.75"))) %>%
  mutate(quant_clay = cut(dat$clay,
                          breaks = c(-Inf,quant_clay_2$quantile.dat.clay.),
                          labels = c("Clay <= q0.25", "Clay q0.25-q0.75", "Clay >= q0.75")))-> dat_class

#merge quantile categories to 1 factor
dat_class %>%
  mutate(quant_cat = paste(quant_c_org, quant_clay, sep = " | ")) -> dat_class

#export classified archive data
write_excel_csv(dat_class, file = "archive_data_classified.xls")

# 2_Plots ----------------------------------------------------------------------

cols_C <- c("#3D2645","#832161","#DA4167")



#Plot clay + C_org with quantile classes, TO DO: chose colors & INDICATE median
#1
ggplot(dat_class) +
  geom_hline(yintercept = quant_clay[2,], linetype = "dashed", size = .3, color = "gray40")+
  geom_hline(yintercept = quant_clay[4,], linetype = "dashed", size = .3, color = "gray40")+
  geom_point(aes(x = c_org, y = clay, color = quant_clay), show.legend = FALSE, size = .9) +
  geom_rug(aes(x = c_org, y = clay), color = "gray40", size = .2)+
  annotate(geom = "text", label = "q.25", x = 20, y = 3.8, size = 1.5, color = "gray40")+
  annotate(geom = "text", label = "q.75", x = 20, y = 17.6, size = 1.5, color = "gray40")+

  xlab("C(org) %")+
  ylab("Clay %")+

  scale_color_manual(values = cols_C)+

  theme_classic() -> p1
p1

#2
ggplot(dat_class) +

  geom_vline(xintercept = quant_c_org[2,], linetype = "dashed", size = .3, color = "gray40")+
  geom_vline(xintercept = quant_c_org[4,], linetype = "dashed",  size = .3, color = "gray40")+
  geom_point(aes(x = c_org, y = clay, color = quant_c_org ), show.legend = FALSE, size = .9) +
  geom_rug(aes(x = c_org, y = clay), color = "gray40", size =.2)+
  annotate(geom = "text", label = "q.25", x = 1, y = 55, size = 1.5, angle = 90, color = "gray40")+
  annotate(geom = "text", label = "q.75", x = 2.9, y = 55, size = 1.5, angle = 90, color = "gray40")+
  xlab("C(org) %")+
  ylab("Clay %")+
  scale_color_manual(values = cols_C)+
  theme_classic()  -> p2
p2

#3
ggplot(dat_class, aes(x = c_org, y = clay, color = quant_cat)) +
  geom_point(show.legend = FALSE, size =0.9) +
  geom_hline(yintercept = quant_clay[2,], linetype = "dashed" , size = .3, color = "gray40")+
  geom_hline(yintercept = quant_clay[4,], linetype = "dashed", size = .3, color = "gray40")+
  geom_vline(xintercept = quant_c_org[2,], linetype = "dashed", size = .3, color = "gray40")+
  geom_vline(xintercept = quant_c_org[4,], linetype = "dashed", size = .3, color = "gray40")+
  geom_rug(aes(x = c_org, y = clay), color = "gray40", size = .2)+
  
  annotate(geom = "text", label = "q.25", x = 1, y = 55, size = 1.5, angle = 90, color = "gray40")+
  annotate(geom = "text", label = "q.75", x = 2.9, y = 55, size = 1.5, angle = 90, color = "gray40")+
  annotate(geom = "text", label = "q.25", x = 20, y = 3.8, size = 1.5, color = "gray40")+
  annotate(geom = "text", label = "q.75", x = 20, y = 17.6, size = 1.5, color = "gray40")+

  scale_color_manual(values = c("#ff5722", "#ff9800", "#3f51b5", "#673ab7", "#9c27b0", "#e91e63", "#0097a7", "#1976d2"))+
  xlab("C(org) %")+
  ylab("Clay %")+
  
  theme_classic()  -> p3
p3

#export plots 1-3

ggsave(p1, filename = "Plot_clay.png", path = paste0(here(), "/2_classification"),
       width = 9, height = 9, units = "cm", dpi = 300, type = "cairo-png")


ggsave(p2, filename = "Plot_corg.png", path = paste0(here(), "/2_classification"),
       width = 9, height = 9, units = "cm", dpi = 300, type = "cairo-png")


ggsave(p3, filename = "Plot_clay_corg.png", path = paste0(here(), "/2_classification"),
       width = 9, height = 9, units = "cm", dpi = 300, type = "cairo-png")


#Plot MP
ggplot(dat_class, aes(x = MP_N_Kg, y = reorder(Site, dat_class$MP_N_Kg), fill = Person)) +
  geom_col() +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_manual("Project", values = c("#832161","#DA4167")) +
  xlab("MP N Particles < 5mm") +
  ylab("Site")+
  theme_classic() +
  theme(legend.position = "bottom") -> p4
p4

ggsave(p4, filename = "MP_N.png", path = paste0(here(), "/2_classification"),
                                                width = 12, height = 12, units = "cm", dpi = 300, type = "cairo-png")



