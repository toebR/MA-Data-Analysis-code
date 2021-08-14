library(tidyverse)
library(here)
library(readxl)
library(Cairo)



# data --------------------------------------------------------------------

readxl::read_excel(path = paste0(here(),r"(\OM_filter_weights.xlsx)")) -> OMdata



# data wrangling ----------------------------------------------------------
#get duplicate names together to plot on 1 x slot later and show ranges
OMdata$Sample %>%
  str_replace("_I", "") %>%
  str_replace("I","") %>%
  str_replace("ns", "Ins")-> OMdata$Sample
#drop blank

OMdata %>%
  filter(Sample != "Blank") -> plotdat

#calculate dataframe for plotting ranges between duplicate values for Forest_Soil and Brugg sites.
OMdata %>%
  filter(Sample == "Brugg") %>%
  group_by(Sample, Batch) %>%
  summarise(min = min(`Corg_recovered_[%]`),
            max = max(`Corg_recovered_[%]`)) -> rangedat

#calculate duplicate means:


OMdata %>%
  filter(Sample == "Brugg") %>%
  group_by(Sample, Batch) %>%
  summarise(`Corg_recovered_[%]` = mean(`Corg_recovered_[%]`)) -> mean_dup

#bring back to df
OMdata %>%
  select(c(Sample, Batch,`Corg_recovered_[%]` )) ->  Omdata_sel


Omdata_sel %>%
  filter(Sample != "Brugg") %>%
  filter(Sample != "Forest_Soil") %>%
  rbind(mean_dup) -> data_stat



# statistics --------------------------------------------------------------
#transform data to wide
data_stat %>%
  pivot_wider(names_from = Batch, values_from = `Corg_recovered_[%]`) -> data_stat

## test normal distribution ------------------------------------------------
# p-value > 0.05 -> distribution of the data are not significantly different from normal distribution. assume normality

shapiro.test(data_stat$Fn) #p-value = 0.6223 -> assume normal distr
shapiro.test(data_stat$`F+`) #p-value =  0.5061 -> assume normal distr



## test for equal variance (F-test) ---------------------------------------
# p-value > significance level 0.05, there is no significant difference between the two variances.
var.test(data_stat$Fn,data_stat$`F+`) #p-value = 0.2824 -> assume equal variance



## parametric test ---------------------------------------------------------
# (> 0.05 -> null hypothesis is accepted, no significant difference between filter groups)
t.test(data_stat$Fn, data_stat$`F+`, var.equal = TRUE, paired = TRUE) #df = 3, p-value = 0.1061 -> 0-hyp accepted, no difference




# plots -------------------------------------------------------------------

plotdat %>%
  filter(Sample != "Forest_Soil") -> plotdat

plotdat$Sample %>%
  str_replace("_", " ") -> plotdat$Sample


ggplot(plotdat) +
  geom_segment(data = rangedat, aes(x = Sample, xend = Sample, y = min, yend = max, color = Batch), show.legend = FALSE)+
  geom_point(aes(x = Sample, y = `Corg_recovered_[%]`, color = Batch)) +
  xlab("Site")+
  ylab("C(org) recovery [%]") +
  scale_color_manual("",values = c("#ff9800", "#0097a7"))+
  theme_bw()+
  ggsave(filename = "weight_plot.png", path = paste0(here()), dpi = 300, width = 10, height = 10, units = "cm", type = "cairo-png")


ggplot(plotdat) +
  geom_segment(data = rangedat, aes(x = Sample, xend = Sample, y = min, yend = max, color = Batch), show.legend = FALSE)+
  geom_point(aes(x = Sample, y = `Corg_recovered_[%]`, color = Batch)) +
  xlab("Site")+
  ylab("C(org) recovery [%]") +
  scale_color_manual("",values = c("#ff9800", "#0097a7"))+
  theme_bw()+
  ggsave(filename = "weight_plot.svg", path = paste0(here()), dpi = 300, width = 10, height = 10, units = "cm")



