# tobias stalder
# June 2021

#Script purpose: Analyse and plot results of area covered by particles after OM experiment




# libraries ---------------------------------------------------------------
library(readxl)
library(tidyverse)
library(here)



# data --------------------------------------------------------------------

data <- read_excel(path = paste0(here(),"/data.xlsx"))



# data wrangling ----------------------------------------------------------

#bring duplicates together

data$Sample %>%
  str_replace(" I", "") %>%
  str_replace("gI", "g") %>%
  str_replace("lI", "l")-> data$Sample


# df for duplicate range

data %>%
  filter(Sample == "Brugg" | Sample == "Forest Soil") %>%
  group_by(Sample, Batch) %>%
  summarise(min = min(`Area_covered_[%]`),
            max = max(`Area_covered_[%]`))-> dup



#calculate duplicate means:


data %>%
  filter(Sample == "Brugg" | Sample == "Forest Soil") %>%
  group_by(Sample, Batch) %>%
  summarise(`Area_covered_[%]` = mean(`Area_covered_[%]`)) -> mean_dup

#bring back to df

data %>%
  filter(Sample != "Brugg") %>%
  filter(Sample != "Forest Soil") %>%
rbind(mean_dup) -> data_stat

# statistics --------------------------------------------------------------

#transform data to wide
data_stat %>%
  pivot_wider(names_from = Batch, values_from = `Area_covered_[%]`) -> data_stat



## test normal distribution ------------------------------------------------
# p-value > 0.05 -> distribution of the data are not significantly different from normal distribution. assume normality

shapiro.test(data_stat$Fn) #p-value = 0.8848 -> assume normality
shapiro.test(data_stat$`F+`) #p-value = 0.1703 -> assume normality



## test for equal variance (F-test) ---------------------------------------
# p-value > significance level 0.05, there is no significant difference between the two variances.
var.test(data_stat$Fn,data_stat$`F+`) #p-value = 0.6709 -> assume equal variance



## parametric test ---------------------------------------------------------
# (> 0.05 -> null hypothesis is accepted, no significant difference between filter groups)
t.test(data_stat$Fn, data_stat$`F+`, var.equal = TRUE, paired = TRUE) #p-value = 0.01605, df = 4 -> assume difference between groups


# plot --------------------------------------------------------------------


ggplot(data, aes(x = Sample, y = `Area_covered_[%]`, color = Batch))+
  geom_segment(data = dup, aes(x = Sample, xend = Sample, y = min, yend = max), show.legend = FALSE)+
  geom_point() +
  xlab("Site")+
  ylab("Area covered [%]") +
  scale_color_manual("",values = c("#ff9800", "#0097a7"))+
  theme_bw()+
  ggsave(filename = "area_plot.png", path = paste0(here()), dpi = 300, width = 10, height = 10, units = "cm", type = "cairo-png")

ggplot(data, aes(x = Sample, y = `Area_covered_[%]`, color = Batch))+
  geom_segment(data = dup, aes(x = Sample, xend = Sample, y = min, yend = max), show.legend = FALSE)+
  geom_point() +
  xlab("Site")+
  ylab("Area covered [%]") +
  scale_color_manual("",values = c("#ff9800", "#0097a7"))+
  theme_bw()+
  ggsave(filename = "area_plot.svg", path = paste0(here()), dpi = 300, width = 10, height = 10, units = "cm")


