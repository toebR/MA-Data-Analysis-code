# script by Tobias Stalder
# 30.04.2021

# Script purpose: Plot size distribution exported from FTIR
# calculations: correct data with corresponding blanks, calculate triplicate means and RSDs
# plot bar diagrams of all measured samples and plot bar diagrams of the corrected samples + deviations.
#plot 0.45 group mean and standard deviaation (see how it looks like but it does not mean much..)


# libraries ---------------------------------------------------------------
library(readxl)
library(tidyverse)
library(ggpubr)
library(here)


# data --------------------------------------------------------------------
data <- read_excel(path = paste0(here(), "/data_FTIR_size_dist.xlsx"))



# data wrangling ----------------------------------------------------------

data %>%
  select(-c(DB)) %>%
  pivot_longer(cols = -c(Sample, Batch)) %>%
  rename(category = name,
         count = value)  %>%
  mutate(category = factor(category))-> data_clean

data_clean$category <- factor(data_clean$category,
                              levels = c("11-30", "30-60", "60-90", "90-120", "120-150", "150-180", "180-210", "210-240", "240-270", "270-300" ))
# plot --------------------------------------------------------------------


ggplot(data_clean, aes(x = Sample, y = count, group = category, fill = category))+
  geom_col(position = "dodge2") +
  scale_fill_viridis_d("Size [um]")+
  scale_x_discrete(expand = c(0.2,0))+
    scale_y_continuous(expand = c(0,0), limits = c(0,850))+
  facet_grid(Batch~Sample, scales = "free_x") +
  ggtitle("Raw Particle Size Distribution")+
  labs(subtitle = "Retrieved from Lumos II Particle detection")+
  theme_bw() +
  ggsave(path = paste0(here()),filename = "Plot_size_dist.png",
         width = 20, height = 12, units = "cm", dpi = 300)

# blank correction with Blank 3! --------------------------------------------------------
data_corr <- data_clean

data_corr %>%
  spread(Sample, count) %>%
  mutate(Att_1_c = Att_1 - Blank_3,
         Att_2_c = Att_2 - Blank_3,
         Att_3_c = Att_3 - Blank_3,
         Ins_1_c = Ins_1 - Blank_3,
         Ep_c = Ep - Blank_3,
         Dal_c = Dal - Blank_3) %>%
  select(-c(Att_3, Att_2, Att_1, Blank_2, Ep, Dal, Ins_1)) %>%
  pivot_longer(cols = -c(Batch, category)) %>%
  rename(Sample = name,
         count = value) -> dat_corr_long


# calculate triplicate means and rsd per group ----------------------------

dat_corr_long %>%
  filter(Sample %in% c("Att_1_c", "Att_2_c", "Att_3_c")) %>%
  group_by(Batch, category) %>%
  summarise(mean = mean(count),
            stdev = sd(count),
            rsdev = stdev/mean)  %>% #rsd calculated as a factor of 1, i.e. 0.2 = 20%
  mutate(Sample = "Att" ) %>%
  rename(count = mean)-> att_mean_sd



# NEXT: bring Att values back to df and calculate deviations with rsd of triplicates for all samples --------

left_join(dat_corr_long, att_mean_sd, by = c("Batch" = "Batch", "category" = "category")) %>%
  select(-c(Sample.y, stdev, count.y)) %>%
  rename(Sample = Sample.x,
         count = count.x) %>%
  filter(Sample != "Att_1_c",
         Sample != "Att_2_c",
         Sample != "Att_3_c",
         Sample != "Blank_3") -> dat_corr_long_dev

att_mean_sd %>% #bind Att triplicate mean values and rsd back to dat corr long
  select(-c(stdev)) -> Att

rbind(dat_corr_long_dev, Att) -> dat_long_fin

#calculate deviation from count (i.e. mean of triplicate for Att) with rsd for later plots

dat_long_fin %>%
  mutate(dev = (count/1)*rsdev,
         dev_up = count + dev,
         dev_low = count - dev) -> dat_long_fin



# plots -------------------------------------------------------------------


# blank corrected plot bar ----------------------------------------------------

ggplot(dat_long_fin, aes(x = Sample, y = count, group = category, fill = category))+
  geom_col(position = "dodge2") +
  geom_errorbar(aes(x = Sample, ymin = dev_low, ymax = dev_up), position=position_dodge(width=0.9), width = 0.5, size = 0.2)+
  scale_fill_viridis_d("Size [µm]")+
  scale_x_discrete(expand = c(0.2,0))+
  scale_y_continuous(expand = c(0,0), limits = c(0,1000))+
  facet_wrap(Sample~Batch, scales = "free_x", ncol=2) +
  # ggtitle("Blank-corrected Particle Size Distribution")+
  # labs(subtitle = "Retrieved from Lumos II Particle detection")+
  theme_bw() +
  theme(legend.position = "bottom") +
  ggsave(path = paste0(here()),filename = "Plot_size_dist_blank_corrected.svg",
         width = 15, height = 20, units = "cm", dpi = 300)



# statistics over the whole dataset ---------------------------------------

#investigate difference between all samples per category between batches
ggplot(dat_long_fin) +
  geom_boxplot(aes(x = category, y = count, fill = as.factor(Batch), color = as.factor(Batch)), alpha = 0.4, outlier.alpha = 0) +
  geom_point(aes(x = category, y = count, color = as.factor(Batch)), alpha = 1, position=position_dodge(width=0.76)) +
  scale_color_manual("Filter", values = c( "#7fb3d5", "#85929e")) +
  scale_fill_manual("Filter", values = c( "#7fb3d5", "#85929e")) +
  xlab("Size [µm]")+
  ylab("Particle Count")+
  theme_bw() +
  ggsave(filename = "Boxplot_dist_particles_categories.svg", path = here(), width = 10, height = 7)


# statistics --------------------------------------------------------------
# we have a paired dataset (as we have batches, taken from the same subject but under different conditions)
#tests will be conducted for categories 11-30, 30-60 and 60-90. First proof of concept only on 11-30.

#data wranling to wide format for shapiro wilks test
dat_long_fin %>%
  select(Batch, category, Sample, count) %>%
  pivot_wider(names_from = category, values_from = count) -> dat_wide

dat_wide %>%
  group_by(Batch) %>%
  summarise(median11_30 = median(`11-30`),
            median30_60 = median(`30-60`),
            median60_90 = median(`60-90`))

Batch26 <- dat_wide %>%
  filter(Batch == "26")
  
Batch45 <-dat_wide %>%
  filter(Batch == "45")

#test for normal  distribution (shapiro-wilks)  p-value > 0.05 -> distribution of the data are not significantly different from normal distribution. assume normality

shapiro.test(Batch26$`11-30`) #p = 0.1932, normality
shapiro.test(Batch26$`30-60`) #p = 0.0023, no normality
shapiro.test(Batch26$`60-90`) #p = 0.0607, normality

shapiro.test(Batch45$`11-30`) #p = 0.7925, normality
shapiro.test(Batch45$`30-60`) #p = 0.0134, no normality
shapiro.test(Batch45$`60-90`) #p = 0.02614, no normality

#test for equal variance (F-test)
dat_long_fin %>%
  mutate(group = paste0(Batch,"_",category)) %>%
  select(group, count, Batch, category) -> dat_F_test

#seperate tibble to tibbles on which you want to compare variances (e.g. df with 26_11-30 and 45_11-30)
cat_11_30 <- dat_F_test %>%
  filter(category == "11-30")

cat_30_60 <- dat_F_test %>%
  filter(category == "30-60")

cat_60_90 <- dat_F_test %>%
  filter(category == "60-90")

#F-test along groups of categories: i p-value > significance level 0.05, there is no significant difference between the two variances.

var.test(count ~ group, data = cat_11_30) #p = 0.4316 no difference in variance
var.test(count ~ group, data = cat_30_60) #p = 0.05049 no difference in variance
var.test(count ~ group, data = cat_60_90) #p = 0.03952 difference in variance


#if normally distribution and equal viarance, compute t-test pairwise with equal variance (var.equal = TRUE), one-tailed "greater"
#if normally distributed and unequal varaince, pairwise t-test , one-tailed "greater" with default var.equal = FALSE (assumes unequal variance and applies the Welsh df modification.)
#if non-normal distributed, compute wilcoxon test (computes ranks, is non-parametric)


t.test(value ~ group, data = df, var.equal = TRUE, paired = TRUE)

## 11-30 category (normally distributed, equal variance) -> t.test pairwise with equal variance, one-tailed greater
t.test(count ~ group, data = cat_11_30, var.equal = TRUE, paired = TRUE, alternative = "less") #checks if group 1 (26um, code 26) is lesser than group2 (0.45um, code 45)
#p-value = 0.1284 (> 0.05 -> null hypothesis is accepted, no significant difference between filter groups)

## 30-60 (not normally distributed, equal variance) -> wilcoxon
wilcox.test(count ~ group, data = cat_11_30, var.equal = TRUE, paired = TRUE, alternative = "less")
#p-value = 0.1875 (> 0.05 -> null hypothesis is accepted, no significant difference between filter groups)

## 60-90 (one sample not normally distributed, unequal variance) -> wilcox test pairwise
wilcox.test(count ~ group, data = cat_60_90, var.equal = FALSE, paired = TRUE, alternative = "less") #checks if group 1 (26um, code 26) is lesser than group2 (0.45um, code 45)
#p-value = 0.3125 (> 0.05 -> null hypothesis is accepted, no significant difference between filter groups)


# TO DO: update plot in inkscape (Legend, axes etc.), make p-value annotation etc. --------


