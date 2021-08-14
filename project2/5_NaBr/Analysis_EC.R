library(tidyverse)
library(readxl)

options(scipen = 999)

dat <- read_excel(path = r"(C:\Users\tobia\OneDrive\Desktop\MA2_local\experiment_results\3_experiment_NaBr_EC\EC_raw.xlsx)")

dat %>%
  select(-c(Vol_before_decanting_ml, Vol_filtrate_raw_ml,
            Vol_raw_pure, dec_aid_depth)) %>%
  pivot_longer(cols = -c(Sample, Group, Vol_raw_rinse)) %>%
  rename(rinse_ml = name,
         EC_uS_cm = value) %>%
  mutate(rinse_ml = as.numeric(rinse_ml),
         rinse_ml_corrected = rinse_ml + Vol_raw_rinse)-> dat_long

ggplot(dat_long, aes(x = rinse_ml_corrected, y = EC_uS_cm)) +

  geom_smooth(method = "loess", color = "grey30", span = 0.7, size = .5, alpha = .2) +
  geom_errorbar(aes(x = rinse_ml_corrected, y = EC_uS_cm,
                    xmin = rinse_ml_corrected -2,
                    xmax = rinse_ml_corrected + 2, color = factor(rinse_ml)))+
  geom_point(aes(, color = factor(rinse_ml))) +

  # geom_line(aes(color = Sample)) +
  geom_hline(yintercept = 265000, color = "black", linetype = "dotted")+
  geom_hline(yintercept = 1, color = "blue", linetype = "dotted")+
  annotate(geom = "text", x = 75, y = 40000, label = "Loessian Regression:\nSpan = 0.7, Degrees = 2, RSE = 24'480",
  size = 3)+
  annotate(geom = "text", x = 1, y = 7000, label = "EC MiliQ",
           size = 3, color = "blue")+
  annotate(geom = "text", x = 35, y = 260000, label = "EC NaBr (p = 1.5)",
           size = 3, color = "black")+
  # ggtitle("Electric Conductivity in Filtration II")+
  # labs(subtitle = "EC in rinsing steps",
       # caption= "Data comprises values from triplicate samples individually\nData is corrected for initial rinsing volume")+
  theme_bw() +
  xlab("Rinsing Volume MiliQ [ml]")+
  ylab("EC [µS/cm]")+
  scale_x_continuous(breaks = c(seq(0,130,15)))+
  scale_y_continuous(breaks = seq(0,280000, 50000))+
  scale_color_discrete("Rinsing\nAliquot [ml]")
  # scale_color_manual(values = c("#d81b60", "#0d47a1", "#4caf50", "#fb8c00"))+
  ggsave(filename = "EC_pre_plot.svg", path = r"(C:\Users\tobia\OneDrive\Desktop\MA2_local\experiment_results\3_experiment_NaBr_EC)", dpi = 300,
         width = 20, height = 15, units = "cm") -> p1




p1




#loess model
loessMod7 <- loess(EC_uS_cm ~ rinse_ml_corrected, data=dat_long, span=0.7)             
summary(loessMod7)
loessMod7
loessMod7$fitted

#nice explanation of loess:
# https://www.bing.com/search?pc=COSP&ptag=D111520-N9997AD26CBEB7DD&form=CONBDF&conlogo=CT3335811&q=R%20calculate%20loess%20regression



