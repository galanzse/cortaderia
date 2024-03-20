

# RESULTS OF PORTABLE TRAPS IN COLLADO VILLALBA


library(readxl)
library(tidyverse)

grass <- read_excel("data/data.xlsx", sheet = "completo") 

grass <- grass[grass$treatment %in% c("d0", "Control") & grass$site != 'Parla', ]
grass$sample_name <- paste(grass$trap, grass$time, "min", sep = " ")

ggplot(data = grass, aes(y = grass_concentration+1  , x = factor(treatment, levels = c("d0", "Control")), fill = treatment)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(aes(shape = sample_name), size = 4, colour = "gray40", alpha = 0.7) +
  scale_fill_brewer(palette = 5, guide = "none") +
  labs(y = "Pollen grains/m\u00B3") +
  scale_y_log10(limits = c(1, 1000))+
  annotate("segment", x = 1, xend = 2, y = 750, yend = 750, size = 1.1) +
  annotate("text", label = "p < 0.01", x = 1.5, y = 900, size = 5) +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.8),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, face = "bold"))

# png("figures/Fig4_pollen_exposure.png", height = 17, width = 12, res = 500, units = "cm")
ggplot(data = grass, aes(y = log(grass_concentration+1)  , x = factor(treatment, levels = c("d0", "Control")), fill = treatment)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(aes(shape = sample_name), size = 4, colour = "gray40", alpha = 0.7) +
  scale_fill_brewer(palette = 5, guide = "none") +
  labs(y = "Log(pollen grains/m\u00B3 + 1)") +
  ylim(0, 6.9) +
  annotate("segment", x = 1, xend = 2, y = 6.7, yend = 6.7, size = 1.1) +
  annotate("text", label = "p < 0.001", x = 1.5, y = 6.9, size = 5) +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.8),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, face = "bold"))
# dev.off()

t.test(log(grass$grass_concentration[grass$treatment == "d0"]+1), log(grass$grass_concentration[grass$treatment == "Control"]+1),
       paired=F)

wilcox.test(grass$grass_concentration[grass$treatment == "d0"], grass$grass_concentration[grass$treatment == "Control"])
