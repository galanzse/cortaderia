

# PLOTS OF POPULATION LEVEL PHENOLOGY

library(readxl)
library(tidyverse)
library(AeRobiology)
library(scales)
library(grid)
library(ggpubr)


# data
ph1 <- read_excel("data/data.xlsx", sheet = "phenodates")
ph1 <- gather(ph1, key = "var", value = "days", -code, -site, -year)

ph.autumn <- data.frame(read_excel("data/data.xlsx", sheet = "autumn_phenology_grasses"))
ph.autumn$days <- as.numeric(strftime(ph.autumn$date, "%j"))
ph.autumn$year <- as.numeric(strftime(ph.autumn$date, "%Y"))
colnames(ph.autumn)[colnames(ph.autumn)=='observation'] <- 'var'

ph1 <- rbind(ph1[,colnames(ph1)], ph.autumn[,colnames(ph1)])

# acronyms
names.df1 <- read_excel("data/data.xlsx", sheet = "code")
ph1 <- merge(ph1, names.df1)

# summarise
ph2 <- ph1[!ph1$var %in% c("length", "end", "peak"),] %>% group_by(code) %>%
  summarise(days=mean(days))
ord1 <- ph2$code[order(ph2$days, decreasing = T)]

# bibliographical data
bib1 <- read_excel("data/data.xlsx", sheet = "phenology_biblio2")
bib1$end[bib1$end==as.Date('2024-12-31 UTC')] <- as.Date('2024-12-15 UTC')
bib1 <- merge(bib1, names.df1)
str(bib1)


ph2$code[which(!ph2$code %in% bib1$code)]
ph3 <- merge(ph1[!ph1$var %in% c("length", "peak"), ], bib1[,c('code','origin','start','end')])


# fig 3A
pheno.plot1 <- ggplot(data = ph3) +
  geom_violin(aes(x = factor(code, levels = ord1), y = as.Date(strptime(days, "%j")), fill = origin),
              position=position_dodge(width = 0), varwidth = T, width = 1.5) +
  geom_point(aes(x = factor(code, levels = ord1), y = as.Date(strptime(start, "%Y-%m-%d")), color = origin),
             shape = 2,
             position=position_dodge(width = 0)) +
  geom_point(aes(x = factor(code, levels = ord1), y = as.Date(strptime(end, "%Y-%m-%d")), color = origin),
             shape = 4,
             position=position_dodge(width = 0)) +
  labs(title = "A) Flowering phenology of dominant grasses") +
  coord_flip() +
  scale_y_date(labels = date_format("%b"), date_breaks = "1 month",
               limits = c(as.Date(strptime(1, "%j")),as.Date(strptime(365, "%j"))), expand = c(0, 0)) + 
  # scale_fill_manual(values = c("#99badd", "#5b77cc", "#d88c02")) +
  guides(fill = guide_legend(byrow = TRUE)) +
  theme_classic() +
  theme(axis.text.y = element_text(face = "italic", size = 10),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold"),
        plot.margin = margin(0, 5.5, 5.5, 5.5, "points"),
        legend.position = c(0.9, 0.7),
        legend.background=element_rect(fill='transparent'),
        legend.spacing.y = unit(0.5, 'cm'))


# fig 3B
pheno.ori <- ggplot(data = ph3) +
  geom_violin(aes(x = origin, y = as.Date(strptime(days, "%j")), fill = origin),
              position=position_dodge(width = 0), varwidth = T, width = 1) +
  labs(title = "B) Flowering phenology per origin") +
  coord_flip() +
  scale_y_date(labels = date_format("%b"), date_breaks = "1 month",
               limits = c(as.Date(strptime(1, "%j")),as.Date(strptime(365, "%j"))), expand = c(0, 0)) + 
  # scale_fill_manual(values = c("#99badd", "#5b77cc", "#d88c02")) +
  guides(fill = guide_legend(byrow = TRUE)) +
  theme_classic() +
  theme(axis.text.y = element_text(face = "italic", size = 10),
        axis.text.x = element_text(size = 10, hjust = 0),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 14, face = "bold"),
        plot.margin = margin(0, 5.5, 5.5, 5.5, "points"),
        legend.position = 'none',
        legend.background=element_rect(fill='transparent'),
        legend.spacing.y = unit(0.5, 'cm'))


# final figure
ggarrange(pheno.plot1, pheno.ori, nrow=2, heights=c(1.6,0.35))
