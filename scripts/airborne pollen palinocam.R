

# COMPARISON OF PALINOCAM STATIONS


library(readxl)
library(tidyverse)
library(AeRobiology)
library(scales)
library(grid)
library(ggpubr)


# set system time
Sys.setlocale(locale = "en_GB.UTF-8")

# data
df <- read_excel("data/data.xlsx", sheet = "STATIONS 94_22", 
                   col_types = c("date", "numeric", "text"))
colnames(df) <- c("date", "pollen", "site")
summary(df)


# filter data after 2015
df <- df %>% subset(date > as.Date("01-01-2015", "%d-%m-%Y"))


# new variables: year, month and day+month
df$year <- as.numeric(strftime(df$date, "%Y"))
df$month <- as.numeric(strftime(df$date, "%m"))

df$month2 <- as.factor(df$month)
levels(df$month2) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

df$dat <- substring(df$date, 6, 10)



# barplot
# monthly.plot <- ggplot(data = df, aes(x=month, y=pollen, fill=site)) +
#   geom_bar(stat = "identity", position=position_dodge()) +
#   labs(title = "A) Monthly average of grass pollen", y = "Pollen grains/m\u00B3") +
#   scale_fill_brewer(palette = "Pastel2") +
#   theme_classic() +
#   theme(axis.text = element_text(size = 12),
#         axis.title.x = element_blank(),
#         axis.title.y = element_text(size = 12, face = "bold"),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 12),
#         plot.title = element_text(size = 14, face = "bold"))



# tendencies
df.range <- merge(
  merge(
    setNames(aggregate(data = df, pollen ~ dat + site, FUN = function(x) mean(x, na.rm = T)), c("dat", "site", "mean")),
    setNames(aggregate(data = df, pollen ~ dat + site, FUN = function(x) max(x, na.rm = T)), c("dat", "site", "max"))),
  setNames(aggregate(data = df, pollen ~ dat + site, FUN = function(x) min(x, na.rm = T)), c("dat", "site", "min"))
)

# fig 2A
pollen.plot <- ggplot(data = df.range[order(df.range$site), ], aes(x = as.Date(paste0("2023-", dat)))) + 
  geom_ribbon(aes(ymin = AeRobiology::ma(min, man = 3), ymax = AeRobiology::ma(max, man = 3), fill = site), alpha = 0.2) + 
  geom_line(aes(y = AeRobiology::ma(mean, man = 3), color = site), size = 0.8) + 
  labs(title = "A) Pollen concentration per station") +
  scale_colour_brewer(palette = "Dark2") + 
  scale_fill_brewer(palette = "Dark2") + 
  scale_y_continuous(expand = c(0,0), limits = c(0, 550)) +
  # scale_x_date(labels = date_format("%b"), date_breaks = "1 month", limits = c(as.Date(strptime(1, "%j")), as.Date(strptime(365, "%j"))), expand = c(0, 0)) + 
  labs(y = "Pollen grains/m\u00B3") +
  theme_classic() + 
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position = c(0.9, 0.9), # plot.margin = margin(0.3, 0.2, 0, 2.5, "cm")
        plot.title = element_text(size = 14, face = "bold"))



# prepare data

df.range$mean[df.range$site%in%c("FACF","ARAN","ROZA") & between(as.Date(df.range$dat, "%m-%d"), as.Date("01-04", "%d-%m"), as.Date("30-07", "%d-%m"))] %>% mean(na.rm=T)
df.range$mean[df.range$site%in%c("FACF","ARAN","ROZA") & between(as.Date(df.range$dat, "%m-%d"), as.Date("01-04", "%d-%m"), as.Date("30-07", "%d-%m"))] %>% sd(na.rm=T)

df.range$mean[df.range$site=="VILL" & between(as.Date(df.range$dat, "%m-%d"), as.Date("01-04", "%d-%m"), as.Date("30-07", "%d-%m"))] %>% mean(na.rm=T)
df.range$mean[df.range$site=="VILL" & between(as.Date(df.range$dat, "%m-%d"), as.Date("01-04", "%d-%m"), as.Date("30-07", "%d-%m"))] %>% sd(na.rm=T)

df.range$mean[df.range$site%in%c("FACF","ARAN","ROZA") & between(as.Date(df.range$dat, "%m-%d"), as.Date("01-09", "%d-%m"), as.Date("30-10", "%d-%m"))] %>% mean(na.rm=T)
df.range$mean[df.range$site%in%c("FACF","ARAN","ROZA") & between(as.Date(df.range$dat, "%m-%d"), as.Date("01-09", "%d-%m"), as.Date("30-10", "%d-%m"))] %>% sd(na.rm=T)

df.range$mean[df.range$site=="VILL" & between(as.Date(df.range$dat, "%m-%d"), as.Date("01-09", "%d-%m"), as.Date("30-10", "%d-%m"))] %>% mean(na.rm=T)
df.range$mean[df.range$site=="VILL" & between(as.Date(df.range$dat, "%m-%d"), as.Date("01-09", "%d-%m"), as.Date("30-10", "%d-%m"))] %>% sd(na.rm=T)




# clinical seasons
df.ps <- data.frame()

for(sit in c("FACF","ARAN","ROZA")){
df.ps <- rbind(df.ps, data.frame(site = sit, curve = "first", calculate_ps(df[df$site == sit ,c("date", "pollen")], interpolation = F, method = "clinical", perc = 90, type = "grasses")[,c("st.jd", "en.jd")]))
}

df.temp <- df[df$site == "VILL" , ]
df.temp$pollen[which(df.temp$month2 %in% levels(df.temp$month2)[8:12])] <- NA
df.ps <- rbind(df.ps, data.frame(site = "VILL", curve = "first", calculate_ps(df.temp[ ,c("date", "pollen")], interpolation = F, method = "clinical", perc = 90, type = "grasses")[,c("st.jd", "en.jd")]))


df.temp <- df[df$site == "VILL" , ]
df.temp$pollen[which(df.temp$month2 %in% levels(df.temp$month2)[1:7])] <- NA
df.ps <- rbind(df.ps, data.frame(site = "VILL", curve = "second", calculate_ps(df.temp[ ,c("date", "pollen")], interpolation = F, method = "clinical", perc = 90, type = "grasses")[,c("st.jd", "en.jd")]))

df.pheno <- gather(df.ps, key = "param", value = "jd", -site, -curve)
df.pheno$type = paste0(df.pheno$site, df.pheno$curve, df.pheno$param)
df.pheno$param <- as.factor(df.pheno$param); levels(df.pheno$param) <- c('end','start')
df.pheno$param <- factor(df.pheno$param, levels=c('start','end'))


# fig 2B
season.plot <- ggplot(data = df.pheno, aes(y = as.Date(strptime(jd, "%j")), x = site,
                              fill = site, alpha = curve,
                              colour = factor(param, levels = c("start", "end"), labels = c("End-date", "Start-date")))) +
  geom_boxplot(position=position_dodge(width=0)) + # varwidth = T, width = 2,
  coord_flip() +
  labs(title = "B) Grass pollen season (clinical definition)") +
  scale_fill_brewer(palette = "Pastel2", guide = "none") +
  scale_colour_manual(values = c("black","gray60")) +
  scale_alpha_manual(values = c(1, 1), guide = "none") +
  scale_y_date(labels = date_format("%b"), date_breaks = "1 month", limits = c(as.Date(strptime(1, "%j")), as.Date(strptime(365, "%j"))), expand = c(0, 0)) +
  theme_classic() +
  theme(#axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position = c(0.9, 0.5),
        plot.title = element_text(size = 14, face = "bold"))
  


# final figure
ggarrange(pollen.plot, season.plot, nrow=2, heights=c(0.9,0.6))
