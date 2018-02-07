# Figures for SAFE report
# ben.williams@alaska.gov
# last updated 2018-2

# need to run the data_cleaning.R script prior to creating these figures

# load ----
library(tidyverse)
library(extrafont)
library(gridExtra)
#font_import() only do this one time - it takes a while
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))

theme_set(theme_bw(base_size=12,base_family='Times New Roman')+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))

# data ----
sh <- read_csv("output/sh.csv")
yak <- read_csv("output/yak.csv") %>% 
  filter(bed!=6)
d16 <- read_csv("output/d16.csv")

# figure function ----
f.safe <- function(x){
  y = deparse(substitute(x))
x %>% 
  left_join(sh) %>% 
  ggplot(aes(Shell_Height, fill = Rtnd_Disc, color = Rtnd_Disc)) + 
  geom_density(alpha = 0.2) + 
  facet_grid(FY~.) +
  scale_color_manual(values = c('D' = "darkgray", "R" = "black"), labels = c('discard', 'retained'), name = "") + 
  scale_fill_manual(values = c('D' = "darkgray", "R" = "white"), labels = c('discard', 'retained'), name = "") +
  theme(legend.key = element_blank(),
        legend.position = c(0.2, 0.2),
        legend.background = element_rect(fill = "#ffffffaa", color = NA),
        legend.key.height = unit(0.6, 'line'),
        strip.background = element_blank(),
        panel.border = element_blank()) +
  xlab("Shell height (mm)") + ylab("Density") -> x1

x %>% 
  left_join(sh) %>% 
  ggplot(aes(Shell_Height, fill = Rtnd_Disc, color = Rtnd_Disc)) + 
  geom_histogram(alpha = 0.2, bins = 60) + 
  facet_grid(FY~.) +
  scale_color_manual(values = c('D' = "darkgray", "R" = "black"), labels = c('discard', 'retained'), name = "") + 
  scale_fill_manual(values = c('D' = "darkgray", "R" = "white"), labels = c('discard', 'retained'), name = "") +
  theme(legend.key = element_blank(),
        legend.position = c(0.2, 0.2),
        legend.background = element_rect(fill = "#ffffffaa", color = NA),
        legend.key.height = unit(0.6, 'line'),
        strip.background = element_blank(),
        panel.border = element_blank()) +
  xlab("Shell height (mm)") + ylab("Count") +
  guides(fill = FALSE, color = FALSE) -> x2

grid.arrange(x1, x2, ncol = 2) -> g
ggsave(file = paste('safe_figs/safe_fig_', y, ".png"), g, dpi = 200, height = 8, width = 6.5, units = "in")
}

# figs ----

f.safe(yak)
f.safe(d16)
