# set working directory on Windows
setwd("C:/Users/dhardy/Dropbox/sesync/manuscripts/unpublished/sharing_meanings_approach/R")

rm(list=ls())

library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)

## set data directory
datadir <- '/Users/dhardy/Dropbox/r_data/web-of-science' 

## import data
df <- read.delim(file.path(datadir, "data/interdhd.txt"), header = TRUE, sep = "\t") %>%
  head(., -2) %>%
  mutate(count = records, year = as.character(Publication.Years)) %>%
  mutate(year = year(as.Date(year, "%Y"))) %>%
  select("year", "count") %>%
  arrange(year)

fig <- ggplot(df, aes(year, count)) + 
  geom_col() + 
  scale_y_continuous(name = "Records", limits = c(0,100), breaks = seq(0, 100, 10),
                     sec.axis = sec_axis(~., breaks = seq(0,100,10), labels = NULL)) + 
  scale_x_continuous(name = "Year", breaks = seq(1980, 2015, 5),
                     sec.axis = sec_axis(~., breaks = seq(1980,2015, 5), labels = NULL)) + 
  # scale_shape_manual(name = "", values = c(18,1), labels = c("Vulnerability", "AND (Hazards OR Disasters)")) + 
  theme(legend.position = c(0.35,0.85),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "white", color = "white"),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(margin = unit(c(0.5,0.5,0.5,0.5), "cm")),
        axis.text.y = element_text(margin = unit(c(0.5,0.5,0.5,0.5), "cm")),
        axis.ticks.length = unit(-0.15, "cm"),
        axis.ticks = element_line(color = "black"),
        text = element_text(size = 12, color = "black"),
        plot.margin = margin(1,1,0.5,0.5, "cm")
        )
fig

tiff('figures/interd.tiff', width=5, height=5, units="in", res=300, compression = "lzw")
fig
dev.off()


# fig <- ggplot(df, aes(year, records/1000), group = identity) + 
#   geom_point(aes(shape = identity), size = 5, color = "white") +
#   scale_shape_manual(name = "", values = c(18,1), labels = c("Vulnerability", "AND (Hazards OR Disasters)")) +
#   # geom_smooth(method = "loess", se = FALSE, lwd = 1.2, aes(color = identity)) +
#   # scale_color_manual(name = "", values = c("steelblue1", "tomat3"), labels = c("Vulnerability", "AND (Hazards OR Disasters)")) +
#   scale_y_continuous(name = "Records (1000s)", limits = c(0,12), breaks = seq(0, 12, 2),
#                      sec.axis = sec_axis(~., breaks = seq(0,12,2), labels = NULL)) + 
#   scale_x_continuous(name = "Year", breaks = seq(1945, 2020, 10), 
#                      sec.axis = sec_axis(~., breaks = seq(1945,2020, 10), labels = NULL)) + 
#   theme(legend.position = c(0.45,0.85),
#         legend.key = element_rect(fill = "transparent", color = "transparent"),
#         legend.background = element_rect(fill = "transparent"),
#         panel.background = element_rect(fill = "transparent", color = "transparent"),
#         panel.grid = element_blank(),
#         plot.background = element_rect(fill = "transparent", color = "transparent"),
#         axis.line = element_line(color = "white"),
#         axis.text = element_text(color = "white"),
#         axis.text.x = element_text(margin = unit(c(0.5,0.5,0.5,0.5), "cm")),
#         axis.text.y = element_text(margin = unit(c(0.5,0.5,0.5,0.5), "cm")),
#         axis.ticks.length = unit(-0.15, "cm"),
#         axis.ticks = element_line(color = "white"),
#         text = element_text(size = 20, color = "white"),
#         plot.margin = margin(1,1,0.5,0.5, "cm")
#   )
# fig
# png('figures/vuln_slide.png', width=6, height = 6, units="in", res = 150, bg = "transparent")
# fig
# dev.off()





