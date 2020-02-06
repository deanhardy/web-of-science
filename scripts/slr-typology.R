
rm(list=ls())

library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)

## set data directory
datadir <- '/Users/dhardy/Dropbox/r_data/web-of-science' 
  
## import data
slr <- read.delim(file.path(datadir, "data/slr.txt"), header = TRUE, sep = "\t") %>%
  head(., -2) %>%
  mutate(year = as.character(Publication.Years)) %>%
  mutate(year = year(as.Date(year, "%Y")), id = 'slr') %>%
  select("year", "records", 'id')

risk <- read.delim(file.path(datadir, "data/slr-risk-vuln-exp.txt"), header = TRUE, sep = "\t") %>%
  head(., -2) %>%
  mutate(year = as.character(Publication.Years)) %>%
  mutate(year = year(as.Date(year, "%Y")), id = 'risk') %>%
  select("year", "records", 'id')

pop <- read.delim(file.path(datadir, "data/slr-pop.txt"), header = TRUE, sep = "\t") %>%
  head(., -2) %>%
  mutate(year = as.character(Publication.Years)) %>%
  mutate(year = year(as.Date(year, "%Y")), id = 'pop') %>%
  select("year", "records", 'id')

poprisk <- read.delim(file.path(datadir, "data/slr-risk-vuln-exp-pop.txt"), header = TRUE, sep = "\t") %>%
  head(., -2) %>%
  mutate(year = as.character(Publication.Years)) %>%
  mutate(year = year(as.Date(year, "%Y")), id = 'pop+risk') %>%
  select("year", "records", 'id')

# slr <- slr[1,] %>%
#   mutate(year = NA, records = NA, id = 'slr')

all <- rbind(poprisk, risk, pop, slr) %>%
  #mutate(id = factor(id, levels = c('slr', 'risk', 'pop', 'pop+risk'))) %>%
  mutate(id = factor(id, levels = c('pop+risk', 'risk', 'pop', 'slr')))

# plot as points
# fig <- ggplot(all, aes(year, records), group = id) +
#   geom_point(aes(shape = id), size = 2) +
#   scale_shape_manual(name = "", values = c(1,18,8,2), labels = c("Sea-Level Rise\nOR Sea Level Rise", "AND Risk", "AND Population",
#                                                                 "AND Population AND Risk")) +
#   # geom_smooth(method = "loess", se = FALSE, lwd = 1.2, aes(color = identity)) +
#   # scale_color_manual(name = "", values = c("steelblue1", "tomat3"), labels = c("Vulnerability", "AND (Hazards OR Disasters)")) +
#   scale_y_continuous(name = "No. of Records", limits = c(0,320), breaks = seq(0, 320, 20),
#                      sec.axis = sec_axis(~., breaks = seq(0,320,20), labels = NULL)) +
#   scale_x_continuous(name = "Year", breaks = seq(1970, 2020, 10),
#                      sec.axis = sec_axis(~., breaks = seq(1970,2020, 10), labels = NULL)) +
#   theme(legend.position = c(0.25,0.85),
#         legend.key = element_rect(fill = "transparent", color = "transparent"),
#         legend.background = element_rect(fill = "transparent"),
#         panel.background = element_rect(fill = "transparent", color = "transparent"),
#         panel.grid = element_blank(),
#         plot.background = element_rect(fill = "transparent", color = "transparent"),
#         axis.line = element_line(color = "black"),
#         axis.text = element_text(color = "black"),
#         axis.text.x = element_text(margin = unit(c(0.5,0.5,0.5,0.5), "cm")),
#         axis.text.y = element_text(margin = unit(c(0.5,0.5,0.5,0.5), "cm")),
#         axis.ticks.length = unit(-0.1, "cm"),
#         axis.ticks = element_line(color = "black"),
#         text = element_text(size = 10, color = "black"),
#         plot.margin = margin(1,1,0.5,0.5, "cm")
#   )
# fig

## still working on getting columns to overlay and show legend in desired order. 
## plot as overlaid columns
fig_col <- ggplot() + 
  geom_col(aes(year, records, fill = 'grey90'), filter(all, id == 'slr' & year > 1989)) + 
  geom_col(aes(year, records, fill = 'grey60'), filter(all, id == 'risk')) + 
  geom_col(aes(year, records, fill = 'grey30'), filter(all, id == 'pop')) + 
  geom_col(aes(year, records, fill = 'black'), filter(all, id == 'pop+risk')) + 
  #geom_bar(stat = "identity", position = "identity", alpha=0.9) +
  scale_fill_manual(name = "", values = rev(c('grey90', 'grey60', 'grey30', 'black')), labels =
                     rev(c("Sea-Level Rise\nOR Sea Level Rise", "AND Population", "AND Risk OR Vulnerability OR Exposure", 
                           "AND Risk OR Vulnerability OR Exposure AND Population"))) +
  scale_y_continuous(name = "No. of Records", limits = c(0,1600), breaks = seq(0, 1600, 200),
                     minor_breaks = seq(0, 1600, 100),
                     sec.axis = sec_axis(~., breaks = seq(0,1600,200), labels = NULL),
                     expand = c(0,0)) + 
  scale_x_continuous(name = "Year", breaks = seq(1990, 2020, 5),
                     sec.axis = sec_axis(~., breaks = seq(1990,2020, 5), labels = NULL)
                     ) + 
  theme(legend.position = c(0.44,0.87),
        # legend.key = element_rect(fill = "transparent", color = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        legend.text = element_text(size = 8),
        panel.background = element_rect(fill = "transparent", color = "transparent"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "transparent", color = "transparent"),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(margin = unit(c(0.5,0.5,0.5,0.5), "cm")),
        axis.text.y = element_text(margin = unit(c(0.5,0.5,0.5,0.5), "cm")),
        axis.ticks.length = unit(-0.1, "cm"),
        axis.ticks = element_line(color = "black"),
        text = element_text(size = 9, color = "black"),
        plot.margin = margin(1,1,0.5,0.5, "cm")
  )
# fig_col

tiff(file.path(datadir, '/figures/slr-pop-risk-columns.tiff'), width=5, height=5, units="in", res = 150, compression = 'lzw')
fig_col
dev.off()





