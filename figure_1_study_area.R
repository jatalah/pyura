library(ggrepel)
library(ggmap)
library(ggspatial)
library(mapdata)
library(ggplot2)
library(tidyverse)
library(cowplot)
source('theme_javier.R')

# get nz maps---
nz_high <- map_data('nzHires')
nz_low <- map_data('nz')

# map of whole nz---
nz_map_all <- 
  ggplot(nz_low) +
  geom_polygon(aes(x = long, y = lat, group = group),
               colour = "black",
               fill = 'grey80') + 
  theme_void() +
  coord_sf(
  xlim = c(166, 178.8),
  ylim = c(-47.35, -34.35), crs = 4326
)  +
  annotation_scale(style = 'ticks', location = 'br') +
  # geom_rect(xmin = 172.348022, xmax = 174.396973,
  #           ymin = -35.600607, ymax = -34.324066, fill = 'transparent', color = 1) +
  theme(
    # axis.title.x = element_blank(),
    # axis.title.y = element_blank(),
    # axis.text.x = element_blank(),
    # axis.ticks.x = element_blank(),
    # axis.text.y = element_blank(),
    # axis.ticks.y = element_blank(),
    panel.border = element_rect(colour = "gray30", fill=NA, size=.5),
    # panel.background = element_blank()
  ) 
nz_map_all

# northland map----
northland_map <-
  nz_high %>%
  filter(lat > -35.7 & lat < -34 & long > 172.5 & long < 174.6) %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group),
               colour = "black",
               fill = 'grey80') +
  theme_javier() +
  coord_sf(
    xlim = c(172.348022, 174.396973),
    ylim = c(-35.600607,-34.324066),
    crs = 4326
  ) +
  annotation_scale(style = 'ticks', location = 'br') +
  annotation_north_arrow(location = "tl", style = north_arrow_orienteering) +
  # geom_rect(
  #   xmin = 173.05,
  #   xmax = 173.15,
  #   ymin = -35.225,
  #   ymax = -35.15,
  #   fill = 'transparent',
  #   color = 1
  # ) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank())
  
# Tauroa peninsula---
sites <- 
  tibble(
    lat = c(-35.167,-35.171344, -35.16797),
    lon = c(173.086, 173.107281, 173.154144),
    Site = c("Shipwreck", "Koutau", "Ahipara"),
    Type = c("Site", "Site", "Place")
  )

tauroa_map <-
  nz_high %>%
  filter(lat > -35.3 & lat< -35.1 & long<173.3 & long>172 ) %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group),
               colour = "black",
               fill = 'grey80') +
  theme_void() +
  coord_sf(
    xlim = c(173.05, 173.15),
    ylim = c(-35.225,-35.15),
    crs = 4326
  ) +
  geom_point(data = sites,
             aes(y = lat , x = lon, shape = Type),
             size = 3) +
  # geom_text_repel(data = sites,
  #                 aes(y = lat , x = lon, label = Site),
  #                 size = 4) +
  scale_shape(guide = NULL) +
  annotation_scale(style = 'ticks', location = 'br') +
  # geom_text(aes(y = -35.19 , x = 173.1, label = "Tauroa Peninsula"),
  #           size = 4) +
  theme(panel.border = element_rect(colour = "gray30", fill=NA, size=.5)) 

# 
# figure_1 <- 
#   northland_map +
#   annotation_custom(
#     grob = ggplotGrob(tauroa_map),
#     xmin = 172.19,
#     xmax = 173,
#     ymin = -35.7,
#     ymax = -35.1
#   ) +
#   annotation_custom(
#     grob = ggplotGrob(nz_map_all),
#     xmin = 173.5,
#     xmax = 174.8,
#     ymin = -34.95,
#     ymax = -34.24
#   ) 



# ggsave(figure_1, 
#        filename = 'figures/figure_1_map.svg',
#        width = 8,
#        height = 4.95)



# using cowplot---

figure_1 <- 
  ggdraw(northland_map) +
  draw_plot(tauroa_map, width = 0.35, height = .35  , 
            x = 0.09, y = 0.055) +
  draw_plot(nz_map_all, width = .4, height = .4, 
            x = .61, y = .582)
figure_1

ggsave(figure_1, 
       filename = 'figures/figure_1_map.tiff',
       device = 'tiff',
       compression = 'lzw',
       width = 12,
       height = 7.5,
       dpi = 600)

ggsave(figure_1, 
       filename = 'figures/figure_1_map.svg',
       width = 12,
       height = 7.5)

ggsave(figure_1, 
       filename = 'figures/figure_1_map.eps',
       width = 12,
       height = 7.5)

ggsave(figure_1, 
       filename = 'figures/figure_1_map.pdf',
       width = 12,
       height = 7.5)

# postscript('figures/figure_1_map.eps',
#            width = 12,
#            height = 7.5)
# print(figure_1)
# dev.off()

