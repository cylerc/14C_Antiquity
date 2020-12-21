#create map of SEA with specific site locations from text identified
#create box around inset area in Mae Hong Son

library(here)
library(raster)
library(ggmap)
library(ggspatial)
library(ggrepel)
library(tidyverse)
# devtools::install_github('3wen/legendMap')
library(legendMap)
library(ggthemes)

map <-
  get_stamenmap(bbox = c(left = 90,
                         bottom = 5,
                         right = 115,
                         top = 25),
                zoom = 8)

site_locations_and_names <- 
  tribble(~site_name, ~lat, ~lon,
          "Sai-Yok", 98.75, 14.42,
          "Badahlin", 96.34, 21.13,
          "Doi Pha Kan", 99.46, 18.26
          )

regional_area_map <- 
ggmap(map)  +
  geom_point(data = site_locations_and_names,
             aes(lat, 
                 lon),
             size = 3,
             colour = "white") +
  geom_point(data = site_locations_and_names,
                  aes(lat, 
                      lon),
             size = 2,
             colour = "black") +
  geom_text_repel(data = site_locations_and_names,
                  aes(lat, 
                      lon,
                      label = site_name),
                  bg.color = "white",
                  bg.r = 0.1,
                  size = 4.5,
                  force = 50,
                  nudge_y = -1,
                  nudge_x = -1) + 
  annotate("rect",
           xmin = 97.5,
           xmax = 99,
           ymin = 18.5,
           ymax = 20,
           fill = NA,
           colour = "red",
           size = 0.75
  ) +
  theme_minimal(base_size = 6) +
  xlab("Longitude") +
  ylab("Latitude") +
  legendMap::scale_bar(
    # edit these numbers to select a suitable location
    # for the scale bar where it does not cover
    # important details on the map
    lon = 109.25,
    lat = 7.5,
    # text size on scale bar 
    legend_size = 2,
    # distance of one section of scale bar, in km
    distance_lon = 250,
    # height of the scale bar, in km
    distance_lat = 20,
    # distance between scale bar and units, in km
    distance_legend = 75,
    # units of scale bar
    dist_unit = "km",
    # add the north arrow
    orientation = TRUE,
    # length of N arrow, in km
    arrow_length = 200,
    # distance between scale bar & base of N arrow, in km
    arrow_distance = 150,
    # size of letter 'N' on N arrow, in km
    arrow_north_size = 8)

# Inset map

##----------------------------------------------------

# create inset hillshade for Mae Hong Son
# reference: https://stanford.edu/~vbauer/teaching/hillshade.html
# DEM source ("E47"): http://viewfinderpanoramas.org/Coverage%20map%20viewfinderpanoramas_org3.htm
# unzip and att "E47" folder to /data before proceeding

sites_coord <- readxl::read_excel(here("data/Sites.xlsx"))

unproj <- CRS("+proj=longlat +datum=WGS84")
files <- list.files(here("data/E47"), recursive=TRUE, full.names = TRUE)
rasters.list <- sapply(files, raster)
names(rasters.list) <- NULL
rasters.list$fun <- mean
mosaic <- do.call(mosaic, rasters.list)
# crop mosaic to inset area here to save time on later processing
mosaic_crop <- crop(mosaic,  extent(97.5, 99, 18.5, 20))
mosaic_crop_10 <- mosaic_crop * 10
slope <- terrain(mosaic_crop_10, opt="slope", unit='radians')
aspect <- terrain(mosaic_crop_10, opt="aspect", unit='radians')
hillshade <- hillShade(slope, aspect, angle=45, direction=315)
hillshade2 <- aggregate(hillshade , fact = 5 , method = "bilinear" )
hillshade2 <- focal(hillshade2, w=matrix(1/9, nc=3, nr=3), mean)

# convert for plotting with ggplot, from https://stackoverflow.com/a/33234951/1036500
hillshade2_spdf <- as(hillshade2, "SpatialPixelsDataFrame")
hillshade2_spdf_df <- as_tibble(hillshade2_spdf)
names(hillshade2_spdf_df) = c("hillshade", "x", "y")

# plot
hillshade_inset_plot <- 
ggplot() +
  geom_raster(data = hillshade2_spdf_df,
              aes(x = x, 
                  y = y,
                  fill = hillshade))  +
  geom_point(data = sites_coord,
             aes(y = Latitude,
                 x = Longitude),
             colour = "black",
             size = 1.5) +
  geom_point(data = sites_coord,
             aes(y = Latitude,
                 x = Longitude),
             colour = "white",
             size = 0.5) +
  geom_text_repel(data = sites_coord,
             aes(y = Latitude,
                 x = Longitude,
                 label = Site),
             colour = "white",
             size = 1.75,
             bg.colour = "black") +
  scale_fill_viridis_c() +
  coord_equal() +
  theme_minimal(base_size = 4) +
  labs(x = "Longitude", 
       y = "Latitude") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  # add a white background to the plot
  theme(plot.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "red", 
                                        size=2, 
                                        fill=NA),
        legend.key.width = unit(0.1, 'cm') #change legend key width
        )

#-------------------------------
# combine both plots

library(cowplot) 

ggdraw() +
  draw_plot(regional_area_map) +
  draw_plot(plot = hillshade_inset_plot,
            x = 0.24, # x location of inset placement
            y = 0.28, # y location of inset placement
           scale = 0.4 # Inset scale
  )

ggsave(here::here("figures/001-map.png"),
       width = 13.5,
       height = 10,
       units = "cm")




