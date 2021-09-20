rm(list=ls())

library(tidyverse)
library(magrittr)
library(sf)
library(nngeo)
library(janitor)
library(tictoc)
library(tmap)

# ---------------------------------------------------------
# Load data
# ---------------------------------------------------------
setwd("/Users/jeppeviero/Dropbox/02 PhD/18 tutorials/intersections_points")

# ----- Load charging station data
chargers <- st_read("points_in/chargers/opladning.gpx")

st_crs(chargers)

# ----- Load zones data
zones <- st_read("points_in/zones/new",
                 layer = "Alle",
                 options = "ENCODING=UTF8")

st_crs(zones)

zones <- zones %>% 
  st_transform(4326)

# ----- Load municipality data
muni <- st_read(dsn = "kommune_shapes",
                layer = "Kommuner")

st_crs(muni)

muni <- muni %>% 
  st_transform(4326)

st_crs(muni) == st_crs(chargers) # TRUE

# ---------------------------------------------------------
# Create municipality-specific maps

# NB: add alt text
# https://twitter.com/hadleywickham/status/1394995611427029000
# https://medium.com/nightingale/writing-alt-text-for-data-visualization-2a218ef43f81
# ---------------------------------------------------------
# ----- Split municipality data to a list
muni_list <- split(muni,
                   f = muni$NAME)

municipality <- muni %>% 
  filter(NAME == "København")

muni_chargers <- chargers %>% 
  st_intersection(municipality)

muni_zones <- zones %>% 
  st_crop(st_geometry(municipality))


# ----- Define function
fun_muni <- function(municipality,
                     muni_chargers,
                     #muni_zones,
                     muni_extent,
                     xmin,
                     ymin,
                     xmax,
                     ymax,
                     muni_name,
                     muni_plot) {
  
  muni_chargers <- chargers %>% 
    st_intersection(municipality)
  
  # muni_zones <- zones %>% 
  #   st_intersection(municipality)
  
  muni_extent <- st_bbox(municipality)
  xmin <- muni_extent[1] %>% unclass() %>% as.numeric()
  ymin <- muni_extent[2] %>% unclass() %>% as.numeric()
  xmax <- muni_extent[3] %>% unclass() %>% as.numeric()
  ymax <- muni_extent[4] %>% unclass() %>% as.numeric()
  
  muni_name <- municipality$NAME[1]
  
  muni_plot <- ggplot() +
    geom_sf(data = muni, fill = "grey90", alpha = 0.8) +
    geom_sf(data = subset(zones, NAME )) +
    geom_sf(data = muni_zones) +
    geom_sf(data = municipality, fill = "red", alpha = 0.3) +
    geom_sf(data = muni_chargers,
            size = 2.5,
            shape = 21,
            fill = "black",
            alpha = 0.3,
            color = "black") +
    geom_sf(data = muni_chargers,
            size = 2.5,
            shape = 21,
            fill = NA,
            alpha = 1.0,
            color = "black") +
    coord_sf(xlim = c(xmin, xmax),
             ylim = c(ymin, ymax)) +
    #ggtitle(muni_name) +
    labs(title = muni_name,
         subtitle = paste("Ladestationer i",
                     muni_name,
                     "Kommune pr. 29. juli 2021",
                     sep = " ")) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          plot.title = element_text(face = "bold"),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
  return(muni_plot)
  
}

plot_list <- purrr::map(muni_list, fun_muni)

plot_list[[23]]
