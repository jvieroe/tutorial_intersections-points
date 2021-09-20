rm(list=ls())

# library(devtools)
# if(!require("devtools")) install.packages("devtools")
# devtools::install_github("sebastianbarfort/mapDK", force = T)

library(tidyverse)
library(magrittr)
library(sf)
library(nngeo)
library(janitor)
library(tictoc)
library(tmap)
library(mapDK)

set_crs <- 3035

# ---------------------------------------------------------
# Load data
# ---------------------------------------------------------
setwd("/Users/jeppeviero/Dropbox/02 PhD/18 tutorials/intersections_points")

# ----- Load charging station data
chargers <- st_read("points_in/chargers/opladning.gpx") %>% 
  st_transform(set_crs)

st_crs(chargers)

# ----- Load zones data
zones <- st_read("points_in/zones/new",
                 layer = "Alle",
                 options = "ENCODING=UTF8") %>% 
  st_transform(set_crs)

st_crs(zones)

# ----- Plot it!
ggplot() + 
  geom_sf(data = zones) +
  geom_sf(data = chargers) # No zones on Bornholm, otherwise very good!


# ---------------------------------------------------------
# Match charging stations to zones using exact matching
# ---------------------------------------------------------

# ----- What zone information do we cant at the charging station level?
head(zones, 10)
# We probably want 'Namn' and 'Id'

# ----- Use st_join with st_intersects
joins <- st_join(chargers, zones,
                 join = st_intersects)

# ----- Check if any charging stations didn't match
tabyl(is.na(joins$Namn)) # 18 NAs (~ 2 pct.)

joins <- joins %>% 
  mutate(is_na = ifelse(is.na(Namn), "Yes", "No"))

ggplot() + 
  geom_sf(data = zones) +
  geom_sf(data = joins, aes(color = is_na)) 

# it seems to only make be the ones on Bornholm. That makes sense

ggplot() + 
  geom_sf(data = zones) +
  geom_sf(data = subset(joins, is_na == "Yes"), color = "red") 

# there is actully one other that should have been matched
# It's close to the coast so probably just an issue of small error in the coordinates

tic()
joins %>% select(name, Namn) %>% group_by(Namn) %>% tally()
toc()

joins2 <- joins %>% as.data.frame() %>% dplyr::select(-geometry)

tic()
joins2 %>% select(name, Namn) %>% group_by(Namn) %>% tally()
toc()
# ---------------------------------------------------------
# Match charging stations to zones using distance threshold
# ---------------------------------------------------------
# When working with distance calculations, some projections (CRS's) make the proces much faster!
# It's not a huge issue here but consider changing it
# The 3035 projection didn't help much so I have commented it out but give other CRS's a shot if you want

# # ----- Define new CRS
# set_crs <- 3035
# 
# chargers <- chargers %>% 
#   st_transform(set_crs)
# 
# zones <- zones %>% 
#   st_transform(set_crs)

st_crs(chargers)
st_crs(zones)

# both have length units = meters (NB: they need to have the same CRS so I shouldn't have to check twice...)

# ----- Use st_join with st_nn
max_dist <- 10 # let's say 10 km

joins_vol2 <- st_join(chargers,
                      zones,
                      join = st_nn, # nngeo::st_nn()
                      k = 1, # match to the 1 nearest feature (polygon)
                      maxdist = max_dist*10^3, # 10,000 = 10km (units are in meters)
                      progress = T)

tabyl(is.na(joins_vol2$Namn)) # hopefully this is 17 NAs
# 17 NAs!

joins_vol2 <- joins_vol2 %>% 
  mutate(is_na = ifelse(is.na(Namn), "Yes", "No"))

ggplot() + 
  geom_sf(data = zones) +
  geom_sf(data = subset(joins_vol2, is_na == "Yes"), color = "red") 

# Voila!

# ---------------------------------------------------------
# Custom plotting
# ---------------------------------------------------------
# ----- tmap allows for interactive viewing. Nice for dashboards!
tmap_mode("view")
tm_shape(zones) +
  tm_polygons(col = "red", alpha = 0.3) +
  tm_shape(joins_vol2) +
  tm_dots(col = "black", size = 0.001)

# ----- With ggplot and zooming
# coord_sf(), the zooming, did not work properly with the new CRS. Set it to 4326 again
temp1 <- zones %>% 
  st_transform(4326)

temp2 <- joins_vol2 %>% 
  st_transform(4326)

ggplot() +
  geom_sf(data = temp1, size = 0.35,
          fill = "grey80",
          alpha = 0.75) +
  geom_sf(data = temp2,
          color = "black",
          size = 1.0) +
  coord_sf(xlim = c(12.00, 12.7),
           ylim = c(55.55, 56.0)) +
  theme_minimal() +
  theme(panel.grid = element_blank())

# ---------------------------------------------------------
# Municipality data
# ---------------------------------------------------------
muni <- st_read(dsn = "kommune_shapes",
                layer = "Kommuner")

st_crs(muni)

muni <- muni %>% 
  st_transform(4326)

nrow(muni)

ggplot() +
  geom_sf(data = muni)

tmap_mode("view")
tm_shape(muni) +
  tm_polygons(col = "red", alpha = 0.3)

temp <- muni %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)

# ---------------------------------------------------------
# Crop zones + chargers to Copenhagen municipality
# ---------------------------------------------------------
cph <- muni %>% 
  filter(NAME == "Roskilde")

# tmap_mode("view")
# tm_shape(cph) +
#   tm_polygons(col = "red", alpha = 0.3)

chargers <- chargers %>% 
  st_transform(4326)

cph_chargers <- chargers %>% 
  st_intersection(cph)

# tmap_mode("view")
# tm_shape(cph) +
#   tm_polygons(col = "red", alpha = 0.3) +
#   tm_shape(cph_chargers) +
#   tm_dots()

st_bbox(cph) # get zoom dimension

muni_extent <- st_bbox(cph)
xmin <- muni_extent[1] %>% unclass() %>% as.numeric()
ymin <- muni_extent[2] %>% unclass() %>% as.numeric()
xmax <- muni_extent[3] %>% unclass() %>% as.numeric()
ymax <- muni_extent[4] %>% unclass() %>% as.numeric()

ggplot() +
  geom_sf(data = muni, fill = "grey90", alpha = 0.8) +
  geom_sf(data = cph, fill = "red", alpha = 0.3) +
  geom_sf(data = cph_chargers,
          size = 2.5,
          shape = 21,
          fill = "black",
          alpha = 0.3,
          color = "black") +
  geom_sf(data = cph_chargers,
          size = 2.5,
          shape = 21,
          fill = NA,
          alpha = 1.0,
          color = "black") +
  coord_sf(xlim = c(xmin, xmax),
           ylim = c(ymin, ymax)) +
  theme_minimal() +
  theme(panel.grid = element_blank())
  




# ---------------------------------------------------------
# Use the mapDK package
# ---------------------------------------------------------
# mapDK()
# 
# muni <- rio::import("municipality.rda")
# tabyl(muni$id)
# 
# class(muni)
# head(muni, 10)
# 
# muni <- muni %>% 
#   st_as_sf(coords = c("long", "lat"),
#            crs = 4326)
# 
# polys <- st_sf(
#   aggregate(
#     muni$geometry,
#     list(muni$group),
#     function(g){
#       st_cast(st_combine(g),"POLYGON")
#     }
#   ))
# 
# polys <- polys %>% 
#   rename(group = Group.1)
# 
# temp <- muni %>% 
#   as.data.frame() %>% 
#   dplyr::select(c(id, group))
# 
# muni_sf <- polys %>% 
#   left_join(.,
#             temp,
#             by = "group")
# 
# rm(temp, muni)
# 
# muni_sf <- muni_sf %>% 
#   st_cast("MULTIPOLYGON")
# 
# class(muni_sf)
# class(muni_sf$geometry)
# 
# ggplot() +
#   geom_sf(data = muni_sf,
#           aes(fill = id)) +
#   coord_sf(xlim = c(12.00, 12.7),
#            ylim = c(55.55, 56.0))


