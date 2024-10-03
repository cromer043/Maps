################################################################################
### This script takes state/county shapefiles from the Census Bureau, and moves
### Alaska, Hawaii, Puerto Rico, US Virgin Islands, Guam, Samoa, and the Mariana
### Islands to be just under the continental US.   This format is very useful for
### mapping data.
###
### I originally used the "state_laea" and "county_laea" maps from the 
### "tidycensus" package, but kept hitting various issues:
###
### 1) The Tidycensus maps are low-resolution, so counties don't look good if 
###    you zoom in on a particular state
###
### 2) The "Tidycensus" maps are old and require various FIPS mods every time I 
###    use them, such as changing fips 46113 -> 46102 to handle Oglala Co SD, 
###    changing fips 02270 -> 02158 to handle Kusilvak Census Area, AK
###
### 3) The Tidycensus map lacked the smaller US territories.
###
### I took inspiration for this script from the URL below:
###
### https://rud.is/b/2014/11/16/moving-the-earth-well-alaska-hawaii-with-r/
###
### If you find any bugs or issues with the script or with the maps it generates
### please let me know - /u/MetricT
###
### KNOWN BUGS:
###
### * Sourcing the script will cause errors at these lines near the bottom:
###
###    > state_map  %>% st_as_sf() %>% write_sf(mod_state_map)
###    There were 50 or more warnings (use warnings() to see the first 50)
###    > county_map %>% st_as_sf() %>% write_sf(mod_county_map)
###    There were 50 or more warnings (use warnings() to see the first 50)
###
###   Just run them manually at that point and it should work fine.
################################################################################

library(tidyverse)
library(maptools)
library(mapproj)
#library(rgeos)
#library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(sf)
library(tigris)
tigris::states
setwd('V:/Carl/AI/03.24 Map table recreation')

### Path/filename to save modified shapefiles at when we're done
mod_state_map <- tigris::states(cb = TRUE)
mod_county_map <- tigris::core_based_statistical_areas(year = 2019, cb = TRUE)
### We want our map to use the Albers projection
map_crs <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
#map_crs <- "epsg:5070"

state_map <-mod_state_map %>% 
  st_transform(map_crs) %>%
  as(Class = "Spatial")

county_map <- mod_county_map %>% 
  st_transform(map_crs) %>%
  as(Class = "Spatial")

state_map@data$id  <- rownames(state_map@data)
county_map@data$id <- rownames(county_map@data)

### We want to strip out states/territories that aren't in the continental US
### and move them around to make a more convenient map
non_conus_fips <- c("02", "15", "60", "66", "69", "72", "78")

################################################################################
### Adjust the state-level map
################################################################################

### Alaska
alaska_state <- state_map[state_map$STATEFP=="02",]
alaska_state <- elide(alaska_state, rotate=-50)
alaska_state <- elide(alaska_state, scale=max(apply(bbox(alaska_state), 1, diff)) / 1.8)
alaska_state <- elide(alaska_state, shift=c(-2400000, -2800000))
proj4string(alaska_state) <- proj4string(state_map)

### Hawaii
hawaii_state <- state_map[state_map$STATEFP=="15",]
hawaii_state <- elide(hawaii_state, rotate=-35)
hawaii_state <- elide(hawaii_state, shift=c(5800000, -1900000))
proj4string(hawaii_state) <- proj4string(state_map)

### Puerto Rico
puertorico_state <- state_map[state_map$STATEFP=="72",]
puertorico_state <- elide(puertorico_state, rotate=+13)
puertorico_state <- elide(puertorico_state, scale=max(apply(bbox(puertorico_state), 1, diff)) / 0.5)
puertorico_state <- elide(puertorico_state, shift=c(+600000, -2600000))
proj4string(puertorico_state) <- proj4string(state_map)

### US Virgin Islands
usvi_state <- state_map[state_map$STATEFP=="78",]
usvi_state <- elide(usvi_state, rotate=+13)
usvi_state <- elide(usvi_state, scale=max(apply(bbox(usvi_state), 1, diff)) / 0.25)
usvi_state <- elide(usvi_state, shift=c(+1500000, -2600000))
proj4string(usvi_state) <- proj4string(state_map)

### Guam
guam_state <- state_map[state_map$STATEFP=="66",]
guam_state <- elide(guam_state, rotate=-65)
guam_state <- elide(guam_state, scale=max(apply(bbox(guam_state), 1, diff)) / 0.15)
guam_state <- elide(guam_state, shift=c(+1200000, -3200000))
proj4string(guam_state) <- proj4string(state_map)

### Northern Mariana Islands
noma_state <- state_map[state_map$STATEFP=="69",]
noma_state <- elide(noma_state, rotate=-55)
noma_state <- elide(noma_state, scale=max(apply(bbox(noma_state), 1, diff)) / 0.85)
noma_state <- elide(noma_state, shift=c(+300000, -3400000))
proj4string(noma_state) <- proj4string(state_map)

### American Samoa
amsam_state <- state_map[state_map$STATEFP=="60",]
amsam_state <- elide(amsam_state, rotate=-55)
amsam_state <- elide(amsam_state, scale=max(apply(bbox(amsam_state), 1, diff)) / 0.25)
amsam_state <- elide(amsam_state, shift=c(-2300000, -3400000))
proj4string(amsam_state) <- proj4string(state_map)

### Add the moved states/territories back to the CONUS map
state_map <- state_map[!state_map$STATEFP %in% non_conus_fips,]
state_map <- rbind(state_map, 
                   alaska_state, 
                   hawaii_state, 
                   puertorico_state,
                   usvi_state, 
                   noma_state, 
                   guam_state, 
                   amsam_state
)

################################################################################
### Adjust the county-level map
################################################################################

### Alaska
alaska_county <- county_map[str_detect(county_map$NAME, ', AK')==T,]
alaska_county <- elide(alaska_county, rotate=-50)
alaska_county <- elide(alaska_county, scale=max(apply(bbox(alaska_county), 1, diff)) / 1.8)
alaska_county <- elide(alaska_county, shift=c(-2400000, -2800000))
proj4string(alaska_county) <- proj4string(county_map)

### Hawaii
hawaii_county <- county_map[str_detect(county_map$NAME, ', HI')==T,]
hawaii_county <- elide(hawaii_county, rotate=-35)
hawaii_county <- elide(hawaii_county, shift=c(5800000, -1900000))
proj4string(hawaii_county) <- proj4string(county_map)

### Puerto Rico
puertorico_county <- county_map[str_detect(county_map$NAME, ', PR')==T,]
puertorico_county <- elide(puertorico_county, rotate=+13)
puertorico_county <- elide(puertorico_county, scale=max(apply(bbox(puertorico_county), 1, diff)) / 0.5)
puertorico_county <- elide(puertorico_county, shift=c(+600000, -2600000))
proj4string(puertorico_county) <- proj4string(county_map)

### US Virgin Islands
usvi_county <- county_map[str_detect(county_map$NAME, ', VI')==T,]
usvi_county <- elide(usvi_county, rotate=+13)
usvi_county <- elide(usvi_county, scale=max(apply(bbox(usvi_county), 1, diff)) / 0.25)
usvi_county <- elide(usvi_county, shift=c(+1500000, -2600000))
proj4string(usvi_county) <- proj4string(county_map)

### Guam
guam_county <- county_map[str_detect(county_map$NAME, ', GU')==T,]
guam_county <- elide(guam_county, rotate=-65)
guam_county <- elide(guam_county, scale=max(apply(bbox(guam_county), 1, diff)) / 0.15)
guam_county <- elide(guam_county, shift=c(+1200000, -3200000))
proj4string(guam_county) <- proj4string(county_map)

### Northern Mariana Islands
noma_county <- county_map[str_detect(county_map$NAME, ', MP')==T,]
noma_county <- elide(noma_county, rotate=-55)
noma_county <- elide(noma_county, scale=max(apply(bbox(noma_county), 1, diff)) / 0.85)
noma_county <- elide(noma_county, shift=c(+300000, -3400000))
proj4string(noma_county) <- proj4string(county_map)

### American Samoa
amsam_county <- county_map[str_detect(county_map$NAME, ', AS')==T,]
amsam_county <- elide(amsam_county, rotate=-55)
amsam_county <- elide(amsam_county, scale=max(apply(bbox(amsam_county), 1, diff)) / 0.25)
amsam_county <- elide(amsam_county, shift=c(-2300000, -3400000))
proj4string(amsam_county) <- proj4string(county_map)

### Add the moved states/territories back to the CONUS map
county_map <- county_map[str_detect(county_map$NAME, ', AS|, MP|, GU|, VI|, PR|, AK|, HI')==F,]
county_map <- rbind(county_map, 
                    alaska_county, 
                    hawaii_county, 
                    puertorico_county,
                    usvi_county, 
                    noma_county, 
                    guam_county, 
                    amsam_county
)

################################################################################
### Save maps, reload, and graph to make sure it worked ok
###############################################################################

### Save modified maps to new shapefile
state_map  %>% st_as_sf() %>% write_sf("state_map.shp")
county_map %>% st_as_sf() %>% write_sf('county_map.shp')

### Load our newly created maps
state_map <- read_sf('state_map.shp')
county_map <- read_sf('county_map.shp')

### Overlay the state/county maps so we can verify that the map looks correct
ggplot() +
  theme_void() +
  geom_sf(data = county_map, size = 0.1) +
  geom_sf(data = state_map,  size = 0.6, fill = NA)