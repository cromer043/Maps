#March 2024
#Carl Romer
#This file
###################################################
#Setup
###################################################

library(tidyverse)
library(sf)
library(scales)
library(tidycensus)
library(ggrepel)
library(tigris)
###################################################
#Data importation
###################################################

setwd('V:/Carl/AI/03.24 Map table recreation')
### Load our newly created maps
state_map <- read_sf('state_map.shp')
county_map <- read_sf('county_map.shp')
ai_data <- read_csv('1. Data/100 Metros Gen AI job postings Jan 2023-Jan 2024.csv')

###################################################
#data cleaning
###################################################

ai_data10 <- ai_data %>% 
  slice_max(`Unique Postings from Jan 2023 - Feb 2024`, n = 11) %>% 
  select(-c(`Unique Postings from Jan 2023 - Jan 2024`,
            MSA))

write_csv(ai_data10,
          file = "2. Output/AI data top 10.csv")

ai_data <- ai_data %>% 
  slice_max(`Unique Postings from Jan 2023 - Feb 2024`, n = 50)
###################################################
#graphs
###################################################

map_merged <- inner_join( 
  county_map %>% 
    mutate(GEOID = as.numeric(GEOID)),
  ai_data %>% 
    mutate(GEOID = as.numeric(MSA)))

map <- ggplot()+
  geom_sf(color = alpha("white"), 
          fill = "grey", 
          
          data = state_map[!state_map$STATEFP %in% c(non_conus_fips <- c("60", "66", "69", "72", "78")),])+
  geom_point(
    aes(size = `Unique Postings from Jan 2023 - Feb 2024`,
        geometry = geometry),
    color = "white",
    fill = "#00649f",
    stat = "sf_coordinates",
    position = "identity",
    shape = 21,
    data = map_merged 
  )+
  scale_size_continuous(labels = scales::comma,
                        range = c(2,15),
                        breaks = c(150, 500, 1000, 2000))+ 
labs(
  size = "Number of postings",
  title = "Active Title Me",
  subtitle = "Top 30 metros generative AI job postings Jan. 2023-Feb. 2024",
  caption = "Source: Brookings analysis of 2023-2024 Lightcast jobs postings data"
)+
  coord_sf(crs = st_crs(9311))+
  theme_void() 

ggsave(plot = map, 
       filename = "2. Output/Map.pdf", 
       width = 10, 
       height = 6, 
       device = "pdf")
