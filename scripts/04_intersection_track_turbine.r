
#libraries
library(dplyr)
library(tidyverse)
library(sf)
library(mapview)
library(lubridate)

# show tibbles with all the columns!
options(dplyr.width = Inf)

######## Read file (one example) 
klm_211282 <- read_csv("C:/Users/jeroe/OneDrive/Documenten/RUG/Master/FE Flyway Ecology/Mini_project_windturbines/tracks/211282_010821-310322.txt")
klm_211307 <- read_csv("C:/Users/jeroe/OneDrive/Documenten/RUG/Master/FE Flyway Ecology/Mini_project_windturbines/tracks/211307_010821-310322.txt")
klm_211093 <- read_csv("C:/Users/jeroe/OneDrive/Documenten/RUG/Master/FE Flyway Ecology/Mini_project_windturbines/tracks/201093_20220331_134700.csv")
klm_211096 <- read_csv("C:/Users/jeroe/OneDrive/Documenten/RUG/Master/FE Flyway Ecology/Mini_project_windturbines/tracks/201096_20220331_000000.csv")
klm_211097 <- read_csv("C:/Users/jeroe/OneDrive/Documenten/RUG/Master/FE Flyway Ecology/Mini_project_windturbines/tracks/201097_20220331_000000.csv")
klm_211098 <- read_csv("C:/Users/jeroe/OneDrive/Documenten/RUG/Master/FE Flyway Ecology/Mini_project_windturbines/tracks/201098_20220331_000000.csv")
klm_211103 <- read_csv("C:/Users/jeroe/OneDrive/Documenten/RUG/Master/FE Flyway Ecology/Mini_project_windturbines/tracks/201103_20220331_000000.csv")
klm_211110 <- read_csv("C:/Users/jeroe/OneDrive/Documenten/RUG/Master/FE Flyway Ecology/Mini_project_windturbines/tracks/201110_20220331_000000.csv")
klm_211112 <- read_csv("C:/Users/jeroe/OneDrive/Documenten/RUG/Master/FE Flyway Ecology/Mini_project_windturbines/tracks/201112_20220331_000000.csv")
klm_211113 <- read_csv("C:/Users/jeroe/OneDrive/Documenten/RUG/Master/FE Flyway Ecology/Mini_project_windturbines/tracks/201113_20220331_000000.csv")

windturbines <- read_sf("C:/Users/jeroe/OneDrive/Documenten/RUG/Master/FE Flyway Ecology/Mini_project_windturbines/export.kml") 

# make dates
klm_211282$UTC_datetime <- dmy_hms(klm_211282$UTC_datetime)
klm_211307$UTC_datetime <- dmy_hms(klm_211307$UTC_datetime) 
klm_211093$UTC_datetime <- dmy_hms(klm_211093$UTC_datetime)

# make spatial
klm_211282_sf <- st_as_sf(x = klm_211282, 
                   coords = c("Longitude2", "Latitude2"),
                   crs = 4326) 
klm_211307_sf <- st_as_sf(x = klm_211307, 
                   coords = c("Longitude2", "Latitude2"),
                   crs = 4326)
klm_211093_sf <- st_as_sf(x = klm_211093,
                   coords = c("Longitude", "Latitude"),
                   crs = 4326)
klm_211096_sf <- st_as_sf(x = klm_211096,
                   coords = c("Longitude", "Latitude"),
                   crs = 4326)
klm_211097_sf <- st_as_sf(x = klm_211097,
                   coords = c("Longitude", "Latitude"),
                   crs = 4326)
klm_211098_sf <- st_as_sf(x = klm_211098,
                   coords = c("Longitude", "Latitude"),
                   crs = 4326)
klm_211103_sf <- st_as_sf(x = klm_211103,
                   coords = c("Longitude", "Latitude"),
                   crs = 4326)
klm_211110_sf <- st_as_sf(x = klm_211110,
                   coords = c("Longitude", "Latitude"),
                   crs = 4326)
klm_211112_sf <- st_as_sf(x = klm_211112,
                   coords = c("Longitude", "Latitude"),
                   crs = 4326)
klm_211113_sf <- st_as_sf(x = klm_211113,
                   coords = c("Longitude", "Latitude"),
                   crs = 4326)

# remove empty points (Latitude = 0)
klm_211282 <- klm_211282 %>% filter(Latitude2 >0)
klm_211282_sf <- klm_211282_sf %>% filter(Latitude >0)
mapview(klm_211282_sf)

klm_211307 <- klm_211307 %>% filter(Latitude2 >0)
klm_211307_sf <- klm_211307_sf %>% filter(Latitude >0)
mapview(klm_211307_sf)

# make line between points
klm_211282_sf_line <- klm_211282_sf %>%
  summarise(do_union = F) %>%
  st_cast("LINESTRING") 
klm_211307_sf_line <- klm_211307_sf %>%
  summarise(do_union = F) %>%
  st_cast("LINESTRING") 
klm_211093_sf_line <- klm_211093_sf %>%
  summarise(do_union = F) %>%
  st_cast("LINESTRING")

# make buffer around windturbines
windturbines_100m <- st_buffer(windturbines, 200)
windturbines_1000m <- st_buffer(windturbines, 1000)
windturbines_2000m <- st_buffer(windturbines, 2000)

klm_211307_10000 <- st_buffer(klm_211307_sf_line, 10000)
klm_211282_10000 <- st_buffer(klm_211282_sf_line, 10000)
klm_211093_10000 <- st_buffer(klm_211093_sf_line, 10000)


# intersection between 2000m buffer of individual and windturbines
intersection_10000_1 <- st_intersection(klm_211307_10000, windturbines)
intersection_10000_2 <- st_intersection(klm_211282_10000, windturbines)

#### plot the interactive maps with individual tracks and wind turbines ####
mapview(klm_211282_sf) + mapview(klm_211282_sf_line)
mapview(klm_211307_sf) + mapview(klm_211307_sf_line)

mapview(klm_211282_10000, color = "yellow",col.regions = "yellow", map.types = "CartoDB.Positron") + # map.types sets the theme to light theme
  mapview(klm_211307_10000, color = "blue") + 
  mapview(windturbines, col.regions = "red", color = "red",alpha = 0.2, cex = 2)+ 
  mapview(intersection_10000_1) + 
  mapview(intersection_10000_2)


windturbines_combined <- rbind(intersection_10000_1, intersection_10000_2)
mapview(windturbines_combined


