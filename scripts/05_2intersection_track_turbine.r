
#libraries
library(tidyverse)
library(sf)
library(mapview)
library(lubridate)

# show tibbles with all the columns!
options(dplyr.width = Inf)

windturbines <- read_sf("C:/Users/daank/OneDrive - University of Twente/Documents/Rijksuniversiteit Groningen/Year 1 - 24-25/Flyway Ecology/Mini project - Gull migration wind farms/QGIS_FE2024/overpass_turbo_single_turbines.kml")


############################################
#Loading of overall combined data set
combined_data_csv <- read_csv("C:/Users/daank/OneDrive - University of Twente/Documents/Rijksuniversiteit Groningen/Year 1 - 24-25/Flyway Ecology/Mini project - Gull migration wind farms/Bird tracks/combined_gps_tracks.csv") |>
  #Filter for the Iberian region only, latitude between 45.1 and 34.8 and longitude between -10.2 and 4.9
  dplyr::filter(Latitude > 34.8, Latitude < 45.1, Longitude > -10.2, Longitude < 4.9) |>
  dplyr::filter(Latitude > 0) #Filter out the empty points

#Make the CSV file spatial
combined_data_csv_sf <- st_as_sf(x = combined_data_csv, 
                                 coords = c("Longitude", "Latitude"),
                                 crs = 4326)

############################################
#Original script

# make line between points
combined_data_csv_sf_line <- combined_data_csv_sf %>%
  group_by(device_id) %>%          # Group data by bird ID
  summarise(do_union = FALSE) %>%  # Prevent merging of geometries
  st_cast("LINESTRING")            # Cast points to lines within each group 

# make buffer around windturbines
windturbines_500m <- st_buffer(intersection_5000, 500)
windturbines_1000m <- st_buffer(intersection_5000, 1000)
windturbines_2000m <- st_buffer(intersection_5000, 2000)

combined_data_csv_sf_5000 <- st_buffer(combined_data_csv_sf_line, 5000)

# intersection between buffer of individual and windturbines
#intersection_5000 <- st_intersection(combined_data_csv_sf_5000, windturbines)
intersection_5000 <- read_csv("C:/Users/daank/OneDrive - University of Twente/Documents/Rijksuniversiteit Groningen/Year 1 - 24-25/Flyway Ecology/Mini project - Gull migration wind farms/Wind turbines next to bird track/intersection_5000.csv")


insec_tracks_turbines_500 <- st_intersection(combined_data_csv_sf_line, 
                                             windturbines_500m)
insec_tracks_turbines_1000 <- st_intersection(combined_data_csv_sf_line, 
                                              windturbines_1000m)
insec_tracks_turbines_2000 <- st_intersection(combined_data_csv_sf_line, 
                                              windturbines_2000m)

#### plot the interactive maps with individual tracks and wind turbines ####
combined_data_csv_sf_5000$device_id <- as.factor(combined_data_csv_sf_5000$device_id)
combined_data_csv_sf_line$device_id <- as.factor(combined_data_csv_sf_line$device_id)
mapview(combined_data_csv_sf_line, zcol = "device_id")

# lines, wind turbines and the intersection turbines
mapview(combined_data_csv_sf_line, zcol = "device_id", map.types = "CartoDB.Positron") + # map.types sets the theme to light theme
  mapview(windturbines, col.regions = "red", color = "red",alpha = 0.2, cex = 2)+ 
  mapview(intersection_5000)

intersection_5000$device_id <- as.factor(intersection_5000$device_id)

# buffers, wind turbines and the intersection turbines
mapview(combined_data_csv_sf_5000, zcol = "device_id", map.types = "CartoDB.Positron") + # map.types sets the theme to light theme
  mapview(windturbines, col.regions = "red", color = "red",alpha = 0.2, cex = 2)+ 
  mapview(intersection_5000, zcol = "device_id")

insec_tracks_turbines_1000$device_id <- as.factor(insec_tracks_turbines_1000$device_id)
mapview(insec_tracks_turbines_1000, zcol = "device_id", map.types = "CartoDB.Positron") 
#  mapview(windturbines_1000m, col.regions = "red", color = "red",alpha = 0.5, cex = 2) +
#  mapview(combined_data_csv_sf_line)


################################################
# Calculate how many turbines each bird encounters along its route through Spain
turbine_encounter <- intersection_5000 |>
  mutate(turbine = 1) |>
  group_by(device_id) |>
  summarise(count = sum(turbine)) |>
  arrange(desc(count)) |>
  mutate(migration_route = case_when(
    device_id %in% c("211293", "211280") ~ "Mediterranean",
    device_id %in% c("211284", "211292", "201098", "211284", "211291", 
                     "211294", "211296", "201110") ~ "Mainland Spain",
    device_id %in% c("201097", "201103", "211276", "211277", "211279", 
                     "211283", "211289", "211290") ~ "Atlantic coast")) |>
  dplyr::group_by(migration_route) |>
  summarise(mean_n_turbines = mean(count))
turbine_encounter










