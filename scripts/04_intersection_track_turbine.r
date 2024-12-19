
#libraries
library(tidyverse)
library(sf)
library(mapview)
library(lubridate)
library(patchwork)
library(emmeans)

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
# make line between points
combined_data_csv_sf_line <- combined_data_csv_sf %>%
  group_by(device_id) %>%          # Group data by bird ID
  summarise(do_union = FALSE) %>%  # Prevent merging of geometries
  st_cast("LINESTRING")            # Cast points to lines within each group 

# make buffer around windturbines
windturbines_500m <- st_buffer(intersection_5000, 500)
windturbines_1000m <- st_buffer(intersection_5000, 1000)
windturbines_2000m <- st_buffer(intersection_5000, 2000)

# make buffer around tracks
combined_data_csv_sf_5000 <- st_buffer(combined_data_csv_sf_line, 5000)
combined_data_csv_sf_2000 <- st_buffer(combined_data_csv_sf_line, 2000)
combined_data_csv_sf_1000 <- st_buffer(combined_data_csv_sf_line, 1000)

# Calculating intersection between buffers of individuals and windturbines
#Running takes a long time!!
intersection_5000 <- st_intersection(combined_data_csv_sf_5000, windturbines)
#Takes about 30 minutes to run
intersection_2000 <- st_intersection(combined_data_csv_sf_2000, windturbines)
#Takes more than 2 hours to run - ignored for mini project:
intersection_1000 <- st_intersection(combined_data_csv_sf_1000, windturbines)


#Running these commands take again a very long time. We could not run these commands
#for the mini project, due to the long time it takes to run
# insec_tracks_turbines_500 <- st_intersection(combined_data_csv_sf_line, 
#                                              windturbines_500m)
# insec_tracks_turbines_1000 <- st_intersection(combined_data_csv_sf_line, 
#                                               windturbines_1000m)
# insec_tracks_turbines_2000 <- st_intersection(combined_data_csv_sf_line, 
#                                               windturbines_2000m)

#### plot the interactive maps with individual tracks and wind turbines ####
combined_data_csv_sf_5000$device_id <- as.factor(combined_data_csv_sf_5000$device_id)
combined_data_csv_sf_line$device_id <- as.factor(combined_data_csv_sf_line$device_id)

line_buffer <- combined_data_csv_sf_5000
map1 <- mapview(line_buffer, zcol = "device_id") + mapview(windturbines, col.regions = "red", color = "red",alpha = 0.2, cex = 1)
map1

#Plot the above map and save it using mapshot
#First, once run the 2 commands below:
#install.packages("webshot")
#webshot::install_phantomjs()
#First run the below code line
mapviewOptions(fgb = FALSE)
#Now again run the map and then run the save command below
mapshot(map1,file = "C:/Users/daank/OneDrive - University of Twente/Documents/Github/flyway_gulls_windturbine/plots/mapview_buffered_lines_wturb.png",remove_controls = c("zoomControl", "layersControl","homeButton","drawToolbar", "easyButton"))

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


################################################ Statistics
# Calculate how many turbines each bird encounters along its route through Spain
# 5000m buffer around bird tracks
turbine_encounter_ind5 <- intersection_5000 |>
  mutate(turbine = 1) |>
  group_by(device_id) |>
  summarise(count = sum(turbine)) |>
  arrange(desc(count)) |>
  mutate(migration_route = case_when(
    device_id %in% c("211293", "211280") ~ "Mediterranean",
    device_id %in% c("211284", "211292", "201098", "211291", 
                     "211294", "211296", "201110") ~ "Mainland Spain",
    device_id %in% c("201097", "201103", "211276", "211277", "211279", 
                     "211283", "211289", "211290") ~ "Atlantic coast"))
turbine_encounter_ind5

turbine_encounter_avg5 <- turbine_encounter_ind5 |>
  dplyr::group_by(migration_route) |>
  summarise(mean_n_turbines = mean(count))
turbine_encounter_avg5

#Make a glm of the turbine_encounter_ind5 data comparing turbine counts and migration route
glm_5000 <- glm(count ~ migration_route, data = turbine_encounter_ind5, family = "poisson")
summary(glm_5000)

#Use emmeans to check if the difference is significant between migration routes based on turbine count
emmeans(glm_5000, pairwise ~ migration_route, type = "response")


# 2000m buffer around bird tracks
# Calculate how many turbines each bird encounters along its route through Spain
# intersecion_2000 strangely misses 1 bird; this could have happened during the buffer calculation...?
turbine_encounter_ind2 <- intersecion_2000 |>
  mutate(turbine = 1) |>
  group_by(device_id) |>
  summarise(count = sum(turbine)) |>
  arrange(desc(count)) |>
  mutate(migration_route = case_when(
    device_id %in% c("211293", "211280") ~ "Mediterranean",
    device_id %in% c("211284", "211292", "201098", "211291", 
                     "211294", "211296", "201110") ~ "Mainland Spain",
    device_id %in% c("201097", "201103", "211276", "211277", "211279", 
                     "211283", "211289", "211290") ~ "Atlantic coast"))
turbine_encounter_ind2

turbine_encounter_avg2 <- turbine_encounter_ind2 |>
  dplyr::group_by(migration_route) |>
  summarise(mean_n_turbines = mean(count))
turbine_encounter_avg2

#Make a glm of the turbine_encounter_ind5 data comparing turbine counts and migration route
glm_2000 <- glm(count ~ migration_route, data = turbine_encounter_ind2, family = "poisson")
summary(glm_2000)

#Use emmeans to check if the difference is significant between migration routes based on turbine count
emmeans(glm_2000, pairwise ~ migration_route, type = "response")


# Plot the average number of wind turbines encountered per route
letters_df1 <- data.frame(
  group = c("Atlantic coast", "Mainland Spain", "Mediterranean"),
  cld = c("a", "b", "c"),
  ypos = c(1250, 1250, 1250))

p1 <- ggplot(turbine_encounter_ind5, aes(x = migration_route, y = count)) +
  geom_boxplot() +
  geom_point() +
  geom_text(
    data = letters_df1,
    aes(x = group, y = ypos, label = cld),
    inherit.aes = FALSE,
    vjust = -0.5,
    size = 8) +
  labs(title = "Average number of turbines encountered (5000 m buffer)",
       x = "Migration route",
       y = "Average number of wind turbines encountered") +
  coord_cartesian(ylim = c(0, 1250)) +
  scale_y_continuous(breaks = seq(0, 1250, by = 250)) +
  theme_minimal() +
  theme(text = element_text(size = 16, hjust = 0.5))
p1

# Plot the average number of wind turbines encountered per route
p2 <- ggplot(turbine_encounter_ind2, aes(x = migration_route, y = count)) +
  geom_boxplot() +
  geom_point() +
  geom_text(
    data = letters_df1,
    aes(x = group, y = ypos, label = cld),
    inherit.aes = FALSE,
    vjust = -0.5,
    size = 8) +
  labs(title = "Average number of turbines encountered (2000 m buffer)",
       x = "Migration route",
       y = "") +
  coord_cartesian(ylim = c(0, 1250)) +
  scale_y_continuous(breaks = seq(0, 1250, by = 250)) +
  theme_minimal() +
  theme(text = element_text(size = 16, hjust = 0.5))
p2

# Add together p1 and p2 with patchwork
double_box <- p1 + p2 + patchwork::plot_layout(ncol = 2)
double_box
#Save using ggsave
ggsave("C:/Users/daank/OneDrive - University of Twente/Documents/Github/flyway_gulls_windturbine/plots/boxplot_turbines_encountered.png", plot = double_box, width = 40, height = 20, units = "cm")



