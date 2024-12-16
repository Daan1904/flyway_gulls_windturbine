# New packages
renv::update()

library(sf)


#Script Roos
Turbines <- read_sf("C:/Users/daank/OneDrive - University of Twente/Documents/Rijksuniversiteit Groningen/Year 1 - 24-25/Flyway Ecology/Mini project - Gull migration wind farms/QGIS_FE2024/overpass_turbo_single_turbines.kml")

str(Turbines)

mapview(Turbines)

#Remove the black line around the points and make the points red, 25% transparent and smaller
mapview(Turbines, col.region='red', lwd=0, alpha.regions=0.25, size=0.5)


#Takes very very very long:
turb_NF_1000m <- st_buffer(turb_NF, 1000)
turb_NF_5000m <- st_buffer(turb_NF, 5000)

mapview(klm_211307_sf_line) + mapview(turb_NF_5000m, col.region='yellow')





NF5000_intersection <- st_intersection(klm_211307_sf_line, turb_NF_5000m)



mapview(klm_211307_sf_line) + mapview(NF5000_intersection, color='red', lwd = 2) +
  
  mapview(turb_NF, col.region='yellow')





