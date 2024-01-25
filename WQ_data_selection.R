####Water Quality Data Selection###
#
##Selection WQ stations for data from WQ Portal or Atlas 
##Output of selected data and map
#
#
#Use Alt+O to collapse all sections, Alt+Shift+O to expand all sections
#
#Load require packages (install as necessary)
if (!require("pacman")) install.packages("pacman")
pacman::p_unlock()
pacman::p_load(plyr, tidyverse, readxl, writexl, #Df manipulation, basic summary
               ggmap, tibble, zoo, measurements,
               sf, raster, spData, 
               tmap, tmaptools, htmltools, htmlwidgets,
               install = TRUE) 
#
#
#
###Setup
Estuary_code <- c("TB") #Two letter estuary code
Data_source <- c("Portal") #"Portal", "WA" , or "FIM"
#
#Years of data:
Start_year <- c("2015")
End_year <- c("2023")
#
#
#
####Load File###
#
WQ_data <- as.data.frame(read_excel(paste0("../Water-Quality-Processing-Data/Data/Raw_cleaned/", Estuary_code, "_", Data_source, "_combined_filtered_",Start_year,"_", End_year,".xlsx"), na = c("NA", " ", "", "Z")))

###Use widget map to select stations if only including or excluding a few stations.
#NEED TO WRITE CODE TO READ IN STATION LIST AND EXCLUDE (SCALLOP CODE BASE)
#
#Use following code to limit stations to specified distance from points (i.e., stations)
Station_locations <- as.data.frame(read_excel("../Water-Quality-Processing-Data/Data/Reference_data/Stations_area_selections.xlsx", na = c("NA", " ", "", "Z")))
#
##Estuary area  
Estuary_area <- st_read(paste0("KML/", Estuary_code, ".kml"))
plot(Estuary_area[2])
#
#
#
####Station selection by distance####
#
Distance <- c(200) #distance in meters
WQ_data_t <- st_as_sf(WQ_data, coords = c(9,8), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
#
Stations_t <- st_as_sf(Station_locations, coords = c(3,4), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
#
#
test <- WQ_data_t %>% mutate(Within_dist = lengths(st_is_within_distance(., Stations_t, dist = Distance)))
#
#
#Need to convert to CRS of meters for buffer area. 
tm_shape(Estuary_area) + #Estuary area
               tm_polygons() + 
              # tm_shape(Combined_data) + #Stations relation to estuary area
              # tm_dots("KML", palette = c(In = "red", Out = "black"), size = 0.25, legend.show = TRUE,
               #        popup.vars = c("StationID" = "MonitoringLocationIdentifier", "Latitude" = "LatitudeMeasure", "Longitude" = "LongitudeMeasure")) +
               tm_shape(WQ_data_t) + tm_dots("Within_dist", size = 0.25, legend.show = TRUE)
