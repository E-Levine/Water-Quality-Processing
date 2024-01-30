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
#Code to limit stations if list contains more than needed:

#
##Estuary area  
Estuary_area <- st_read(paste0("KML/", Estuary_code, ".kml"))
plot(Estuary_area[2])
###State Outline
FL_outline <- st_read("KML/FL_Outlines/FL_Outlines.shp")
plot(FL_outline)
#
#
####Station selection by distance####
#
F_Distance <- c(1000) #distance in meters
S_Distance <- c(2000) #secondary distance for additional search area of potential stations 
WQ_data_t <- st_as_sf(WQ_data, coords = c(9,8), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
#
Stations_t <- st_as_sf(Station_locations, coords = c(3,4), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
#
#
#
#
WQ_Stations <- data.frame() #create blank data.frame to fill 
#
for(i in 1:nrow(Stations_t)){
  ##Create each buffer zone for all stations of interest
  Stations_FD <- st_buffer(Stations_t[i,], F_Distance)
  Stations_SD <- st_difference(st_buffer(Stations_t[i,], S_Distance), Stations_FD)
  ##Find all points within each buffer
  Points_FD <- st_filter(WQ_data_t, Stations_FD) %>% mutate(Station = i, Buffer = paste0(F_Distance, "m"))
  Points_SD <- st_filter(WQ_data_t, Stations_SD) %>% mutate(Station = i, Buffer = paste0(S_Distance, "m"))
  ##Join data for each buffer area
  temp <- bind_rows(Points_FD, Points_SD)
  ##Join into final output dataframe of all stations
  WQ_Stations <- rbind(WQ_Stations, temp)
}
#
WQ_Stations <- WQ_Stations %>% mutate(Station = as.factor(Station))
#
#
#
(map <- tmap_leaflet(tm_shape(Estuary_area) + tm_polygons(col = "lightblue")+ #Estuary area
                    tm_shape(FL_outline) + tm_borders()+ #Outline of shoreline
                    tm_shape(Stations_t) + tm_dots(size = 1, legend.show = TRUE)+ #All possible stations
                    tm_shape(WQ_data_t) + tm_dots(popup.vars = c("StationID" = "MonitoringLocationIdentifier"))+ #Reference stations
                    tm_shape(WQ_Stations) + tm_dots(col = "Station", size = 0.25, legend.show = TRUE,
                                                    popup.vars = c("StationID" = "MonitoringLocationIdentifier", "Station" = "Station", "Buffer" = "Buffer")))) #Selected stations and buffer area
#
# List of any stations to include: need station ID and station of reference 
To_include <- data.frame(StationID = c(),
                         Station = c())
#
##Run line 92 is not including any other stations, run line 93 is includine more stations to selection
WQ_stations_final <- WQ_Stations
WQ_stations_final 
WQ_data_t %>% subset(MonitoringLocationIdentifier %in% To_include$StationID) %>% mutate(Buffer = "Extra") %>% left_join(To_include)