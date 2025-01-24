####Water Quality Data Selection###
#
##Selection of WQ stations for data from WQ Portal or Atlas 
##Output of selected data and map
#
#
#Use Alt+O to collapse all sections, Alt+Shift+O to expand all sections
#
#Load require packages (install as necessary)
if (!require("pacman")) {install.packages("pacman")}
pacman::p_unlock()
pacman::p_load(plyr, tidyverse, readxl, writexl, #Df manipulation, basic summary
               ggmap, tibble, zoo, measurements,
               sf, raster, spData, nngeo,
               tmap, tmaptools, htmltools, htmlwidgets,
               install = TRUE) 
#
#
#
###Setup
Estuary_code <- c("SL") #Two letter estuary code
Data_source <- c("Portal") #"Portal" or "WA". Code not currently updated for FIM"
#
#Years of possible data:
Start_year <- c("2023")
End_year <- c("2024")
#
##Years of desired data:
Begin_data <- c("2023")
End_data <- c("2024")
#
#
####Load Files#####
#
##Raw cleaned data
WQ_data <- as.data.frame(read_excel(paste0("Data/Raw_cleaned/", Estuary_code, "_", Data_source, "_combined_filtered_",Start_year,"_", End_year,".xlsx"), na = c("NA", " ", "", "Z")))
#
#
#Fixed station locations
Station_locations <- as.data.frame(read_excel("Data/Reference_data/Stations_area_selections.xlsx", na = c("NA", " ", "", "Z"))) %>% 
  subset(grepl(Estuary_code, Site))
#
##Estuary area  
Estuary_area <- st_read(paste0("KML/", Estuary_code, ".kml"))
plot(Estuary_area[2])
###State Outline
FL_outline <- st_read("KML/FL_Outlines/FL_Outlines.shp")
plot(FL_outline)
#
#
####Limit data ranges####
#
##Limit WQ to years of interest
if(Data_source == "Portal"){
WQ_selected <- WQ_data %>% subset(ActivityStartDate >= as.Date(paste0(Begin_data, "-01-01")) & ActivityStartDate <= as.Date(paste0(End_data,"-12-31")))
} else if(Data_source == "WA"){
  WQ_selected <- WQ_data %>% subset(SampleDate >= as.Date(paste0(Begin_data, "-01-01")) & SampleDate <= as.Date(paste0(End_data,"-12-31")))
}
#
##Code to limit list of monitoring stations if list contains more stations than needed: update sub-setting code for desired stations
Stations_selected <- Station_locations %>% subset(Station < 6)
#
#
#
####Station selection - specified buffer distance - run only if selecting stations based on proximity to established locations ("Station_locations")####
#
F_Distance <- c(1500) #distance in meters
S_Distance <- c(3000) #secondary distance for additional search area of potential stations 
WQ_data_t <- st_as_sf(WQ_selected, coords = c(9,8), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
#
Stations_t <- st_as_sf(Stations_selected, coords = c(3,4), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
#
#
#
#Loop to select WQ stations located within specified buffers
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
head(WQ_Stations)
#
#
#
if(Data_source == "Portal"){
  (map <- tmap_leaflet(tm_shape(Estuary_area) + tm_polygons(col = "lightblue")+ #Estuary area
                       tm_shape(FL_outline) + tm_borders()+ #Outline of shoreline
                       tm_shape(Stations_t) + tm_dots(size = 1, legend.show = TRUE)+ #All possible stations
                       tm_shape(WQ_data_t %>% subset(KML == "In")) + tm_dots(popup.vars = c("StationID" = "MonitoringLocationIdentifier"))+ #Reference stations
                       tm_shape(WQ_Stations %>% subset(KML == "In")) + tm_dots(col = "Station", size = 0.25, legend.show = TRUE,
                                                       popup.vars = c("StationID" = "MonitoringLocationIdentifier", "Station" = "Station", "Buffer" = "Buffer"))+
                       tm_layout(main.title = paste(Estuary_code, Data_source, "WQ Stations", Begin_data, "-", End_data, sep = " ")))) #Selected stations and buffer area
  } else if(Data_source == "WA"){
  (map <- tmap_leaflet(tm_shape(Estuary_area) + tm_polygons(col = "lightblue")+ #Estuary area
                         tm_shape(FL_outline) + tm_borders()+ #Outline of shoreline
                         tm_shape(Stations_t) + tm_dots(size = 1, legend.show = TRUE)+ #All possible stations
                         tm_shape(WQ_data_t %>% subset(KML == "In")) + tm_dots(popup.vars = c("StationID" = "StationID"))+ #Reference stations
                         tm_shape(WQ_Stations %>% subset(KML == "In")) + tm_dots(col = "Station", size = 0.25, legend.show = TRUE,
                                                         popup.vars = c("StationID" = "StationID", "Station" = "Station", "Buffer" = "Buffer"))+
                         tm_layout(main.title = paste(Estuary_code, Data_source, "WQ Stations", Begin_data, "-", End_data, sep = " ")))) #Selected stations and buffer area
}
#
#saveWidget(map, paste0("Maps/Station_selection/", Estuary_code, "_", Data_source,"_WQ_stations_", Begin_data, "_", End_data, "_widget.html"))
#
# List of any stations to include or exclude from selection: need station ID and station of reference (both within "")- keep matched on one line/column
To_include <- data.frame(StationID = c("21FLCOSP_WQX-32-03", "21FLHILL_WQX-28", "21FLHILL_WQX-25"),
                         Station = c("5", "1", "1"))
To_exclude <- data.frame(StationID = c("21FLTPA_WQX-G5SW0146", "21FLCOSP_WQX-45-03", "21FLCOSP_WQX-CENTRAL CANAL"),
                         Station = c("4", "4", "4"))
#
##Run line 121 if not including or excluding any stations, run line 122 to include more stations to selection
WQ_stations_final <- WQ_Stations
WQ_stations_final <- rbind(WQ_Stations, 
                           #Stations to include
                           WQ_data_t %>% 
                             subset(MonitoringLocationIdentifier %in% To_include$StationID) %>% mutate(Buffer = "Extra") %>% 
                             left_join(To_include, by = c("MonitoringLocationIdentifier" = "StationID")))  %>%
  #Stations to exclude
  subset(!MonitoringLocationIdentifier %in% To_exclude$StationID)
#
##Get coordinates into columns
WQ_stations_final_df <- WQ_stations_final %>% st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>%
  mutate(Longitude = st_coordinates(.)[,1],
         Latitude = st_coordinates(.)[,2]) %>% dplyr::select(-geometry)
#
##Code (3-4 letters preferred) to identify project selected data is for:
Project_code <- c("SPV1")
#
##Export cleaned final data
write_xlsx(WQ_stations_final_df, paste0("Data/Raw_cleaned/", Estuary_code, "_", Data_source, "_selected_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
#
#           
#
#
####Station selection - closest N stations####
#
##Selection continues from data narrowed by desired date range. If spatial selection is not needed, skip section.
##Desired number of WQ stations per location
Stations_N <- 3
maxDist_m <- Inf #Maximum distance (in  meters) allowed in selection. If no max, set as Inf
#
Stations_t <- st_as_sf(Stations_selected, coords = c(3,4), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
#All WQ data possible:
WQ_data_t <- st_as_sf(WQ_selected, coords = c(9,8), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
#
if(Data_source == "Portal"){
  #List of possible stations and their coordinates:
  WQ_locations_t <- st_as_sf(WQ_selected %>% dplyr::select(LatitudeMeasure, LongitudeMeasure) %>% distinct() %>% mutate(Lat_n = LatitudeMeasure, Lon_n = LongitudeMeasure),
           coords = c(2,1), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
  #
  #Empty data frame to fill with selected data
  WQ_closest_selected <- data.frame()
  #
  for(i in 1:nrow(Stations_t)){
    #Determine closest N stations for each station
    temp_station <- Stations_t$geometry[i]
    Selected_WQstation_info <- st_nn(temp_station, WQ_locations_t, k = Stations_N, maxdist = maxDist_m, returnDist = TRUE)
    #Create data frame of information of selected stations
    Selected_WQstations <- data.frame(Latitude_match = WQ_locations_t$Lat_n[Selected_WQstation_info$nn[[1]]],
                                      Longitude_match = WQ_locations_t$Lon_n[Selected_WQstation_info$nn[[1]]],
                                      Distance = Selected_WQstation_info$dist[[1]],  #Distances from specified point
                                      LocationID = (Stations_selected %>% unite(LocationID, Site, Station, sep = "_", remove = TRUE))[i,1]) #Location ID related to
    #Limit WQ data to selected stations and add information to relate each station to the specified location
    Station_identification <- left_join(WQ_data %>% filter(LatitudeMeasure %in% Selected_WQstations$Latitude_match & LongitudeMeasure %in% Selected_WQstations$Longitude_match) %>% dplyr::select(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure) %>% distinct(), Selected_WQstations, by = c("LatitudeMeasure" = "Latitude_match", "LongitudeMeasure" = "Longitude_match"))
    Selected_data <- left_join(WQ_data_t %>% filter(MonitoringLocationIdentifier %in% Station_identification$MonitoringLocationIdentifier), (Station_identification %>% dplyr::select(-LatitudeMeasure, -LongitudeMeasure)))
    #Combine filtered data to final output
    WQ_closest_selected <<- rbind(WQ_closest_selected, Selected_data)
    #Map of all possible stations and stations selected for each location
    map <- tm_shape(Estuary_area) + tm_polygons() + #Estuary area
      tm_shape(FL_outline) + tm_borders() + #Outline of shoreline
      tm_shape(WQ_locations_t) + tm_dots(size = 2) + #Stations relation to estuary area
      tm_shape(WQ_closest_selected) + #Stations selected per location
        tm_dots(size = 1.25, col = "LocationID", legend.show = FALSE) + tm_facets(by = "LocationID", free.coords = FALSE)
    print(map)
  }
} else if (Data_source == "WA"){#List of possible stations and their coordinates:
  WQ_locations_t <- st_as_sf(WQ_selected %>% dplyr::select(Latitude, Longitude) %>% distinct() %>% mutate(Lat_n = Latitude, Lon_n = Longitude),
                             coords = c(2,1), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
  #
  #Empty data frame to fill with selected data
  WQ_closest_selected <- data.frame()
  #
  #
  for(i in 1:nrow(Stations_t)){
    #Determine closest N stations for each station
    temp_station <- Stations_t$geometry[i]
    Selected_WQstation_info <- st_nn(temp_station, WQ_locations_t, k = Stations_N, maxdist = maxDist_m, returnDist = TRUE)
    #Create data frame of information of selected stations
    Selected_WQstations <- data.frame(Latitude_match = WQ_locations_t$Lat_n[Selected_WQstation_info$nn[[1]]],
                                      Longitude_match = WQ_locations_t$Lon_n[Selected_WQstation_info$nn[[1]]],
                                      Distance = Selected_WQstation_info$dist[[1]],  #Distances from specified point
                                      LocationID = (Stations_selected %>% unite(LocationID, Site, Station, sep = "_", remove = TRUE))[i,1]) #Location ID related to
    #Limit WQ data to selected stations and add information to relate each station to the specified location
    Station_identification <- left_join(WQ_data %>% filter(Latitude %in% Selected_WQstations$Latitude_match & Longitude %in% Selected_WQstations$Longitude_match) %>% dplyr::select(StationID, Latitude, Longitude) %>% distinct(), Selected_WQstations, by = c("Latitude" = "Latitude_match", "Longitude" = "Longitude_match"))
    Selected_data <- left_join(WQ_data_t %>% filter(StationID %in% Station_identification$StationID), (Station_identification %>% dplyr::select(-Latitude, -Longitude)))
    #Combine filtered data to final output
    WQ_closest_selected <<- rbind(WQ_closest_selected, Selected_data)
    #Map of all possible stations and stations selected for each location
    map <- tm_shape(Estuary_area) + tm_polygons() + #Estuary area
      tm_shape(FL_outline) + tm_borders() + #Outline of shoreline
      tm_shape(WQ_locations_t) + tm_dots(size = 2) + #Stations relation to estuary area
      tm_shape(WQ_closest_selected) + #Stations selected per location
      tm_dots(size = 1.25, col = "LocationID", legend.show = FALSE) + tm_facets(by = "LocationID", free.coords = FALSE)
    print(map)
  }
} else {
  print(paste0("Code not yet written for data source ", Data_source))
}
rm(temp_station, Selected_WQstation_info, Selected_WQstations, Selected_data)
#
#
#
#Widget output to identify stations.
if(Data_source == "Portal"){
  (map <- tmap_leaflet(tm_shape(Estuary_area) + tm_polygons(col = "lightblue")+ #Estuary area
                         tm_shape(FL_outline) + tm_borders()+ #Outline of shoreline
                         tm_shape(WQ_locations_t) + tm_dots(size = 0.5, legend.show = FALSE, popup.vars = c("StationID" = "MonitoringLocationIdentifier"))+ #Possible stations
                         tm_shape(WQ_closest_selected %>% dplyr::select(MonitoringLocationIdentifier, LocationID, Distance) %>% unique()) + 
                          tm_dots(col = "red", size = 0.75, legend.show = TRUE, popup.vars = c("StationID" = "MonitoringLocationIdentifier", "LocationID" = "LocationID", "Distance" = "Distance"))+
                         tm_layout(main.title = paste(Estuary_code, Data_source, "Closest", Stations_N,  "WQ Stations", Begin_data, "-", End_data, sep = " ")))) #Selected stations and buffer area
} else if(Data_source == "WA"){
  (map <- tmap_leaflet(tm_shape(Estuary_area) + tm_polygons(col = "lightblue")+ #Estuary area
                         tm_shape(FL_outline) + tm_borders()+ #Outline of shoreline
                         tm_shape(WQ_locations_t) + tm_dots(size = 0.5, legend.show = FALSE, popup.vars = c("StationID" = "StationID"))+ #Possible stations
                         tm_shape(WQ_closest_selected %>% dplyr::select(StationID, LocationID, Distance) %>% unique()) + 
                          tm_dots(col = "red", size = 0.75, legend.show = TRUE, popup.vars = c("StationID" = "StationID", "LocationID" = "LocationID", "Distance" = "Distance"))+
                         tm_layout(main.title = paste(Estuary_code, Data_source, "Closest", Stations_N, "WQ Stations", Begin_data, "-", End_data, sep = " ")))) #Selected stations and buffer area
} else {
  print(paste0("Code not yet written for data source ", Data_source))
}
#
#saveWidget(map, paste0("Maps/Station_selection/", Estuary_code, "_", Data_source,"_closest_stations_", Begin_data, "_", End_data, "_widget.html"))
#
# List of any stations to include or exclude from selection: need station ID and station of reference (both within "")- keep matched on one line/column
To_include <- data.frame(StationID = c("21FLCOSP_WQX-32-03", "21FLHILL_WQX-28", "21FLHILL_WQX-25"),
                         LocationID = c("LXN-1", "LXN-1", "LXN-1"))
To_exclude <- data.frame(StationID = c("21FLTPA_WQX-G5SW0146", "21FLCOSP_WQX-45-03", "21FLCOSP_WQX-CENTRAL CANAL"),
                         LocationID = c("LXN-4", "LXN-4", "LXN-4"))
#
##Run first line if not including or excluding any stations, run second line to include more stations to selection
WQ_stations_final <- WQ_closest_selected
if(Data_source == "Portal"){
  WQ_stations_final <- rbind(WQ_closest_selected, 
                             #Stations to include
                             WQ_data_t %>% 
                               subset(MonitoringLocationIdentifier %in% To_include$StationID) %>% mutate(Distance = "NA") %>% 
                               left_join(To_include, by = c("MonitoringLocationIdentifier" = "StationID")))  %>%
    #Stations to exclude
    subset(!MonitoringLocationIdentifier %in% To_exclude$StationID)
} else {
  print(paste0("Code not yet written for data source ", Data_source))
}
#
##Get coordinates into columns
WQ_stations_final_df <- WQ_stations_final %>% st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>%
  mutate(Longitude = st_coordinates(.)[,1],
         Latitude = st_coordinates(.)[,2]) %>% dplyr::select(-geometry)
#
##Code (3-4 letters preferred) to identify project selected data is for:
Project_code <- c("Cage")
#
##Export cleaned final data
write_xlsx(WQ_stations_final_df, paste0("Data/Compiled_data/", Estuary_code, "_", Data_source, "_selected_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
#
#
#
#
#
####Output all stations possible####
#
WQ_data_t <- st_as_sf(WQ_selected, coords = c(9,8), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
#
##Convert FL_outline to same CRS
FL_outline2 <- FL_outline %>% st_transform(crs = st_crs(3086))
##Determine stations that overlap with FL shape (land) and don't overlap (water)
Stations_land <- WQ_data_t[lengths(st_intersects(WQ_data_t, FL_outline2))>0,] %>% mutate(Location = "Land")
Stations_water <- WQ_data_t[!lengths(st_intersects(WQ_data_t, Stations_land)),] %>% mutate(Location = "Water")
##Join into final output dataframe of all stations
WQ_Stations <- rbind(Stations_water, Stations_land) %>% mutate(Location = as.factor(Location))
head(WQ_Stations)
#
if(Data_source == "Portal"){
  (map <- tmap_leaflet(tm_shape(Estuary_area) + tm_polygons(col = "lightblue")+ #Estuary area
                         tm_shape(FL_outline) + tm_borders()+ #Outline of shoreline
                         tm_shape(WQ_data_t %>% subset(KML == "In")) + tm_dots(popup.vars = c("StationID" = "MonitoringLocationIdentifier"))+ #Reference stations
                         tm_shape(WQ_Stations %>% subset(Location == "Water")) + tm_dots(col = "KML", size = 0.25, legend.show = TRUE,
                                                                                 popup.vars = c("StationID" = "MonitoringLocationIdentifier", "Location" = "Location"))+
                         tm_layout(main.title = paste(Estuary_code, Data_source, "WQ Stations", Begin_data, "-", End_data, sep = " ")))) #Selected stations and buffer area
} else if(Data_source == "WA"){
  (map <- tmap_leaflet(tm_shape(Estuary_area) + tm_polygons(col = "lightblue")+ #Estuary area
                         tm_shape(FL_outline) + tm_borders()+ #Outline of shoreline
                         tm_shape(WQ_data_t %>% subset(KML == "In")) + tm_dots(popup.vars = c("StationID" = "StationID"))+ #Reference stations
                         tm_shape(WQ_Stations %>% subset(Location == "Water")) + tm_dots(col = "KML", size = 0.25, legend.show = TRUE,
                                                                                 popup.vars = c("StationID" = "StationID", "Location" = "Location"))+
                         tm_layout(main.title = paste(Estuary_code, Data_source, "WQ Stations", Begin_data, "-", End_data, sep = " ")))) #Selected stations and buffer area
}
#
#saveWidget(map, paste0("Maps/Station_selection/", Estuary_code, "_", Data_source,"_WQ_stations_", Begin_data, "_", End_data, "_widget.html"))
#
# List of any stations to include or exclude from selection: need station ID within ""
To_include <- data.frame(StationID = c("21FLCOSP_WQX-32-03", "21FLHILL_WQX-28", "21FLHILL_WQX-25"))
To_exclude <- data.frame(StationID = c("21FLTPA_WQX-G5SW0169", "21FLPDEM_WQX-24-07", "21FLTPA_WQX-G5SW0187", "21FLPDEM_WQX-24-01", "21FLTPA_WQX-G5SW0152", "21FLPDEM_WQX-19-02"))
#
##Run to include and/or exclude based on lines above
if(exists("To_include") & !exists("To_exclude")){
  WQ_stations_final <- rbind(WQ_Stations, WQ_data_t %>% subset(MonitoringLocationIdentifier %in% To_include$StationID) %>% mutate(Buffer = "Extra") %>% 
                               left_join(To_include, by = c("MonitoringLocationIdentifier" = "StationID")))
  } else if(!exists("To_include") & exists("To_exclude")){
    WQ_stations_final <- WQ_Stations %>% subset(!MonitoringLocationIdentifier %in% To_exclude$StationID)
  } else if(exists("To_include") & exists("To_exclude")){
    WQ_stations_final <- rbind(WQ_Stations, WQ_data_t %>% subset(MonitoringLocationIdentifier %in% To_include$StationID) %>% mutate(Buffer = "Extra") %>% 
                                 left_join(To_include, by = c("MonitoringLocationIdentifier" = "StationID"))) %>% 
      subset(!MonitoringLocationIdentifier %in% To_exclude$StationID)
  } else {
    WQ_stations_final <- WQ_Stations} 
#
# 
#
##Get coordinates into columns
WQ_stations_final_df <- WQ_stations_final %>% st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>%
  mutate(Longitude = st_coordinates(.)[,1],
         Latitude = st_coordinates(.)[,2]) %>% dplyr::select(-geometry)
#
##Code (3-4 letters preferred) to identify project selected data is for:
Project_code <- c("AllTB")
#
##Export cleaned final data
write_xlsx(WQ_stations_final_df, paste0("Data/Compiled_data/", Estuary_code, "_", Data_source, "_selected_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
