##Functions used for water quality selection
#
#
#
####Set up####
#Combining filtered files
combine_files <- function(Number_files, Estuarycode, DataSource, Start1, End1, Start2, End2, Start3, End3){
if(Filtered_files == 1){
  WQ_data <- as.data.frame(read_excel(paste0("Data/Raw_cleaned/", Estuary_code, "_", Data_source, "_combined_filtered_",Start_year,"_", End_year,".xlsx"), na = c("NA", " ", "", "Z")))
} else if(Filtered_files == 2){
  WQ_data <- rbind(as.data.frame(read_excel(paste0("Data/Raw_cleaned/", Estuary_code, "_", Data_source, "_combined_filtered_",Start_year,"_", End_year,".xlsx"), na = c("NA", " ", "", "Z"))),
                   as.data.frame(read_excel(paste0("Data/Raw_cleaned/", Estuary_code, "_", Data_source, "_combined_filtered_",Start_year_2,"_", End_year_2,".xlsx"), na = c("NA", " ", "", "Z"))))
} else {
  WQ_data <- rbind(
    rbind(as.data.frame(read_excel(paste0("Data/Raw_cleaned/", Estuary_code, "_", Data_source, "_combined_filtered_",Start_year,"_", End_year,".xlsx"), na = c("NA", " ", "", "Z"))),
          as.data.frame(read_excel(paste0("Data/Raw_cleaned/", Estuary_code, "_", Data_source, "_combined_filtered_",Start_year_2,"_", End_year_2,".xlsx"), na = c("NA", " ", "", "Z")))),
    as.data.frame(read_excel(paste0("Data/Raw_cleaned/", Estuary_code, "_", Data_source, "_combined_filtered_",Start_year_3,"_", End_year_3,".xlsx"), na = c("NA", " ", "", "Z"))))
}
return(WQ_data)
}
#
#
#
#
#Limiting to specified date range
date_window <- function(DateBegin, DateEnd){
  if(Data_source == "Portal"){
    WQ_selected <- Filtered_data %>% subset(ActivityStartDate >= as.Date(paste0(DateBegin, "-01-01")) & ActivityStartDate <= as.Date(paste0(DateEnd,"-12-31")))
  } else if(Data_source == "WA"){
    WQ_selected <- Filtered_data %>% subset(SampleDate >= as.Date(paste0(DateBegin, "-01-01")) & SampleDate <= as.Date(paste0(DateEnd,"-12-31")))
  }
  return(WQ_selected)
}
#
#
#
#
#Spatial transformation of data
Spatial_data <- function(DataInput){
  WQ_sp <- spTransform(SpatialPointsDataFrame(coords = DataInput[,c(9,8)], data = DataInput,
                                              proj4string = CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs +type=crs")),
                       "+proj=longlat +datum=WGS84 +no_defs +type=crs")
  #
  if(Data_source == "Portal"){
    Combined_data_counts <- SpatialPointsDataFrame(coords = (WQ_sp@data %>% distinct(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure, ActivityStartDate, KML) %>% group_by(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure, KML) %>% summarise(N = n()) %>% as.data.frame())[,c(3,2)], 
                                                   data = WQ_sp@data %>% distinct(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure, ActivityStartDate, KML) %>% group_by(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure, KML) %>% summarise(N = n()) %>% as.data.frame(), 
                                                   proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
  } else if(Data_source == "WA"){
    Combined_data_counts <- SpatialPointsDataFrame(coords = (WQ_sp@data %>% distinct(StationID, Actual_Latitude, Actual_Longitude, as.Date(SampleDate), KML) %>% group_by(StationID, Actual_Latitude, Actual_Longitude, KML) %>% summarise(N = n()) %>% as.data.frame())[,c(3,2)], 
                                                   data = WQ_sp@data %>% distinct(StationID, Actual_Latitude, Actual_Longitude, as.Date(SampleDate), KML) %>% group_by(StationID, Actual_Latitude, Actual_Longitude, KML) %>% summarise(N = n()) %>% as.data.frame(), 
                                                   proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
  } else if (Data_source == "FIM") {
    Combined_data_counts <- SpatialPointsDataFrame(coords = (WQ_sp@data %>% distinct(Reference, Latitude, Longitude, Sampling_Date, KML) %>% group_by(Reference, Latitude, Longitude, KML) %>% summarise(N = n()) %>% as.data.frame())[,c(3,2)], 
                                                   data = WQ_sp@data %>% distinct(Reference, Latitude, Longitude, Sampling_Date, KML) %>% group_by(Reference, Latitude, Longitude, KML) %>% summarise(N = n()) %>% as.data.frame(), 
                                                   proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
    
  }
  return(Combined_data_counts)
}
#
#
#
#
####Station selection - buffers####
#Requires: WQ_selected, Stations_selected, Estuary_area, FL_outline
buffer_selection <- function(FirstBuffer, SecondBuffer, WidgetSave, EstuaryCode, DataSource, DateBegin, DateEnd){
  WQ_data_t <- st_as_sf(WQ_selected, coords = c(9,8), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
  Stations_t <- st_as_sf(Stations_selected, coords = c(3,4), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
  #
  #Loop to select WQ stations located within specified buffers
  WQ_Stations <- data.frame() #create blank data.frame to fill 
  #
  for(i in 1:nrow(Stations_t)){
    ##Create each buffer zone for all stations of interest
    Stations_FD <- st_buffer(Stations_t[i,], FirstBuffer)
    Stations_SD <- st_difference(st_buffer(Stations_t[i,], SecondBuffer), Stations_FD)
    ##Find all points within each buffer
    Points_FD <- st_filter(WQ_data_t, Stations_FD) %>% mutate(Station = i, Buffer = paste0(FirstBuffer, "m"))
    Points_SD <- st_filter(WQ_data_t, Stations_SD) %>% mutate(Station = i, Buffer = paste0(SecondBuffer, "m"))
    ##Join data for each buffer area
    temp <- bind_rows(Points_FD, Points_SD)
    ##Join into final output dataframe of all stations
    WQ_Stations <- rbind(WQ_Stations, temp)
  }
  #
  WQ_Stations <- WQ_Stations %>% mutate(Station = as.factor(Station))
  #
  if(Data_source == "Portal"){
    (map <- tmap_leaflet(tm_shape(Estuary_area) + tm_polygons(col = "lightblue")+ #Estuary area
                           tm_shape(FL_outline) + tm_borders()+ #Outline of shoreline
                           tm_shape(Stations_t, "Fixed stations") + tm_dots(size = 1, legend.show = TRUE, col = "navyblue")+ #All possible stations
                           tm_shape(WQ_data_t %>% subset(KML == "In"), "Possible stations") + tm_dots(popup.vars = c("StationID" = "MonitoringLocationIdentifier"))+ #Reference stations
                           tm_shape(WQ_Stations %>% subset(KML == "In"), "Selected stations") + tm_dots(title = "Station within buffer of:", col = "Station", size = 0.25, legend.show = TRUE,
                                                                                   popup.vars = c("StationID" = "MonitoringLocationIdentifier", "Station" = "Station", "Buffer" = "Buffer"))+
                           tm_layout(main.title = paste(EstuaryCode, DataSource, "WQ Stations", DateBegin, "-", DateEnd, sep = " ")))) #Selected stations and buffer area
  } else if(Data_source == "WA"){
    (map <- tmap_leaflet(tm_shape(Estuary_area) + tm_polygons(col = "lightblue")+ #Estuary area
                           tm_shape(FL_outline) + tm_borders()+ #Outline of shoreline
                           tm_shape(Stations_t, "Fixed stations") + tm_dots(size = 1, legend.show = TRUE, col = "navyblue")+ #All possible stations
                           tm_shape(WQ_data_t %>% subset(KML == "In"), "Possible stations") + tm_dots(popup.vars = c("StationID" = "StationID"))+ #Reference stations
                           tm_shape(WQ_Stations %>% subset(KML == "In"), "Selected stations") + tm_dots(title = "Station within buffer of:", col = "Station", size = 0.25, legend.show = TRUE,
                                                                                   popup.vars = c("StationID" = "StationID", "Station" = "Station", "Buffer" = "Buffer"))+
                           tm_layout(main.title = paste(EstuaryCode, DataSource, "WQ Stations", DateBegin, "-", DateEnd, sep = " ")))) #Selected stations and buffer area
  }
  #
  if(WidgetSave == "Y"){saveWidget(map, paste0("Maps/Station_selection/", EstuaryCode, "_", DataSource,"_WQ_stations_buffer", FirstBuffer, "_buffer", SecondBuffer, "_widget.html"))}
  #
  #
  return(list(Selected_map = map, Stations = WQ_Stations, print(head(WQ_Stations))))
  #
}
#
##Station edits, final output file
Selected_data <- function(Adding, Removing, ProjectCode){
  if(is.na(Adding) & is.na(Removing)){
    WQ_stations_final <- WQ_Stations
  } else {
    WQ_stations_final <- rbind(WQ_Stations, 
                               #Stations to include
                               WQ_data_t %>% 
                                 subset(MonitoringLocationIdentifier %in% Adding$StationID) %>% mutate(Buffer = "Extra") %>% 
                                 left_join(Adding, by = c("MonitoringLocationIdentifier" = "StationID")))  %>%
      #Stations to exclude
      subset(!MonitoringLocationIdentifier %in% Removing$StationID)
  }
  #
  ##Get coordinates into columns
  WQ_stations_final_df <- WQ_stations_final %>% st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>%
    mutate(Longitude = st_coordinates(.)[,1],
           Latitude = st_coordinates(.)[,2]) %>% dplyr::select(-geometry)
  #
  ##Code (3-4 letters preferred) to identify project selected data is for:
  Project_code <- ProjectCode
  #
  ##Export cleaned final data
  write_xlsx(WQ_stations_final_df, paste0("Data/Compiled/", Estuary_code, "_", Data_source, "_selected_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
  #
  return(print(glimpse(WQ_stations_final)))
}
#
#
#
#
#
####Station selection - closest N####
#
Nclosest_selection <- function(NumStations, maxDistance, WidgetSave){
  Stations_t <- st_as_sf(Stations_selected, coords = c(3,4), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
  #All WQ data possible:
  WQ_data_t <- st_as_sf(WQ_selected, coords = c(9,8), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
  #
  if(Data_source == "Portal"){
    #List of possible stations and their coordinates:
    WQ_locations_t <- st_as_sf(WQ_selected %>% dplyr::select(LatitudeMeasure, LongitudeMeasure) %>% distinct() %>% mutate(Lat_n = LatitudeMeasure, Lon_n = LongitudeMeasure) %>% drop_na(),
                               coords = c(2,1), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
    #
    #Empty data frame to fill with selected data
    WQ_closest_selected <- data.frame()
    #
    for(i in 1:nrow(Stations_t)){
      #Determine closest N stations for each station
      temp_station <- Stations_t$geometry[i]
      Selected_WQstation_info <- st_nn(temp_station, WQ_locations_t, k = NumStations, maxdist = maxDistance, returnDist = TRUE)
      #Create data frame of information of selected stations
      Selected_WQstations <- data.frame(Latitude_match = WQ_locations_t$Lat_n[Selected_WQstation_info$nn[[1]]],
                                        Longitude_match = WQ_locations_t$Lon_n[Selected_WQstation_info$nn[[1]]],
                                        Distance = Selected_WQstation_info$dist[[1]],  #Distances from specified point
                                        LocationID = (Stations_selected %>% unite(LocationID, Site, Station, sep = "_", remove = TRUE))[i,1]) #Location ID related to
      #Limit WQ data to selected stations and add information to relate each station to the specified location
      Station_identification <- left_join(Filtered_data %>% filter(LatitudeMeasure %in% Selected_WQstations$Latitude_match & LongitudeMeasure %in% Selected_WQstations$Longitude_match) %>% dplyr::select(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure) %>% distinct(), Selected_WQstations, by = c("LatitudeMeasure" = "Latitude_match", "LongitudeMeasure" = "Longitude_match"))
      Selected_data <- left_join(WQ_data_t %>% filter(MonitoringLocationIdentifier %in% Station_identification$MonitoringLocationIdentifier), (Station_identification %>% dplyr::select(-LatitudeMeasure, -LongitudeMeasure)))
      #Combine filtered data to final output
      WQ_closest_selected <- rbind(WQ_closest_selected, Selected_data)
      WQ_closest_selected <- WQ_closest_selected %>% mutate(Distance = round(Distance, 1))
      #Map of all possible stations and stations selected for each location
      All_map <- tm_shape(Estuary_area) + tm_polygons() + #Estuary area
        tm_shape(FL_outline) + tm_borders() + #Outline of shoreline
        tm_shape(WQ_locations_t, "Possible stations") + tm_dots(size = 2) + #Stations relation to estuary area
        tm_shape(WQ_closest_selected, "Selected stations") + #Stations selected per location
        tm_dots(size = 1.25, col = "LocationID", legend.show = FALSE) + tm_facets(by = "LocationID", free.coords = FALSE)
    }
  } else if (Data_source == "WA"){#List of possible stations and their coordinates:
    WQ_locations_t <- st_as_sf(WQ_selected %>% dplyr::select(Latitude, Longitude) %>% distinct() %>% mutate(Lat_n = Latitude, Lon_n = Longitude),
                               coords = c(2,1), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
    #
    #Empty data frame to fill with selected data
    WQ_closest_selected <- data.frame()
    #
    for(i in 1:nrow(Stations_t)){
      #Determine closest N stations for each station
      temp_station <- Stations_t$geometry[i]
      Selected_WQstation_info <- st_nn(temp_station, WQ_locations_t, k = NumStations, maxdist = maxDistance, returnDist = TRUE)
      #Create data frame of information of selected stations
      Selected_WQstations <- data.frame(Latitude_match = WQ_locations_t$Lat_n[Selected_WQstation_info$nn[[1]]],
                                        Longitude_match = WQ_locations_t$Lon_n[Selected_WQstation_info$nn[[1]]],
                                        Distance = Selected_WQstation_info$dist[[1]],  #Distances from specified point
                                        LocationID = (Stations_selected %>% unite(LocationID, Site, Station, sep = "_", remove = TRUE))[i,1]) #Location ID related to
      #Limit WQ data to selected stations and add information to relate each station to the specified location
      Station_identification <- left_join(Filtered_data %>% filter(Latitude %in% Selected_WQstations$Latitude_match & Longitude %in% Selected_WQstations$Longitude_match) %>% dplyr::select(StationID, Latitude, Longitude) %>% distinct(), Selected_WQstations, by = c("Latitude" = "Latitude_match", "Longitude" = "Longitude_match"))
      Selected_data <- left_join(WQ_data_t %>% filter(StationID %in% Station_identification$StationID), (Station_identification %>% dplyr::select(-Latitude, -Longitude)))
      #Combine filtered data to final output
      WQ_closest_selected <<- rbind(WQ_closest_selected, Selected_data)
      #Map of all possible stations and stations selected for each location
      All_map <- tm_shape(Estuary_area) + tm_polygons() + #Estuary area
        tm_shape(FL_outline) + tm_borders() + #Outline of shoreline
        tm_shape(WQ_locations_t, "Possible stations") + tm_dots(size = 2) + #Stations relation to estuary area
        tm_shape(WQ_closest_selected, "Selected stations") + #Stations selected per location
        tm_dots(size = 1.25, col = "LocationID", legend.show = FALSE) + tm_facets(by = "LocationID", free.coords = FALSE)
    }
  } else {
    print(paste0("Code not yet written for data source ", Data_source))
  }
  #
  #Widget output to identify stations.
  if(Data_source == "Portal"){
    WQ_closest_selected_info <- WQ_closest_selected %>% dplyr::select(MonitoringLocationIdentifier, LocationID, Distance) %>% unique() %>% group_by(MonitoringLocationIdentifier) %>% summarise(LocationID = paste(LocationID, collapse = ", "), Distance = paste(Distance, collapse = ", "))
    (map <- tmap_leaflet(tm_shape(Estuary_area) + tm_polygons(col = "lightblue")+ #Estuary area
                           tm_shape(FL_outline) + tm_borders()+ #Outline of shoreline
                           tm_shape(WQ_locations_t, "Possible stations") + tm_dots(size = 0.5, legend.show = FALSE, popup.vars = c("Latitude" = "Lat_n", "Longitude" = "Lon_n"))+ #Possible stations
                           tm_shape(WQ_closest_selected_info,  "Selected stations") + 
                           tm_dots(col = "red", size = 0.75, legend.show = TRUE, popup.vars = c("StationID" = "MonitoringLocationIdentifier", "LocationID" = "LocationID", "Distance" = "Distance"))+
                           tm_layout(main.title = paste(Estuary_code, Data_source, "Closest", Stations_N,  "WQ Stations", Begin_data, "-", End_data, sep = " ")))) #Selected stations and buffer area
  } else if(Data_source == "WA"){
    WQ_closest_selected_info <- WQ_closest_selected %>% dplyr::select(StationID, LocationID, Distance) %>% unique() %>% group_by(StationID) %>% summarise(LocationID = paste(LocationID, collapse = ", "), Distance = paste(Distance, collapse = ", "))
    (map <- tmap_leaflet(tm_shape(Estuary_area) + tm_polygons(col = "lightblue")+ #Estuary area
                           tm_shape(FL_outline) + tm_borders()+ #Outline of shoreline
                           tm_shape(WQ_locations_t,  "Possible stations") + tm_dots(size = 0.5, legend.show = FALSE, popup.vars = c("Latitude" = "Lat_n", "Longitude" = "Lon_n"))+ #Possible stations
                           tm_shape(WQ_closest_selected_info,  "Selected stations") + 
                           tm_dots(col = "red", size = 0.75, legend.show = TRUE, popup.vars = c("StationID" = "StationID", "LocationID" = "LocationID", "Distance" = "Distance"), popup.format = list())+
                           tm_layout(main.title = paste(Estuary_code, Data_source, "Closest", Stations_N, "WQ Stations", Begin_data, "-", End_data, sep = " ")))) #Selected stations and buffer area
  } else {
    print(paste0("Code not yet written for data source ", Data_source))
  }
  #
  if(WidgetSave == "Y"){saveWidget(map, paste0("Maps/Station_selection/", Estuary_code, "_", Data_source,"_closest_stations_", Begin_data, "_", End_data, "_widget.html"))}
  #
  return(list(StationsMap = All_map, SelectionMap = map, WQclosest = WQ_closest_selected))
}
#
##Station edits, final output file
Closest_data <- function(Adding, Removing, ProjectCode){
  if(is.na(Adding) & is.na(Removing)){
    WQ_stations_final <- WQ_closest_selected
  } else {
    if(Data_source == "Portal"){
      WQ_stations_final <- rbind(WQ_closest_selected, 
                                 #Stations to include
                                 WQ_data_t %>% 
                                   subset(MonitoringLocationIdentifier %in% Adding$StationID) %>% mutate(Distance = "NA") %>% 
                                   left_join(Adding, by = c("MonitoringLocationIdentifier" = "StationID")))  %>%
        #Stations to exclude
        subset(!MonitoringLocationIdentifier %in% Removing$StationID)
    } else {
      print(paste0("Code not yet written for data source ", Data_source))
    }
  }
  #
  ##Get coordinates into columns
  WQ_stations_final_df <- WQ_stations_final %>% st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>%
    mutate(Longitude = st_coordinates(.)[,1],
           Latitude = st_coordinates(.)[,2]) %>% dplyr::select(-geometry)
  #
  ##Code (3-4 letters preferred) to identify project selected data is for:
  Project_code <- ProjectCode
  #
  ##Export cleaned final data
  write_xlsx(WQ_stations_final_df, paste0("Data/Compiled_data/", Estuary_code, "_", Data_source, "_closest_selected_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
  #
  return(print(glimpse(WQ_stations_final)))
}
#
#
#
#
####Station selection - location/boundary####
#
#
location_boundary <- function(SelectionType, SelectedStations, BoundingBox, ProjectCode, WidgetSave){
  Type <- SelectionType
  Station_names <- SelectedStations
  bbox <- BoundingBox
  Project_code <- ProjectCode
  #
  #
  if(Type == "Station_name"){
    #Selection by station name
    if(Data_source == "Portal"){
      WQ_name_selected <- Filtered_data %>% subset(grepl(paste(Station_names, collapse = "|"), MonitoringLocationIdentifier))
      ##Export cleaned final data
      write_xlsx(WQ_name_selected, paste0("Data/Compiled_data/", Estuary_code, "_", Data_source, "_name_selected_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
      WQ_stations_final <- WQ_name_selected
      return(WQ_stations_final)
      #
    } else {paste("Code needs to be updated for selection of stations by name for ", Data_source, " data.", sep = "")}
    #
    } else if(Type == "Bounding_box"){
      #Selection within specified bounding box
      if(Data_source == "Portal"){
        WQ_bb_selected <- Filtered_data %>% filter(LatitudeMeasure < bbox[4] & LatitudeMeasure > bbox[2] & LongitudeMeasure > bbox[1] & LongitudeMeasure < bbox[3])
        WQ_stations_final <- WQ_bb_selected
        #Widget output to identify stations.
        Possible_stations <- st_as_sf(Filtered_data, coords = c(9,8), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
        WQ_locations_t <- st_as_sf(WQ_bb_selected, coords = c(9,8), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
        (map <- tmap_leaflet(tm_shape(Estuary_area) + tm_polygons(col = "lightblue")+ #Estuary area
                               tm_shape(FL_outline) + tm_borders()+ #Outline of shoreline
                               tm_shape(Possible_stations,  "Possible stations") + tm_dots(col = "black", size = 0.3, legend.show = TRUE, popup.vars = c("StationID" = "MonitoringLocationIdentifier"))+
                               tm_shape(WQ_locations_t,  "Selected stations") + tm_dots(col = "red", size = 0.75, legend.show = TRUE, popup.vars = c("StationID" = "MonitoringLocationIdentifier"))+
                               tm_layout(main.title = paste(Estuary_code, Data_source, "WQ Stations", Begin_data, "-", End_data, sep = " ")))) #Selected stations and buffer area
        if(WidgetSave == "Y"){saveWidget(map, paste0("Maps/Station_selection/", Estuary_code, "_", Data_source,"_bounding_box_", Begin_data, "_", End_data, "_widget.html"))}
        return(list(BoundedStations = WQ_stations_final, BoundedMap = map))
        #
        } else if(Data_source == "WA"){
          print(paste0("Code needs to be updated for Water Atlas data."))
          #Possible_stations <- st_as_sf(Filtered_data, coords = c(3,4), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
          #WQ_locations_t <- st_as_sf(WQ_bb_selected, coords = c(3,4), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
          #(map <- tmap_leaflet(tm_shape(Estuary_area) + tm_polygons(col = "lightblue")+ #Estuary area
          #                       tm_shape(FL_outline) + tm_borders()+ #Outline of shoreline
          #                       tm_shape(Possible_stations,  "Possible stations") + tm_dots(col = "black", size = 0.5, legend.show = TRUE, popup.vars = c("StationID" = "MonitoringLocationIdentifier"))+
          #                       tm_shape(WQ_locations_t,  "Selected stations") + tm_dots(col = "red", size = 0.75, legend.show = TRUE, popup.vars = c("StationID" = "StationID"), popup.format = list())+
          #                       tm_layout(main.title = paste(Estuary_code, Data_source, "WQ Stations", Begin_data, "-", End_data, sep = " ")))) #Selected stations and buffer area
          } else {print(paste0("Code not yet written for selection of stations within a bounding box for ", Data_source, " data."))}
      #
      #
    } else {
      #Other selection types
      paste("Code needs to be updated.")}
  }
#
#
#
##Station additions or removals, final output file
Modified_data <- function(Selection_Method, Adding, Removing, ProjectCode){
  ##Code (3-4 letters preferred) to identify project selected data is for:
  Project_code <- ProjectCode
  #
  if(is.na(Adding) & is.na(Removing)){
    if(Selection_Method == "Station_name"){
      #Selection by station name - no changes
      WQ_stations_final <- WQ_stations_selected
      write_xlsx(WQ_stations_final, paste0("Data/Compiled_data/", Estuary_code, "_", Data_source, "_name_selected_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
      #
      } else if(Selection_Method == "Bounding_box"){
        #Selection by bounding box - no changes
        WQ_stations_final <- WQ_stations_selected$BoundedStations
        write_xlsx(WQ_stations_final, paste0("Data/Compiled_data/", Estuary_code, "_", Data_source, "_bounding_box_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
      } else {print(paste0("Code not yet written for using ", Selection_Method, "."))}
  } else if(!is.na(Adding) | !is.na(Removing)){
    if(Selection_Method == "Station_name"){
      #Selection by station - with changes
      if(Data_source == "Portal"){
        if(is.na(Adding) & !is.na(Removing)){WQ_stations_final <- WQ_stations_selected %>% subset(!MonitoringLocationIdentifier %in% Removing$StationID)
        } else {WQ_stations_final <- rbind(WQ_stations_selected, WQ_selected %>% subset(MonitoringLocationIdentifier %in% Adding$StationID) %>% left_join(Adding, by = c("MonitoringLocationIdentifier" = "StationID")))}
        write_xlsx(WQ_stations_final, paste0("Data/Compiled_data/", Estuary_code, "_", Data_source, "_name_selected_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
        #Selection by station with other data sources:
        } else {print(paste0("Adding or removing stations by name for ", Data_source, "is not yet supported."))}
      } else if(Selection_Method == "Bounding_box"){
        #Selection by bounding box - with changes
        WQ_stations_selected_bb <- WQ_stations_selected$BoundedStations
        if(Data_source == "Portal"){
          if(is.na(Adding) & !is.na(Removing)){WQ_stations_final <- WQ_stations_selected_bb %>% subset(!MonitoringLocationIdentifier %in% Removing$StationID)
          } else {WQ_stations_final <- rbind(WQ_stations_selected, WQ_selected %>% subset(MonitoringLocationIdentifier %in% Adding$StationID) %>% left_join(Adding, by = c("MonitoringLocationIdentifier" = "StationID")))}
          write_xlsx(WQ_stations_final, paste0("Data/Compiled_data/", Estuary_code, "_", Data_source, "_bounding_box_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
          #Selection by bounding box with other data sources
        } else {print(paste0("Code not yet written for using bounding box to include/exclude ", Data_source, " stations."))}
        }
    } else if(!is.na(Adding) & !is.na(Removing)){
      if(Selection_Method == "Station_name"){
        if(Data_source == "Portal"){
          WQ_stations_final <- rbind(WQ_stations_selected, 
                                     WQ_selected %>% subset(MonitoringLocationIdentifier %in% Adding$StationID) %>% left_join(Adding, by = c("MonitoringLocationIdentifier" = "StationID")))  %>% subset(!MonitoringLocationIdentifier %in% Removing$StationID)
          write_xlsx(WQ_stations_final, paste0("Data/Compiled_data/", Estuary_code, "_", Data_source, "_name_selected_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
          #Selection by station with other data sources:
          } else {print(paste0("Adding or removing stations by name for ", Data_source, "is not yet supported."))}
        } else if(Selection_Method == "Bounding_box"){
          if(Data_source == "Portal"){
            WQ_stations_final <- rbind(WQ_stations_selected, 
                                       WQ_selected %>% subset(MonitoringLocationIdentifier %in% Adding$StationID) %>% left_join(Adding, by = c("MonitoringLocationIdentifier" = "StationID")))  %>% subset(!MonitoringLocationIdentifier %in% Removing$StationID)
            write_xlsx(WQ_stations_final, paste0("Data/Compiled_data/", Estuary_code, "_", Data_source, "_bounding_box_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
          } else {print(paste0("Adding or removing stations by name for ", Data_source, "is not yet supported."))}
        }
      }
  return(print(head(WQ_stations_final)))
  }
#
#
#
####Output all####
#
output_all <- function(WQ_selected){
  WQ_data_t <- st_as_sf(WQ_selected, coords = c(9,8), crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>% st_transform(crs = st_crs(3086))
  ##Convert FL_outline to same CRS
  FL_outline2 <- FL_outline %>% st_transform(crs = st_crs(3086))
  ##Determine stations that overlap with FL shape (land) and don't overlap (water)
  Stations_land <- WQ_data_t[lengths(st_intersects(WQ_data_t, FL_outline2))>0,] %>% mutate(Location = "Land")
  Stations_water <- WQ_data_t[!lengths(st_intersects(WQ_data_t, Stations_land)),] %>% mutate(Location = "Water")
  ##Join into final output dataframe of all stations
  WQ_Stations <- rbind(Stations_water, Stations_land) %>% mutate(Location = as.factor(Location))
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
  if(WidgetSave == "Y"){saveWidget(map, paste0("Maps/Station_selection/", Estuary_code, "_", Data_source,"_WQ_stations_", Begin_data, "_", End_data, "_widget.html"))}
  return(AllStations = map, WQStations = WQ_Stations)
}
#
##Station edits, final output file
finalize_data <- function(Adding, Removing, ProjectCode){
  if(is.na(Adding) & is.na(Removing)){
    WQ_stations_final <- WQ_closest_selected
  } else {
    if(Data_source == "Portal"){
      WQ_stations_final <- rbind(WQ_Stations, 
                                 #Stations to include
                                 WQ_data_t %>% 
                                   subset(MonitoringLocationIdentifier %in% Adding$StationID) %>% mutate(Buffer = "Extra") %>% 
                                   left_join(Adding, by = c("MonitoringLocationIdentifier" = "StationID")))  %>%
        #Stations to exclude
        subset(!MonitoringLocationIdentifier %in% Removing$StationID)
    } else {
      (WQ_stations_final <- WQ_Stations)
    }
  }
  #
  ##Get coordinates into columns
  WQ_stations_final_df <- WQ_stations_final %>% st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs") %>%
    mutate(Longitude = st_coordinates(.)[,1],
           Latitude = st_coordinates(.)[,2]) %>% dplyr::select(-geometry)
  #
  ##Code (3-4 letters preferred) to identify project selected data is for:
  Project_code <- ProjectCode
  #
  ##Export cleaned final data
  write_xlsx(WQ_stations_final_df, paste0("Data/Compiled_data/", Estuary_code, "_", Data_source, "_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)
  #
  return(print(glimpse(WQ_stations_final)))
}
#
#
#
#
#
#
#