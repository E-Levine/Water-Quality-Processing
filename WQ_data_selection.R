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
#Years of possible data:
Start_year <- c("2015")
End_year <- c("2023")
#
##Years of desired data:
Begin_data <- c("2018")
End_data <- c("2022")
#
#
####Load Files#####
#
##Raw cleaned data
WQ_data <- as.data.frame(read_excel(paste0("../Water-Quality-Processing-Data/Data/Raw_cleaned/", Estuary_code, "_", Data_source, "_combined_filtered_",Start_year,"_", End_year,".xlsx"), na = c("NA", " ", "", "Z")))
#
#
#Fixed station locations
Station_locations <- as.data.frame(read_excel("../Water-Quality-Processing-Data/Data/Reference_data/Stations_area_selections.xlsx", na = c("NA", " ", "", "Z")))
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
##Code to limit list of monitoring stations if list contains more stations than needed: update subsetting code for desired stations
Stations_selected <- Station_locations %>% subset(Station < 6)
#
#
#
####Station selection by distance####
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
#saveWidget(map, paste0("../Water-Quality-Processing-Data/Maps/Station_selection/", Estuary_code, "_", Data_source,"_WQ_stations_", Begin_data, "_", End_data, "_widget.html"))
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
write_xlsx(WQ_stations_final_df, paste0("../Water-Quality-Processing-Data/Data/Raw_cleaned/", Estuary_code, "_", Data_source, "_selected_", Project_code, "_", Begin_data, "_", End_data,".xlsx"), format_headers = TRUE)

            
