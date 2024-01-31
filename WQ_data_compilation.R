####Water Quality Data Compilation###
#
##Compile WQ data from WQ Portal or Atlas - Select parameters, combine station and WQ data
##Output of cleaned data
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
#remotes::install_github("rstudio/htmltools") #After Rtools install: write('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', file = "~/.Renviron", append = TRUE);; Sys.which("make")
#
####Compilation setup####
#
#Set parameters - run for each data source type
Estuary_code <- c("TB") #Two letter estuary code
Data_source <- c("Portal") #"Portal", "WA" , or "FIM"
#
#Years of data:
Start_year <- c("2015")
End_year <- c("2023")
#
#Skip to line 47-52, then to 123 if working with FIM data
#
####Load files####
#
##Read in Excel site file
Location_data <- as.data.frame(read_excel(paste0("../Water-Quality-Processing-Data/Data/Raw_data/", Estuary_code, "_", Data_source,"_Site data_", Start_year, "_", End_year,".xlsx"), na = c("NA", " ", "", "Z")))
#
#Read in Excel results file (for 1 file) - skip to next section if only 1 results file
Results_data <- as.data.frame(read_excel(paste0("../Water-Quality-Processing-Data/Data/Raw_data/", Estuary_code, "_", Data_source,"_Results_", Start_year, "_", End_year,".xlsx"), na = c("NA", " ", "", "Z")))
#Read in Excel results file (for 2 files)
Results1 <- as.data.frame(read_excel(paste0("../Water-Quality-Processing-Data/Data/Raw_data/", Estuary_code, "_", Data_source,"_Results_", Start_year, "_", "2007",".xlsx"), na = c("NA", " ", "", "Z")))
Results2 <- as.data.frame(read_excel(paste0("../Water-Quality-Processing-Data/Data/Raw_data/", Estuary_code, "_", Data_source,"_Results_", "2008", "_", "2014",".xlsx"), na = c("NA", " ", "", "Z")))
Results3 <- as.data.frame(read_excel(paste0("../Water-Quality-Processing-Data/Data/Raw_data/", Estuary_code, "_", Data_source,"_Results_", "2015", "_", "2019",".xlsx"), na = c("NA", " ", "", "Z")))
Results4 <- as.data.frame(read_excel(paste0("../Water-Quality-Processing-Data/Data/Raw_data/", Estuary_code, "_", Data_source,"_Results_", "2020", "_", End_year,".xlsx"), na = c("NA", " ", "", "Z")))
Results_data <- rbind(Results1, Results2, Results3, Results4)
#If more files are needed, copy and edit the proper number of Results# lines of code and make sure to add all versions to the Results_data <- rbind() line.
#
##Estuary area  
Estuary_area <- st_read(paste0("KML/", Estuary_code, ".kml"))
plot(Estuary_area[2])
#
##State Outline
FL_outline <- st_read("KML/FL_Outlines/FL_Outlines.shp")
plot(FL_outline)
#
#
#
####Select data columns####
#
##Select location data
#List of columns to keep from original file for locating/mapping stations - (minimum columns that must be included to run code)
keep_site <- c("MonitoringLocationIdentifier", "OrganizationIdentifier", "OrganizationFormalName", "MonitoringLocationName", 
               "MonitoringLocationTypeName", "MonitoringLocationDescriptionText", "LatitudeMeasure", "LongitudeMeasure", 
               "HorizontalCoordinateReferenceSystemDatumName", "StateCode", "CountyCode", "ProviderName")
#Subset columns and add estuary ID to column
Location_data <- Location_data[keep_site] %>% add_column(Estuary = Estuary_code, .before = "MonitoringLocationIdentifier")
#Check columns
head(Location_data, 4)
#
#
#
##Select Results data
#Subset df by columns to keep - change list in include more columns as needed 
if(Data_source == "Portal"){
  keep_results_portal <- c("MonitoringLocationIdentifier", "ResultIdentifier", "ActivityStartDate", "ActivityStartTime/Time", 
                           "ActivityStartTime/TimeZoneCode", "CharacteristicName", "ResultMeasureValue", "ResultMeasure/MeasureUnitCode")
  Results <- Results_data[keep_results_portal]} else{
    keep_results_atlas <- "TEMP"
    Results <- Results_data[keep_results_atlas]}
#
#
#
#Confirm desired columns and naming
head(Results, 4)
#
#
#
####Combine data by station - limit to desired parameters####
#
Combined <- merge(Location_data, Results, by = "MonitoringLocationIdentifier")
#
###Filter combined file to only include specified characteristics 
#List of possible characteristics to select from
unique(Combined$CharacteristicName)
#
#Assemble list of characteristics to KEEP
Characters <- c("Salinity", "Temperature, water", "Depth, bottom", "Depth, Secchi disk depth", "Temperature, air, deg C", "Turbidity", 
                "Conductivity", "Specific conductance", "pH", "Dissolved oxygen (DO)", "Dissolved oxygen saturation", 
                "Chlorophyll a, corrected for pheophytin", "Chlorophyll a", "Total dissolved solids", "Total suspended solids", "Zooplankton", 
                "Diatoms", "Stream flow, instantaneous", "Flow, severity (choice list)", "Stream stage", "Flow", "Stream flow, mean. Daily")
#
#Filter to only the desired characteristics and check remaining list
Combined_filtered <- Combined %>% filter(CharacteristicName %in% Characters)
#
##Correct basic typos in units/provide clarification - mg/L, mg/m3 = ug/L
Combined_filtered$`ResultMeasure/MeasureUnitCode` <- str_replace(Combined_filtered$`ResultMeasure/MeasureUnitCode`, "mg/l", "mg/L")
Combined_filtered$`ResultMeasure/MeasureUnitCode` <- str_replace(Combined_filtered$`ResultMeasure/MeasureUnitCode`, "mg/m3", "ug/L")
Combined_filtered$`ResultMeasure/MeasureUnitCode` <- str_replace(Combined_filtered$`ResultMeasure/MeasureUnitCode`, "ug/l", "ug/L")
Combined_filtered$`ResultMeasure/MeasureUnitCode` <- str_replace(Combined_filtered$`ResultMeasure/MeasureUnitCode`, "ft3/sec", "cfs")
#
#Confirm list of characters selected.
unique(Combined_filtered$CharacteristicName)
#
#
#
#
####Map of stations and limitation of stations to estuary area####
#
#Transform CRS and data of WQ data to spatial data
WQ_sp <- spTransform(SpatialPointsDataFrame(coords = Combined_filtered[,c(9,8)], data = Combined_filtered,
                                            proj4string = CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs +type=crs")),
                     "+proj=longlat +datum=WGS84 +no_defs +type=crs")
#
#SKIP 123-125 if NOT working with FIM data:: Assign FIM data to working data frame
Combined_filtered <- as.data.frame(read_excel(paste0("../Water-Quality-Processing-Data/Data/Raw_data/", Estuary_code, "_", Data_source,"_", Start_year, "_", End_year,".xlsx"), na = c("NA", " ", "", "Z"))) %>% 
  dplyr::select(TripID, Reference, Sampling_Date, StartTime, Depth, Temperature, pH, Latitude, Longitude, everything()) %>% filter(Longitude != "NULL") %>% mutate(Longitude = as.numeric(Longitude), Latitude = as.numeric(Latitude))
WQ_sp <- SpatialPointsDataFrame(coords = Combined_filtered[,c(9,8)], data = Combined_filtered, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs"))
#
#Check CRS s
crs(Estuary_area)
crs(WQ_sp)
#
##Crop to estuary area
Estuary_data <- WQ_sp[as.vector(st_intersects(Estuary_area, st_as_sf(WQ_sp), sparse = FALSE)), ]
##Run following lines only if issue with Loop above
#sf_use_s2(FALSE)
#Estuary_data <- WQ_sp[lengths(st_intersects(Estuary_area, st_as_sf(WQ_sp))>0,]
#sf_use_s2(TRUE)
#
Estuary_data@data <- Estuary_data@data %>% mutate(KML = "In") #Add KML classification for Inside KML area
Outside_data <- WQ_sp[as.vector(st_disjoint(Estuary_area, st_as_sf(WQ_sp), sparse = FALSE)), ]
Outside_data@data <- Outside_data@data %>% mutate(KML = "Out")
Combined_data <- union(Estuary_data, Outside_data)
#
#Visualize data locations
if(Data_source == "Portal"){
  (map <- tmap_leaflet(tm_shape(Estuary_area) + #Estuary area
                         tm_polygons() + 
                         tm_shape(FL_outline) + #Outline of shoreline
                         tm_borders()+
                         tm_shape(Combined_data) + #Stations relation to estuary area
                         tm_dots("KML", palette = c(In = "red", Out = "black"), size = 0.25, legend.show = TRUE,
                                 popup.vars = c("StationID" = "MonitoringLocationIdentifier", "Latitude" = "LatitudeMeasure", "Longitude" = "LongitudeMeasure")) +
                         tm_layout(main.title = paste(Estuary_code, Data_source, "WQ Stations", sep = " "))))
} else if (Data_source == "FIM") {
  (map <- tmap_leaflet(tm_shape(Estuary_area) + #Estuary area
                         tm_polygons() + 
                         tm_shape(FL_outline) + #Outline of shoreline
                         tm_borders()+
                         tm_shape(Combined_data) + #Stations relation to estuary area
                         tm_dots("KML", palette = c(In = "red", Out = "black"), size = 0.25, legend.show = TRUE,
                                 popup.vars = c("StationID" = "Reference", "Latitude" = "Latitude", "Longitude" = "Longitude")) +
                         tm_layout(main.title = paste(Estuary_code, Data_source, "WQ Stations", sep = " "))))
}
#
saveWidget(map, paste0("../Water-Quality-Processing-Data/Maps/", Estuary_code, "_", Data_source,"_WQ_stations_", Start_year, "_", End_year, "_widget.html"))
#
#
####Clean parameter data####
#
Combined_filteredk <- Combined_data@data 
#Skip 171-182 if working with FIM data
Combined_filteredk <- Combined_filteredk %>% 
  mutate(ResultMeasureValue = as.numeric(ifelse(CharacteristicName == "Specific conductance" & 'ResultMeasure/MeasureUnitCode' == "mS/cm", #Convert Spec Cond mS to uS
                                                ResultMeasureValue*1000, 
                                                ifelse(CharacteristicName == "Stream flow, instantaneous" & 'ResultMeasure/MeasureUnitCode' == "ft3/s", #Convert ft3 to m3
                                                       ResultMeasureValue*0.0283168, ResultMeasureValue))),
         'ResultMeasure/MeasureUnitCode' = ifelse(CharacteristicName == "Salinity", "ppt",  #Change all Salinity values to 'ppt' units
                                                  ifelse(CharacteristicName == "Conductivity", "uS/cm", #Correct all Conductivity results
                                                         ifelse(CharacteristicName == "Specific conductance", "uS/cm",#Correct Specific conductance units
                                                                ifelse(CharacteristicName == "pH", NA,  #Correct pH units
                                                                       ifelse(CharacteristicName == "Stream flow, instantaneous", "m3/s", #Correct Stream flow units
                                                                              Combined_filtered$'ResultMeasure/MeasureUnitCode')))))) %>% 
  dplyr::relocate(KML, .after = last_col())
#
head(Combined_filteredk)
#
#
Comb_fil_1 <- Combined_filteredk %>% filter(ActivityStartDate < "2012-01-01")
Comb_fil_2 <- Combined_filteredk %>% filter(ActivityStartDate >= "2012-01-01")
#
#
####Save filtered data####
#
#
write_xlsx(Combined_filteredk, paste0("../Water-Quality-Processing-Data/Data/Raw_cleaned/", Estuary_code, "_", Data_source, "_combined_filtered_", Start_year, "_", End_year,".xlsx"), format_headers = TRUE)
#
##Dividing files - use below
write_xlsx(Comb_fil_2, paste0("../Water-Quality-Processing-Data/Data/Raw_cleaned/", Estuary_code, "_", Data_source, "_combined_filtered_2012_", End_year,".xlsx"), format_headers = TRUE)
#