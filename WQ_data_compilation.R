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
pacman::p_load(plyr, tidyverse, readxl, writeXL, #Df manipulation, basic summary
               ggmap, tibble, zoo, measurements,
               install = TRUE) 
#
#
####Compilation setup####
#
#Set parameters - run for each data source type
Estuary_code <- c("LW") #Two letter estuary code
Data_source <- c("Portal") #"Portal" or "WA" 
#
#Years of data:
Start_year <- c("2000")
End_year <- c("2022")
Split_files <- c("N") #Are the results/data files split due to size ("Y" or "N")?
#
#
#
####Load files####
#
##Read in Excel site file
Location_data <- as.data.frame(read_excel(paste0("../Water-Quality-Processing-Data/Data/Raw_data/", Estuary_code, "_", Data_source,"_Site data_", Start_year, "_", End_year,".xlsx"), na = c("NA", " ", "", "Z")))
#
#Read in Excel results file (for 1 file) - skip to next section if only 1 results file
Results_data <- as.data.frame(read_excel(paste0("../Water-Quality-Processing-Data/Data/Raw_data/", Estuary_code, "_", Data_source,"_Results_", Start_year, "_", End_year,".xlsx"), na = c("NA", " ", "", "Z")))
#Read in Excel results file (for 2 files)
Results1 <- as.data.frame(read_excel(paste0("Raw_data/", Estuary_code, "_", Data_source,"_Results_", Start_year, "_", "YYYY",".xlsx"), na = c("NA", " ", "", "Z")))
Results2 <- as.data.frame(read_excel(paste0("Raw_data/", Estuary_code, "_", Data_source,"_Results_", "YYYY", "_", End_year,".xlsx"), na = c("NA", " ", "", "Z")))
Results_data <- rbind(Results1, Results2)
#If more files are needed, copy and edit the proper number of Results# lines of code and make sure to add all versions to the Results_data <- rbind() line.
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
  Results <- Results_data[keep_results_portal]} else
  {keep_results_atlas <- "TEMP"
  Results <- Results_data[keep_results_atlas]}
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
####Clean parameter data####
#
Combined_filtered <- Combined_filtered %>% 
  mutate(ResultMeasureValue = as.numeric(ifelse(CharacteristicName == "Specific conductance" & 'ResultMeasure/MeasureUnitCode' == "mS/cm", #Convert Spec Cond mS to uS
                                                ResultMeasureValue*1000, 
                                                ifelse(CharacteristicName == "Stream flow, instantaneous" & 'ResultMeasure/MeasureUnitCode' == "ft3/s", #Convert ft3 to m3
                                                       ResultMeasureValue*0.0283168, ResultMeasureValue))),
         'ResultMeasure/MeasureUnitCode' = ifelse(CharacteristicName == "Salinity", "ppt",  #Change all Salinity values to 'ppt' units
                                                  ifelse(CharacteristicName == "Conductivity", "uS/cm", #Correct all Conductivity results
                                                         ifelse(CharacteristicName == "Specific conductance", "uS/cm",#Correct Specific conductance units
                                                                ifelse(CharacteristicName == "pH", NA,  #Correct pH units
                                                                       ifelse(CharacteristicName == "Stream flow, instantaneous", "m3/s", #Correct Stream flow units
                                                                              Combined_filtered$'ResultMeasure/MeasureUnitCode'))))))
#
head(Combined_filtered)
#
#
#
#
####Save filtered data####
#
#
write_xlsx(Combined_filtered, paste0("../Water-Quality-Processing-Data/Data/Raw_cleaned/", Estuary_code, "_", Data_source, "_combined_filtered_", Start_year, "_", End_year,".xlsx"), format_headers = TRUE)
#
#