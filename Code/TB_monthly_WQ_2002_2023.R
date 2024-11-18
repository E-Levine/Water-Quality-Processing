##Tampa Bay water quality data exercsise
#
#Using portal data 2002-2017 (Perna project) and 2015-2023, and Molluscan Fisheries data
#Comparing months for "Seasonal" groups
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
####Load files#####
#
##TB Molluscan Fisheries
TB_WQ <- read_excel("Data/Compiled_data/TB_Molluscan_2002_2023.xlsx", na = c("NA", " ", "", "Z"), skip = 0, 
                    col_names = TRUE, col_types = c("date", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"),
                    trim_ws = TRUE, .name_repair = "unique")
head(TB_WQ)
colnames(TB_WQ) <- c("Date", "Site", "Station", "Time", "Depth", "Temperature", "Salinity", "pH", "DO_mgL", "DO", "Secchi", "Turbidity", "T_Hach")
#TB stations LatLong
TB_WQ_LL <- read_excel("Data/Reference_data/Stations_area_selections.xlsx", na = c("NA", " ", "", "Z"), skip = 0, 
                       col_names = TRUE, trim_ws = TRUE, .name_repair = "unique") %>% mutate(Station = as.factor(Station)) 
head(TB_WQ_LL)
#
##TB Portal 2002-2017 - Perna
TB_Portal <- read.csv("Data/Compiled_data/TB_combined_filtered_modified_2002_2017.csv", na = c("NA", " ", "", "Z"))
head(TB_Portal)
#
##TB Portal 2015-2023
TB_Portal_2 <- read_excel("Data/Compiled_data/TB_Portal_selected_AllTB_2015_2023.xlsx", na = c("NA", " ", "", "Z"), skip = 0, 
                    col_names = TRUE, trim_ws = TRUE, .name_repair = "unique")
head(TB_Portal_2)
#
#
#
#
####Data frame cleaning####
#
head(TB_WQ)
head(TB_Portal)
head(TB_Portal_2)
#
(Compiled_df <- full_join(TB_WQ %>% dplyr::select(-Time) %>% gather("Type", "Measure", -Date, -Site, -Station) %>% 
            mutate(Station = as.factor(Station), Month = as.factor(format(Date, "%m")), Year = as.factor(year(Date)),
                   Bay = case_when(Station == 1 | Station == 5 ~ "MTB", Station == 7 | Station == 8 ~ "LR", TRUE ~ "LTB"),
                   Unit = case_when(Type == "Temp" ~ 'deg C', Type == "Salinity" ~ "PSS", Type == "pH" ~ "None", Type == "DO_mgL" ~ "mg/l", 
                                    Type == "DO" ~ "%", Type == "T_Probe" ~ "NTU", Type == "T_Hach" ~ "NTU", TRUE ~ "m")) %>% left_join(TB_WQ_LL),
          TB_Portal %>% dplyr::select(-CharacteristicName) %>% mutate(ResultMeasureValue = as.numeric(ResultMeasureValue), 
                                                                      Month = as.factor(sprintf("%02d", Month)), Year = as.factor(Year)) %>%
            rename("Station" = MonitoringLocationIdentifier, "Measure" = ResultMeasureValue, 
                   "Unit" = 'ResultMeasure.MeasureUnitCode', "Latitude" = LatitudeMeasure, 
                   "Longitude" = LongitudeMeasure)) %>%
  full_join(TB_Portal_2 %>% dplyr::select(ActivityStartDate, Estuary, MonitoringLocationIdentifier, CharacteristicName, ResultMeasureValue, Result_Unit, Longitude, Latitude) %>%
              mutate(ActivityStartDate = as.Date(ActivityStartDate, format = "%Y-%m-%d"), Month = as.factor(format(ActivityStartDate, "%m")), Year = as.factor(format(ActivityStartDate, "%Y"))) %>%
              rename("Date" = ActivityStartDate, "Bay" = Estuary, "Station" = MonitoringLocationIdentifier, "Type" = CharacteristicName, 
                     "Measure" = ResultMeasureValue, "Unit" = 'Result_Unit')) %>% 
    dplyr::select(Date, Year, Month, Bay, Longitude, Latitude, everything()) %>% ungroup() %>% drop_na(Month))
#
#
#
#
###Compile into data to analyze
#Individual = Month 
#Parameters = min/max/mean Temp, Sal, DO_mgL, pH, Turb
#
##Check parameters, rename as needed, subset to desired comparisons
unique(Compiled_df$Type)
Compiled_df$Type <- recode_factor(Compiled_df$Type, Tempature = "Temperature", 'Temperature, water' = "Temperature", 'Dissolved oxygen (DO)' = "DO_mgL", 'Dissolved oxygen saturation' = "DO", 'Depth, Secchi disk depth' = "Secchi") 
#
(Monthly_summ <- Compiled_df %>% subset(Type %in% c("Temperature", "Salinity", "pH", "DO_mgL", "Secchi", "Turbidity")) %>% 
  dplyr::select(Month, Type, Measure) %>% group_by(Month, Type) %>% summarise(min = min(Measure, na.rm = T), max = max(Measure, na.rm = T), mean = mean(Measure, na.rm = T)) %>%
  gather(Measurement, Value, -Month, -Type) %>% mutate(Helper = paste(Type, Measurement, sep = "_")) %>% dplyr::select(-Type, -Measurement) %>%
  spread(Helper, Value))

Compiled_df %>% subset(Type %in% c("Temperature", "Salinity", "pH", "DO_mgL", "Secchi", "Turbidity")) %>% 
  dplyr::select(Month, Type, Measure) %>% spread(Type, Measure)
#