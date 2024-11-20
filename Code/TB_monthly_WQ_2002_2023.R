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
               sf, raster, spData, psych,
               tmap, tmaptools, htmltools, htmlwidgets,
               factoextra, devtools, ggbiplot, corrplot, Hmisc,
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
#
##TB Portal 2002-2017 - Perna
TB_Portal <- read.csv("Data/Compiled_data/TB_combined_filtered_modified_2002_2017.csv", na = c("NA", " ", "", "Z"))
head(TB_Portal)
TB_Portal$ResultMeasure.MeasureUnitCode <- recode_factor(TB_Portal$ResultMeasure.MeasureUnitCode, 'mg/l' = "mg/L")
TB_Portal <- TB_Portal %>% mutate(Type = case_when((CharacteristicName == "Dissolved oxygen (DO)" & ResultMeasure.MeasureUnitCode == "mg/L") ~ "DO_mgL", 
                                                   (CharacteristicName == "Dissolved oxygen (DO)" & ResultMeasure.MeasureUnitCode == "%") ~ "DO",
                                                   (CharacteristicName == "Dissolved oxygen saturation" & ResultMeasure.MeasureUnitCode == "%") ~ "DO", 
                                                   TRUE ~ Type))
#
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
                   Unit = case_when(Type == "Temperature" ~ 'deg C', Type == "Salinity" ~ "PSS", Type == "pH" ~ "None", Type == "DO_mgL" ~ "mg/L", 
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
##Check parameters, rename as needed, subset to desired comparisons
unique(Compiled_df$Type)
Compiled_df$Type <- recode_factor(Compiled_df$Type, Tempature = "Temperature", 'Temperature, water' = "Temperature", "Dissolved oxygen (DO)" = "DO_mgL", "Dissolved oxygen saturation" = "DO", "Depth, Secchi disk depth" = "Secchi")
#
#
data_checks <- Compiled_df %>% group_by(Type) %>% summarise(min = min(Measure, na.rm = T), max = max(Measure, na.rm = T), mean = mean(Measure, na.rm = T))
#
Compiled_df_c <- Compiled_df %>% subset(Type %in% c("Temperature", "Salinity", "pH", "DO_mgL", "Secchi", "Turbidity")) %>% 
  mutate(Measure = case_when(Type == "Temperature" & Measure == 43696.00 ~ NA, #Remove incorrect Temp data
                             Type == "Secchi" & Unit == "ft" ~ Measure * 0.3048, 
                             Type == "Depth" & Unit == "ft" ~ Measure * 0.3048, #Convert to meters
                             Type == "DO_mgL" & Measure > 20 ~ NA, #Anything above 20 possibly %? - remove
                             Type == "Salinity" & Measure > 55 ~ NA, 
                             Type == "Salinity" & Measure == 0 ~ NA, 
                             Type == "Turbidity" & Measure > 200 ~ NA, 
                             TRUE ~ Measure)) %>%
  mutate(Unit = case_when(Type == "Secchi" | Type == "Depth" ~ "m", TRUE ~ Unit))
#
#
###Compile into data to analyze
#Individual = Month 
#Parameters = min/max/mean Temp, Sal, DO_mgL, pH, Turb
#
#
(Monthly_summ <- Compiled_df_c %>% 
  dplyr::select(Month, Type, Measure) %>% group_by(Month, Type) %>% summarise(range = max(Measure, na.rm = T)-min(Measure, na.rm = T), max = max(Measure, na.rm = T), mean = mean(Measure, na.rm = T)) %>%
  gather(Measurement, Value, -Month, -Type) %>% mutate(Helper = paste(Type, Measurement, sep = "_")) %>% dplyr::select(-Type, -Measurement) %>%
  spread(Helper, Value))
#
#
#
#
#
####PCA Fun-means and individuals####
#

##Means
#Summarize by Month, Year, Station
(WQ_means <- Compiled_df_c %>% 
   group_by(Year, Month, Station, Type) %>% summarise(Mean = mean(Measure, na.rm = T)) %>% spread(Type, Mean))
#
#
#
(Mon_summ <- Monthly_summ %>% #remove_rownames() %>% column_to_rownames("Month") %>% 
    dplyr::select(-contains("Secchi")))
#
model <- prcomp(Mon_summ[-1], scale = TRUE, center = TRUE)
summary(model)
model$rotation #Component loadings
fviz_eig(model) #Scree plot: 3-4 dimensions
#
ggbiplot(model, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE, ellipse.prob = 0.75) + theme_bw()
#
#
#
#
##Individuals
#
#Summarize by Month, Year, Station
(WQ_means <- Compiled_df_c %>% 
   group_by(Year, Month, Station, Type) %>% summarise(Mean = mean(Measure, na.rm = T)) %>% spread(Type, Mean))#
#
(WQ_data <- Compiled_df_c %>% ungroup() %>% drop_na(Measure) %>% group_by(Year, Month, Station, Type) %>% 
    summarise(Value = mean(Measure, na.rm = T)) %>% spread(Type, Value) %>% ungroup() %>% drop_na() %>% dplyr::select(-Year, -Station))
#(WQ_data_months <- WQ_data$Month)
#(WQ_data_values <- WQ_data[4:9])
#
##Select variables
#(corr_WQ <- WQ_data_values)
(cWQ <- cor(WQ_data[-1], method = "s"))
ctestWQ <- cor.mtest(WQ_data[-1], conf.level = 0.95)
corrplot(cor(WQ_data[-1], method = "s"), p.mat = ctestWQ$p, method = 'circle', type = 'lower', insig='blank',
         addCoef.col ='black', number.cex = 0.8, order = 'AOE', diag=FALSE)
rcorr(as.matrix(WQ_data[-1]), type = "spearman") #p-values of the correlations: ph & Salinity, Secchi & Salinity 
#Correlations between salinity and pH and Secchi - PCA can handle but removing for now
(WQ_fin <- WQ_data %>% dplyr::select(-pH, -Secchi))
#
#
WQ_pca <- prcomp(~Temperature + Salinity + DO_mgL  + Turbidity, data = WQ_fin, center = TRUE, scale. = TRUE)
str(WQ_pca)
summary(WQ_pca)
ggscreeplot(WQ_pca)
ggbiplot(WQ_pca, groups = WQ_data$Month, alpha = 0.4, 
         #scale = 1, #0-form biplot, 1-covariance biplot
         obs.scale = 1, 
         var.scale = 1, varname.size = 4.5, var.axes = TRUE,
         circle = TRUE) + theme_bw()
#
WQ_pca$rotation #"loadings"
#
#
fviz_pca_biplot(WQ_pca, 
                geom = "point", habillage = WQ_fin$Month, addEllipses = T, ellipse.level = 0.95)
#
#
#
####Cluster anlayses####

