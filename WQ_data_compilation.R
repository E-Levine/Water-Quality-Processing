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
pacman::p_load(plyr, tidyverse, readxl, #Df manipulation, basic summary
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