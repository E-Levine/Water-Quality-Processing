####Water Quality Data Combination###
#
##Combination of WQ data into single file
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
source("Code/WQ_functions.R")
#
#
###Setup - specs####
Estuary_code <- c("SL") #Two letter estuary code
Data_source <- c("FIM") #"Portal" or "WA". Code not currently updated for "FIM"
#
#Number of files to combine (Enter 1 if only one file. Current max is 3 files):
Filtered_files <- c(1)
#Years of possible data (from file names). Start and end years required for each file. Use 'NA" for any unused files:
Start_year <- c("1997")
End_year <- c("2022")
Start_year_2 <- c(NA)
End_year_2 <- c(NA)
Start_year_3 <- c(NA)
End_year_3 <- c(NA)
#
##Years of desired data:
Begin_data <- c("2000")
End_data <- c("2023")
#
#