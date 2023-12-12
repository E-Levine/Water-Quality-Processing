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
#Set parameters
Data_source <- c("Portal") #"Portal" or "WA" 
#Need to continue.