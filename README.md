# Water-Quality-Processing
Code for compiling, cleaning, and summarizing water quality data from multiple sources. 

USEAGE:<br>
Set up R Project folder on local instance for Water-Quality-Processing work.<br>
Within the R Project folder, confirm the approriate folder schema uisng "Data", "Maps", "KML", and "Reference_documents" folders. 
Within the "Data" folder set up folders for "Raw_data", "Raw_cleaned", "Compiled_data", and "Reference_data" subfolders. *To limit project size, data files will not be synced.*<br> <br>
SCHEMA: <br>
\local\Water-Quality-Processing:   Files from repo\R Project files<br>
<br>
_Nested within the Water Quality Processing folder:_ 
*   *\Data :   Location for all data files with nested folders based on data types <br>
    *   *\Data\Raw_data :   Location for all portal and atlas raw data files <br>
    *   *\Data\Raw_cleaned:   Location for all cleaned data files (proper units, outliers removed, etc.)<br>
    *   *\Data\Compiled_data:   Location for summary data outputs (i.e. daily, monthly, etc.) <br>
    *   *\Data\Reference_data:   Location for data used to select or limit water quality data (i.e. daily, monthly, etc.) <br>
*   *\Maps :   Location for output of map files <br>
    *   *\Maps\Station_selection:   Location for output of WQ station map showing possible stations and selected stations<br> 
*   *\Reference documents :   Location for reference files <br>
*   *\KML:  Location for KML files for estuary boundaries and FL outline
