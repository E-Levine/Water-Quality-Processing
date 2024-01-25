# Water-Quality-Processing
Code for compiling, cleaning, and summarizing water quality data from multiple sources. 

USEAGE:<br>
Set up R Project folder on local instance for Water-Quality-Processing work.<br>
Within the same directory, set up a data folder schema named Water-Quality-Processing-Data with "Data", "Maps", and "Reference documents" folders. 
Within the "Data" folder set up folders for "Raw_data", "Raw_cleaned", "Compiled_data", and "Reference_data" subfolders.<br> <br>
SCHEMA: <br>
\local\Water-Quality-Processing:   Files from repo\R Project files<br>
\local\Water-Quality-Processing-Data:   Relational folders<br> <br>
_Nested within the Water Quality Processing Data folder:_ 
*   *\Data :   Location for all data files with nested folders based on data types <br>
*   *\Data\Raw_data :   Location for all portal and atlas raw data files <br>
*   *\Data\Raw_cleaned:   Location for all cleaned data files (proper units, outliers removed, etc.)<br>
*   *\Data\Compiled_data:   Location for summary data outputs (i.e. daily, monthly, etc.) <br>
*   *   *\Data\Reference_data:   Location for data used to select or limit water quality data (i.e. daily, monthly, etc.) <br> 
*   *\Maps :   Location for output of map files <br>
*   *\Reference documents :   Location for reference files <br>
