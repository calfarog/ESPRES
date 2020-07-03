# ESPRES (Efficient Strategies for anthropogenic Pressure Reduction in European waterSheds) 

Authors:  Angel Udias, Cesar Alfaro, Alberto Pistocchi

Date: 25/06/2020


There are two sets (directories) of scripts, one for the quantity module and one for the quality module.
In each one of the folders there is a main code (main_ESPRES_QL.r, main_ESPRES_QT.r), and 
the rest of the scripts contain the different functions that are executed from the main
 code.


loadData_ESPRES_QT.r: This script contains the functions in charge of data loading and 
the shapefiles of the maps of each basin.

createScen_ESPRES_QT.r: This script contains the functions in charge of the general
 scenario with the current conditions of the basin that you decide to analyze.

evalStg_ESPRES_QT.r: This script contains the functions in charge of evaluating the
 pressure reduction strategies in the considered scenario. A strategy consists of the
 pressure reduction ratios in each region and each sector.

mooFunc_ESPRES_QT.r: This script contains the functions related to the multi-target 
optimization process, to find compromise strategies between pressure reductions and 
 
