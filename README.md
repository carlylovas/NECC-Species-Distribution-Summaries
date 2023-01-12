# NECC-Species-Distribution-Summaries

## Overivew
This repository supports analysis of the NOAA NMFS Northeast Fisheries Science Center spring and fall bottom trawl survey data to calculate a suite of metrics summarizing the spatial and temporal distribution of ~40 focal species. Ultimately, these spatial and temporal distribution summaries will be used to see if there are relationships among changes in spatial distribution and changes in marine species diet as part of the NSF funded Nutritional Ecology of Climate Change Project with folks from UNH. 

## Navigating the Repo
#DATA
This folder contains the list of fishes included in the NECC study as well as the Rds and RData workspace associated with this project. 

#R
This folder contains the prep code for cleaning the survey data (01_SurveyDataPrep.R), code for the NECC species distribution analysis (02_SpeciesDistAnalysis) and @carlylovas' workspace (NECC.R). Additionally, this folder contains an R script entitled helper_funcs.R which contains the code for three functions used in this analysis. 

#TEMP_RESULTS
There are two subfolders: Plot_maps and Tables. These folders contain output results from the above analysis. Plot_maps will contain graphs and distribution maps pertaining to 02_SpeciesDistAnaylsis and Tables contains multiple CSV files of outputs. Within Tables you will find three CSV files:
         
         
         (1) NEFSC_YearSeasonDistributionSummaries.csv
                      This table contains the year-season center of latitude and longitude per species. Additionally, there are two columns including the                         rate of change associated with the center of latitude and center of longitude, respectively. 
         
         
         
         (2) NEFSC_YearlyDistributionSummaries.csv
                      This table includes the changes in center of latitude and longtitude per species per year. Rate of change is also included. 
         
         
         
         (3) seasonal_distance.csv (name subject to change)
                      This table includes the rate of change of distance (in meters) between the Spring and Fall center of gravity (lat and long)                               per species and the recorded number of observations. 
