# NECC-Species-Distribution-Summaries

## Overview

This repository supports analysis of the NOAA NMFS Northeast Fisheries Science Center spring and fall bottom trawl survey data to calculate a suite of metrics summarizing the spatial and temporal distribution of \~40 focal species. Ultimately, these spatial and temporal distribution summaries will be used to see if there are relationships among changes in spatial distribution and changes in marine species diet as part of the NSF funded Nutritional Ecology of Climate Change Project with folks from UNH.

## Navigating the Repo

#### Markdowns containing recent predator/prey overlap can be found here

-   [Movement classifications](https://carlylovas.github.io/NECC-Species-Distribution-Summaries/R/Continuing%20work%20scripts/movement.html)
-   [Overlap metrics](https://carlylovas.github.io/NECC-Species-Distribution-Summaries/R/Continuing%20work%20scripts/overlap.html)
-   [Seasonal migration](https://carlylovas.github.io/NECC-Species-Distribution-Summaries/R/Continuing%20work%20scripts/seasonal_migration.html)
-   [Species profiles](https://carlylovas.github.io/NECC-Species-Distribution-Summaries/R/NECC%20scripts/Updated_Species_Dist.html)

#### Data

This folder contains the list of fishes included in the NECC study as well as the Rds and RData workspace associated with this project.

#### R

This folder contains the prep code for cleaning the survey data `01_SurveyDataPrep.R`, code for the NECC species distribution analysis `02_SpeciesDistAnalysis`, code for plotting `03_Plotting_Mapping`. There are two additional scripts; , @carlylovas' workspace `NECC.R`, and `helper_funcs.R` which contains the code for three functions used in this analysis.

#### Temp_Results

There are three folders: **Plots**, **Maps** and **Tables**. These folders contain output results from the above analysis.

**Plots**

There are two subfolders within **Plots**; `NECC_plots` and `Decadal_plots`. `NECC_plots` contains five plots depicting changes in center of latitude, seasonal distance, and rate of change. All are appropriately labeled. `Decadal_plots` contains three separate folders to house plots depicting changes in surface temperature, bottom temperature, and depth over time.

**Maps**

There is currently one folder within **Maps**. `NECC_maps` show changes in Fall and Spring center of biomass for each species *per decade*.

**Tables**

There are six CSV files within **Tables**

`NEFSC_YearSeason_Center_of_Biomass.csv`

This file contains the year-season center of latitude and longitude per species.

`NEFSC_Yearly_Center_of_Biomass.csv`

This file includes the changes in center of latitude and longtitude per species per year.

`NEFSC_YearSeason_Rate_of_Change.csv`

This file contains the rate of change for center of latitude and longitude (`slopeLat` and `slopeLon`, respectively) per species *per season*, along with the associated signficance values (p.value).

`NEFSC_Yearly_Rate_of_Change.csv`

This file contains the rate of change for center of latitude and longitude (`slopeLat` and `slopeLon`, respectively) per speices, along with the associated signficance values (p.value).

`Seasonal_Distance_CofBiomass.csv`

This file contains the distance (in km) in which the center of biomass moved between spring and fall surveys per species, per year.

`Seasonal_Rate_Change.csv`

This file contains the rate of which the distance between spring and fall center of biomass changed over time, per species.
