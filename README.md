# petrels-plastics
Global assessment of marine plastic exposure for oceanic birds.

Link to publication when available: 

## This repository contains two folders

### 1_full_analysis_petrels
This contains the full code used for the analysis presented in the linked publication. 
The input data folder contains the files and required to run the analysis except for the raw tracking data because those files cannot be shared publically. However, we have included the relevant outputs needed to create all figures and tables. 

### 2_analysis_with_example_datasets
This folder contains a simplified version of the code that removes any code relating to the specific datasets in the study. The input folder contains all the files required to run the analysis for three example tracking datasets that are included in the folder.

## How to Run
Both folders contain codes that should be run in the order provided by the numbers in the script names:
- 01_cleaning_data
  - Reads in tracking data files
  - Filters based on speed, location, exquinox periods, etc. appropriate to tag type (GPS/PTT/GPS)
  - Subsamples to 2 locations per day
  - Creates plots and exports to csvs in a new folder
- 02_populations
  -  Reads in cleaned tracking data
  -  Groups tracking datasets into populations
  -  Exports tracking data in 1 csv per population
  -  Exports csv of colony locations
- 03_kernels
  - Read in tracking data csvs for each population
  - Group into months, removing any with fewer than 5 locations
  - Use kernel density to create rasters of density of tracked locations for each month
  - Export rasters as tifs
  - Plot density maps and locations for checking
- 04_phenology
  - Read in tracking data csvs for each population
  - Calculate distance to the colony for each location
  - Estimate whether each month is more likely to be breeding or non-breeding for the tracked individuals in each population
  - Export csv containing estimated breeding schedules "04_phenology.csv"
  - Export plots for each population for checking
- 05_aggregate_1by1_months
  - Read in the plastics density raster
  - For each population for each month, read in the tracking location density rasters
  - Reformat the grid cells for each tracking data raster to match the plastic raster in 1x1 degrees
  - Scale so that the plastics raster and bird location rasters sum to 1
  - Multiply plastic x bird raster to produce exposure map
  - Sum scores in each exposure map to produce exposure score
  - Export exposure maps as tifs and images
  - Export exposure scores for eahc month for each population as csv
