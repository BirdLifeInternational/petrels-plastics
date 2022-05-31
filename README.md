# petrels-plastics
Global assessment of marine plastic exposure for oceanic birds.

Link to publication when available: 

## Folder descriptions

### 1_full_analysis_petrels
This contains the full code used for the analysis presented in the linked publication. 
The input data folder contains the files and required to run the analysis except for the raw tracking data because those files cannot be shared publically. However, we have included the relevant outputs needed to create all figures and tables. 

### 2_analysis_with_example_datasets
This folder contains a simplified version of the code that removes any code relating to the specific datasets in the study. The input folder contains all the files required to run the analysis for three example tracking datasets that are included in the folder.

## How to Run
Both folders contain codes that should be run in the order provided by the numbers in the script names:
- 01_cleaning_data
  - Reads in tracking data files
  - Filters based on species, speed, location, exquinox periods, etc. appropriate to tag type (GPS/PTT/GPS)
  - Subsamples to 2 locations per day
  - Creates plots and exports to csvs in a new folder
- 02_populations
  -  Reads in cleaned tracking data
  -  Groups tracking datasets into populations
  -  Exports tracking data in 1 csv per population
  -  Exports csv of colony locations, and sample sizes for population and species
- 03_kernels
  - Read in tracking data csvs for each population
  - Group into months, removing any with fewer than 5 locations
  - Use kernel density to create rasters of density of tracked locations for each month
  - Export rasters as tifs
  - Plot density maps and locations for checking
- 04_aggregate_1by1_months
  - Read in the plastics density raster
  - For each population for each month, read in the tracking location density rasters
  - Reformat the grid cells for each tracking data raster to match the plastic raster in 1x1 degrees
  - Scale so that the plastics raster and bird location rasters sum to 1
  - Multiply plastic x bird raster to produce exposure map
  - Sum scores in each exposure map to produce exposure score
  - Export exposure maps as tifs and images
  - Export exposure scores for each month for each population as csv
- 05_combine_by_population
  - read in rasters for each population, export 1 mean raster per population
  - import the monthly exposure scores and export mean by population
- 06_phenology
  - Read in tracking data csvs for each population
  - Calculate distance to the colony for each location
  - Estimate whether each month is more likely to be breeding or non-breeding for the tracked individuals in each population
  - Export csv containing estimated breeding schedules "06_phenology.csv"
  - Export plots for each population for checking - 06_phenology
- 07_combine_by_seasons
  - read in rasters for each month
  - read in breeding schedules (from phenology script or literature)
  - export 1 mean exposure map per population per season
  - import the monthly exposure scores and export mean by population
  - test for differences from using different breeding schedules (literature v 06_phenology script)
- 08_combine_by_species
  - read in rasters for each population
  - read in population sizes 
  - read in breeding schedules (from phenology script or literature)
  - export 1 mean raster per species weighted by population size and seasons tracked
  - import the monthly exposure scores and export mean by species weighted by population size and seasons tracked
- ##_combine by country
- ##_overlay_with_EEZ
- ##_plot maps
- ##_plot results graphs



## Input files
- 01_cleaning_data
  - Tracking data csvs in folder "input_data/tracking_data"
  - World land boundary shapefile "world-dissolved" in folder input_data/baselayer"
  - "Species_list_IUCN.csv" in folder "input_data"
  - "equinoxes.csv" in folder "input_data"
- 02_populations
  - World land boundary shapefile "world-dissolved" in folder input_data/baselayer"
  - Cleaned tracking data csvs per datasets produced by script "01_cleaning_data" in folder "outputs"
- 03_kernels
  - World land boundary shapefile "world-dissolved" in folder input_data/baselayer"
  - Cleaned tracking data csvs per population produced by script "02_populations" in folder "outputs"
- 04_aggregate_1by1_months
  - World land boundary shapefile "world-dissolved" in folder input_data/baselayer"
  - Plastics density raster ".tif" in fodler "input_data"
  - Kernel density rasters ".tif" produced by "03_kernels" in folder "outputs"
- 05_combine_by_population
  - monthly tracking data distribution .tifs in folder "/outputs/04_aggregate_1by1_grid"
  - exposure scores in file "/outputs/05_exposure_scores_by_month.csv"
- 06_phenology
  - World land boundary shapefile "world-dissolved" in folder input_data/baselayer"
  - Cleaned tracking data csvs per population produced by script "02_populations" in folder "outputs"
- 07_combine_by_season
  - World land boundary shapefile "world-dissolved" in folder input_data/baselayer"
  - Plastics density raster ".tif" in fodler "input_data"
  - monthly tracking data distribution .tifs in folder "/outputs/04_aggregate_1by1_grid"
  - exposure scores in file "/outputs/05_exposure_scores_by_month.csv"
  - breeding schedules from "/input_data/breeding_months.csv" or "/outputs/06_phenology.csv"
- 08_combine_by_species
  - exposure scores in file "/outputs/05_exposure_scores_by_month.csv"
  - breeding schedules from "/input_data/breeding_months.csv" or "/outputs/06_phenology.csv"
  - "population_sizes.csv" in folder "input_data"
- 09_overlay_with_EEZ
  - EEZ shapefile in folder "input_data"


## Graphical representation of the method 
Methods for calculating marine plastic exposure scores from tracking data. Input datasets are represented by rectangles, method steps are represented by white rounded rectangles, sections of related method steps are represented by grey rounded rectangles, and outputs are represented by circles. Scores can be used to compare exposure to plastic pollution among populations, seasons, and species. Maps can be used to locate areas of high plastic exposure, and percentages within spatial boundaries can be used to identify and rank regions of high exposure among all species and relate these to specific populations from specific jurisdictions.

![image](https://user-images.githubusercontent.com/56324426/170684706-b4631aaa-f54d-4da8-8329-d4a3d1e45803.png)
