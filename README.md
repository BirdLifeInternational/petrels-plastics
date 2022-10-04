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
- 00_plastics_raster
  - If needed, read in the plastic model estimate csvs, take and average and write out as a raster .tif
- 01_cleaning_data
  - Read in tracking data files
  - Filter based on species, speed, location, exquinox periods, etc. appropriate to tag type (GPS/PTT/GPS)
  - Subsample to 2 locations per day
  - Create plots and exports to csvs in a new folder
- 02_populations
  -  Read in cleaned tracking data
  -  Group tracking datasets into populations
  -  Export tracking data in 1 csv per population
  -  Export csv of colony locations, and sample sizes for population and species
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
  - Read in rasters for each population, export 1 mean raster per population
  - Import the monthly exposure scores and export mean by population
- 06_phenology
  - Read in tracking data csvs for each population
  - Calculate distance to the colony for each location
  - Estimate whether each month is more likely to be breeding or non-breeding for the tracked individuals in each population
  - Export csv containing estimated breeding schedules "06_phenology.csv"
  - Export plots for each population for checking - 06_phenology
- 07_combine_by_seasons
  - Read in rasters for each month
  - Read in breeding schedules (from phenology script or literature)
  - Export 1 mean exposure map per population per season
  - Import the monthly exposure scores and export mean by population
  - Test for differences from using different breeding schedules (literature v 06_phenology script)
- 08_combine_by_species
  - Read in rasters for each population
  - Read in population sizes 
  - Read in breeding schedules (from phenology script or literature)
  - Export 1 mean raster per species weighted by population size and seasons tracked
  - Import the monthly exposure scores and export mean by species weighted by population size and seasons tracked
- 09_plot maps
  - Read in the all species maps from 08_combine_by_species and the plastic raster and creates world maps for Fig. 1 and Fig. 4a
    - Species richness with study colony locations
    - All species tracking location distribution
    - Plastic density estimate
    - Exposure of petrels to plastics
    - Exposure overlayed with EEZ outlines
  - Read in distributions for case studies and produces maps for Fig. 3
    - European Storm-petrels from 5 colonies
    - Yelkuoan and Scopoli's Shearwaters from Malta
    - Cook's Petrels from 2 colonies
- 10_exposure_score_plots
  - Read in the csvs of exposure scores by season and species and creates plots in Fig. 2 and Fig. 3a
- 11_eez_all_species_combined
  - Calculate proportions of exposure among marine political regions (EEZs and the high seas)
  - Plot the results for Fig. 4b
- 12_combine_countries
  - Combine population distributions by country for each species
- 13_eez_by_population
  - Calculation the proportion of exposure in each EEZ (or high seas) for each species in each breeing country
  - Plot the results for Fig. 4c

## Input files
- 00_plastics_raster
  - In folder "/input_data/plastics_data/": "lebretonmodel_abundance.csv", "maximenkomodel_abundance.csv" & "vansebillemodel_abundance.csv"
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
  - Plastics density raster "/outputs/00_PlasticsRaster.tif"
  - Kernel density rasters ".tif" produced by "03_kernels" in folder "outputs"
- 05_combine_by_population
  - Monthly tracking data distribution .tifs in folder "/outputs/04_aggregate_1by1_grid"
  - Exposure scores in file "/outputs/05_exposure_scores_by_month.csv"
- 06_phenology
  - World land boundary shapefile "world-dissolved" in folder input_data/baselayer"
  - Cleaned tracking data csvs per population produced by script "02_populations" in folder "outputs"
- 07_combine_by_season
  - World land boundary shapefile "world-dissolved" in folder input_data/baselayer"
  - Plastics density raster "/outputs/00_PlasticsRaster.tif
  - Monthly tracking data distribution .tifs in folder "/outputs/04_aggregate_1by1_grid"
  - Exposure scores in file "/outputs/05_exposure_scores_by_month.csv"
  - Breeding schedules from "/input_data/breeding_months.csv" or "/outputs/06_phenology.csv"
- 08_combine_by_species
  - Exposure scores in file "/outputs/05_exposure_scores_by_month.csv"
  - Breeding schedules from "/input_data/breeding_months.csv" or "/outputs/06_phenology.csv"
  - "population_sizes.csv" in folder "input_data"
- 09_plot_maps
  - World land boundary shapefile "world-dissolved" in folder input_data/baselayer"
  - "/outputs/08_all_species_distribution.tif"
  - "/outputs/00_PlasticsRaster.tif"
  - "/outputs/02_colony_locations.csv"
  - "/outputs/08_species_richness.tif"
  - EEZ shapefile in "/input_data/EEZ_land_union_v3_202003"
  - For case studies:
    - Season-specific distribution .tifs in "/outputs/07_seasons"
    - "/outputs/07_exposure_scores_by_season.csv"
- 10_exposure_score_plots
  - "/outputs/07_exposure_scores_by_season.csv"
  - "/outputs/08_exposure_scores_by_species.csv"
  - "/input_data/Species_list_IUCN.csv"
- 11_eez_all_species_combined
  - EEZ shapefile in "/input_data/EEZ_land_union_v3_202003"
  - World land boundary shapefile "world-dissolved" in folder input_data/baselayer"
  - "/outputs/06_phenology.csv"
  - "/outputs/08_all_species_distribution.tif"
  - "/outputs/00_PlasticsRaster.tif"
- 12_combine_countries
  - Rasters in "/outputs/05_populations"
  - "/outputs/07_exposure_scores_by_season.csv"
  - "/outputs/05_exposure_scores_by_population.csv"
- 13_eez_by_population
  - "/outputs/11_eezs_used_all_species.csv"
  - "/outputs/12_species_country_scores.csv"
 

## Graphical representation of the method 
Methods for calculating marine plastic exposure scores from tracking data. Input datasets are represented by rectangles, method steps are represented by white rounded rectangles, sections of related method steps are represented by grey rounded rectangles, and outputs are represented by circles. Scores can be used to compare exposure to plastic pollution among populations, seasons, and species. Maps can be used to locate areas of high plastic exposure, and percentages within spatial boundaries can be used to identify and rank regions of high exposure among all species and relate these to specific populations from specific jurisdictions.

![8_plastic_modeling_flowchart_suppl](https://user-images.githubusercontent.com/56324426/184148175-6b1633d7-ca93-4b10-9593-b201377fb755.png)
