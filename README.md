# petrels-plastics
Clark et al. (2023) Global assessment of marine plastic exposure risk for oceanic birds. Nature Communications.

Link to publication: https://doi.org/10.1038/s41467-023-38900-z

Zenodo release: https://doi.org/10.5281/zenodo.7814662

Each folder contains a separate analysis and an R project file, which, when opened, automatically sets the working directory to that folder. All filepaths are then relative from there.

## Folder descriptions

### 1_full_analysis_petrels
This contains the full code used for the analysis presented in the linked publication. 
The input data folder contains the files and required to run the analysis except for the raw tracking data because those files cannot be shared publically. However, we have included the relevant outputs needed to create all figures and tables. 

### 2_analysis_with_example_datasets
This folder contains a simplified version of the code that removes any code relating to the specific datasets in the study. The input folder contains all the files required to run the analysis for three example tracking datasets that are included in the folder.

### 3_sampling_frequency_test
This contains the full code used to test the correlation between exposure scores using low and high resolution tracking data.

### 4_interannual_test
This contains the full code used for the long-term trend across years analysis presented in the linked publication. 

## Example tracking datasets

The example tracking datasets are provided only for use with the code in this repository. References for these datasets are below. Please contact the authors of these papers if you wish to use the tracking datasets for any other purpose.

https://data.seabirdtracking.org/dataset/609
- Phillips, R.A., Silk, J.R.D., Croxall, J.P. and Afanasyev, V. (2006) Year-round distribution of white-chinned petrels from South Georgia: relationships with oceanography and fisheries. Biological Conservation 129, 336-347. https://doi.org/10.1016/j.biocon.2005.10.046

https://data.seabirdtracking.org/dataset/609
- Catry, P., Dias, M.P., Phillips, R.A., Granadeiro, J.P. (2011) Different Means to the Same End: Long-Distance Migrant Seabirds from Two Colonies Differ in Behaviour, Despite Common Wintering Grounds. PLoS ONE 6, e26079. https://doi.org/10.1371/journal.pone.0026079 

https://data.seabirdtracking.org/dataset/1386
- Frankish, C.K., Phillips, R.A., Clay, T.A., Someveille, M. and Manica, A. (2020) Environmental drivers of movement in a threatened seabird: insights from a mechanistic model and implications for conservation. Diversity and Distributions 26, 1315-1329. https://doi.org/10.1111/ddi.13130

## Graphical representation of the method 
Methods for calculating marine plastic exposure risk scores from tracking data. Input datasets are represented by rectangles, method steps are represented by white rounded rectangles, sections of related method steps are represented by grey rounded rectangles, and outputs are represented by circles. Scores can be used to compare exposure risk to plastic pollution among populations, seasons, and species. Maps can be used to locate areas of high plastic exposure risk, and percentages within spatial boundaries can be used to identify and rank regions of high exposure risk among all species and relate these to specific populations from specific jurisdictions.

![8_plastic_modeling_flowchart_suppl](https://user-images.githubusercontent.com/56324426/184148175-6b1633d7-ca93-4b10-9593-b201377fb755.png)

## How to Run

Open the R Project file in the folder, e.g. `1_full_analysis_petrels.Rproj`.

All folders contain codes that should be run in the order provided by the numbers in the script names:
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
  - Multiply plastic x bird raster to produce exposure risk map
  - Sum scores in each exposure risk map to produce exposure risk score
  - Export exposure risk maps as tifs and images
  - Export exposure risk scores for each month for each population as csv
- 05_combine_by_population
  - Read in rasters for each population, export 1 mean raster per population
  - Import the monthly exposure risk scores and export mean by population
- 06_phenology
  - Read in tracking data csvs for each population
  - Calculate distance to the colony for each location
  - Estimate whether each month is more likely to be breeding or non-breeding for the tracked individuals in each population
  - Export csv containing estimated breeding schedules "06_phenology.csv"
  - Export plots for each population for checking - 06_phenology
- 07_combine_by_seasons
  - Read in rasters for each month
  - Read in breeding schedules (from phenology script or literature)
  - Export 1 mean exposure risk map per population per season
  - Import the monthly exposure risk scores and export mean by population
  - Test for differences from using different breeding schedules (literature v 06_phenology script)
- 08_combine_by_species
  - Read in rasters for each population
  - Read in population sizes 
  - Read in breeding schedules (from phenology script or literature)
  - Export 1 mean raster per species weighted by population size and seasons tracked
  - Import the monthly exposure risk scores and export mean by species weighted by population size and seasons tracked
- 09_plot maps
  - Read in the all species maps from 08_combine_by_species and the plastic raster and creates world maps for Fig. 1 and Fig. 4a
    - Species richness with study colony locations
    - All species tracking location distribution
    - Plastic density estimate
    - Exposure risk of petrels to plastics
    - Exposure risk overlayed with EEZ outlines
  - Read in distributions for case studies and produces maps for Fig. 3
    - European Storm-petrels from 5 colonies
    - Yelkuoan and Scopoli's Shearwaters from Malta
    - Cook's Petrels from 2 colonies
- 10_exposure_score_plots
  - Read in the csvs of exposure risk scores by season and species and creates plots in Fig. 2 and Fig. 3a
- 11_eez_all_species_combined
  - Calculate proportions of exposure risk among marine political regions (EEZs and the high seas)
  - Plot the results for Fig. 4b
- 12_combine_countries
  - Combine population distributions by country for each species
- 13_eez_by_population
  - Calculation the proportion of exposure risk in each EEZ (or high seas) for each species in each breeing country
- 14_eez_by_population_plot
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
  - Exposure risk scores in file "/outputs/05_exposure_scores_by_month.csv"
- 06_phenology
  - World land boundary shapefile "world-dissolved" in folder input_data/baselayer"
  - Cleaned tracking data csvs per population produced by script "02_populations" in folder "outputs"
- 07_combine_by_season
  - World land boundary shapefile "world-dissolved" in folder input_data/baselayer"
  - Plastics density raster "/outputs/00_PlasticsRaster.tif
  - Monthly tracking data distribution .tifs in folder "/outputs/04_aggregate_1by1_grid"
  - Exposure risk scores in file "/outputs/05_exposure_scores_by_month.csv"
  - Breeding schedules from "/input_data/breeding_months.csv" or "/outputs/06_phenology.csv"
- 08_combine_by_species
  - Exposure risk scores in file "/outputs/05_exposure_scores_by_month.csv"
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
- 14_eez_by_population_plot
  - "/outputs/13_eezs_used_per_species.csv"

## Other Datasets
Access can be obtained by making a request to the owners of each dataset using the mechanisms provided by each database. Zoatrack (https://zoatrack.org/) under dataset IDs: 57, 93, 102–112, 159, 253, 254, 762, 817. Movebank (https://www.movebank.org/) under dataset IDs: 944960474, 200628745, 241140274. SEATRACK (https://seapop.no/en/seatrack/) for relevant northern fulmar data. U.S. Geological Survey data release: doi:10.5066/P9NTEXM6.  Seabird Tracking Database (https://www.seabirdtracking.org/) under dataset IDs: 434, 438, 439, 448, 466, 467, 506–511, 517, 518, 554, 555, 561, 571, 607, 609, 610, 627, 628, 634, 635, 637, 639, 658, 659, 662, 663, 667, 668, 670, 672–678, 683, 684, 686, 694–696, 704–706, 708–715, 736, 741, 783–786, 788, 789, 826, 827, 829–831, 836–842, 844, 854, 858–872, 879, 883–886, 888–893, 900, 945, 946, 949, 951–954, 959–963, 966, 967, 970–983, 986–998, 1004, 1028, 1029, 1031–1033, 1055–1061, 1081, 1083, 1084, 1086–1091, 1120, 1121, 1140–1142, 1233–1236, 1238, 1239, 1258, 1259, 1279, 1280, 1282, 1285–1289, 1298, 1314, 1317, 1326, 1343–1347, 1360–1362, 1375, 1386, 1401, 1404, 1409, 1410, 1413–1415, 1422–1425, 1440, 1443, 1449, 1452, 1453, 1460, 1461, 1463, 1481, 1482, 1485–1488, 1494, 1497–1500, 1520–1523, 1541, 1544, 1546, 1549–1551, 1553–1558, 1562–1570, 1574–1577, 1579–1582, 1585–1592, 1594–9, 1600, 1602, 1603, 1606–1608, 1610, 1618, 1619, 1621–1625, 1630, 1665, 1668–1672, 1690, 1711–1717, 1738, 1908–1923, 2036–2038, 2042, 2044–2046–2049, 2051–2056, 2059, 2060, 2063–2066.
