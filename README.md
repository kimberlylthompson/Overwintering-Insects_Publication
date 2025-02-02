<<<<<<< HEAD
# Overwintering-Insects_Publication

Code author: 

The vulnerability of overwintering insects to loss of the subnivium



Preprint DOI:

Data DOI:

# Data
## Data openly available to reproduce results include:
* Daily air temperature data available from Daymet (https://daymet.ornl.gov/) or by using the download script 00_Downloading Daymet tmax and tmin.R’ provided.
* Land-cover data were available from the United States Geological Survey’s National Land Cover Database (https://www.usgs.gov/centers/eros/science/national-land-cover-database)
* Daily snow depth and snow water equivalent data available from the National Snow and Ice Data Center’s Snow Data Assimilation System Product (SNODAS,  https://nsidc.org/data/g02158/versions/1#anchor-data-access-tools) 
* Daily wind speed data available from the National Centers for Environmental Protection’s North American Regional Reanalysis product (NARR, https://psl.noaa.gov/data/gridded/data.narr.html) 
* Data on insect cold tolerances were extracted from published literature and are summarized in Table 1 of the preprint.

## Data provided include:
* FOLDER: 00_Data/Minimum Subnivium Temperature Csv files for the daily minimum ground temperature at each site for each treatment (9 files) plus a csv that combines this data into one file (Master_Min Subnivium Temps.csv)
* FOLDER: 00_Data/Wide Form String Data Csv files for the raw ground temperature data measured in 15-minute interval at each site (9 files) plus a .docx file that contains metadata for the column headings of the csv files. 
* 00_Data/Blank resampling raster_1km resolution.tif Used to get spatial predictor tif files at the same resolution.
* 00_Data/Data from Lam and Pedigo LLT at 0C.csv Csv file containing data extracted from Lam and Pedigo (2000) on time until mortality and the percentage mortality in a population of bean leaf beetles (Ceratoma trifurcata) held at a constant temperature of 0C.
* 00_Data/Great_Lakes.shp Shapefile of the Great Lakes used in making figures.
* 00_Data/Great_Lakes_States.shp Shapefile of the states in the Great Lakes Region used in making figures.
* 00_Data/Latitude_degrees.tif Raster of latitudinal ranges in the Great Lakes Region used in making figures.
* 00_Data/Subniv Temps and Predictors_complete.csv Csv containing experimental data that were used in models generated from active-warming experiments in the Great Lakes Region.

# Reproducibility
To ensure reproducibility, the working environment (R version and package version) was documented using the package renv (https://rstudio.github.io/renv/index.html) and stored in the renv.lock file. Running renv::restore() will download and install all missing packages (with the same versions used in this workflow based on the renv.lock file) necessary to run the below scripts. Therefore, it is recommended to run the renv::restore() function before running any of the below scripts.

# R Scripts
R Script folders, which are listed below, are numbered and listed in the order they should be used. Within each folder, scripts are also numbered in the order they should be used. Note that several scripts within the 02_Boosted_Regression_Trees are computationally intensive to run on a local machine.

* **00_Data_Preparation** The scripts in this folder derive the minimum daily ground temperature at each site in each treatment and combine them, along with data on other conditions at each site (e.g., ambient temperature, wind speed, snow depth, snow density, etc.), into a master dataframe. Original data and the data resulting from these processing scripts are included at 00_Data.
* **01_Spatial_Predictor_Preparation** The scripts in this folder download and clean the spatial predictor data (temperature, snow depth, snow water equivalent, etc.). Downloaded data and the data resulting from these processing scripts are not included.
* **02_Boosted_Regression_Trees** The scripts in this folder test different settings for creating boosted regression tree (BRT) models, build the BRT models with the selected settings and the selected number of iterations, generate spatial predictions using the models, evaluate model performance, correct the predictions for the influence of the greenhouse structure, and summarize the predictions by treatment and day. Data resulting from these scripts are not included.
* **03_Spatial_Prediction_Preparation** The one script in this folder (00_Separating Lakes.R) creates a csv that is used to ensure that the Great Lakes are not included in summaries of vulnerability. Data resulting from this script is not included.
* **04_Insect_Vulnerability** The scripts in this folder use the daily predictions of ground temperatures generated through scripts in the 02_Boosted_Regression_Trees folder and insect supercooling point data extracted from the literature to generate species-specific maps of vulnerability based on both the number of days below the supercooling point and the areal extent with ground temperatures below the supercooling point. Data resulting from these scripts are not included.
* **05_Figure 1** The scripts in this folder generate the maps in Figure 1 that depict the number of days in the winter season with ground temperatures under the highest published supercooling point for each insect (i.e., worst-case scenario). Data resulting from these scripts are not included.
* **06_Figure S8** The scripts in this folder generate the maps in Figure S8 that depict the number of days in the winter season with ground temperatures under the lowest published supercooling point for each insect (i.e., best-case scenario). Data resulting from these scripts are not included.
* **07_Duration_SCP** The scripts in this folder summarize the number of days in the winter season with ground temperatures under the supercooling point of each species and use this data to create Figures 2 and S9. Data resulting from these scripts are not included.
* **08_Extent_SCP** The scripts in this folder summarize areal extent with ground temperatures under the supercooling point of each species and use this data to create Figures 3 and S10. Data resulting from these scripts are not included.
* **09_Comparing_Bean Leaf_LLT_and_SCP_durations** The scripts in this folder extract data on mortality in Bean leaf beetles held at a constant temperature of 0C, calculates the number of consecutive days with ground temperatures below 0C for the Great Lakes Region, uses these data to predict mortality for bean leaf beetles, and compares these predictions to those generated through data on their supercooling point. Figures 4, 5, S11, and S12 are created. With the exception of 00_Data/Data from Lam and Pedigo LLT at 0C.csv, data resulting from these scripts are not included.
=======
# Overwintering-Insects_Publication

Code author: 

The vulnerability of overwintering insects to loss of the subnivium



Preprint DOI:

Data DOI:

# Data
## Data openly available to reproduce results include:
* Daily air temperature data available from Daymet (https://daymet.ornl.gov/) or by using the download script 00_Downloading Daymet tmax and tmin.R’ provided.
* Land-cover data were available from the United States Geological Survey’s National Land Cover Database (https://www.usgs.gov/centers/eros/science/national-land-cover-database)
* Daily snow depth and snow water equivalent data available from the National Snow and Ice Data Center’s Snow Data Assimilation System Product (SNODAS,  https://nsidc.org/data/g02158/versions/1#anchor-data-access-tools) 
* Daily wind speed data available from the National Centers for Environmental Protection’s North American Regional Reanalysis product (NARR, https://psl.noaa.gov/data/gridded/data.narr.html) 
* Data on insect cold tolerances were extracted from published literature and are summarized in Table 1 of the preprint.

## Data provided include:
* FOLDER: 00_Data/Minimum Subnivium Temperature Csv files for the daily minimum ground temperature at each site for each treatment (9 files) plus a csv that combines this data into one file (Master_Min Subnivium Temps.csv)
* FOLDER: 00_Data/Wide Form String Data Csv files for the raw ground temperature data measured in 15-minute interval at each site (9 files) plus a .docx file that contains metadata for the column headings of the csv files. 
* 00_Data/Blank resampling raster_1km resolution.tif Used to get spatial predictor tif files at the same resolution.
* 00_Data/Data from Lam and Pedigo LLT at 0C.csv Csv file containing data extracted from Lam and Pedigo (2000) on time until mortality and the percentage mortality in a population of bean leaf beetles (Ceratoma trifurcata) held at a constant temperature of 0C.
* 00_Data/Great_Lakes.shp Shapefile of the Great Lakes used in making figures.
* 00_Data/Great_Lakes_States.shp Shapefile of the states in the Great Lakes Region used in making figures.
* 00_Data/Latitude_degrees.tif Raster of latitudinal ranges in the Great Lakes Region used in making figures.
* 00_Data/Subniv Temps and Predictors_complete.csv Csv containing experimental data that were used in models generated from active-warming experiments in the Great Lakes Region.

# Reproducibility
To ensure reproducibility, the working environment (R version and package version) was documented using the package renv (https://rstudio.github.io/renv/index.html) and stored in the renv.lock file. Running renv::restore() will download and install all missing packages (with the same versions used in this workflow based on the renv.lock file) necessary to run the below scripts. Therefore, it is recommended to run the renv::restore() function before running any of the below scripts.

# R Scripts
R Script folders, which are listed below, are numbered and listed in the order they should be used. Within each folder, scripts are also numbered in the order they should be used. Note that several scripts within the 02_Boosted_Regression_Trees are computationally intensive to run on a local machine.

* **00_Data_Preparation** The scripts in this folder derive the minimum daily ground temperature at each site in each treatment and combine them, along with data on other conditions at each site (e.g., ambient temperature, wind speed, snow depth, snow density, etc.), into a master dataframe. Original data and the data resulting from these processing scripts are included at 00_Data.
* **01_Spatial_Predictor_Preparation** The scripts in this folder download and clean the spatial predictor data (temperature, snow depth, snow water equivalent, etc.). Downloaded data and the data resulting from these processing scripts are not included.
* **02_Boosted_Regression_Trees** The scripts in this folder test different settings for creating boosted regression tree (BRT) models, build the BRT models with the selected settings and the selected number of iterations, generate spatial predictions using the models, evaluate model performance, correct the predictions for the influence of the greenhouse structure, and summarize the predictions by treatment and day. Data resulting from these scripts are not included.
* **03_Spatial_Prediction_Preparation** The one script in this folder (00_Separating Lakes.R) creates a csv that is used to ensure that the Great Lakes are not included in summaries of vulnerability. Data resulting from this script is not included.
* **04_Insect_Vulnerability** The scripts in this folder use the daily predictions of ground temperatures generated through scripts in the 02_Boosted_Regression_Trees folder and insect supercooling point data extracted from the literature to generate species-specific maps of vulnerability based on both the number of days below the supercooling point and the areal extent with ground temperatures below the supercooling point. Data resulting from these scripts are not included.
* **05_Figure 1** The scripts in this folder generate the maps in Figure 1 that depict the number of days in the winter season with ground temperatures under the highest published supercooling point for each insect (i.e., worst-case scenario). Data resulting from these scripts are not included.
* **06_Figure S8** The scripts in this folder generate the maps in Figure S8 that depict the number of days in the winter season with ground temperatures under the lowest published supercooling point for each insect (i.e., best-case scenario). Data resulting from these scripts are not included.
* **07_Duration_SCP** The scripts in this folder summarize the number of days in the winter season with ground temperatures under the supercooling point of each species and use this data to create Figures 2 and S9. Data resulting from these scripts are not included.
* **08_Extent_SCP** The scripts in this folder summarize areal extent with ground temperatures under the supercooling point of each species and use this data to create Figures 3 and S10. Data resulting from these scripts are not included.
* **09_Comparing_Bean Leaf_LLT_and_SCP_durations** The scripts in this folder extract data on mortality in Bean leaf beetles held at a constant temperature of 0C, calculates the number of consecutive days with ground temperatures below 0C for the Great Lakes Region, uses these data to predict mortality for bean leaf beetles, and compares these predictions to those generated through data on their supercooling point. Figures 4, 5, S11, and S12 are created. With the exception of 00_Data/Data from Lam and Pedigo LLT at 0C.csv, data resulting from these scripts are not included.
>>>>>>> f71b9b43c2ce4a4993c0575a12ed45b45c29af71
