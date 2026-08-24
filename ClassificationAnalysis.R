## Raster analysis

### Load Packages ###
library(sf)
library(dplyr)
library(here)
library(ggplot2)
library(stars) # Used for loading and managing geometry data
library(terra) # Used for loading and managing raster data
library(exactextractr) ## Used for extracting raster data that is within polygons

## How to make it easy to use this with several rasters, update existing collated data?
## Maybe:
  ## Manually create a .csv file in a folder where analyzed data is collated.
    ## Code can check that file for which dates have been analyzed.
    ## Then, it will check the raster folder for any un-analyzed rasters (based on date), analyze them, and append them.
    ## File with appended data will be saved as a new file with an analysis date. This is to make it easy to undo analyses.

#### User Inputs ####

## Site input. This will be used to instruct the model of which folder to look in.
site <- "HP2"

## Define what type of polygon data should be extracted from.
geotype <- "grid"

#### |||| #### |||| ####

#### Loading Data ####

## Check for an appended dataset for this site. If it exists, read it. If it doesn't exist, make one at the end of the script.
ifelse(file.exists(here("ClassificationResults", site, paste0(site, "_classification_analysis_", geotype, ".csv"))) == TRUE,
               existed <- read.csv(here("ClassificationResults", site, paste0(site, "_classification_analysis_", geotype, ".csv"))),
               existingfile <- 'false'
       )

## Retrieve the name of each raster file. Not sure that I can load each...
filenames_rasters <- list.files(here("ClassificationRasters", site), pattern = "*.tif", full.names = TRUE)

## Make list of filenames in the appended datafile
#alreadAnalyzed <- unique(existed$fileName)
#filenames_rasters <- filenames_rasters[ !filenames_rasters %in% alreadyAnalyzed]

## Make a list of the rasters
rasters_list <- lapply(filenames_rasters, function(x) {rast(x)})

## Retrieve the analysis geometry.
analysis_geometry <- st_read(here("AnalysisGrid", site, paste0(site, "_analysis_geometry_", geotype, ".shp")))

## Make a list of each column in the analysis geometry.
geo_column_names <- colnames(analysis_geometry)

# For each raster in the list of rasters, extract the area of each land cover
  ## class in each polygon.
extracted <- data.frame(bind_rows(lapply(rasters_list, function(x) {
  exact_extract(x, analysis_geometry, append_cols = geo_column_names, include_area = TRUE,
                function(value, coverage_area) {table(value)}) %>% 
    mutate(name = names(x), site = site)## Add the raster filename to the table
}))) %>% 
  mutate(date = substr(.$name, 5, 10))

# extracted2 <- extracted %>% 
#   mutate(date = substr(.$name, 5, 10))










## Remove rasters from list that have already been analyzed.
# if(existingfile == 'false'){
#   ## check raster dates against dates in appended data file. remove matches.
# }


## Iterate through the raster list, extracting the coverage fraction of each value within each analysis geometry
# test <- mapply(rasters_list, filenames_rasters, FUN = function(x, y) {
#   exact_extract(x, analysis_geometry, append_cols = geo_column_names,
#                 function(value, coverage_fraction ) {table(value)}) %>% 
#     mutate(filename = gsub(paste0(here("ClassificationRasters", site), "/"), "", y))## Add the raster filename to the table
# })

## mapply takes multiple arguments but seems to spit it out in a bad format??





