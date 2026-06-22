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

## Site input. This will be used to instruct the model of which folder to look in.
site <- "HP2"

geotype <- "grid"

## Check for an appended dataset for this site. If it exists, read it. If it doesn't exist, make one at the end of the script.
ifelse(file.exists(here("ClassificationResults", site, paste0(site, "_classification_analysis.csv"))) == TRUE,
               existed <- read.csv(here("ClassificationResults", site, paste0(site, "_classification_analysis.csv"))),
               existingfile <- 'false'
       )

## Retrieve the name of each raster file. Not sure that I can load each...
filenames_rasters <- list.files(here("ClassificationRasters", site), pattern = "*.tif", full.names = TRUE)

## Make a list of the rasters
rasters_list <- lapply(filenames_rasters, function(x) {rast(x)})

## Retrieve the analysis geometry.
analysis_geometry <- st_read(here("AnalysisGrid", site, paste0(site, "_analysis_geometry_", geotype, ".shp")))

## Make a list of each column in the analysis geometry.
geo_column_names <- colnames(analysis_geometry)

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

test <- lapply(rasters_list, function(x) {
  exact_extract(x, analysis_geometry, append_cols = geo_column_names,
                function(value, coverage_fraction ) {table(value)}) %>% 
    mutate(name = names(x), site = site)## Add the raster filename to the table
})

test[[1]] <- test[[1]] %>%
  mutate(date = substr(.$name(5,10)))


## Add the date from the raster file name to the table


## Stitch the tables together
complete_data <- bind_rows(test)




raster <- rast(here("ClassificationRasters", site, "HP2_230428_RF_241230.tif"))


test <- exact_extract(raster, analysis_geometry, function(value, coverage_fraction ) {table(value)}, 
                      append_cols = geo_column_names)










