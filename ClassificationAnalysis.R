
############################## Raster Analysis ##############################
## This script is used to extract classification raster data (.tiff) within
## user-provided analysis geometries (.shp). The user should run the            
## CollateData.R script with the sample data and accuracy assessment data
## first. User must define the area of interest and analysis geometry type.
## The script will check for existing curated classification data for the 
## area of interest and only extract data for rasters not previously analyzed.
## Ouput is a curated .csv of land cover percent cover within each geometry 
## for each classification raster with standardized site and date data for 
## cross-referencing.
##
## Elkhorn Slough National Estuarine Reserved
## Sean McCollum, seanmccollum98@gmail.com
############################################################################



###////Load Packages\\\\###

library(tidyverse) # Used for managing and manipulating datasets
library(here) # Used for locating files within the project directory
library(stars) # Used for loading and managing geometry data
library(terra) # Used for loading and managing raster data
library(exactextractr) ## Used for extracting raster data that is within polygons



####////User Inputs\\\\####

## Site input. This will be used to instruct the model of which folder to look in.
site <- "HP1"

## Define what type of polygon data should be extracted from.
geotype <- "grid"

## Define what classes are considered "vegetated"
vegetatedClasses <- c("vegetated", "juicyvegetation", "woodyvegetation", "salicornia",
                      "jaumea", "frankenia", "distichlis", "astroplex", "submergedVegetation")

## Define what classes are considered "unvegetated"
unvegetatedClasses <- c("unvegetated", "mud", "wetmud", "drymud", "sediment", "water",
                      "wrack", "ulvawrack", "seagrasswrack", "algae", "shallowwater")



####////Loading Data\\\\####

## Check for an appended dataset for this site. If it exists, read it. If it doesn't exist, make one at the end of the script.
ifelse(file.exists(here("ClassificationResults", site, paste0(site, "_classification_analysis_", geotype, ".csv"))) == TRUE,
            previouslyExtracted <- read.csv(here("ClassificationResults", site, paste0(site, "_classification_analysis_", geotype, ".csv"))),
            previouslyExtracted <- 'FALSE')

## Retrieve the name of each raster file.
filenames_rasters <- list.files(here("ClassificationRasters", site), pattern = "*.tif", full.names = TRUE)

## Remove all rasters from the raster list that have names that match with filenames in the previouslyextracted dataframe
if(is.data.frame(previouslyExtracted) == TRUE){
  previouslyExtractedRasters <- unique(previouslyExtracted$filename)
  filenames_rasters <- filenames_rasters[!grepl(paste(previouslyExtractedRasters, collapse = "|"), 
                                                filenames_rasters)]
}

## Make a list of the rasters
rasters_list <- lapply(filenames_rasters, function(x) {rast(x)})

## Retrieve the analysis geometry.
analysis_geometry <- st_read(here("AnalysisGeometry", site, paste0(site, "_analysis_geometry_", geotype, ".shp"))) %>% 
  mutate(row = row_number()) ## Make a unique ID for each geometry, which is necessary for later code.

## Make a list of each column in the analysis geometry.
geo_column_names <- colnames(analysis_geometry)

# For each raster in the list of rasters, extract the area of each land cover class in each polygon.
extracted <- data.frame(bind_rows(lapply(rasters_list, function(x) { # Use lapply to apply function to each item in list
  exact_extract(x, analysis_geometry, append_cols = geo_column_names,
                function(value, coverage_fraction) {table(value)}) %>% # extract coverage_fraction of each class in each geometry 
    mutate(filename = names(x), SiteCode = site) ## Add the raster filename to the table
}))) %>%
  mutate(orthomosaicdate = as.numeric(substr(.$filename, 5, 12))) %>% # Pull the date out of the filename
  mutate(`result.value` = as.numeric(as.character((`result.value`)))) %>% # Convert the extraction output to numeric
  rename(label = `result.value`,
         count = `result.Freq`) %>%
  mutate(
    filedate = substr(.$filename, 17, 24),
    year = substr(.$orthomosaicdate, 1, 4),
    month = substr(.$orthomosaicdate, 5, 6),
    day = substr(.$orthomosaicdate, 7, 8),
    date = paste0(year, "-", month, "-", day)
  ) %>% 
  merge(., siteNames, by = "SiteCode") %>% 
  # left_join( # Join the standardized site and date data from the samples dataset. Shouldn't I just use the metadata?
  #   read.csv(list.files(here("Samples"), pattern = "*.csv", full.names = TRUE)) %>%
  #     select(c(SiteCode, label, name, orthomosaicdate,
  #              year, month, day, date,
  #              Region, Subregion, Area)) %>%
  #     filter(!duplicated(paste0(SiteCode, label, name, orthomosaicdate))),
  #   by = c("SiteCode", "label", "orthomosaicdate"),
  #   suffix = c("", ".y")) %>%
  # select(-c(ends_with(".y"), "label")) %>%
  pivot_wider(names_from = name, values_from = count, values_fill = 0) %>% # Pivot so that each class is a column
  rowwise() %>%
  mutate(overallVegetated = sum(c_across(any_of(vegetatedClasses))), # Sum all vegetated classes
         overallUnvegetated = sum(c_across(any_of(unvegetatedClasses))), # Sum all unvegetated classes
         percentVegetated = round((overallVegetated/(overallVegetated+overallUnvegetated))*100, 3)) # Calculate the percent veg cover

#### |||| #### |||| ####



#### Exporting Data ####

## Check if the collated data exists for this site already.
  ## If it does, upload it, join the current extracted data to it, and re-export it, replacing the file.
  ## If it doesn't, export the current extracted data as an excel.
if(file.exists(here("ClassificationResults", site, paste0(site, "_classification_analysis_", geotype, ".csv"))) == TRUE){
  new <- bind_rows(previouslyExtracted, extracted)
  write.csv(new, 
            file = here("ClassificationResults", site, paste0(site, "_classification_analysis_", geotype, ".csv")),
            row.names = FALSE)
} else {
  write.csv(extracted, 
            file = here("ClassificationResults", site, paste0(site, "_classification_analysis_", geotype, ".csv")),
            row.names = FALSE)
}

#### |||| #### |||| ####








