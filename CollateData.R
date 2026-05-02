#############################
#### Collate data script ####
#############################

## The goal of this script is to collate sample and accuracy assessment data 
  ## used in the production and analysis of UAV classifications.

#### Install packages ####

library(here)
library(dplyr)
library(tidyr)
library(readxl)




#### Initializing global variables and dataframes ####

### Load data frames ###

## Classification Tracking Sheet
trackingSheet <- read_xlsx(here("Classification_Tracking_Sheet.xlsx"), sheet = "TrackingData")

## Site name metadata
siteNames <- read_xlsx(here("Metadata", "SiteNamesMetadata.xlsx"))

## Categorize possible land cover classes into vegetated or unvegetated
landCoverClasses <- a ## Add!

#### |||| #### |||| ####


#### Classification Samples ####

### Load individual datasets ###

## Retrieve name of each file in the individual sample file folder
filenames_samples <- list.files(here("Samples", "IndividualFiles"), pattern = "*.csv", full.names = TRUE)

## Read each of those sample files, make a list of them. Add the filename (sans pathway) as a parameter value.
sample_dataframe_list <- lapply(filenames_samples, function(x) {read.csv(x) %>% 
    mutate(filename = gsub(paste0(here("Samples", "IndividualFiles"), "/"), "", x))})

## Bind each dataframe to each other
samples <- bind_rows(sample_dataframe_list)

## Pull the site code, ortho date, and date that the file was created from the file name.
samples <- samples %>% 
  mutate(SiteCode = substr(.$filename, 1, 3)) %>% 
  mutate(orthomosaicdate = substr(.$filename, 6, 13)) %>% 
  mutate(filedate = substr(.$filename, 15, 22)) %>% 
  mutate(year = substr(.$orthomosaicdate, 1, 4)) %>% 
  mutate(month = substr(.$orthomosaicdate, 5, 6)) %>% 
  mutate(day = substr(.$orthomosaicdate, 7, 8)) %>% 
  mutate(date = paste0(year, "-", month, "-", day))

## Add appropriate site information from site name metadata
samples <- merge(samples, siteNames, by = "SiteCode")

## Verify that all land cover class names are lowercase
samples$name <- tolower(samples$name)

### Export collated dataset ###

## Export to the samples folder. Add the current date to end for posterity.

write.csv(samples, here("Samples", paste0("ESNERR_Classification_Samples_", format(Sys.Date(), format = "%Y%m%d"), ".csv")), row.names = FALSE)

#### |||| #### |||| ####


#### Accuracy Assessment Data ####

### Load individual datasets ###

## Retrieve name of each file in the individual sample file folder
filenames_accuracy <- list.files(here("AccuracyAssessment"), pattern = "*.csv", full.names = TRUE)

## Read each of those sample files, make a list of them. Add the filename (sans pathway) as a parameter value. 
accuracy_dataframe_list <- lapply(filenames_accuracy, function(x) {read.csv(x) %>% ## Read each .csv file in the folder.
    mutate(filename = gsub(paste0(here("AccuracyAssessment"), "/"), "", x)) %>%  ## Make a column with file names.
    mutate(binary = ifelse(substr(.$filename, 22, 27) == "binary", 1, 0)) ## If the data is binary, make the "binaryclass" value 1. This informs the program of how to refer to this data.
})

## Bind each dataframe to each other
accuracy_assessment_points <- bind_rows(accuracy_dataframe_list) %>% 
  select(-c(RASTERVALU))  ## Remove the "RASTERVALU" column, it's redundant with the "Classified" column. Also OK to remove before bringing the data in.

### Export the collated accuracy assessment points ###

write.csv(accuracy_assessment_points, here("Samples", paste0("ESNERR_AccuracyAssessment_", format(Sys.Date(), format = "%Y%m%d"), ".csv")), row.names = FALSE)

#### |||| #### |||| ####


#### Classification Results ####

## The choice here is to use the classification results from the ArcGIS analysis or pull the data using R.

### Load datasets ###

## Retrieve name of each file in the individual sample file folder
filenames_classification <- list.files(here("ClassificationResults"), pattern = "*.csv", full.names = TRUE)

## Read each of those sample files, make a list of them. Add the filename (sans pathway) as a parameter value. 
classification_dataframe_list <- lapply(filenames_classification, function(x) {read.csv(x) %>% ## Read each .csv file in the folder.
    mutate(filename = gsub(paste0(here("ClassificationResults"), "/"), "", x))  ## Make a column with file names.
})

## Bind each dataframe to each other
classification_results <- bind_rows(classification_dataframe_list)

## As is, this data represents what we would want to create. I want to be able to work with the raw output of the Arc model.


## Populate the f_cover column with <1> for suspect for any row without a <-3> (rejected/missing) and a greater than 50% difference from the previous year.
## The math will need work but the idea is there.
## This code maintains a <-3> code which is rejected or missing and would have been applied in ArcGIS Pro.
  ## If the data shows a large jump, flag is as suspect.
classification_results %>% mutate(f_cover = ifelse(f_cover == "<-3>", "<-3>", ifelse(cover_change > .5*cover, "<1>", "<0>")))









