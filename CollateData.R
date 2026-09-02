#############################
#### Collate data script ####
#############################

## The goal of this script is to collate sample and accuracy assessment data 
  ## used in the production and analysis of UAV classifications.

#### Install packages ####

library(here) # Locate files within project folder
library(dplyr) # Manipulate data
library(tidyr) # Manipulate data
library(readxl) # Read excel format files

#### Initializing global variables and dataframes ####

### Load data frames ###

## Classification Tracking Sheet
trackingSheet <- read_xlsx(here("Metadata", "Classification_Tracking_Sheet.xlsx"), sheet = "TrackingData")

## Site name metadata
siteNames <- read_xlsx(here("Metadata", "SiteNamesMetadata.xlsx"))

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

## Pull from the file name the site code, ortho date, and date that the file was created.
samples <- samples %>% 
  mutate(SiteCode = substr(.$filename, 1, 3)) %>% 
  mutate(orthomosaicdate = substr(.$filename, 5, 12)) %>% 
  mutate(filedate = substr(.$filename, 17, 24)) %>% 
  mutate(year = substr(.$orthomosaicdate, 1, 4)) %>% 
  mutate(month = substr(.$orthomosaicdate, 5, 6)) %>% 
  mutate(day = substr(.$orthomosaicdate, 7, 8)) %>% 
  mutate(date = paste0(year, "-", month, "-", day))

## Add standard ESNERR region, sub-region, area, and sub-area data from siteNames.
samples <- merge(samples, siteNames, by = "SiteCode")

## Verify that all land cover class names are lowercase
samples$name <- tolower(samples$name)

## Create column for vegetated/unvegetated classification (convert land cover class names).

## Create a list of land cover classes that are considered "vegetated".
vegetatedClasses <- c('vegetated', 'woodyvegetation', 'juicyvegetation', 'pickleweed', 'spergularia', 
                     'frankenia', 'jaumea', 'distichlis')

## Create a "vegetation" column. For each samples, if the class name is in the vegetatedClasses list, 
  ## assign it the value "vegetated". Otherwise, it is "unvegetated".
samples <- samples %>% 
  mutate(name=replace(name,name=="vegetatedwoody","woodyvegetation")) %>% 
  mutate(vegetation = ifelse(name %in% vegetatedClasses, 'vegetated', 'unvegetated'))

### Export collated dataset ###

## Export to the samples folder. Add the current date to end for posterity.

write.csv(samples, here("Samples", paste0("ESNERR_Classification_Samples_", format(Sys.Date(), format = "%Y%m%d"), ".csv")), row.names = FALSE)

#### |||| #### |||| ####


#### Accuracy Assessment Data ####

### Load individual datasets ###

## Retrieve name of each file in the individual sample file folder
filenames_accuracy <- list.files(here("AccuracyAssessment", "IndividualFiles"), pattern = "*.csv", full.names = TRUE)

## Read each of those sample files, make a list of them. Add the filename (sans pathway) as a parameter value. 
accuracy_dataframe_list <- lapply(filenames_accuracy, function(x) {read.csv(x) %>% ## Read each .csv file in the folder.
    mutate(filename = gsub(paste0(here("AccuracyAssessment", "IndividualFiles"), "/"), "", x)) #%>%  ## Make a column with file names.
    # mutate(binary = ifelse(grepl('binary', filename), 1, 0)) ## If the data is binary, make the "binaryclass" value 1. This informs the program of how to refer to this data.
})

## Bind each dataframe to each other
accuracy_assessment_points <- bind_rows(accuracy_dataframe_list) %>% 
  select(-c(RASTERVALU, ## Remove the "RASTERVALU" column, it's redundant with the "Classified" column. Also OK to remove before bringing the data in.
            GrndTruth2)) %>%   ## Remove "GrndTruth2" (a copy of the ground truth created in some instances in ArcGIS Pro).
  mutate(orthomosaicdate = as.numeric(substr(.$filename, 5, 12)),
         SiteCode = substr(.$filename, 1, 3),
         filedate = substr(.$filename, 17, 24)) %>% 
  mutate( ## A second mutate() call so that it can reference columns created in the previous call
         year = substr(.$orthomosaicdate, 1, 4),
         month = substr(.$orthomosaicdate, 5, 6),
         day = substr(.$orthomosaicdate, 7, 8),
         date = paste0(year, "-", month, "-", day)
         ) %>% 
  rename(
    trueLabel = GrndTruth,
    predictedLabel = Classified
  ) %>% 
  left_join(siteNames, by = "SiteCode") %>% 
  left_join(
    read.csv(list.files(here("Samples"), pattern = "*.csv", full.names = TRUE)) %>%
      select(SiteCode, orthomosaicdate, label, name) %>%
      filter(!duplicated(paste0(SiteCode, label, name, orthomosaicdate))) %>%
      rename(trueLabel = label) %>%
      rename(trueName = name),
    by = c("SiteCode", "trueLabel", "orthomosaicdate")
  ) %>% 
  left_join(
    read.csv(list.files(here("Samples"), pattern = "*.csv", full.names = TRUE)) %>%
      select(SiteCode, orthomosaicdate, label, name) %>%
      filter(!duplicated(paste0(SiteCode, label, name, orthomosaicdate))) %>%
      rename(predictedLabel = label) %>%
      rename(predictedName = name),
    by = c("SiteCode", "predictedLabel", "orthomosaicdate")
  ) %>% 
  select(-c(predictedLabel, trueLabel)) %>%
  mutate(trueNameVegUnveg = ifelse(trueName %in% vegetatedClasses, 'vegetated', 'unvegetated'),
         predictedNameVegUnveg = ifelse(predictedName %in% vegetatedClasses, 'vegetated', 'unvegetated')) %>% 
  mutate(correct = ifelse(trueName == predictedName, 1, 0),
         correctVegUnveg = ifelse(trueNameVegUnveg == predictedNameVegUnveg, 1, 0)
  )

## Need to left_join to create a "PredictedClass" and "TrueClass" column with names - Check
## Need to make a "correct" column with 1/0 for correct or incorrect
## Need to make an overall vegetated and overall unvegetated column - check
## Need to add the date and site data from the data curation standards.

### Export the collated accuracy assessment points ###

write.csv(accuracy_assessment_points, here("AccuracyAssessment", paste0("ESNERR_AccuracyAssessment_", format(Sys.Date(), format = "%Y%m%d"), ".csv")), row.names = FALSE)

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


## Below code is assuming that there is an F column. This would have to be made manually.

## Populate the f_cover column with <1> for suspect for any row without a <-3> (rejected/missing) and a greater than 50% difference from the previous year.
## The math will need work but the idea is there.
## This code maintains a <-3> code which is rejected or missing and would have been applied in ArcGIS Pro.
  ## If the data shows a large jump, flag is as suspect.
classification_results %>% mutate(f_cover = ifelse(f_cover == "<-3>", "<-3>", ifelse(cover_change > .5*cover, "<1>", "<0>")))









