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
siteNames <- read_xlsx(here("Metadata", "Site_Names_Metadata.xlsx"))

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

## Move old version to the "deprecated" folder.

existing_sp_collated <- list.files(here("Samples"), pattern = "*.csv", full.names = TRUE)

existing_sp_collated %>% lapply(function(x){
  filename <- sub('.*/', '', x) ## Remove the slashes and everything before them to get the filename.
  file.rename(from = x,
              to = 
                paste0(sub(filename, '', x), ## Get the file pathway excluding the filename.
                       "/Deprecated/", ## Add the deprecated folder to the file pathway
                       filename)) ## Add the filename back to the end of the pathway
})


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


### Export the collated accuracy assessment points ###

## Move old files to "deprecated" folder

existing_aa_collated <- list.files(here("AccuracyAssessment"), pattern = "*.csv", full.names = TRUE)

existing_aa_collated %>% lapply(function(x){
  filename <- sub('.*/', '', x) ## Remove the slashes and everything before them to get the filename.
  file.rename(from = x,
              to = 
                paste0(sub(filename, '', x), ## Get the file pathway excluding the filename.
                       "/Deprecated/", ## Add the deprecated folder to the file pathway
                       filename)) ## Add the filename back to the end of the pathway
})

## Create new file with the new data

write.csv(accuracy_assessment_points, here("AccuracyAssessment", paste0("ESNERR_AccuracyAssessment_", format(Sys.Date(), format = "%Y%m%d"), ".csv")), row.names = FALSE)

#### |||| #### |||| ####





