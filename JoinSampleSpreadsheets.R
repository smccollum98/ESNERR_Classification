
## This script will join each individual sample spreadsheet

### Load packages

library(here)
library(dplyr)
library(tidyr)
library(readxl)

#### |||| #### |||| ####

### Establish which classes are equivalent to vegetated or unvegetated ###

#!!!! Issue: not all datasets have names. Can pull from the classification tracking document to apply names based on label and date.



#### |||| #### |||| ####

### Load datasets files ###

## Retrieve name of each file in the individual sample file folder
filenames <- list.files(here("Samples", "IndividualFiles"), pattern = "*.csv", full.names = TRUE)

## Read each of those sample files, make a list of them. Add the filename (sans pathway) as a parameter value.
ldf <- lapply(filenames, function(x) {read.csv(x) %>% mutate(filename = gsub(paste0(here("Samples", "IndividualFiles"), "/"), "", x))})

## Bind each dataframe to each other
samples <- bind_rows(ldf, .id = "column_label")

## Pull the site code, ortho date, and date that the file was created from the file name.
samples <- samples %>% 
  mutate(SiteCode = substr(.$filename, 1, 3)) %>% 
  mutate(orthomosaicdate = substr(.$filename, 6, 13)) %>% 
  mutate(filedate = substr(.$filename, 15, 22)) %>% 
  mutate(year = substr(.$orthomosaicdate, 1, 4)) %>% 
  mutate(month = substr(.$orthomosaicdate, 5, 6)) %>% 
  mutate(day = substr(.$orthomosaicdate, 7, 8)) %>% 
  mutate(date = paste0(year, "-", month, "-", day))

## Import classifiation tracking sheet
trackingSheet <- read_xlsx(here("Classification_Tracking_Sheet.xlsx"), sheet = "TrackingData")

## Import site name metadata
siteNames <- read_xlsx(here("Metadata", "SiteNamesMetadata.xlsx"))

#### |||| #### |||| ####

### Add appropriate site values according to the site code ###

samples <- merge(samples, siteNames, by = "SiteCode")



#### |||| #### |||| ####

### Add class names to data without them ###

## Need to transpose the "class" columns so that there is a "label" column and multiple rows with 1, 2, 3, etc. and a class name column...

classNames <- trackingSheet %>% 
  select(c("Subarea", "Date8Digit", "Class0", "Class1", "Class2", "Class3", "Class4", "Class5")) %>% 
  pivot_longer(c(Class0, Class1, Class2, Class3, Class4, Class5), names_to = "label", values_to = "name") %>% 
  mutate(label = gsub("Class", "", label)) %>% 
  mutate(label = as.integer(label))




test <- merge(samples, classNames)


#test <- pivot_longer(classNames, c(Class0, Class1, Class2, Class3, Class4, Class5), names_to = "Label", values_to = "Name")

### Create or fill in certain data columns ###

## For each dataframe, if there are no "Name" values, 
##pull names from the classification tracking document according to the date and label


## Create a veg/unveg column. Populate it with "Vegetated" or "Bare" based on the translation established above.




#### |||| #### |||| ####







