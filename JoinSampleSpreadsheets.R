
## This script will join each individual sample spreadsheet

### Load packages

library(here)
library(dplyr)
library(tidyr)
library(readxl)

#### |||| #### |||| ####

### Establish which classes are equivalent to vegetated or unvegetated ###

binary_names <- data.frame(label = c(0, 1),
                          name = c("unvegetated", "vegetated"))

other_names <- data.frame(label = c(0, 1),
                          name = c('name1', 'name 2'))

#### |||| #### |||| ####

### Load datasets files ###

## Retrieve name of each file in the individual sample file folder
filenames_samples <- list.files(here("Samples", "IndividualFiles"), pattern = "*.csv", full.names = TRUE)

## Read each of those sample files, make a list of them. Add the filename (sans pathway) as a parameter value.
sample_dataframe_list <- lapply(filenames_samples, function(x) {read.csv(x) %>% mutate(filename = gsub(paste0(here("Samples", "IndividualFiles"), "/"), "", x))})

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

## Import classifiation tracking sheet
trackingSheet <- read_xlsx(here("Classification_Tracking_Sheet.xlsx"), sheet = "TrackingData")

## Import site name metadata
siteNames <- read_xlsx(here("Metadata", "SiteNamesMetadata.xlsx"))

#### |||| #### |||| ####

### Add appropriate site values according to the site code ###

samples <- merge(samples, siteNames, by = "SiteCode")

## Verify that all names are entirely lowercase

samples$name <- tolower(samples$name)

#### |||| #### |||| ####


### Load the accuracy assessment points.


## Retrieve name of each file in the individual sample file folder
filenames_accuracy <- list.files(here("AccuracyAssessment"), pattern = "*.csv", full.names = TRUE)

## Read each of those sample files, make a list of them. Add the filename (sans pathway) as a parameter value. 
accuracy_dataframe_list <- lapply(filenames_accuracy, function(x) {read.csv(x) %>% ## Read each .csv file in the folder.
    mutate(filename = gsub(paste0(here("AccuracyAssessment"), "/"), "", x)) %>%  ## Make a column with file names.
    mutate(binary = ifelse(substr(.$filename, 22, 27) == "binary", 1, 0)) %>% ## If the data is binary, make the "binaryclass" value 1. This informs the program of how to refer to this data.
    #mutate(true_name = ifelse(binary == 1, "hello", "bye"))
    #left_join(ifelse(substr(.$filename, 22, 27) == "binary", binary_names, other_names))
    left_join(binary_names, by = "label")
  })

## Bind each dataframe to each other
accuracy_assessment_points <- bind_rows(accuracy_dataframe_list) %>% 
  select(-c(RASTERVALU))  ## Remove the "RASTERVALU" column, it's redundant with the "Classified" column. Also OK to remove before bringing the data in.

## Populate the f_cover column with <1> for suspect for any row without a <-3> (rejected/missing) and a greater than 50% difference from the previous year.
  
  
### !!!
# Maybe instead of applying the name to the dataset, we can add names when displaying results from that data. 
  # When displaying, we will have subset to a given file (date/site combo) so we won't need to do complicated conditional statements or iterate through df
  # Instead, before displaying, check date, site, and binary code. Join to binary_names by label or other_names by label, date, and site.









### Add class names to data without them ###

## Need to transpose the "class" columns so that there is a "label" column and multiple rows with 1, 2, 3, etc. and a class name column...

# classNames <- trackingSheet %>% 
#   select(c("Subarea", "Date8Digit", "Class0", "Class1", "Class2", "Class3", "Class4", "Class5")) %>% 
#   pivot_longer(c(Class0, Class1, Class2, Class3, Class4, Class5), names_to = "label", values_to = "name") %>% 
#   mutate(label = gsub("Class", "", label)) %>% 
#   mutate(label = as.integer(label))






#test <- pivot_longer(classNames, c(Class0, Class1, Class2, Class3, Class4, Class5), names_to = "Label", values_to = "Name")

### Create or fill in certain data columns ###

## For each dataframe, if there are no "Name" values, 
##pull names from the classification tracking document according to the date and label


## Create a veg/unveg column. Populate it with "Vegetated" or "Bare" based on the translation established above.




#### |||| #### |||| ####







