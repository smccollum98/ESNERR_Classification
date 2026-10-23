
## Load Packages ##
library(here)
library(tidyverse)
library(stars)
library(terra)
library(exactextractr)

## This script is for workshopping how we handle accuracy assessment data.

## The accuracy assessment data is a spreadsheet of map and reference classes for a number of samples.
    ## The samples are collected according to a stratified random sampling design and 
      ## the classified map is used to inform the strata.

## The accuracy assessment data should be used to create a matrix of Nij. For each
  ## sample that is classified as i, the number that are actually j (according to reference data).

## The proportion of area mapped as i (Wi) is derived from the raster.

## A table of Pij for the entire map is produced from the Nij table and Wi of each class.

## The OA, UA, and PA and 95% CI are calculated for the entire map from the Pij table.

## The Wi of each class within each sub-region is derived from the raster.

## The Pij for each sub-region is produced from the Nij and sub-region-specific Wi.
  ## lapply through table of pulled Wi for each 


aa <- read.csv(here("AccuracyAssessment", "IndividualFiles", "HP1_20241008_AA_20241206_binary.csv")) %>% 
  rename(reference = trueclass,
         prediction = predictedclass) %>% 
  mutate(prediction = ifelse(prediction == 1, 1, 0)) %>% 
  mutate(prediction = ifelse(prediction == 1, "veg", "unveg"),
         reference = ifelse(reference == 1, "veg", "unveg"))

con <- confusionMatrix(factor(aa$prediction), factor(aa$reference))$table


ras <- rast(here("ClassificationRasters", "HP1", "HP1_20241008_cl_20241206.tif"))

shp <- st_read(here("AnalysisGeometry", "HP1", "grid", "HP1_analysis_geometry_grid.shp")) %>% 
  mutate(row = row_number())

geo_column_names <- colnames(shp)

dat_raw <- exact_extract(ras, shp, append_cols = geo_column_names, 
                     function(value, coverage_fraction) {table(value)})

dat <- dat_raw %>% mutate(result.value = ifelse(result.value == 1, 1, 0))

dat_fill <- dat %>% 
  rename("label" = `result.value`) %>% 
  # mutate(area = summarize(sum(result.Freq))) %>% 
  group_by(Fill_Class, label) %>% 
  summarize(area = sum(result.Freq)) %>% #Make a custom name?
  ungroup() %>% 
  mutate(areaProp = area/sum(area))#%>% 
  # mutate(areaProp = result.Freq/area)


## Could loop through i and j

# for each i, loop through j. for each value nij, calculate nij/nj+

## for each Nij, calculate Pij.

# Input: Nij table (confusionmatrix), Wi of each class i (list).
conProp <- con

for(i in 1:nrow(con)){
  for(j in 1:ncol(con)){
    conProp[i,j] <- (con[i,j]/sum(con[i,]))*datOverall[i,"areaProp"][[1]]
  }
}

conProp <- as.data.frame(conProp)

propMatrix <- conProp %>% pivot_wider(names_from = Reference, values_from = Freq) %>% 
  column_to_rownames(var = "Prediction")

output <- data.frame()

dat_fill <- dat %>% 
  rename("label" = `result.value`) %>% 
  # mutate(area = summarize(sum(result.Freq))) %>% 
  group_by(Fill_Class, label) %>% 
  summarize(area = sum(result.Freq)) %>% #Make a custom name?
  ungroup() %>% 
  group_by(Fill_Class) %>%
  mutate(ID = cur_group_id()) %>%
  mutate(areaProp = area/sum(area)) %>% 
  ungroup()
  

for(x in 1:length(unique(dat_fill$ID))){
  
  region <- dat_fill %>% filter(ID == x)

  propMatrix <- con

  for(i in 1:nrow(con)){
    for(j in 1:ncol(con)){
      propMatrix[i,j] <- (con[i,j]/sum(con[i,]))*region[i,"areaProp"][[1]]
    }
  }
  
  propMatrix <- as.data.frame(propMatrix) %>%
    group_by(Reference) %>% 
    summarize(sampleAreaProp = sum(Freq)) %>% 
    mutate(label = row_number()-1)
  
  
  region <- left_join(region, propMatrix)
  
  ## Next to calculate the CI and attach to the region table.
  
  for(i in 1:nrow(con)){
    region[i,"areaProp"][[1]]*propMa
  }
  
  output <- rbind(output, region)
  
  ## Need to get propMatrix to have format
    # label | pred | ref
  
  
  ## Calculating error
  
  
  
}



## Need a separate table for each sub-region. Then, 


# for each i,
  # mapClass = rowName(con[i,])
    # for each j,
        # con[i,j] <- con[i,j]/sum(con[i,])*W[mapClass, propArea]



