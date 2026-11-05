
## Load Packages ##
library(here)
library(tidyverse)
library(stars) # Polygons
library(terra) # Rasters
library(exactextractr) # Used for extracting raster data from within the bounds of polygons
library(caret) ## Used for generating and manipulating confusion matrices

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



## similar file name structure could make it convenient to loop over filenames with gsub

# Load confusion matrix for current classification
con <- read.csv(here("AccuracyAssessment", "ConfusionMatrices", "HP1_20210826_cm_20241206.csv"), row.names = 1, header = TRUE)

# Maybe load classification data? Or do all of this in the ClassificationAnalysis code.

# dat <- read.csv(here("ClassificationResults", "HP1", "HP1_classification_analysis_all.csv")) %>% 
  # filter(filename == "HP1_20241008_cl_20241206")



## Make table of label, area, and proportion of area of entire area of interest
  # The "area" is the total number of map pixels assigned that label
  # The "areaProp"
overallArea <- extracted %>% 
  # rename("label" = `result.value`) %>% 
  # mutate(area = summarize(sum(result.Freq))) %>% 
  group_by(label) %>% ## Group by label so that the summary is for each land cover class
  summarize(area = sum(count)) %>% # Calculate the overall count of pixels for each label
  ungroup() %>% ## Ungroup
  mutate(areaProp = area/sum(area)) %>%  # Calculate the proportion of total area of each label
  mutate()


#### Calculate area of proportion confusion matrix ####
  ## Create a confusion matrix with the proportion of area instead of sample count.
  ## This is used for calculating accuracies down the line.

# pij = Wi*(nij/ni+) (Olofsson et al. 2014)

propMatrixAll <- con # Create a new confusion matrix

for(i in 1:nrow(con)){ # For each row of the confusion matrix
  for(j in 1:ncol(con)){ # For each column of the confusion matrix
    propMatrixAll[i,j] <- (con[i,j]/sum(con[i,]))*overallArea[i,"areaProp"][[1]] # Replace the sample number with the proportion of area.
  }
}

## Calculate the overall accuracy of the map and the user and producer accuracy of each mapped class

accuracies <- data.frame() # Create a new data frame

## Calculate user accuracy
for(u in 1:nrow(propMatrixAll)){ # For each row in propMatrixAll (i.e., for each label)
  
  ua <- propMatrixAll[u,u]/sum(propMatrixAll[u,]) # Calculate user accuracy; Ui = pii/pi+ (Olofsson et al. 2014)
  
  ua_CI <- sqrt((ua*(1-ua))/(sum(con[u,] - 1)))*1.96 # Calculate 95% CI of ua; sqrt((Ui*(1-Ui))/(ni-1))*1.96 (Olofsson et al. 2014)
  
  pa <- propMatrixAll[u,u]/sum(propMatrixAll[,u]) # Calculate producer accuracy; Pj = Pjj/P+j (Olofsson et al. 2014)
  # print(pa)
  
  pa_CI <- "tbd" # Calculate 95% CI of pa; Crazy equation - will do later (Olofsson et al. 2014)
  
  oa <- sum(diag(as.matrix(propMatrixAll))) # Calculate overall accuracy; sum(Pii)
  
  oa_CI <- sqrt((sum(propMatrixAll[u,])^2)*ua*(1-ua)/(sum(con[u,]) - 1))*1.96 # Calculate 95% CI of oa (Olofsson et al. 2014)
  
  tempTable <- data.frame(
    class = row.names(propMatrixAll)[u],
    "oa" = oa,
    "oa_CI" = oa_CI,
    "ua" = ua,
    "ua_CI" = ua_CI,
    "pa" = pa,
    "pa_CI" = pa_CI)
  
  accuracies <- rbind(accuracies, tempTable)
  
}

accuracies <- accuracies %>% 
  mutate(oa_CI = sum(oa_CI))


#### Area estimates

areaEstimates <- data.frame() # Create new data frame



#From the ouput of exact_extract, create a table of subregion, label, area, and areaproportion
## There should be a row for each subregionXlabel combination

areaTable <- data.frame()

subArea <- extracted %>%
  # mutate(area = summarize(sum(result.Freq))) %>% 
  group_by(Type, label) %>% 
  summarize(area = sum(count)) %>% #
  ungroup() %>% 
  group_by(Type) %>%
  mutate(ID = cur_group_id()) %>%
  mutate(areaProp = area/sum(area)) %>% 
  ungroup()

# Calculate the sample-based estimated area for each class within each subregion
  # To do this, need to create proportion of area matrix for each subregion (using subregion map area and overall sample matrix)
  # Once proportion of area matrix is calculated, can calculate 
for(x in 1:length(unique(subArea$ID))){ # For each unique ID 
  
  region <- subArea %>% filter(ID == x) # Declare the current subregion ID
  
  propMatrix <- con # Create a new proportion matrix for that specific region
  
  for(i in 1:nrow(con)){
    for(j in 1:ncol(con)){
      propMatrix[i,j] <- (con[i,j]/sum(con[i,]))*region[i,"areaProp"][[1]]
    }
  }
  
  ## Next to calculate the CI and attach to the region table.
  
  for(k in 1:ncol(con)){ # for each column in the confusion matrix
    
    variance <- 0 # For this column, set the "sumTerm" to 0
    
    for(i in 1:nrow(con)){ # For each row in the confusion matrix
      
      v <- (region[i,"areaProp"][[1]]*propMatrix[i, k]-propMatrix[i, k]^2)/(con[i,]-1) # area variance
      
      variance <- variance + v
      
    }
    
    SE <- sqrt(variance)
    
    CI <- SE*1.96 ## Get the 95% CI of column k
    
    estPropArea <- sum(propMatrix[,k])
    
    mapPropArea <- sum(con[k,])
    
    estArea <- estPropArea*region[k,"areaProp"][[1]]
    
    estAreaCI <- CI*region[k,"areaProp"][[1]]
    
    ## Create a row in a table (region | label | proportion of area | proportion of area CI | area | area CI)
    # print(paste0(x, ", ", k, ", ", mapPropArea, ", ", estPropArea, ", ", CI))
    
    out <- data.frame(regionID = x, label = k, 
                      mappedPropArea = mapPropArea, 
                      estimatedPropArea = estPropArea, estimatedPropAreaCI = CI,
                      estimatedArea = estArea, estimatedAreaCI = estAreaCI)
    
  rbind(areaTable, out)
    
    ## Create a table
    
    # CI <- sqrt(sumTerm)*1.96
    
    ## Proportion of area is going to be the sum of each column
    # 
    # pivot
    # 
    # print(paste0(x, ", ", i, ", ", k, ", ", sumTerm))
    # print(gather(sumTerm, columnnames(sumTerm), "areaCI"))
    
    ## Need to make table with columns region | label | proportion of area | proportion of area CI | area | area CI
    
  }
  
  
  
  # propMatrix <- as.data.frame(propMatrix) %>%
  #   group_by(Reference) %>% 
  #   summarize(sampleAreaProp = sum(Freq)) %>% 
  #   mutate(label = row_number()-1)

  # 
  # region <- left_join(region, propMatrix)

  # 
  # for(i in 1:nrow(con)){
  #   region[i,"areaProp"][[1]]*propMa
  # }
  
  # output <- rbind(output, region)
  
}





output %>% 
  mutate(OA = sum(diag(as.matrix(propMatrixAll))))

## Need a separate table for each sub-region. Then, 
class(sum(diag(as.matrix(propMatrixAll))))

# for each i,
# mapClass = rowName(con[i,])
# for each j,
# con[i,j] <- con[i,j]/sum(con[i,])*W[mapClass, propArea]



### ?? ###



# 
# as.data.frame(con)
# propMatrixAll <- as.matrix(conProp %>% pivot_wider(names_from = Reference, values_from = Freq) %>% 
#                              column_to_rownames(var = "Prediction"))



# ras <- rast(here("ClassificationRasters", "HP1", "HP1_20241008_cl_20241206.tif"))


# 
# aa <- read.csv(here("AccuracyAssessment", "IndividualFiles", "HP1_20241008_AA_20241206.csv")) %>%
#   rename(reference = trueclass,
#          prediction = predictedclass) %>%
#   mutate(prediction = ifelse(prediction == 1, 1, 0)) %>%
#   mutate(prediction = ifelse(prediction == 1, "veg", "unveg"),
#          reference = ifelse(reference == 1, "veg", "unveg"))
# 
# con <- confusionMatrix(factor(aa$prediction), factor(aa$reference))$table


# ras <- rast(here("ClassificationRasters", "HP1", "HP1_20241008_cl_20241206.tif"))

# shp <- st_read(here("AnalysisGeometry", "HP1", "grid", "HP1_analysis_geometry_grid.shp")) %>% 
#   mutate(row = row_number())

# geo_column_names <- colnames(shp)
# 
# dat_raw <- exact_extract(ras, shp, append_cols = geo_column_names, 
#                      function(value, coverage_fraction) {table(value)})

# dat <- dat_raw %>% mutate(result.value = ifelse(result.value == 1, 1, 0))
# 
# dat_fill <- dat %>% 
#   rename("label" = `result.value`) %>% 
#   # mutate(area = summarize(sum(result.Freq))) %>% 
#   group_by(Fill_Class, label) %>% 
#   summarize(area = sum(result.Freq)) %>% #Make a custom name?
#   ungroup() %>% 
#   mutate(areaProp = area/sum(area))#%>% 
#   # mutate(areaProp = result.Freq/area)


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


propMatrixAll <- as.matrix(conProp %>% pivot_wider(names_from = Reference, values_from = Freq) %>% 
  column_to_rownames(var = "Prediction"))

# 
# for(h in 1:nrow(propMatrix)){
#   
#   OA <- sum(diag())
#   
#   
# }

accuracies <- data.frame()

## Calculate user accuracy
for(u in 1:nrow(propMatrixAll)){
  
  ua <- propMatrixAll[u,u]/sum(propMatrixAll[u,])

  ua_CI <- sqrt((ua*(1-ua))/(sum(con[u,] - 1)))*1.96
  # print(paste0("Olofsson: ", ua_CI_Olof))
  
  pa <- propMatrixAll[u,u]/sum(propMatrixAll[,u])
  # print(pa)
  
  pa_CI <- "tbd"
  
  oa <- sum(diag(as.matrix(propMatrixAll)))/sum(propMatrixAll)
  
  oa_CI <- sqrt((sum(propMatrixAll[u,])^2)*ua*(1-ua)/(sum(con[u,]) - 1))*1.96
  
  tempTable <- data.frame(
                    class = row.names(propMatrixAll)[u],
                    "oa" = oa,
                    "oa_CI" = oa_CI,
                    "ua" = ua,
                    "ua_CI" = ua_CI,
                    "pa" = pa,
                    "pa_CI" = pa_CI)
  
  accuracies <- rbind(accuracies, tempTable)
  
}

accuracies <- accuracies %>% 
  mutate(oa_CI = sum(oa_CI))


## Area estimates
output <- data.frame()


# dat_all <- dat %>% 
#   rename("label" = `result.value`) %>% 
#   # mutate(area = summarize(sum(result.Freq))) %>% 
#   group_by(label) %>% 
#   summarize(area = sum(result.Freq)) %>% #Make a custom name?
#   ungroup() %>% 
#   # group_by(Fill_Class) %>%
#   mutate(ID = cur_group_id()) %>%
#   mutate(areaProp = area/sum(area)) %>% 
#   ungroup()

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
  # print(propMatrix)
  ## Next to calculate the CI and attach to the region table.
  
  for(k in 1:ncol(con)){
    
    sumTerm <- 0
    
    for(i in 1:nrow(con)){
      
      a <- (region[i,"areaProp"][[1]]*propMatrix[i, k]-propMatrix[i, k]^2)/(con[i,]-1)
      
      sumTerm <- sumTerm + a
      
    }
    
    ## Create a table
    
    
    
    
    print(gather(sumTerm, columnnames(sumTerm), "areaCI"))
    
  }
  
  
  propMatrix <- as.data.frame(propMatrix) %>%
    group_by(Reference) %>% 
    summarize(sampleAreaProp = sum(Freq)) %>% 
    mutate(label = row_number()-1)
  
  
  
  
  
  
  region <- left_join(region, propMatrix)
  
  
  
  
  # 
  # for(i in 1:nrow(con)){
  #   region[i,"areaProp"][[1]]*propMa
  # }
  
  output <- rbind(output, region)
  
  
  
}





output %>% 
  mutate(OA = sum(diag(as.matrix(propMatrixAll))))

## Need a separate table for each sub-region. Then, 
class(sum(diag(as.matrix(propMatrixAll))))

# for each i,
  # mapClass = rowName(con[i,])
    # for each j,
        # con[i,j] <- con[i,j]/sum(con[i,])*W[mapClass, propArea]



