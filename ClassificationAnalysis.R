## Raster analysis

### Load Packages ###
library(sf)
library(stars)
library(dplyr)
library(here)

grid <- st_read(here("AnalysisGrid", "HP1", "HP1_5m_grid.shp"))

raster <- read_stars(here("ClassificationRasters", "MP1_191025_RF_241215_Mosaic.tif"))

aggregate(raster, grid, mean)



