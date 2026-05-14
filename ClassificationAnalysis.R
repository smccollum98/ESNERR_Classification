## Raster analysis

### Load Packages ###
library(sf)
library(stars)
library(dplyr)
library(here)
library(ggplot2)

grid <- st_read(here("AnalysisGrid", "HP1", "HP1_5m_grid.shp"))

raster <- read_stars(here("ClassificationRasters", "MP1_241008_RF_Classification_241206_Mosaic.tif"))

#aggregate(raster, grid, mean)

ggplot()+
  geom_stars(data = raster)+
  geom_sf(data = grid, alpha = 0)

raster_by_polygon <- aggregate(raster, grid, mean)
