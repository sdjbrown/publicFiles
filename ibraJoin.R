library(dplyr)
library(sf)

# IBRA Version 7 shapefile can be freely downloaded from here:
#https://fed.dcceew.gov.au/datasets/interim-biogeographic-regionalisation-for-australia-ibra-version-7-subregions/explore

#Read in IBRA shapefile

ibraShapefilePath <- "/path/to/ibra7_regions.shp"
ibra <- st_read(ibraShapefilePath)

#Read in data

localityDataPath <- "path/to/Localities.csv"
localities <- read.csv(localityDataPath)

# Test data
# This code assumes that "Longitude" and "Latitude" columns are present, containing point locations in the WGS84 datum
localities <- data.frame(Name = c("ANIC", "WAM", "QM", "AM"), Longitude = c(149.1140, 115.9332, 153.01838, 151.2131), Latitude = c(-35.2743, -31.9855, -27.4728, -33.8744))

locCoord <- st_as_sf(localities, coords = c("Longitude", "Latitude"), crs = "OGC:CRS84")

ibraJoin <- locCoord %>%
	st_transform(crs = "EPSG:4283") %>% 
	st_join(ibra, join = st_within)
