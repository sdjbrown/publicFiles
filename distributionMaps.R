##################################################

# This script provides R code for creating distribution maps of New Zealand taxa

# Samuel Brown
# 4 July 2025


##################################################

library(dplyr)
library(sf)
library(terra)
library(jsonlite)
library(colorspace)

setwd("/home/sam/mappingTemp")


##################################################
# Load external geographic datasets

#---------------------------------------
# Crosby code boundaries

# Shapefile available from 
# https://lris.scinfo.org.nz/layer/48165-nz-area-codes-for-recording-specimen-localities/

ccFull <- st_read("lris-nz-area-codes-for-recording-specimen-localities-SHP/nz-area-codes-for-recording-specimen-localities.shp")
st_crs(ccFull) <- "EPSG:2193"

# The shapefile as downloaded directly from LRIS is large, and takes some time to plot. When preparing multiple, small-scale maps it is useful to have a simplified version of the data available to speed up the plotting.

ccAll <- st_cast(ccFull, "POLYGON")
cc <- ccAll[as.numeric(st_area(ccAll)) > 3000000, ] %>%
	group_by(EA_Code) %>%
	summarise(EA_Abbrev = unique(EA_Abbrev),
		EA_Name = unique(EA_Name))

system.time(plot(st_geometry(ccFull)))
system.time(plot(st_geometry(cc)))


#---------------------------------------
# NZ coastline 

# Shapefile available from 
# https://data.linz.govt.nz/layer/51559-nz-coastlines-and-islands-polygons-topo-1250k/
					
nz_coast <- st_read("kx-nz-coastlines-and-islands-polygons-topo-1250k-SHP/nz-coastlines-and-islands-polygons-topo-1250k.shp")
nzcoast <- nz_coast %>%
	mutate(area = st_area(nz_coast)) %>%
	filter(area > units::set_units(10^7, m^2)) %>%
	st_transform("EPSG:2193")


#---------------------------------------
# Hyposometric raster map of New Zealand
# GeoJPEG courtesy of Geographx available from
# https://koordinates.com/layer/514-nz-hypsometric-raster-100m/

NZ <- rast("kx-nz-hypsometric-raster-100m-JPEG/hypsometric100m.jpg")


##################################################
# Obtain data for plotting

iNatData <- fromJSON("https://api.inaturalist.org/v1/observations?place_id=6803&taxon_id=378078")

inputData <- data.frame(id = iNatData$results$id, species = iNatData$results$taxon$name, date = iNatData$results$observed_on) %>%
	mutate(latitude = sapply(iNatData$results$geojson$coordinates, function(xx) xx[2]),
			longitude = sapply(iNatData$results$geojson$coordinates, function(xx) xx[1]))


# Self-contained dataset, if required
#inputData <- structure(list(id = c(291086685L, 289696910L, 286740604L, 281221950L, 280149995L, 279107956L), species = c("Scolopterus aequus", "Scolopterus penicillatus", "Scolopterus aequus", "Scolopterus aequus", "Scolopterus tetracanthus", "Scolopterus penicillatus"), date = c("2025-06-20", "2025-06-14", "2025-03-10", "2025-01-25", "2025-05-10", "2025-05-07"), latitude = c(-36.95005319, -41.2667860294, -36.99234, -40.8532116667, -41.2668182866, -41.2794952653), longitude = c(174.6626285166, 174.7560930762, 174.60267, 175.43417, 174.7558033976, 174.7736919318)), row.names = c(NA, 6L), class = "data.frame")


head(inputData)


##################################################

# Prepare dataset for plotting

distributionData <- inputData %>%
	st_as_sf(coords = c("longitude", "latitude"), crs = "OGC:CRS84") %>%
	st_transform(stephCoord, crs = "EPSG:2193") %>%  #Transform to New Zealand Transverse Mercator projection
	st_join(cc, join = st_within)

head(distributionData)

#The following code minimally requires a dataframe with 'species', 'longitude' and 'latitude' fields

##################################################
# Plot points over filled-in Crosby Code regions

plotCoordsCrosbyCode <- function(df, plotbg = "#bde9f7", ccBG = "green", ptBG = "auto", mapLabel = NA, extent = 1.03, path = "", outFile = NA, fillChat  = FALSE, ptCEX = 1.3, tadj = c(-0.075, 1.75)){
	if(ptBG == "auto") ptBG <- darken(ccBG, 0.4) else ptBG <- ptBG
	if(is.na(outFile)) output <- FALSE else output <- TRUE
	fcols <- rep(NA, 30)
	fcols[unique(df$EA_Code) + 1] <- ccBG
	mlim <- st_bbox(cc)
	mlim[c(3, 4)] <- mlim[c(3, 4)] * extent
	
	if(output) tiff(paste(path, outFile, sep = ""), width = 669, height = 669, pointsize = 20)
		par(mar = rep(0, 4))
		plot(st_geometry(cc), border = "grey50", col = "white", bgc = plotbg, xlim = mlim[c(1,3)], ylim = mlim[c(2,4)])
		plot(st_geometry(nzcoast), border = "white", col = "white", add = TRUE)
		plot(st_geometry(cc), border = "grey80", col = fcols, add = TRUE)
		if(fillChat == TRUE) plot(st_geometry(nzcoast)[c(1, 5)], col = ccBG, border = "white", add = TRUE)
		plot(st_geometry(nzcoast), border = "grey50", add = TRUE)
		plot(st_geometry(df), pch =21, bg = ptBG, col = lighten(ptBG, 0.2), add = TRUE, cex = ptCEX)
		points(1730000, 5010000, pch = 21, bg = ptBG, col = lighten(ptBG, 0.2), cex = ptCEX)
		text(1730000, 5010000, adj = tadj, label = as.expression(substitute(italic(x), list(x = mapLabel))))
	if(output) dev.off()
}

plotCoordsCrosbyCode(distributionData[grep("aequu", distributionData$species),], ccBG = "#e3bc9d", mapLabel = "Scolopterus\naequus")
plotCoordsCrosbyCode(distributionData[grep("aequu", distributionData$species),], ccBG = "#e3bc9d", mapLabel = "Scolopterus\naequus", outFile = "aequus.tif")

plotCoordsCrosbyCode(distributionData[grep("penicill", distributionData$species),], ccBG = "#ff0007", mapLabel = "Scolopterus\npenicillatus")
plotCoordsCrosbyCode(distributionData[grep("penicill", distributionData$species),], ccBG = "blue", mapLabel = "Scolopterus\npenicillatus", fillChat = TRUE, outFile = "penicillatus.tif")

##################################################
# Plot points for multiple species over hyposometric map of New Zealand


# Helper functions
mm <- function(xx) xx/25.4

sppPoints <- function(df, spp, sPCH = 16, pcex = 0.8, pcol = "black", pout = "auto"){
		if(pout == "auto") poutline <- darken(pcol, 0.4) else poutline <- pout
		df <- df[grep(spp, df$species),]
		plot(st_geometry(df), add = TRUE, pch = sPCH, cex = pcex, col = poutline, bg = pcol, lwd = 8)
}

ei <- function(names) sapply(names, function(yy) as.expression(substitute(italic(x), list(x = yy))))

# Vectors for the desired point shapes and colours
pointsVector <- c(21, 22, 24, -0x2736)
colourVector <- c("#f4cc8f", "#ffd37a", "#9c805b")

#---------------------------------------
# All of New Zealand

#cairo_pdf("map-ScolopterusNewZealand.pdf", width = mm(146), height = mm(215))
tiff("map-ScolopterusNewZealand.tif", width = 1725, height = 2540, pointsize = 50)
par(mar = c(0,0,0,0))
plot(NZ)
sppPoints(distributionData, "peni", sPCH = pointsVector[1], pcex = 1.5, pcol = colourVector[1])
sppPoints(distributionData, "aequ", sPCH = pointsVector[2], pcex = 1.5, pcol = colourVector[2])
sppPoints(distributionData, "tetra", sPCH = pointsVector[3], pcex = 1.5, pcol = colourVector[3])
legend(1500000 - 22000, 5000000, pch = pointsVector[1:3], pt.bg = colourVector[1:3], col = darken(colourVector, 0.4), pt.lwd = 8, legend = ei(c("Scolopterus penicillatus", "Scolopterus aequus", "Scolopterus tetracanthus")), bty = "n", pt.cex = 1, cex = 1)
segments(1500000 - 100000, 4790000, 1500000 + 100000, lwd = 2)
text(1500000, 4790000 + 10000, label = "200 km", cex = 0.5)
dev.off()


#---------------------------------------
# North Island only

#cairo_pdf("map-ScolopterusNorthIsland.pdf", width = mm(146), height = mm(215))
tiff("map-ScolopterusNorthIsland.tif", width = 1725, height = 2540, pointsize = 50)
par(mar = c(0,0,0,0))
plot(NZ, xlim = c(1482161, 2099964), ylim = c(5380000, 6230210))
sppPoints(distributionData, "peni", sPCH = pointsVector[1], pcex = 1.5, pcol = colourVector[1])
sppPoints(distributionData, "aequ", sPCH = pointsVector[2], pcex = 1.5, pcol = colourVector[2])
sppPoints(distributionData, "tetra", sPCH = pointsVector[3], pcex = 1.5, pcol = colourVector[3])
legend(1800000 - 22000, 6200000, pch = pointsVector[1:3], pt.bg = colourVector[1:3], col = darken(colourVector, 0.4), pt.lwd = 8, legend = ei(c("Scolopterus penicillatus", "Scolopterus aequus", "Scolopterus tetracanthus")), bty = "n", pt.cex = 1, cex = 1)
segments(1800000, 6050000, 1800000 + 200000, lwd = 2)
text(1800000 + 100000, 6050000 + 10000, label = "200 km", cex = 0.5)
dev.off()


#---------------------------------------
# South Island only; this time with the boundaries of the Crosby Codes overlaid on the map

#cairo_pdf("map-ScolopterusSouthIsland.pdf", width = mm(146), height = mm(215))
tiff("map-ScolopterusSouthIsland.tif", width = 1725, height = 2540, pointsize = 50)
par(mar = c(0,0,0,0))
plot(NZ, xlim = c(1067219, 1788757), ylim = c(4734479, 5530000))
plot(st_geometry(cc), border = "grey30", lwd = 3, add = TRUE)
text(st_geometry(st_centroid(cc)), labels = cc$EA_Abbrev, col = "grey30")
sppPoints(distributionData, "peni", sPCH = pointsVector[1], pcex = 1.5, pcol = colourVector[1])
sppPoints(distributionData, "aequ", sPCH = pointsVector[2], pcex = 1.5, pcol = colourVector[2])
sppPoints(distributionData, "tetra", sPCH = pointsVector[3], pcex = 1.5, pcol = colourVector[3])
legend(1450000 - 22000, 4950000, pch = pointsVector[1:3], pt.bg = colourVector[1:3], col = darken(colourVector, 0.4), pt.lwd = 8, legend = ei(c("Scolopterus penicillatus", "Scolopterus aequus", "Scolopterus tetracanthus")), bty = "n", pt.cex = 1, cex = 1)
segments(1450000 - 100000, 4790000, 1450000 + 100000, lwd = 2)
text(1450000, 4790000 + 10000, label = "200 km", cex = 0.5)
dev.off()


