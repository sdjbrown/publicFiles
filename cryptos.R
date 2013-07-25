library(maptools)


#A better chloropleth plot
# Courtesy of http://stackoverflow.com/questions/1260965/developing-geographic-thematic-maps-with-r

plot.heat <- function(counties.map, state.map, z, title = NULL, breaks = NULL, reverse = FALSE, cex.legend = 1, bw = 0.2, col.vec = NULL, plot.legend = TRUE, ...) {
  ##Break down the value variable
  if (is.null(breaks)) {
    breaks=
      seq(
          floor(min(counties.map@data[,z],na.rm=TRUE)*10)/10
          ,
          ceiling(max(counties.map@data[,z],na.rm=TRUE)*10)/10
          ,.1)
  }
  counties.map@data$zCat <- cut(counties.map@data[,z],breaks,include.lowest=TRUE)
  cutpoints <- levels(counties.map@data$zCat)
  if (is.null(col.vec)) col.vec <- heat.colors(length(levels(counties.map@data$zCat)))
  if (reverse) {
    cutpointsColors <- rev(col.vec)
  } else {
    cutpointsColors <- col.vec
  }
  levels(counties.map@data$zCat) <- cutpointsColors
  plot(counties.map,border=gray(.8), lwd=bw,axes = FALSE, las = 1,col=as.character(counties.map@data$zCat), ...)
  if (!is.null(state.map)) {
    plot(state.map,add=TRUE,lwd=1)
  }
  ##with(counties.map.c,text(x,y,name,cex=0.75))
  if (plot.legend) legend("bottomright", cutpoints, fill = cutpointsColors,bty="n",title=title,cex=cex.legend)
  ##title("Cartogram")
}


cryptos <- dget("/home/sam/git/publicFiles/cryptos.list")
CC <- readShapePoly("/home/sam/Documents/PhD/External_Data_and_standards/maps/Crosby_code_shapefile/lris_original/nz-area-codes-for-recording-specimen-localities.shp")

cryptoSppDiversity <- table(unlist(cryptos))


crypGen <- split(cryptos, gen)
genDist <- lapply(crypGen, function(xx) unique(unlist(xx)))
cryptoGenDiversity <- table(unlist(genDist))

CC@data$SppDiversity <- cryptoSppDiversity[match(CC@data$EA_Abbrev, names(cryptoSppDiversity))]
CC@data$GenDiversity <- cryptoGenDiversity[match(CC@data$EA_Abbrev, names(cryptoGenDiversity))]

plot.heat(CC, NULL, z = "SppDiversity", breaks = c(0, 10, 20, 40, 60, 80, 120), col.vec = grey(seq(0.1, 0.7, length.out = 6)), reverse = TRUE)
plot.heat(CC, NULL, z = "GenDiversity", breaks = c(0, 5, 10, 15, 20, 25, 30, 40), col.vec = grey(seq(0.1, 0.7, length.out = 7)), reverse = TRUE)


