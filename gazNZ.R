# gazNZ and coordGNZ
# Samuel Brown
# 23 May 2012
# s_d_j_brown@hotmail.com
# http://www.the-praise-of-insects.blogspot.com

# This function retreives details of the given place name, from data contained in the New Zealand Place Names Database (http://www.linz.govt.nz/placenames/find-names/topographic-names-db)

# Please note the following from the website!
############################
# Note that maintenance of the database, previously known as the New Zealand Geographic Place Names database, ENDED ON 31 OCTOBER 2008.

# Because this database is not maintained, it is NOT AUTHORITATIVE and should not be used used as the source of official geographic names defined by the New Zealand Geographic Board Act 2008.
############################

# Use the following function (and the database) with caution.

############################
############################
############################

#Download details from the database
gazNZ <- function(placeName){
	placeName <- gsub(" ", "+", placeName)
	searchURL <- paste0("http://www.linz.govt.nz/placenames/find-names/topographic-names-db/database/index.aspx?name=", placeName, "&submit_name=Find")
	searchRes <- scan(searchURL, what = "character", sep = "\n")
	searchRes <- searchRes[grep("p=", searchRes)]
	if(length(searchRes) == 0) stop("placeName does not exist in database. Please try a different spelling, or spell 'Mt' or other abbreviations in full")
	searchRes <- gsub("[^0123456789]", "", searchRes)
	
	retrieveCoordinate <- function(refNum){
		coURL <- paste0("http://www.linz.govt.nz/placenames/find-names/topographic-names-db/database/index.aspx?p=", refNum)
		coRes <- scan(coURL, what = "character", sep = "\n")
		coValues <- strsplit(coRes[grep("Lat:", coRes)], ">")[[1]]
		coordDistrict <- strsplit(coValues[grep("District:", coValues)], split = ": |<b")[[1]][2]
		coordDescription <- paste(strsplit(coValues[grep("Description:", coValues)], split = ": |<b")[[1]][c(2,3)], collapse = ": ")
		coordLatitude <- as.numeric(strsplit(coValues[grep("Lat:", coValues)], split = " |<")[[1]][2])
		coordLongitude <- as.numeric(strsplit(coValues[grep("Long:", coValues)], split = " |<")[[1]][2])
		coordName <- strsplit(coRes[grep("h5", coRes)], split = "5>|</")[[1]][2]
		rm(list = ls()[!ls() %in% c(ls(pattern="coord"), ls(pattern="res"))])
		coordValue <- as.list(environment())
		coordValue
	}
	coordList <- lapply(searchRes, retrieveCoordinate)
	print(coordGNZ(coordList))
	invisible(coordList)
}

#Extract the lats and longs from the resulting list
coordGNZ <- function(gazNZ){
	coords <- lapply(gazNZ, function(xx) c(xx$coordLatitude, xx$coordLongitude))
	placeNames <- sapply(gazNZ, function(xx) xx$coordName)
	names(coords) <- placeNames
	coords
}

############################
############################
# Examples

#Capitalization is ignored
gazNZ("great moss swamp")
gazNZ("Great Moss Swamp")

#But abbreviations aren't
gazNZ("Mt Maunganui")
gazNZ("Mount Maunganui")

#A longer list---can sometimes take a while to retreive all details
gazNZ("Hamilton")

TheMount <- gazNZ("Mount Maunganui")
coordGNZ(TheMount)
