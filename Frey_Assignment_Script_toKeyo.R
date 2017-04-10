###Frey_Assignment_Script.R - Mapping NYC School District Boundaries
###Begun by Nate Frey on 3 April 2017
###
###Purpose: This script produces maps of NYC School Boundaries.
###
###Inputs:  1)   2016-2017 School Boundaries shapefiles and associated reference files, extracted from
###         zip file downloaded from https://catalog.data.gov/dataset/2016-2017-school-zones 
###         on 4 April 2017. Documentation of these is quite poor, unfortunately, but they come from 
###         NYC Open Data.
###         2)   School names from an older (2011-2012) school point locations file from NYC Open Data 
###         https://data.cityofnewyork.us/Education/School-Point-Locations/jfju-ynrr
###
###Outputs: 1-3) Three Leaflet maps, one each for elementary, middle, and elementary schools
###
###Notes:   1)   School data files are missing names of certain schools. These have been set to their DBN
###         (an internal NYC school ID scheme). Possible to improve this with manual/ad-hoc research.
###         2)   In many parts of NYC, high schools are not tied to geography. These polygons are labeled
###         "Citywide High School Choice" in the high school map

###Last updated: 6 April 2017

##Cleanup
rm(list=ls()) ##Empty the environment
gc() ##Return any memory associated with removed objects to system

setwd("Your/Directory/Here") ##script will not work until you add a working directory or comment this out.

##Load libraries
require(rgdal) ##For loading shapefiles
require(rgeos) ##For spatial data operations
require(leaflet) ##For interactive maps
library(dplyr) ##For easy renaming of variables in data frames

##Bring in the school names for Elementary & Middle schools -- it's not in the data for them
school_names <- readOGR(dsn=".",layer="Public_Schools_Points_2011-2012A",stringsAsFactors = FALSE)
school_names$ATS_CODE <- substr(school_names$ATS_CODE,1,6) ##this doesn't read in properly, trim the junk
school_names <- as.data.frame(school_names) ##We don't actually use any of the spatial information
school_names <- rename(school_names,dbn=ATS_CODE) ##rename this variable to its equivalent in other files
school_names <- school_names[,c(1,5)] ##keep only columns actually used in script
school_names <- school_names[!duplicated(school_names$dbn),] ##one of the DBNs is duplicated, which breaks the merge -- get rid of it


##Load the polygon data from NYC Open Data
ny_elem <- readOGR(dsn="./NYCBoundaries",layer="geo_export_ba04b94a-51b7-4b86-9887-8c45c9011d4e") ##read elementary schools
ny_elem$dbn <- as.character(ny_elem$dbn) ##This reads as factor, but we actually want a character string
nyelem <- merge(ny_elem,school_names,by="dbn",all.x=TRUE) ##merge in the names from above
nyelem$SCHOOLNAME[is.na(nyelem$SCHOOLNAME)] <- paste0("DBN: ",nyelem$dbn[1:length(nyelem$SCHOOLNAME[is.na(nyelem$SCHOOLNAME)])]) ##Replace "NA" school names with their DBN ID
ny_ms <- readOGR(dsn="./NYCBoundaries",layer="geo_export_23deeb62-8e3a-4d71-ab0e-7fa24aef1180") ##read middle schools
ny_ms$dbn <- as.character(ny_ms$dbn) ##same as previous
nyms <- merge(ny_ms,school_names,by="dbn",all.x=TRUE) ##same as previous
nyms$SCHOOLNAME[is.na(nyms$SCHOOLNAME)] <- paste0("DBN: ",nyms$dbn[1:length(nyms$SCHOOLNAME[is.na(nyms$SCHOOLNAME)])]) ##same as previous
ny_hs <- readOGR(dsn="./NYCBoundaries",layer="geo_export_dad593f1-309b-45a8-ab00-95f2326f0176") ##Read high schools -- no manipulation required, already has names

##Map the high schools -- This one is easy, names are in the file
hs_popup <- paste0(ny_hs$sch_name) ##High school names will be popups on interactive map (click to activate)
hs_factored <- colorFactor(palette=rainbow(29),ny_hs$sch_name) ##Palette = rainbow of length = unique values of school name
leaflet(data=ny_hs) %>% addTiles() %>% addPolygons(fillColor = ~hs_factored(sch_name),weight=1,popup=hs_popup) ##draw leaflet map

##Map the middle schools 
ms_popup <- paste0(nyms$SCHOOLNAME) ##Middle school names will be popups on interactive map (click to activate)
ms_factored <- colorFactor(palette=rainbow(337),nyms$msid_no) ##Palette = rainbow of length = unique values of school name
leaflet(data=nyms) %>% addTiles() %>% addPolygons(fillColor = ~ms_factored(msid_no),weight=1,popup=ms_popup) ##draw leaflet map

##Map the elementary schools 
elem_popup <- paste0(nyelem$SCHOOLNAME) ##Elementary school names will be popups on interactive map (click to activate)
elem_factored <- colorFactor(palette=rainbow(337),nyelem$esid_no) ##Palette = rainbow of length = unique values of school name
leaflet(data=nyelem) %>% addTiles() %>% addPolygons(fillColor = ~elem_factored(esid_no),weight=1,popup=elem_popup) ##draw leaflet map


##TO DO: HOW TO OUTPUT LEAFLET OBJECTS?