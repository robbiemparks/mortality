rm(list=ls())

library(plyr)
library(ggplot2)
library(zoo)
library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(raster)
library(sp)
library(plyr)
library(graticule)
library(spatialEco)

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

year.start <- as.numeric(args[1])
year.end <- as.numeric(args[2])

# gender state and age lookup
gender.lookup <- c('Men','Women')

# load population data and make unique table of
filename <- paste0('../../data/population/original/datpopjapan20160307')
dat.pop <- subset(dat.pop, pref!=0)
dat.pop <- readRDS(filename)

# load mortality data
filename <- paste0('../../data/mortality/original/datmortjapan20160307')
dat.mort <- readRDS(filename)

# load shapefile for map and take data from shapefile
jp_national <- readOGR(dsn="../../data/shapefiles/",layer="JPN_adm1")
shapefile.data <- jp_national@data

# unique prefecture names from population file
test <- sort(as.character(unique(dat.pop$pref)))

# unique prefecture names from map file
test2 <- sort(as.character(shapefile.data$NAME_1))

# test if the names are the same
test == test2