rm(list=ls())

# break down the arguments from Rscript
args <- commandArgs(trailingOnly=TRUE)

library(rgeos)
require(ggplot2)
library(rgdal)
library(RColorBrewer)
library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(plyr)
library(scales)

###############################################################
# PREPARING MAP
###############################################################

# for theme_map
#devtools::source_gist("33baa3a79c5cfef0f6df")
theme_map <- function(base_size=9, base_family=""){
    require(grid)
    theme_bw(base_size=base_size,base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    axis.title=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid=element_blank(),
    panel.margin=unit(0,"lines"),
    plot.background=element_blank(),
    legend.justification = c(0,0),
    legend.position = c(0,0)
    )
}

# load shapefile
us <- readOGR(dsn="../../data/shapefiles",layer="states")

# convert shapefile to Albers equal area
us_aea <- spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
us_aea@data$id <- rownames(us_aea@data)

# extract, then rotate, shrink & move alaska (and reset projection)
alaska <- us_aea[us_aea$STATE_FIPS=="02",]
alaska <- elide(alaska,rotate=-50)
alaska <- elide(alaska,scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(us_aea)

# extract, then rotate & shift hawaii
hawaii <- us_aea[us_aea$STATE_FIPS=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5400000, -1400000))
proj4string(hawaii) <- proj4string(us_aea)

# remove old versions of Alaska and Hawaii and put new ones back in
us_aea <- us_aea[!us_aea$STATE_FIPS %in% c("02", "15"),]
us_aea <- rbind(us_aea, alaska, hawaii)

# extract data from shapefile
shapefile.data <- us_aea@data

# add climate regions from Karl and Koss
shapefile.data$climate_region <- c('Northwest','Northern Rockies and Plains','Northeast','Northern Rockies and Plains','Northern Rockies and Plains',
'Northern Rockies and Plains','Upper Midwest','Northwest','Northeast','Upper Midwest',
'Northwest','Northeast','Upper Midwest','Northeast','Northern Rockies and Plains',
'Northeast','Northeast','Northeast','Northeast','Northeast',
'Ohio Valley','West','Southwest','West','Ohio Valley',
'Ohio Valley','Northeast','Northeast','Ohio Valley','Northeast',
'Southwest','Ohio Valley','South','Southeast','Ohio Valley',
'Southwest','South','Southeast','Ohio Valley','South',
'Southwest','Southeast','South','Southeast','Southeast',
'South','South','Southeast','Upper Midwest','Northwest',
'West')

# reinsert shapefile.data with climate regions back into shapefile
us_aea@data <- shapefile.data

# extract superregions
for(i in unique(shapefile.data$climate_region)){
    temp <- us_aea[us_aea$climate_region==i,]
    assign(gsub(' ','_',i),temp)
}

# function to create borders around superregions
borders <- function(superregion) {
    lps <- getSpPPolygonsLabptSlots(superregion)
    IDOneBin <- cut(lps[,1], range(lps[,1]), include.lowest=TRUE)
    dissolve   <- unionSpatialPolygons(superregion ,IDOneBin)
}

# combine all superregions
superregions <- rbind(  borders(Northwest),borders(West_North_Central),borders(Northeast),borders(Upper_Midwest),
borders(East_North_Central),borders(West),borders(Southwest),borders(South),borders(Southeast))

# fortify to prepare for ggplot
map.superregions <- fortify(superregions)

