## Started 7 May 2023 ##
## By Lizzie so far ##

## On the plane from Zurich to Boston ##

# housekeeping
rm(list=ls()) 
options(stringsAsFactors=FALSE)

# packages
library(tidyverse)

# following along:
# https://www.geeksforgeeks.org/how-to-make-world-map-with-ggplot2-in-r/

# working directory
setwd("~/Documents/git/projects/vin/general/docs/manuscripts/asevdiversity/rootstockxscion/")

d <- read.csv("rootstockscionlatlon.csv")

duse <- subset(d, use=="yes")

world_coordinates <- map_data("world")
ggplot() + geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1  ) + 
geom_point(
    data = duse,
    aes(lon, lat, color="darkblue"), # darkblue
    alpha = 0.7
) +
    theme_bw() + 
    theme(legend.position="none")

## Looking at all recs...
alld <- read.csv("isi_5May2023.csv")

alld$notgrapevine <- "maybe grapevine"
alld$notgrapevine[grep("grapefruit", alld$Article.Title)] <- "grapefruit"
alld$notgrapevine[grep("citr", alld$Article.Title)] <- "citrus"
alld$notgrapevine[grep("Prunus", alld$Article.Title)] <- "peach"
# alld$notgrapevine[grep("wine", alld$Article.Title)] <- "wine"
# alld$notgrapevine[grep("grapevine", alld$Article.Title)] <- "grapevine"

graped <- subset(alld, notgrapevine=="maybe grapevine")
grapeyears <- aggregate(graped["notgrapevine"], graped["Publication.Year"], FUN=length)

names(grapeyears)[names(grapeyears)=="notgrapevine"] <- "n_publications"
grapeyears2022 <- subset(grapeyears, Publication.Year<2023)

plot(n_publications~Publication.Year, data=grapeyears2022, pch=16)
lines(n_publications~Publication.Year, data=grapeyears2022)

