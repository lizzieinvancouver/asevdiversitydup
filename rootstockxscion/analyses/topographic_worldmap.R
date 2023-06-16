##======================================================================================================
##  Topographic World Map
##  
##
## Project:         Asevdiversitydup
## Date:            06.06.2023
## Author:          Lizzie and Christophe
## Source from:     https://milospopovic.net/crisp-topography-map-with-r/
#======================================================================================================

rm(list=ls(all=TRUE))

####TOPOGRAPHIC WORLD MAP#####
#------
# Path directory

#Directory Lizzie
#setwd("~/Documents/git/projects/vin/general/docs/manuscripts/asevdiversity/rootstockxscion/")

#Directory Christophe
directory_path <- "/Users/christophe_rouleau-desrochers/Documents/github"
# Set Working Directory
setwd(directory_path)

#work directory
d <- read.csv("asevdiversitydup/rootstockxscion/rootstockscionlatlon.csv")
duse_cut <- subset(d, use=="yes")

#libraries
libs <- c("elevatr", "terra", "tidyverse", 
          "sf", "giscoR", "marmap")

installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

invisible(lapply(libs, library, character.only = T))

###### 1. GET WORLD MAP ######
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

get_sf <- function() {
  world_sf <- giscoR::gisco_get_countries(
    year = "2016",
    epsg = "4326",
    resolution = "10"
  )
  
  world_transformed <- st_transform(world_sf, crs = crsLONGLAT)
  
  return(world_transformed)
}

world_transformed <- get_sf()

###### 2. GET ELEVATION DATA ######
get_elevation_data <- function(world_elevation, world_elevation_df) {
  
  world_elevation <- get_elev_raster(
    locations = world_transformed, 
    z = 3, 
    clip = "locations") 
  
  world_elevation_df <- as.data.frame(world_elevation, xy = T) %>%
    na.omit()
  
  colnames(world_elevation_df)[3] <- "elevation"
  
  return(world_elevation_df)
}

world_elevation_df <- get_elevation_data()


###### 3. MAP ######
get_elevation_map <- function(world_map) {
  
  world_map <- ggplot() +
    geom_tile(data = world_elevation_df, 
              aes(x = x, y = y, fill = elevation)) +
    scale_fill_etopo() +
    coord_sf(crs = crsLONGLAT)+
    theme_minimal() +
    theme(text = element_text(family = "georg", color = "#22211d"),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "none",
          panel.grid.major = element_line(color = "white", size = 0.2),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size=18, color="grey20", hjust=1, vjust=-5),
          plot.caption = element_text(size=8, color="grey70", hjust=.15, vjust=20),
          plot.margin = unit(c(t=0, r=0, b=0, l=0),"lines"), #added these narrower margins to enlarge maps
          plot.background = element_rect(fill = "white", color = NA), 
          panel.background = element_rect(fill = "white", color = NA),
          panel.border = element_blank()) +
    labs(x = "", 
         y = NULL, 
         title = "Topographic map world", 
         subtitle = "", 
         caption = "CRD")+
    geom_point(
      data = duse_cut,
      aes(lon, lat), 
      alpha = 0.7) +
    theme_bw() + 
    theme(legend.position="none")
  
  return(world_map)
}
world_map <- get_elevation_map()
#world_map
ggsave(filename="asevdiversitydup/figures/world_topo_map.png", world_map, width=7, height=8.5, dpi = 600, device='png')


