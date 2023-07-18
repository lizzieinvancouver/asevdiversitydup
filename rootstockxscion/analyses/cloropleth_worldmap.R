##======================================================================================================
##  Chloropleth map
##  
##
## Project:         Asevdiversitydup
## Date:            16.06.2023
## Author:          Christophe
## Source from:     https://plotly.com/r/choropleth-maps/
#===================================



### Install packages if not already in system library
# install.packages("devtools")
# install.packages("kaleido")

library("devtools")
# devtools::install_github("ropensci/plotly")
library(ggplot2)
library(dplyr)
library(plotly)
library(rjson)
library(orca)
library("RColorBrewer")
# Directory Lizzie
#setwd("~/Documents/git/projects/vin/general/docs/manuscripts/asevdiversity/rootstockxscion/")

# Directory Christophe
directory_path <- "/Users/christophe_rouleau-desrochers/Documents/github"
# Set Working Directory
setwd(directory_path)

# read csv
d <- read.csv("asevdiversitydup/rootstockxscion/rootstockscionlatlon.csv")

duse <- subset(d, use=="yes")
head(duse)
# Delete rows where multiple locations for the same paper to avoid redundancy
duse_no_dupplicats<-duse[!duplicated(duse$title),]
# Create a new table grouped by country and the number of papers published in each
occurence <- duse_no_dupplicats %>%
  group_by(COUNTRY) %>%
  summarize(code = first(CODE), count = n())
head(occurence)


#===================================

#### WORLD MAP ####

# set boundaries to light grey  
l <- list(color = "grey", width = 0.1)

# specify map projection options, resolution, the scode, etc.
g <- list(
  projection = list(
    type = 'mercator.' # the ones I like: orthographic, mercator. Source: https://plotly.com/r/map-configuration/#map-projections
  ),
  resolution = 50,
  scope = "world",
  showcoastlines=TRUE,
  showland=TRUE,
  landcolor = "white"  # white smoke # for rgb colors: https://www.rapidtables.com/web/color/RGB_Color.html
)
# Set country color gradient according to the number of papers in each country
fig <- plot_geo(occurence)
fig <- fig %>% add_trace(
  z = ~count, color = ~count, colors = 'GnBu', #For color brewer palettes: http://www.sthda.com/english/wiki/colors-in-r
  text = ~COUNTRY, locations = ~code, marker = list(line = l)
)

# Add lon and lat points to the map, where each location point is related to papers title
fig <- fig %>% add_markers(
  x = ~duse$lon, y = ~duse$lat,
  text =~duse$title , color = I("black"), size = I(8)
)
# Set color bar
fig <- fig %>% colorbar(title = 'Occurence')
# Set title
fig <- fig %>% layout(
  title = 'Rootstock x Scion Experiment Locations',
  geo = g
)
fig


# Calculate the dot size based on `duse$nb_scion`
dot_size <- 2 + log(duse$n_rootstock)
dusecut <- duse [, c("id", "year", "use", "lat", "lon", "n_scions", "n_rootstock", "COUNTRY", "CODE")]
head(dusecut)
dusecut$n_rootstock <- as.numeric(dusecut$n_rootstock)
dusecut$dotsize_scions <- 2 + log(dusecut$n_scions)
dusecut$dotsize_rootstock <- log(dusecut$n_rootstock)
dusecut$dotsize_rootstock2 <- (dusecut$n_rootstock)


fig <- plot_geo(occurence) %>%
  layout(
    geo = list(
      #scope = "world",
      showframe = TRUE,
      showcoastlines = TRUE,
      showland = TRUE,
      landcolor = toRGB("white"),
      countrycolor = toRGB("darkgrey"),
      coastlinecolor = toRGB("black"),
      # projection = list(type = "natural earth"),
      lataxis = list(
        range = c(-55, 80),
        showgrid = FALSE
      ),
      lonaxis = list(
        range = c(-130, 160),
        showgrid = FALSE
      )
    )
  ) %>%
  add_trace(
    z = ~count, color = ~count, colors = 'GnBu',
    text = ~COUNTRY, locations = ~code, marker = list(line = l)) %>%
  add_markers(
    x = ~duse$lon, y = ~duse$lat,
    text = ~duse$title, color = I("black"), size = dusecut$dotsize_rootstock2
  )
fig



save_image(fig,
           file="/Users/christophe_rouleau-desrochers/Documents/github/asevdiversitydup/figures/mercator_scaled_dots.pdf")

 ### Install reticulate for export
# install.packages('reticulate')
# reticulate::install_miniconda()
# reticulate::conda_install('r-reticulate', 'python-kaleido')
# reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
# reticulate::use_miniconda('r-reticulate')
# 
# 
# 
# save_image(fig, "asevdiversitydup/figures/mercator_worldmap3.png", scale=15)

