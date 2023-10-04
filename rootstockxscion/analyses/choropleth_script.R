## === === === === === === === === === === === === === === === === === === === 
##  Choropleth map - for publication
##  
##
## Project:         Asevdiversitydup
## Date:            16.06.2023
## Author:          Christophe Rouleau-Desrochers
## Source from:     https://plotly.com/r/choropleth-maps/
## === === === === === === === === === === === === === === === === === === === 

# housekeeping
rm(list=ls())  
options(stringsAsFactors=FALSE)

## === === === === === === === === === === === === === === === === === === === 

### Install packages if not already in system library
# install.packages("devtools")
# install.packages("kaleido")

# load libraries
library(devtools)
library(ggplot2)
library(dplyr)
library(plotly)
library(rjson)
library(RColorBrewer)

# Directory
directory_path <- "/Users/christophe_rouleau-desrochers/Documents/github"

# Set Working Directory
setwd(directory_path)

# read csv
d <- read.csv("asevdiversitydup/rootstockxscion/rootstockscionlatlon.csv")
# standardize NAs
d[d == ''] <- 'NA'
d[d=='na'] <- 'NA'


# Data cleaning -----------------------------------------------------------
# Subset for papers that will be used
duse <- subset(d, use=="yes")
# Delete rows where multiple locations for the same paper to avoid redundancy
duse_no_duplicate <- duse[!duplicated(duse$title),]

#### Relevant information for figure legend ####
# Sum of papers with no number of rootstock that they used
sum(is.na(duse_no_duplicate$n_rootstock))
# Sum of papers used for the map
sum(length(duse_no_duplicate$id))

# Min value of N.rootstock
min(as.numeric(duse_no_duplicate$n_rootstock), na.rm = TRUE)
# Max value of N.rootstock
max(as.numeric(duse_no_duplicate$n_rootstock), na.rm = TRUE)
# Mean value of N.rootstock
mean(as.numeric(duse_no_duplicate$n_rootstock), na.rm = TRUE)

# Min value of N.scion
min(as.numeric(duse_no_duplicate$n_scions), na.rm = TRUE)
# Max value of N.scion
max(as.numeric(duse_no_duplicate$n_scions), na.rm = TRUE)
# Mean value of N.scion
mean(as.numeric(duse_no_duplicate$n_scions), na.rm = TRUE)

# Jitter locations for later
duse_no_duplicate <- duse_no_duplicate %>%
  mutate(lat_jittered = jitter(lat, factor = 10),
         lon_jittered = jitter(lon, factor = 10))

# Create a new table grouped by country and the number of papers published in each
occurence <- duse_no_duplicate %>%
  group_by(COUNTRY) %>%
  summarize(code = first(CODE), count = n())

# === === === === === === === === === === === === === === === === === === === 

# World map ---------------------------------------------------------------
duse_no_duplicate$n_rootstock <- as.numeric(duse_no_duplicate$n_rootstock)
# Create column for dot size gradient
duse_no_duplicate$dotsize_rootstock <-  as.numeric(0.75*(duse_no_duplicate$n_rootstock), na.rm=FALSE)
# Replace NA by zero in order to display a dot, even when there is no mention 
duse_no_duplicate["dotsize_rootstock"][is.na(duse_no_duplicate["dotsize_rootstock"])] <- 0.1


#### For HTML - Plotly map with jittered locations ####
fig_jittered <- plot_geo(occurence) %>%
  layout(
    geo = list(
      showframe = TRUE,
      showcoastlines = TRUE,
      showland = TRUE,
      landcolor = toRGB("white"),
      countrycolor = toRGB("darkgrey"),
      coastlinecolor = toRGB("black"),
      coastlinewidth = 0.5,
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
  # Color gradient set to the number of papers in each country
  add_trace(
    z = ~count, color = ~count, colors = 'GnBu',
    text = ~COUNTRY, locations = ~code,
    marker = list(
      line = list(width = 0.5, color = "black")
    ),
    # Edit the colar bar position --> make the X value negative if I want to set it on the left
    colorbar = list(title = "Number of
papers published
in each country", x = 1, y = 1.1, len = 1.03)
  ) %>%
  # Dots for which the size is set to the number of rootstocks used in the paper
  add_trace(
    type = "scattergeo",
    lat = ~duse_no_duplicate$lat_jittered, 
    lon = ~duse_no_duplicate$lon_jittered,
    text = ~paste("Title: ", duse_no_duplicate$title, "<br>Number of Rootstocks: ", duse_no_duplicate$n_rootstock),
    mode = "markers",
    marker = list(
      size = duse_no_duplicate$dotsize_rootstock,
      symbol = "circle",
      color = "lightgrey",
      line = list(width = 1.5, color = "black")
    )
  ) %>% 
  layout(title = "")

fig_jittered



####For static version (SVG) - Plotly map with jittered locations ####

fig_jittered_static <- plot_geo(occurence) %>%
  layout(
    geo = list(
      showframe = TRUE,
      showcoastlines = TRUE,
      showland = TRUE,
      landcolor = toRGB("white"),
      countrycolor = toRGB("darkgrey"),
      coastlinecolor = toRGB("black"),
      coastlinewidth = 0.5,
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
  # Color gradient set to the number of papers in each country
  add_trace(
    z = ~count, color = ~count, colors = 'GnBu',
    text = ~COUNTRY, locations = ~code,
    marker = list(
      line = list(width = 0.5, color = "black")
    ),
    # Edit the colar bar position --> make the X value negative if I want to set it on the left
    colorbar = list(title = "Number of
papers published
in each country", x = 1, y = 0.88)
  ) %>%
  # Dots for which the size is set to the number of rootstocks used in the paper
  add_trace(
    type = "scattergeo",
    lat = ~duse_no_duplicate$lat_jittered,
    lon = ~duse_no_duplicate$lon_jittered,
    text = ~paste("Title: ", duse_no_duplicate$title, "<br>Number of Rootstocks: ", duse_no_duplicate$n_rootstock),
    mode = "markers",
    marker = list(
      size = duse_no_duplicate$dotsize_rootstock,
      symbol = "circle",
      color = "lightgrey",
      line = list(width = 1, color = "black")
    )
  ) %>%
  layout(title = "")

fig_jittered_static

save_image(fig_jittered_static,
           file="/Users/christophe_rouleau-desrochers/Documents/github/asevdiversitydup/figures/mercator_scaled_dots.pdf")
