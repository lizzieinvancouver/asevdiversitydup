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
# directory_path <- "/Users/christophe_rouleau-desrochers/Documents/github"
# temp laptop
directory_path <- "/Users/christophe/Documents/github/"
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

# Calculate the dot size based on `duse$nb_scion`
dot_size <- 2 + log(duse$n_rootstock)
dusecut <- duse [, c("id", "year", "use", "lat", "lon", "n_scions", "n_rootstock", "COUNTRY", "CODE")]
head(dusecut)
dusecut$n_rootstock <- as.numeric(dusecut$n_rootstock)
dusecut$dotsize_scions <- 2 + log(dusecut$n_scions)
dusecut$dotsize_rootstock <- 2 + log(dusecut$n_rootstock)
dusecut$dotsize_rootstock2 <- (dusecut$n_rootstock)


fig <- plot_geo(occurence) %>%
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
in each country", x = 1, y = 0.92)
  ) %>%
  # Dots for which the size is set to the number of rootstocks used in the paper
  add_trace(
    type = "scattergeo",
    lat = ~duse$lat, 
    lon = ~duse$lon,
    text = ~duse$title,
    mode = "markers",
    marker = list(
      size = dusecut$dotsize_rootstock2,
      symbol = "circle",
      color = "transparent",
      line = list(width = 1, color = "black")
    )
  ) 

fig



save_image(fig,
           file="/Users/christophe/Documents/github/asevdiversitydup/figures/mercator_scaled_dots.pdf")
save_image(fig,
           file="/Users/christophe_rouleau-desrochers/Documents/github/asevdiversitydup/figures/mercator_scaled_dots.html")

 ### Install reticulate for export
install.packages('reticulate')
reticulate::install_miniconda()
reticulate::conda_install('r-reticulate', 'python-kaleido')
reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
reticulate::use_miniconda('r-reticulate')
# 
# 
# 
# save_image(fig, "asevdiversitydup/figures/mercator_worldmap3.png", scale=15)

