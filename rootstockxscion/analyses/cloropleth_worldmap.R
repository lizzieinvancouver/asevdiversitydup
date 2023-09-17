##*=============================================================================
##  Chloropleth map
##  
##
## Project:         Asevdiversitydup
## Date:            16.06.2023
## Author:          Christophe
## Source from:     https://plotly.com/r/choropleth-maps/
##*=============================================================================

# housekeeping
rm(list=ls())  
options(stringsAsFactors=FALSE)

#*------------------------------------------------------------------------------

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
d[d == ''] <- 'NA'
d[d=='na'] <- 'NA'

# Subset for papers that will be used
duse <- subset(d, use=="yes")

# Delete rows where multiple locations for the same paper to avoid redundancy
duse_no_dupplicates <- duse[!duplicated(duse$title),]

# Sum of papers with no number of rootstock that they used
sum(is.na(duse_no_dupplicates$n_rootstock))

# Sum of papers used for the map
sum(length(duse_no_dupplicates$id))
# Min value of N.rootstock
min(as.numeric(duse_no_dupplicates$n_rootstock), na.rm = TRUE)
# Max value of N.rootstock
max(as.numeric(duse_no_dupplicates$n_rootstock), na.rm = TRUE)
# Mean value of N.rootstock
mean(as.numeric(duse_no_dupplicates$n_rootstock), na.rm = TRUE)

# Min value of N.scion
min(as.numeric(duse_no_dupplicates$n_scions), na.rm = TRUE)
# Max value of N.scion
max(as.numeric(duse_no_dupplicates$n_scions), na.rm = TRUE)
# Mean value of N.scion
mean(as.numeric(duse_no_dupplicates$n_scions), na.rm = TRUE)

# Jitter location for later
duse_no_dupplicats <- duse_no_dupplicats %>%
  mutate(lat_jittered = jitter(lat, factor = 10),
         lon_jittered = jitter(lon, factor = 10))

# Create a new table grouped by country and the number of papers published in each
occurence <- duse_no_dupplicats %>%
  group_by(COUNTRY) %>%
  summarize(code = first(CODE), count = n())

# === === === === === === === === === === === ===

#### WORLD MAP ####

# Calculate the dot size based on `duse$nb_scion`
duse_no_dupplicats$n_rootstock <- as.numeric(duse_no_dupplicats$n_rootstock)
duse_no_dupplicats$dotsize_scions <- 2 + log(duse_no_dupplicats$n_scions)

duse_no_dupplicats$dotsize_rootstock <-  as.numeric(0.75*(duse_no_dupplicats$n_rootstock), na.rm=FALSE)
duse_no_dupplicats$dotsize_rootstock_fixed <- as.numeric(0.5*(duse_no_dupplicats$n_rootstock), na.rm=FALSE)
# Replace NA by zero in order to display a dot, even when there is no mention 
duse_no_dupplicats["dotsize_rootstock"][is.na(duse_no_dupplicats["dotsize_rootstock"])] <- 0.1
duse_no_dupplicats$dotsize_rootstock_nolog <-  duse_no_dupplicats$n_rootstock


#### NOt JITTERED ####
# Add figure
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
    )) %>%
    # Edit the colar bar position --> make the X value negative if I want to set it on the left
    colorbar = list(title = "Number of papers published
in each country", x = -0.1, y = 0.92) %>%
# Dots for which the size is set to the number of rootstocks used in the paper
  add_trace(
    type = "scattergeo",
    lat = ~duse_no_dupplicats$lat, 
    lon = ~duse_no_dupplicats$lon,
    text = ~paste("Title: ", duse_no_dupplicats$title, "<br>Number of Rootstocks: ", duse_no_dupplicats$n_rootstock),
    mode = "markers",
    marker = list(
      size = duse_no_dupplicats$dotsize_rootstock,
      symbol = "circle",
      color = "lightgrey",
      line = list(width = 1.5, color = "black")
    )
  ) %>%
  layout(title = "No jittered locations") %>%
  layout(
  showlegend = TRUE,

)
fig

#### JITTERED LOCATIONS ####
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
    lat = ~duse_no_dupplicats$lat_jittered, 
    lon = ~duse_no_dupplicats$lon_jittered,
    text = ~paste("Title: ", duse_no_dupplicats$title, "<br>Number of Rootstocks: ", duse_no_dupplicats$n_rootstock),
    mode = "markers",
    marker = list(
      size = duse_no_dupplicats$dotsize_rootstock,
      symbol = "circle",
      color = "lightgrey",
      line = list(width = 1.5, color = "black")
    )
  ) %>% 
  layout(title = "")

fig_jittered


#### With 2 legends --> Static version ####
plt <- plot_geo(occurence) %>%
  layout(margin = list(t = 100, b = 40, r = 100, l = 40),
         geo = list(
           showframe = TRUE, showcoastlines = TRUE, showland = TRUE,
           landcolor = toRGB("white"),
           countrycolor = toRGB("darkgrey"),
           coastlinecolor = toRGB("black"),
           coastlinewidth = 0.5,
           lataxis = list(range = c(-55, 80), showgrid = FALSE),
           lonaxis = list(range = c(-130, 160), showgrid = FALSE)
         )
  ) %>%
  add_trace(
    z = ~count, color = ~count, colors = 'GnBu',
    text = ~COUNTRY, locations = ~code,
    marker = list(
      line = list(width = 0.75, color = "black")
    ),
    colorbar = list(title = "Number of papers<br />published in<br />each country",
                    x = 1, y = 1.13, len = 1.08) # longer to give space for title
  ) %>%
  add_trace(showlegend = F,            # make sure to specify!
            type = "scattergeo",
            lat = ~duse_no_dupplicats$lat_jittered, 
            lon = ~duse_no_dupplicats$lon_jittered,
            text = ~paste("Title: ", duse_no_dupplicats$title, "<br>Number of Rootstocks: ", duse_no_dupplicats$n_rootstock),
            mode = "markers",
            marker = list(
              size = duse_no_dupplicats$dotsize_rootstock_fixed,
              symbol = "circle",
              color = "lightgrey",
              line = list(width = 0.5, color = "black")
            )
  )
rg <- range(duse_no_dupplicats$dotsize_rootstock_nolog, na.rm = T) # range of sizes 
ls <- round(seq(ceiling(rg[1]), ceiling(rg[2]), length.out = 4, ))    # make an even seq

# create markers by range of sizes in root stock, showlegend = F; colorbar = NO
plt2<-plot_ly(type = "scatter", mode = "markers", x = 3, y = ls,
              showlegend = F,
              marker = list(sizeref = 0.1, sizemode = "area", size = ls, 
                            color = "white",  
                            line = list(color = "black", width = 0.5))) %>% 
  layout(xaxis = list(showgrid = F, showline = F, zeroline = F, showticklabels = F), 
         yaxis = list(showgrid = F, zeroline = F, tickvals = ls,
                      ticktext = ls, tickmode = "array")) %>% hide_colorbar()

plt_final <- subplot(plt2, plt, widths = c(.2, .8)) %>% 
  layout(annotations = list(     # add title to first plot (one that looks like legend)
    list(xref = "x1", yref = "y1", x = 3, y = 35, showarrow = FALSE,
         text = "<br />Number of Root Stock", font = list(size = 14))
  ))
save_image(plt_final,
           file="/Users/christophe_rouleau-desrochers/Documents/github/asevdiversitydup/figures/static_version2.svg")




#### ARCHIVE FIGURES ####
# 
##### orthographic LOCATIONS #####
# # Add figure
# orthographic <- plot_geo(occurence) %>%
#   layout(
#     geo = list(
#       showframe = TRUE,
#       projection = list(type = 'orthographic'),
#       showcoastlines = TRUE,
#       showland = TRUE,
#       landcolor = toRGB("white"),
#       countrycolor = toRGB("darkgrey"),
#       coastlinecolor = toRGB("black"),
#       coastlinewidth = 0.5
#       # lataxis = list(
#       #   range = c(-55, 80),
#       #   showgrid = FALSE
#       # ),
#       # lonaxis = list(
#       #   range = c(-130, 160),
#       #   showgrid = FALSE
#       # )
#     )
#   ) %>%
#   # Color gradient set to the number of papers in each country
#   add_trace(
#     z = ~count, color = ~count, colors = 'GnBu',
#     text = ~COUNTRY, locations = ~code,
#     marker = list(
#       line = list(width = 0.5, color = "black")
#     ),
#     # Edit the colar bar position --> make the X value negative if I want to set it on the left
#     colorbar = list(title = "Number of
# papers published
# in each country", x = 1, y = 0.92)
#   ) %>%
#   # Dots for which the size is set to the number of rootstocks used in the paper
#   add_trace(
#     type = "scattergeo",
#     lat = ~duse_no_dupplicats$lat_jittered, 
#     lon = ~duse_no_dupplicats$lon_jittered,
#     text = ~paste("Title: ", duse_no_dupplicats$title, "<br>Number of Rootstocks: ", duse_no_dupplicats$n_rootstock),
#     mode = "markers",
#     marker = list(
#       size = duse_no_dupplicats$dotsize_rootstock,
#       symbol = "circle",
#       color = "lightgrey",
#       line = list(width = 1.5, color = "black")
#     )
#   ) %>% 
#   layout(title = "Orthographic")


# 
##### JITTERED for static version #####

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
in each country", x = 1, y = 0.88)
  ) %>%
  # Dots for which the size is set to the number of rootstocks used in the paper
  add_trace(
    type = "scattergeo",
    lat = ~duse_no_dupplicats$lat_jittered,
    lon = ~duse_no_dupplicats$lon_jittered,
    text = ~paste("Title: ", duse_no_dupplicats$title, "<br>Number of Rootstocks: ", duse_no_dupplicats$n_rootstock),
    mode = "markers",
    marker = list(
      size = 2,
      symbol = "circle",
      color = "lightgrey",
      line = list(width = 1.5, color = "black")
    )
  ) %>%
  layout(title = "")

fig_jittered

save_image(fig_jittered,
           file="/Users/christophe_rouleau-desrochers/Documents/github/asevdiversitydup/figures/mercator_scaled_dots.pdf")
#
# 
#  ### Install reticulate for export
# library(reticulate)
# install.packages('reticulate')
# reticulate::install_miniconda()
# reticulate::conda_install('r-reticulate', 'python-kaleido')
# reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
# reticulate::use_miniconda('r-reticulate')
# 
# 
# 
# save_image(fig, "asevdiversitydup/figures/mercator_worldmap3.png", scale=15)




