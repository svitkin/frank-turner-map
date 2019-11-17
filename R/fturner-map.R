rm(list = ls())
library(dplyr)
library(stringr)
library(leaflet)
library(htmltools)
library(htmlwidgets)

fturner_locations <- 
  read.csv("data/fturner-locations.csv",
           stringsAsFactors = FALSE)

# Necessary because of bug in saveWidget with relative paths
# https://stackoverflow.com/questions/41399795/savewidget-from-htmlwidget-in-r-cannot-save-html-file-in-another-folder?noredirect=1&lq=1
save_path <- "output/fturner-map.html"
widget_safe_path <- file.path(normalizePath(dirname(save_path)), 
                              basename(save_path))

leaflet(fturner_locations) %>% 
  addTiles() %>% 
  addMarkers(~lon, ~lat, 
             popup = ~paste0(venue, "<br>", date_string), 
             clusterOptions = markerClusterOptions()) %>% 
  saveWidget(file = widget_safe_path)