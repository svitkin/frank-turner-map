# Setup ----------------------------------------------------------------------
rm(list=ls())
library(rvest)
library(ggmap)
library(tidyr)
library(stringr)
library(dplyr)
library(purrr)
library(assertr)

api_key <- readLines("~/googlemapskey/key.txt")
register_google(api_key)

# Helper functions -----------------------------------------------------------
add_venues <- function(lat_lons, venues) {
  indeces <- (length(lat_lons) + 1):(length(lat_lons) + length(venues) + 1)
  for (i in indeces) {
    lat_lons[[i]] <- geocode(venues[i])
    names(lat_lons)[indeces] <- venues
  }
}

extract_date <- function(date_string) {
  date_string_nonum <-
    date_string %>% 
    str_replace_all("(\\d+)[a-z]+ ", "\\1 ")
  
  month_int <-
    which(month.name == str_extract(date_string_nonum, "[A-Za-z]+")) %>% 
    str_pad(2, "left", "0")
  
  
  paste(str_extract(date_string_nonum, "\\d{4}$"),
        month_int,
        str_replace(str_extract(date_string_nonum, "^\\d+ "), " ", ""),
        sep = "-") %>% 
    as.Date()
}


manually_add_lonlat <- function(df, venue_name, new_lon, new_lat, detect = FALSE) {
  if (!detect) {
    df %>% 
      mutate(lon = ifelse(venue == venue_name,
                          new_lon, lon),
             lat = ifelse(venue == venue_name, 
                          new_lat, lat))
  } else {
    df %>% 
      mutate(lon = ifelse(str_detect(venue, venue_name),
                          new_lon, lon),
             lat = ifelse(str_detect(venue, venue_name),
                          new_lat, lat))
  }
  
}

# Scrape venues from show archive ---------------------------------------------
df_scrape <- 
  read_html("https://frank-turner.com/basic-archive/") %>% 
  html_nodes(".tableHolder li") %>% 
  html_text() %>% 
  data.frame(shows = .,
             stringsAsFactors = FALSE)

df_scrape_clean <-
  df_scrape %>% 
  separate(shows, into = c("date", "venue"), sep = "@") %>% 
  mutate(date = str_trim(date),
         venue = str_trim(venue)) %>% 
  filter(!is.na(venue))

venues <-
  df_scrape_clean %>% 
  pull(venue)


# Geocode lat-lons ------------------------------------------------------------
# If lat-lons not retrieved, then geocode the entire set of venues
if (!file.exists("data-raw/fturner_latlons.rda")) {
  lat_lons <- list()
  add_venues(lat_lons, venues)
  save(lat_lons, file = "~/fturner_latlons.rda")
  
# Otherwise load the last retrieved list of lat-lons
# Geocode any new shows
} else {
  load("~/fturner_latlons.rda")
  # Difference between latest scrape of venues and last saved set of venues
  if (length(setdiff(venues, names(lat_lons))) > 0) {
    venues <- setdiff(venues, names(lat_lons))
    add_venues(lat_lons, venues)
    save(lat_lons, file = "~/fturner_latlons.rda")
  }
}


# Save all data in df and export as csv ---------------------------------------
latlon_df <-
  # Combine list of lat_lons into data frame
  map_df(1:length(lat_lons), function(i) {
    data.frame(venue = df_scrape_clean[i, "venue"],
               date = extract_date(df_scrape_clean[i, "date"]),
               date_string = df_scrape_clean[i, "date"],
               lat = lat_lons[[i]]$lat,
               lon = lat_lons[[i]]$lon,
               stringsAsFactors = FALSE)
  }) %>% 
  # Fill in missing venues manually
  # geocode("Panissières, Rhône-Alpes, France")
  manually_add_lonlat("Yakha Vivre Libre Festival, Pannisierres, France", 4.34, 45.8) %>%
  # geocode("111-113 Camden High St, Camden Town, London, UK")
  manually_add_lonlat("The Camden Crawl, Oh! Bar, Camden, London, UK", -0.141, 51.5) %>% 
  # geocode("Newton Stacey, Winchester, UK")
  manually_add_lonlat("Newton Stacey Festival, Winchester, UK", -1.42, 51.2) %>% 
  # geocode("Pilton, Shepton Mallet BA4 4EE, United Kingdom")
  manually_add_lonlat("Glastonbury", -2.59, 51.2, TRUE) %>% 
  # geocode("El Paso, TX, USA")
  manually_add_lonlat("Take 2, El Paso, TX, USA", -106, 31.8) %>%   
  # geocode("Birmingham, UK")
  manually_add_lonlat("Kerrang Radio, Birmingham, UK", -1.89, 52.5) %>% 
  # geocode("Grossfehen, Germany")
  manually_add_lonlat("Oma’s Teich Festival, Grossfehen, Germany", 7.61, 53.4) %>% 
  # geocode("Douglas, Isle of Man")
  manually_add_lonlat("Mannifest, Isle Of Man", -4.49, 54.2) %>% 
  # geocode("LeBreton Flats, Ottawa, Ontario")
  manually_add_lonlat("Ottawa Blues Festival, Canada", -75.7, 45.4) %>% 
  # geocode("Oude Baan 96, 2450 Meerhout, Belgium")
  manually_add_lonlat("Groezrock Festival", 5.11, 51.1, TRUE) %>%
  # geocode("Kostrzyn nad Odrą, Poland")
  manually_add_lonlat("Woodstock Festival, Poland", 14.6, 52.6) %>% 
  
  # Get rid of international waters venues, can't really geocode too easily
  filter(!str_detect(venue, "International Waters")) %>% 
  # Make sure that all venues have valid longitudes
  verify(nrow(filter(., is.na(lon))) == 0)

write.csv(latlon_df, "data/fturner-locations.csv")

