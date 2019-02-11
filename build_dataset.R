# DATASET BUILDING
library(tidyverse)
library(jsonlite)
library(rgbif)

###
# FUNCTIONS
###

get_lat <- function(node_id){
  return(tb$elements[tb$elements["id"] == node_id, ]$lat)
}

get_lon <- function(node_id){
  return(tb$elements[tb$elements["id"] == node_id, ]$lon)
}

get_height <- function(node_id){
  res <- elevation(latitude = get_lat(node_id), longitude = get_lon(node_id), key = "AIzaSyB8yPUqHwxgOG0-dGbpv2PsSWmcPQn2I0w")
  return(res$elevation)
}

###

# Importazione dati dal file json
tb <- fromJSON("impianti.json")

# Interpretazione dati come una tibble
raw_impianti <- as_tibble(tb$elements$tags)

# Lists of nodes 
first_nodes <- list()
for(node in tb$elements$nodes) {
  if (is.null(node)) {
    first_nodes <- c(first_nodes, 0)
  } else {
    first_nodes <- c(first_nodes, first(node))
  }
}

last_nodes <- list()
for(node in tb$elements$nodes) {
  if (is.null(node)) {
    last_nodes <- c(last_nodes, 0)
  } else {
    last_nodes <- c(last_nodes, last(node))
  }
}

raw_impianti %>%
  mutate(ref = as.numeric(ref)) %>%
  mutate(id = tb$elements$id) %>%
  mutate(first_node = first_nodes, last_node = last_nodes) %>%
  mutate(first_node_lat = lapply(first_node, get_lat), first_node_lon = lapply(first_node, get_lon)) %>%
  mutate(last_node_lat = lapply(last_node, get_lat), last_node_lon = lapply(last_node, get_lon)) %>%
  mutate(base_height = lapply(first_node, get_height), top_height = lapply(last_node, get_height)) %>%
  arrange(ref) -> raw_impianti

#raw_impianti %>%
# mutate(elevation = (top_height - base_height))

impianti <- filter(raw_impianti, aerialway != "goods", aerialway != "magic_carpet", aerialway != "station", aerialway != "rope_tow", aerialway != "platter", aerialway != "yes", name != is.na(name))

impianti %>%
  mutate(ref = row_number()) %>%
  mutate(`aerialway:capacity` = as.double(`aerialway:capacity`)) %>%
  mutate(`aerialway:occupancy` = as.double(`aerialway:occupancy`)) %>%
  mutate(base_height = round(as.double(base_height), 1), top_height = round(as.double(top_height), 1)) %>%
  mutate(elevation = top_height - base_height) %>% 
  select(aerialway, `aerialway:capacity`, `aerialway:occupancy`, name, ref, first_node, last_node, first_node_lat, first_node_lon, last_node_lat, last_node_lon, base_height, top_height, elevation) -> impianti

# MUTAMENTO INTERPRETAZIONE VARIABILI COME VETTORI NORMALI E NON LISTE
impianti %>%
  mutate(first_node = unlist(first_node, use.names = FALSE)) %>%
  mutate(first_node_lat = unlist(first_node_lat, use.names = FALSE)) %>%
  mutate(first_node_lon = unlist(first_node_lon, use.names = FALSE)) %>%
  mutate(last_node = unlist(last_node, use.names = FALSE)) %>%
  mutate(last_node_lat = unlist(last_node_lat, use.names = FALSE)) %>%
  mutate(last_node_lon = unlist(last_node_lon, use.names = FALSE)) -> impianti

# Scrittura dataset impianti
write_csv(impianti, path="dataset_impianti")

# Importazione e scrittura collegamenti impianti [MATRICE DI ADIACENZA]
connessioni_impianti <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQ7xpYTNAz0JUqs2UwEtQ_ONEZDGVZkIGEDilyMRQhbHAsXvpMO8AJH4-ulLCMA3lCbZgzAExg8X865/pub?gid=1915592748&single=true&output=csv")
select(slice(connessioni_impianti,4:n()), num_range("X", 3:141)) -> connessioni_impianti
write_csv(connessioni_impianti, path="connessioni_impianti")
