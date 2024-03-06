## Base packages
library(tidyverse)
library(jsonlite)
library(tmaptools)
library(gtsummary)
library(wesanderson)

## GiS handling
library(osmdata)
library(sf)
library(googleway)
library(secret)
library(ggmap)

## Routing Analysis
library(sfnetworks)
library(igraph)
library(tidygraph)
library(cppRouting)
library(tidygraph)
library(RANN)
# Accessibility
library(accessibility)
library(r5r)
# Network cleaning
library(rgrass)
library(link2GI)
#remotes::install_github('thomasp85/tidygraph')
#https://r-spatial.org/r/2019/09/26/spatial-networks.html
#PCA and Correlation
library('corrr')
library(ggcorrplot)
library(factoextra)


### Enviro Setup
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))

## Placeholders to be able to save and load heavy datasets and variables
#load('Charge_Environment.RData')
#save.image(file='Charge_Environment.RData')
#save(streets, file="streets.RData")

#### Google Places API key
key <- paste(read_file(paste0(dirname(getwd()), "/gm_api_key.txt")), collapse="\n")

########### GiS setup and coordinations #############
### Gothenburg subdivisions
# shape <- read_sf(dsn = "shape_svenska_240104/LAregion_2021_Sweref99TM")
# gothenburg.shape <- shape %>% filter(Namn == "Göteborg")
geo <- st_read(dsn="data/DeSO_2018_v2.gpkg")
regSO_deso_table <- read_csv2("data/kopplingstabell-deso-regso-20210702.csv")
regSO_deso_table <- regSO_deso_table %>% 
  filter(Kommunnamn == "Göteborg") %>% # | 
          # Kommunnamn == "Mölndal"| 
          # Kommunnamn == "Öckerö") %>% 
  rename(deso = DeSO, regso = RegSO) %>%
  select(deso, regso)

gothenburg.subdiv <- geo %>% filter(kommunnamn == "Göteborg") #|
                                    #kommunnamn == "Mölndal" | 
                                    #kommunnamn == "Öckerö")

# Convert Gothenburg subdivision polygons to same crs 4326 as osm data
gothenburg.subdiv <- gothenburg.subdiv %>% st_transform(., 4326)
#view(gothenburg.subdiv)

### OSM data setup
#first option (not recommended in this case)
#town <- 'Gothenburg' 
#location <- town %>% opq()

#second option (recommended)
#coords <- matrix(c(11.6, 12.05, 57.63, 57.80), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c('x','y'),c('min','max'))) 
location <- opq(st_bbox(gothenburg.subdiv))
# Get the boundary for Gothenburg
got.boundary <- opq(bbox = "Gothenburg, Sweden") %>%
  add_osm_feature(key = 'admin_level', value = '7') %>% 
  osmdata_sf %>% unique_osmdata

got.region <- got.boundary$osm_multipolygons %>% filter(osm_id == '935611')

neg_buffer <- st_buffer(got.region, -100) # in meters
got_municipalities_poly <- got.boundary$osm_multipolygons[neg_buffer, ]

## Scaped data from Place to Plug
## Scraped from Placetoplug.com 31-01-2024
## Address: https://placetoplug.com/en/charging-stations/Sweden/Gothenburg/1
## Search term Gothenburg, Sweden
## 380 results, 38 pages.

json <- jsonlite::fromJSON(txt="data/Charging_Points-place-to-plug.json")
p2p.data <- json$chargingZones

p2p.data <- p2p.data %>% select(id, name, address, isFastCharge, powerRange, plugTypes, status, vehicleTypes, -`__typename`)
#str(p2p.data)
p2p.data <- p2p.data %>% mutate(full_address = gsub(" , ", "", paste(ifelse(is.na(address$street), name, address$street), 
                                         address$city, 
                                         address$subdivision)))
getDetailsUrl <- function (x) {
  return(paste0("https://placetoplug.com/en/charging-stations/Sweden/", 
                x$full_address, "/", 
                x$id, "/", 
                str_replace(x$name, " ", "_")))
  
}
p2p.data <- p2p.data %>% mutate(url = getDetailsUrl(.))

ex.chargepoint.demographics <- fromJSON("data/test_charge_details.json")
view(ex.chargepoint.demographics$data$chargingZone$stations %>% as_tibble())
# Set up google api key for ggmap
#register_google(key = key, write=TRUE)
# Fetch long and lat for the charging point
#p2p.long_lat <- geocode(tolower(p2p.data$full_address))
p2p.long_lat <- read_csv("p2p_data_long_lat.csv")
p2p.data <- p2p.data %>% mutate(p2p.long_lat)
# Plug types
#IEC_62196_T2
#IEC_62196_T2_COMBO
#CHADEMO
#OTHER
#DOMESTIC_F
#TESLA_S_EU

# Clean data
p2p.data <- p2p.data %>% mutate(vehicleTypes.car = ifelse(grepl("CAR", p2p.data$vehicleTypes), TRUE, FALSE),
                                vehicleTypes.bike = ifelse(grepl("BICYCLE", p2p.data$vehicleTypes), TRUE, FALSE),
                                vehicleTypes.motorbike = ifelse(grepl("BIKE", p2p.data$vehicleTypes), TRUE, FALSE),
                                status = factor(status),
                                powerRange.min = as.numeric(p2p.data$powerRange$min),
                                powerRange.max = as.numeric(p2p.data$powerRange$max))

p2p.data <- p2p.data %>% na.omit(powerRange.min, powerRange.max) %>% select(id, full_address, lon, lat, status,
                                             vehicleTypes.car, vehicleTypes.bike, vehicleTypes.motorbike, 
                                             isFastCharge, powerRange.min, powerRange.max
                                              )
p2p.data <- p2p.data %>% mutate(lng = lon) %>% filter(powerRange.min > 0)
# Filter out charge points outside of Gothenburg coords
p2p.data <- p2p.data %>% filter(between(lng, coords[1,1], coords[1,2]) &
                                                    between(lat,coords[2,1], coords[2,2]))
# spatial join of subdivisions and charge points
charge_points.p2p.geo <- st_as_sf(charge_points.p2p, coords = c("lng", "lat"), crs = 4326) %>% rownames_to_column(., "charge_id")
charge_points.p2p.geo <- st_filter(charge_points.p2p.geo, got.sub.new)

# Save cleaned and merged data as csv
#write_csv(p2p.long_lat, "p2p_data_long_lat.csv")
#write_csv(p2p.data, "p2p_data_complete.csv")
#p2p.data <- read_csv("p2p_data_complete.csv")

charge_points.p2p <- p2p.data %>% select(lat, lng) 
# Google places API alternative using 'googleway'
# res <- google_places(
#   search_string = NULL,
#   location = c(57.708870, 11.974560),
#   radius = 5000,
#   rankby = NULL,
#   keyword = NULL,
#   language = NULL,
#   name = NULL,
#   place_type = "electric_vehicle_charging_station",
#   price_range = NULL,
#   open_now = NULL,
#   page_token = NULL,
#   simplify = TRUE,
#   curl_proxy = NULL,
#   key = key,
#   radar = NULL
# )

# charge_points <- res$results$geometry$location
# colnames(charge_points)
# colnames(charge_points.p2p)

###### Base graph map ######
main_st <- data.frame(type = c("motorway","trunk","primary","motorway_junction","trunk_link","primary_link","motorway_link"))
st <- data.frame(type = available_tags('highway')$Value)
st <- subset(st, !type %in% main_st$type)
path <- data.frame(type = c("footway","path","steps","service", "cycleway", "track", "corridor", "via_ferrata", "pedestrian", "busway", "bus_guideway", "bridleway", "escape"))
st <- subset(st, !type %in% path$type)
st <- as.character(st$type)
main_st <- as.character(main_st$type)
path <- as.character(path$type)

water <- location %>%
  add_osm_feature(key = "natural", 
                  value = c("water", "lake", "bay", "strait")) %>% osmdata_sf()

main_streets <- location %>%
  add_osm_feature(key = "highway", 
                  value = main_st) %>% osmdata_sf()

streets <- location %>%
  add_osm_feature(key = "highway", 
                  value = st) %>% osmdata_sf()

# parks <- location %>% add_osm_feature(key = "leisure", 
#                                       value = c("park","nature_reserve","recreation_ground","golf_course","pitch","garden")) %>%
#                       osmdata_sf()
#forest <- location %>% add_osm_feature(key = "landuse", value = c("forest")) %>%
#  osmdata_sf()

land <- location %>%
  add_osm_feature(key = "natural", 
                  value = available_tags('natural')$Value) %>% osmdata_sf()


########### ANALYSIS #######

##### CLEAN THE NETWORK #####
# Add data to GRASS spatial database 
# write_VECT(
#   x = streets$osm_lines, 
#   vname = 'gothenburg', 
#   flags = 'overwrite'
# )

# Execute the v.clean tool
# rgrass::execGRASS("g.proj", flags = c("c", "quiet"), proj4 = proj4)
# execGRASS(
#   cmd = 'v.clean', 
#   input = 'muenster_center', 
#   output = 'muenster_cleaned',        
#   tool = 'break', 
#   flags = c('overwrite', 'c')
# )
# 
# # Read back into R
# use_sf()
# muenster_center <- readVECT('muenster_cleaned') %>%
#   rename(geometry = geom) %>%
#   select(-cat)

####---------------------------
##### Preparing Graph of nodes and edges #####
got_streets <- streets$osm_lines
# Give each edge a unique index
edges <- got_streets %>% 
  select(highway) %>%
  mutate(edgeID = c(1:n()))

# Create nodes at the start and end point of each edge
nodes <- edges %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(edgeID = L1) %>%
  group_by(edgeID) %>%
  slice(c(1, n())) %>%
  ungroup() %>%
  mutate(start_end = rep(c('start', 'end'), times = n()/2))

# Give each node a unique index
nodes <- nodes %>%
  mutate(xy = paste(.$X, .$Y)) %>% 
  mutate(nodeID = group_indices(., factor(xy, levels = unique(xy)))) %>%
  select(-xy)

# Combine the node indices with the edges
source_nodes <- nodes %>%
  filter(start_end == 'start') %>%
  pull(nodeID)

target_nodes <- nodes %>%
  filter(start_end == 'end') %>%
  pull(nodeID)

# Merge source nodes and target nodes
edges <- edges %>%
  mutate(from = source_nodes, to = target_nodes)

# Remove duplicate nodes
nodes <- nodes %>%
  distinct(nodeID, .keep_all = TRUE) %>%
  select(-c(edgeID, start_end)) %>%
  st_as_sf(coords = c('X', 'Y')) %>%
  st_set_crs(st_crs(edges))

# Create a graph from the adjacency matrix
graph <- tbl_graph(nodes = nodes, edges = as_tibble(edges), directed = FALSE)

# The activate() verb specifies if we want to manipulate the edges or the nodes. 
graph <- graph %>%
  activate(edges) %>%
  mutate(length = st_length(geometry))

ggplot() +
  geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), linewidth = 0.2, color="#1e00be") + 
  geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), size=0.1, color="#1e00be") +
  geom_sf(data=gothenburg.subdiv, alpha=0.6, linewidth=0.5, colour="#8778D7", fill="#A753AF") +
  coord_sf(lims_method = "geometry_bbox",
           xlim = c(11.71, 12.16), 
           ylim = c(57.58, 57.86), expand = TRUE, clip = "on") +
  geom_sf(data = charge_points.p2p.geo, size=0.9, color="#90C59B") +
  geom_sf(data = charge_points.p2p.geo.snaps, size=0.9, color="red") +
  coord_sf(#lims_method = "geometry_bbox",
    xlim = c(11.71, 12.16), 
    ylim = c(57.58, 57.86), expand = TRUE, clip = "on") +
  labs(x="", y="") +
  theme_minimal() + theme(legend.position = "")

### Convert nodes to coordinates matrix
nodes.df <- graph %>%
  activate(nodes) %>%
  as_tibble()

nodes.sf <- nodes.df %>%st_as_sf()

nodes.coords <- nodes.sf %>%
  st_coordinates()

### Convert charging points to coordinates matrix
p2p.long_lat.matrix <- as.matrix(st_coordinates(charge_points.p2p.geo)) %>% na.omit()
p2p.long_lat.knn %>% as_tibble() %>% st_as_sf()

##### TESTING NETWORK MEASURES #####
## The function distances, for example, returns a numeric matrix containing the 
# distances of the shortest paths between every possible combination of nodes. 
# It will automatically choose a suitable algorithm to calculate these shortest paths.

# test.distances <- distances(
#   graph = graph,
#   weights = graph %>% activate(edges) %>% pull(length)
# )

#### FINDING NEAREST POINT #####

### As the crow flies method
### RANN nn2
### Find the K nearest neighbour charging point to each node (KNN)
knn.5 <- nn2(nodes.coords, 
    query = p2p.long_lat.matrix,
    k = 5,
    treetype = "kd",
    eps = 0)

knn.5.nodes <- nn2(p2p.long_lat.matrix, 
             query = nodes.coords, 
             k = 5,
             treetype = "bd",
             eps = 0)

knn.radius <- nn2(p2p.long_lat.matrix, 
             query = nodes.coords, 
             k = 5,
             treetype = "kd", 
             searchtype = "radius", 
             radius = 0.5,
             eps = 0)

### Function for selecting the node with the closest distance from KNN
getMinKnn <- function(i, ids, distances) {
  min_dist_i <- which.min(distances[i,])
  return(data.frame(ids[i,min_dist_i], distances[i,min_dist_i]))
}

closest <- data.frame()
for (i in 1:nrow(knn.radius[[2]])) {
  new_elements <- getMinKnn(i, knn.radius$nn.idx, knn.radius$nn.dists)
  closest <- rbind(closest, new_elements)
}

colnames(closest) <- c("charge_id", "distance")
nodes.coords.knn <- cbind(nodes.coords, closest) %>% 
  mutate(Lon = X, Lat = Y) %>% select(-X, -Y)

####--- Finding the shortest path to the nodes and points (Dijkstra's) (graph search)
#### !!!! WIP
# from_node <- graph %>%
#   activate(nodes) %>%
#   filter(nodeID == 34) %>%
#   pull(nodeID)
# 
# to_node <- graph %>%
#   activate(nodes) %>%
#   filter(nodeID == 3) %>%
#   pull(nodeID)
# 
# path <- shortest_paths(
#   graph = graph,
#   from = from_node,
#   to = to_node,
#   output = 'both',
#   weights = graph %>% activate(edges) %>% pull(length)
# )

### Snap charging points to nodes
# p2p.long_lat.points <- st_geometrycollection(p2p.long_lat.matrix) %>% as_tibble() %>% st_as_sf()
# class(nodes.df)
# st_snap(nodes.df, charge_points.p2p.geo)

snap_matrix <- closest %>%
  rownames_to_column(., var = "node_id") %>%
  arrange(distance) %>% 
  distinct(charge_id, .keep_all = TRUE) %>%
  arrange(charge_id) %>%
  select(node_id, charge_id)

# Based on the closest network node to a charge point, snap point
getSnapPoint <- function(node) {
  i <- match(node, snap_matrix[,2])
  snap_node <- as.integer(snap_matrix[i,]$node_id)
  snap_point <- nodes.df[snap_node,]$geometry
  return(snap_point)
}
charge_points.p2p.geo.snaps <- charge_points.p2p.geo %>% 
  mutate(geometry = mapply(getSnapPoint, charge_id)) %>%
  select(charge_id, geometry) %>%
  as_tibble() %>%
  st_as_sf(crs = 4326)

## A snap distance tolerance is used to control where snapping is performed. 
## Snapping one geometry to another can improve robustness for overlay operations by 
## eliminating nearly-coincident edges (which cause problems during noding and intersection 
## calculation). Too much snapping can result in invalid topology being created, 
##so the number and location of snapped vertices is decided using heuristics to determine 
##when it is safe to snap. This can result in some potential snaps being omitted, however."

####---- Accessibility measures ---####
distance_matrix <- graph %>% activate(nodes) %>% 
  as_data_frame() %>% 
  mutate(from_id = from, to_id = to, length = as.numeric(length)) %>% 
  select(from_id, to_id, length)

count_occurance_cp <- closest %>% count(charge_id) %>% 
  rownames_to_column(., var = "node_id") %>%
  select(node_id, charge_id, n)

charge_land_use <- closest %>% 
  left_join(.,count_occurance_cp, by="charge_id", keep = TRUE) %>%
  mutate(node_id = rownames(closest), charge_points = n, id=charge_id.x) %>% 
  select(id, charge_points)
head(distance_matrix)

# Potential function (distance decay)
negative_exp <- gravity(
  distance_matrix,
  charge_land_use,
  opportunity = "charge_points",
  travel_cost = "length",
  decay_function = decay_exponential(decay_value = 0.2)
)

#####

#####--- Spatial join for GBG DeSO polygons 

# Function for creating a level set for distance, to be used for discrete cases
getDistanceLevel <- function(x) {
  if (x > 30) {
    return("30+")
  } else if(x >= 5) {
    if (x < 10) {
      return("5-10")
    } else if (x >= 10 & x < 15) {
      return("10-15")
    } else if (x >= 15 & x < 20) {
      return("15-20")
    } else if  (x >= 20 & x < 25) {
      return("20-25") 
    } else {
      return("25-30")
    }
  } else {
    return("< 5")
  }
}

nodes.averages <- graph %>%
  activate(nodes) %>%
  as_tibble() %>% 
  mutate(knn_chargeId = closest$charge_id, 
         knn_distance = closest$distance) %>%
  st_as_sf()

# Convert Gothenburg subdivision polygons to same crs 4326 as osm data
got.sub.new <- gothenburg.subdiv %>% mutate(area = st_area(.)) %>%
  st_as_sf(., coords = c("x","y"), crs = 4326)

## Filter nodes by subdivision (Spatial join)
filtered.nodes <- st_filter(nodes.averages, gothenburg.subdiv)

joined.nodes <- st_join(filtered.nodes, got.sub.new, join = st_within)
joined.nodes <- joined.nodes %>% 
  mutate(knn_distance = knn_distance * 1000) %>% 
  mutate(distance_level = mapply(getDistanceLevel, knn_distance))

avg_dist_deso <- joined.nodes %>% as_tibble() %>% 
  group_by(deso) %>% 
  summarise(n = n(),
            avg_dist = mean(knn_distance)) %>%
  arrange((avg_dist)) %>%
  drop_na()

avg_dist_deso <- merge(avg_dist_deso, regSO_deso_table) %>% 
  arrange(desc(avg_dist))

first_geo_by_deso <- joined.nodes %>% as_tibble() %>% 
  group_by(deso) %>%
  filter(row_number()==1) %>%
  rename("deso.ref" = deso) %>%
  select(deso.ref, geometry)

avg_dist_deso_latlong <- merge(x = avg_dist_deso, 
                               y = first_geo_by_deso, 
                               by.x ="deso",
                               by.y = "deso.ref") %>%
  as_tibble()

joined.nodes %>% count(deso)

got.sub.new <- got.sub.new %>% as_tibble() %>%
  #filter(deso %in% avg_dist_deso$deso & !is.na(deso)) %>%
  st_as_sf()

#Merge new values to the polygons sf object
got.sub.new <- merge(got.sub.new, avg_dist_deso_latlong, by="deso", all=FALSE)

# Create factor levels for distances 
# got.sub.new <- got.sub.new %>% mutate(distance_level = factor(mapply(getDistanceLevel, avg_dist)))

# Filter out outliers
#got.sub.new <- got.sub.new %>% filter(avg_dist > 50)

### Density
got.sub.new %>% as_tibble() %>% 
  ggplot() + geom_density(aes(avg_dist)) + 
  labs(title="Breakdown distance to public charging points",
       subtitle = "Density",
       fill = "Distance",
       x="Average distance",
       y="") +
  theme_light()

ggplot(got.sub.new, aes(x=as.factor(regso), y=avg_dist, size=n)) +
  geom_point(alpha=0.7)


##### SUBDIVISION DEMOGRAPHICS
got.sub.new <- got.sub.new %>% arrange(regso)
regso.list <- avg_dist_deso %>% select(deso, regso)
# Read csvs for deso data per demographic
deso.income <- read_csv("data/income_demographics_deso.csv")
deso.foreignborn <- read_csv("data/foreignborn_demographics_deso.csv")
deso.education <- read_csv("data/education_demographics_deso.csv")
deso.housing <- read_csv("data/housing_demographics_deso.csv")
deso.cars <- read_csv("data/cars_demographics_deso.csv")
deso.age <- read_csv("data/age_demographics_deso.csv")
deso.households <- read_csv("data/households_demographics_deso.csv")

# Cleaning
deso.foreignborn <- deso.foreignborn %>% 
  mutate(swedish = swedish / total, foreign = foreign / total) %>% 
  select(-total) %>%
  filter(deso %in% regso.list$deso)
deso.income <- deso.income %>% 
  select(deso, `Median income tkr`) %>%
  filter(deso %in% regso.list$deso)
deso.education <- deso.education %>% 
  filter(deso %in% regso.list$deso) %>%
  mutate(total = rowSums(.[, -1]),
         middle_school = middle_school / total,
         gymnasium = gymnasium / total,
         post_gymnasium_less_than_3 = post_gymnasium_less_than_3 / total,
         post_gymnasium_more_than_3 = post_gymnasium_more_than_3 / total,
         education_missing = education_missing / total) %>%
  select(-total)
deso.housing <- deso.housing %>% 
  filter(deso %in% regso.list$deso) %>%
  select(deso, hyresrätt, bostadsrätt, äganderätt) %>%
  filter(deso %in% regso.list$deso) %>%
  mutate(total = rowSums(.[, -1]),
         hyresrätt = hyresrätt / total,
         bostadsrätt = bostadsrätt / total,
         äganderätt = äganderätt / total) %>%
  select(-total)
deso.cars <- deso.cars %>%
  filter(deso %in% regso.list$deso) %>% 
  select(deso, bilar_i_trafik)

deso.age <- deso.age %>% 
  filter(deso %in% regso.list$deso) %>%
  # Create percentage of ages
  mutate(across(colnames(deso.age[,3:19]), function(x) x/total))

deso.pop <- deso.age %>% 
  filter(deso %in% regso.list$deso) %>% 
  mutate(pop = total, ages = list(data_frame(across(colnames(deso.age[,3:19]))))) %>% 
  select(deso, pop, ages)

deso.households <- deso.households %>%
  filter(deso %in% regso.list$deso) %>% 
  select(deso, total_households) 
deso_aream2 <- got.sub.new %>% as_tibble() %>% 
  mutate(area_m2 = as.double(area)) %>% 
  select(deso, area_m2)

# Combine demographic data with distances by DeSO subdivision
deso.demographics <- deso_aream2 %>%
  merge(., deso.income) %>%
  merge(., avg_dist_deso, by="deso") %>%
  merge(., deso.pop, by="deso") %>%
  merge(., deso.households, by="deso") %>%
  merge(., deso.foreignborn, by="deso") %>%
  merge(., deso.education, by="deso") %>%
  merge(., deso.housing, by="deso") %>%
  merge(., deso.cars, by="deso") %>%
  # Calculate average # cars per household
  mutate(cars_per_household = bilar_i_trafik / total_households) %>%
  select(deso, regso, n, avg_dist, everything()) %>%
  arrange(desc(avg_dist))
view(deso.demographics)

deso.demographics.WOMajorityHouses <- deso.demographics %>%
  # Filter out desos with majority houses
  filter(äganderätt < 0.5)

deso.demographics.rentals <- deso.demographics %>%
  # Filter out desos with majority houses
  filter(hyresrätt > 0.50)


model1 <- lm(avg_dist~foreign+n+äganderätt+total_households, 
             data = deso.demographics)
summary(model1)
model2 <- lm(avg_dist~foreign+n+äganderätt+`Median income tkr`, 
             data = deso.demographics)
summary(model2)
model3 <- lm(avg_dist~foreign+n+äganderätt+post_gymnasium_more_than_3, 
             data = deso.demographics)
summary(model3)


model1.rentals <- lm(avg_dist~foreign+n+cars_per_household, 
             data = deso.demographics.rentals)
summary(model1.rentals)
model2.rentals <- lm(avg_dist~foreign+n+`Median income tkr`, 
             data = deso.demographics.rentals)
summary(model2.rentals)
model3.rentals <- lm(avg_dist~foreign+n+äganderätt+post_gymnasium_more_than_3, 
             data = deso.demographics.rentals)
summary(model3.rentals)

#scale(deso.demographics$avg_dist)
ggplot(data = deso.demographics, aes(x=avg_dist, y=bilar_i_trafik)) + 
  geom_point(aes(color=ifelse(foreign > 0.49, "foreign majority", "swedish majority"))) +
  labs(color="Swedish vs foreign")
  #scale_x_continuous(limits = c(0, 40))
# demo.income <- demo.income %>% 
#   mutate(deso = gsub("[()]", "", ...1)) %>% 
#   mutate(deso = gsub("Göteborg ", "", deso)) %>%
#   mutate(deso = gsub("Mölndal ", "", deso)) %>%
#   select(deso, everything(), -`...1`) %>%
#   arrange(deso)

#### PCA and Correlations matrix
# https://www.datacamp.com/tutorial/pca-analysis-r
deso.demographics.pca <- deso.demographics %>% select(-deso, -regso, -n)
deso.demographics.normalized <- scale(deso.demographics.pca)
corr_matrix <- cor(deso.demographics.normalized)

# Plot correlation matrix
ggcorrplot(corr_matrix, title = "Correlation matrix, GOT DeSO demographic data")

# Perform PCA and run analysis on variables and relations
data.pca <- princomp(corr_matrix)
summary(data.pca)
data.pca$loadings[, 1:2]
fviz_eig(data.pca, addlabels = TRUE)
fviz_pca_var(data.pca, col.var = "black")
fviz_cos2(data.pca, choice = "var", axes = 1:2)
fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

