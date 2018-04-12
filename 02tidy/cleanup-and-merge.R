
# input: csvs with raw data
# output: a csv that can be read into igraph.

suppressPackageStartupMessages({
  library(tidyverse)
  library(magrittr)
  library(stringdist)
})

## Read raw data ----
# I'd use read_csv but the parsing messages annoy me too much
distmat <- read.csv("01raw/20180412_distance-matrix.csv", stringsAsFactors = FALSE)
stations <- read.csv("01raw/20180412_stations.csv", stringsAsFactors = FALSE)
series <- read.csv("01raw/20180412_treinseries.csv", stringsAsFactors = FALSE)
series_meta <- read.csv("01raw/20180412_treinseries-overview.csv", stringsAsFactors = FALSE)

## some station name cleanup -> station_names, series ----
# remove bracketed remarks like '(sluit aan op...)'
series$station <- str_trim(str_replace(series$station," +\\(.*",""))

stations$naam <- str_replace(stations$naam,"a/d","aan den")
stations$naam <- str_replace(stations$naam," v "," van ")
stations$naam <- str_trim(stations$naam)


unique(series$station)
# Some stations in the 2018 train series are not in the 2017 station table.
# We remove those (new, sometimes foreign) stations from the series. This
# means these nodes are 'skipped' in the final graph, reflecting the 2017 
# situation.
j <- ain(series$station, stations$naam,method='osa',maxDist=2)
series <- series[j,]

station_names <- stations %>% select(code, naam)
# remove suffix '(NS-bus)'
station_names$naam <- str_replace(station_names$naam," +\\(.*","")
## we create our own unique code for these unlisted stations
# newst <- unique(series$station[j])
# new_stations <- data.frame(
#   code = str_to_lower(abbreviate(newst))
#   , naam=newst
#   , stringsAsFactors = FALSE)
# 
# station_names <- bind_rows(station_names, new_stations) %>%
#   arrange(naam)

## Create connection table ----
connections <- series %>% 
  group_by(Serie) %>% 
  transmute(
     Start=lag(station)
    , End=station
    ) %>% ungroup() %>%
  filter(complete.cases(.))

edges <- connections %>% left_join(series_meta,by="Serie")
edges %<>% select(Start, End, Treinsoort)

edges$StartMatch <- station_names[amatch(edges$Start, station_names$naam,maxDist = 2),"naam"]
edges$EndMatch <- station_names[amatch(edges$End, station_names$naam,maxDist = 2),"naam"]
edges$stringdist <- stringdist(edges$Start,edges$StartMatch)

# inspection : maxDist equals 1 for actual matches.
# View(edges)
edges %<>% select(Start=StartMatch, End=EndMatch,Treinsoort) %>%
  filter(complete.cases(.))



# Merge the tarif distances ----
# distance to long form, conversion of type
distance <-  distmat %>% 
  gather(EndCode, Distance, -Station) %>% 
  rename(StartCode=Station) %>%
  mutate(Distance = as.numeric(Distance)) %>%
  filter(complete.cases(.))

# merge full station names
dd <- left_join(distance, station_names, by=c("StartCode"="code")) %>%
  rename(Start=naam)

dd <- left_join(dd, station_names,by=c("EndCode"="code")) %>%
  rename(End=naam)

dd <- dd[complete.cases(dd),]

out <- left_join(edges,dd)

# deduplicate, both A->B and A->B vs B->A
m <- as.matrix(out[1:2])
n <- as.data.frame(t(apply(m,1,sort)))
out[1:2] <- n
out <- out[!duplicated(out[1:2]),]


## prepare output and write ----
out %<>% 
  select(start = StartCode, end = EndCode, distance = Distance, type=Treinsoort) %>%
  distinct(Start, End, .keep_all = TRUE)


# gotta know when we ran it..
file_prefix <- format(Sys.time(),format="%Y%m%d_")
write.csv(out, file.path("02tidy",paste0(file_prefix,"railway-edges.csv")),row.names = FALSE)

# 
nodes <- station_names %>%
  rename(name=naam)
node_data <- stations %>% select(code, type,geo_lat, geo_lng)
nodes %<>% left_join(node_data)
# we just throw out what didn't match (mostly 'facultatiefstation')
j <- nodes$code %in% c(out$start,out$end) 
nodes <- nodes[j,]

write.csv(nodes, file.path("02tidy",paste0(file_prefix,"railway_nodes.csv")),row.names=FALSE)
