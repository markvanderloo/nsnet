
# input: csvs with raw data
# output: a csv that can be read into igraph.

suppressPackageStartupMessages({
  library(tidyverse)
  library(magrittr)
})

## Read raw data ----
# I'd use read_csv but the parsing messages annoy me too much
distmat <- read.csv("01raw/20180412_distance-matrix.csv", stringsAsFactors = FALSE)
stations <- read.csv("01raw/20180412_stations.csv", stringsAsFactors = FALSE)
series <- read.csv("01raw/20180412_treinseries.csv", stringsAsFactors = FALSE)
series_meta <- read.csv("01raw/20180412_treinseries-overview.csv", stringsAsFactors = FALSE)

## Create pairs of stations ----
series %<>% 
  filter(complete.cases(series))

connections <- series %>% 
  group_by(Serie) %>% 
  transmute(
     Start=lag(station)
    , End=station
    ) %>% ungroup() %>%
  filter(complete.cases(.))

# we only need sprinters and stoptrains so we get individual connections and not connections that skip
# stoptrain stations.
edges <- connections %>% left_join(series_meta,by="Serie")
edges %<>%  filter(str_trim(str_to_lower(Treinsoort)) %in% c("sprinter","stoptrein"))

# Merge the tarif distances ----
# distance to long form, conversion of type
distance <-  distmat %>% 
  gather(EndCode, Distance, -Station, -naam) %>% 
  rename(StartCode=Station) %>%
  mutate(Distance = as.numeric(Distance)) %>%
  filter(complete.cases(.))

# merge full station names
stationsnamen <- stations %>% select(code, naam)
dd <- left_join(distance, stationsnamen, by=c("StartCode"="code")) %>%
  rename(Start=naam)

dd <- left_join(dd, stationsnamen,by=c("EndCode"="code")) %>%
  rename(End=naam)

# merge edges with distances
out <- left_join(edges,dd)

## prepare output and write ----
# We still have double entries becuse multiple train series can pass through the same stations.
out %<>% 
  select(Start, End, StartCode, EndCode, Distance) %>%
  distinct(Start, End, .keep_all = TRUE)

# gotta know when we ran it..
file_prefix <- format(Sys.time(),format="%Y%m%d_")
write.csv(out, file.path("02tidy",paste0(file_prefix,"railway-edges.csv")),row.names = FALSE)






