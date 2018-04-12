
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
  gather(EndCode, Distance, -Station) %>% 
  rename(StartCode=Station) %>%
  mutate(Distance = as.numeric(Distance)) %>%
  filter(complete.cases(.))

# merge full station names
stationsnamen <- stations %>% select(code, naam)
stationsnamen %<>% mutate(
  naam=str_replace(naam,"a/d","aan den")
  ,naam=str_replace(naam," v "," van ")) 
# we add 'Zwolle Stadshage by hand'
stationsnamen <- rbind(stationsnamen
  , data.frame(
      code=c("zls","bsks")
    , naam=c("Zwolle Stadshagen","Boskoop Snijdelwijk")))


dd <- left_join(distance, stationsnamen, by=c("StartCode"="code")) %>%
  rename(Start=naam)

dd <- left_join(dd, stationsnamen,by=c("EndCode"="code")) %>%
  rename(End=naam)

# merge edges with distances

pattren <- c(" +\\(.*\\)", "-", "St\\.")
replace <- c(""          , " ", "St")

tm <- edges %>%
  mutate(Start = str_trim(str_replace(Start, pattern, replace))
         ,End  = str_trim(str_replace(End, pattern, replace))) %>%
  filter(!Start %in% c("Onbekend","2018"))



library(stringdist)

j_start <- amatch(str_trim(tm$Start), str_trim(stations$naam), method='osa',maxDist = 2)
View(tm[which(is.na(j_start)),])


out <- left_join(tm,dd)
head(out)
i <- which(is.na(out$StartCode))
View(out[i,])

## prepare output and write ----
# We still have double entries becuse multiple train series can pass through the same stations.
out %<>% 
  select(Start, End, StartCode, EndCode, Distance) %>%
  distinct(Start, End, .keep_all = TRUE)

# gotta know when we ran it..
file_prefix <- format(Sys.time(),format="%Y%m%d_")
write.csv(out, file.path("02tidy",paste0(file_prefix,"railway-edges.csv")),row.names = FALSE)






