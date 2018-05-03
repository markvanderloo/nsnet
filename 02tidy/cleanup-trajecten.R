suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})


dat <- read.csv("01raw/20180502_trajecten.csv",stringsAsFactors = FALSE)
# deduplicate the edges
trajecten <- setNames(as.data.frame(t(apply(as.matrix(dat[1:2]),1,sort)),stringsAsFactors = FALSE),c("start","stop"))
trajecten <- unique(trajecten)

# throw out stations not in NL.
stations <- read.csv("01raw/20180412_stations.csv",stringsAsFactors = FALSE)
stations_nl <- filter(stations,land=="NL")
# 'Waddingsveen Triangel' is missing. We add a dummy record for now.
stations_nl <- bind_rows(stations_nl, data.frame(naam="Waddinxveen Triangel",stringsAsFactors = FALSE))




tr_out <- semi_join(trajecten, stations_nl, by=c(start="naam"))
tr_out <- semi_join(tr_out, stations_nl, by=c(stop="naam"))

# filter out stations that are not in the edge list
tr <- tr_out %>% gather(link_end, naam, start, stop)
st_out <- semi_join(stations_nl, tr,by="naam")
st_out <- st_out[c(4,1:3,5:ncol(stations))]

write.csv(st_out, file="02tidy/trajecten_stations.csv",row.names=FALSE)



# output
write.csv(tr_out, file="02tidy/trajecten.csv",row.names=FALSE)


