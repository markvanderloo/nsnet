
# gotta know when we ran it..
file_prefix <- format(Sys.time(),format="%Y%m%d_")

## Distance matrix and Station info (rijdendetreinen.nl) ----
# Distance matrix (based on tarif)
distmat <- read.csv("https://blog.rijdendetreinen.nl/wp-content/uploads/2017/01/afstandenmatrix-2017-01.csv")
write.csv(distmat, file.path("01raw",paste0(file_prefix,"distance-matrix.csv")), row.names=FALSE)

# station info
stations <- read.csv("https://blog.rijdendetreinen.nl/wp-content/uploads/2017/01/stations-nl-2017-01.csv")
write.csv(stations, file.path("01raw",paste0(file_prefix,"stations.csv")),row.names=FALSE)



## Railway station network from wiki.ovinnederland.nl ----
# To set up the Dutch railway station graph we need to do some scraping
suppressPackageStartupMessages({
  library(rvest)
})

# Get the table of all treinseries, first column has trainseries number.
html <- read_html("http://wiki.ovinnederland.nl/wiki/Huidige_treinseries_in_Nederland")
node <- html_node(html, xpath='//*[@id="mw-content-text"]/table[2]')
series <- html_table(node)
write.csv(series, file.path("01raw",paste0(file_prefix,"treinseries-overview.csv")),row.names=FALSE)

# Only series operated within NL 
series <- subset(series, Vervoerder %in% c("NS Internationaal","NS","Arriva","Keolis Nederland","Hermes","Syntus","Connexxion"))
# Get the train series
urls <- sprintf("http://wiki.ovinnederland.nl/wiki/Treinserie_%d_(2018)",series$Serie)

# Each element is a sequence of stations. Each station is connected to the next.
# some urls are invalid. These all have 'Belgie' in the actual url and are unimportant for our purpose.
station_sequences <- lapply(urls, function(url){
  cat(sprintf("downloading %s ...\n",url))
  html <- tryCatch({
    html <- read_html(url)
  }
    , error=function(e){
      cat(sprintf("Could not download %s:  %s\n",url,e$message))
      NULL
  })
  if (is.null(html)) return(NA)
  xpth = if (grepl("_20100_",url) ){
    # one of the subsites has a slightly different makeup
    '//*[@id="mw-content-text"]/table[3]'
  } else {
    '//*[@id="mw-content-text"]/table[2]'
  }
  
  node <- html_node(html, xpath=xpth)
  dat <- html_table(node,fill=TRUE)
  dat[-1,2]
})

# stick it all in data.frames
seq_dfs <- lapply(seq_along(station_sequences), function(i){
  data.frame(Serie = series$Serie[i], url=urls[i], station=station_sequences[[i]])
})

out <- do.call("rbind",seq_dfs)

write.csv(out,file.path("01raw",paste0(file_prefix,"treinseries.csv")),row.names=FALSE)


