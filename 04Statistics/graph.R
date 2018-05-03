
suppressPackageStartupMessages({
  library(igraph)
  library(tidyr)
  library(dplyr)
})

source("00functions/measures.R")

nsrblauw <- "#003373"
nsgeel <- "#f7d417"

trajecten <- read.csv("02tidy/trajecten.csv",stringsAsFactors = FALSE)
stations <-  read.csv("02tidy/trajecten_stations.csv",stringsAsFactors = FALSE)




h <- graph_from_data_frame(trajecten,directed=FALSE,vertices=stations)
L <- components(h)
pdf("nsnet.pdf")
set.seed(3)
par(mar=rep(0,4),oma=rep(0,4))
plot(h,vertex.label=NA
     , vertex.size=2
     , vertex.color=nsgeel
     , edge.color=nsrblauw
     , vertex.frame.color=nsrblauw)
dev.off()


v <- vulnerability(h)
head(v)
vv <- (v-min(v))/diff(range(v))

pdf("nsnet.pdf")
set.seed(3)
par(mar=rep(0,4),oma=rep(0,4))
plot(h,vertex.label=NA
     , vertex.size=2 + 2*vv
     , vertex.color=nsgeel
     , edge.color=nsrblauw
     , vertex.frame.color=nsrblauw)
dev.off()


information_content(h)

       