
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
#pdf("nsnet.pdf")
set.seed(3)
par(mar=rep(0,4),oma=rep(0,4))
plot(h,vertex.label=NA
     , vertex.size=2
     , vertex.color=nsgeel
     , edge.color=nsrblauw
     , vertex.frame.color=nsrblauw)
#dev.off()

e_ns <- efficiency(h)
le <- local_efficiency(h)

rings <- 3:500
s <- circle_efficiency(rings)

i <- which.min(abs(s-e_ns))
s[i]
rings[i]
e_ns

l <- line_efficiency(3:500)
i <- which.min(abs(l-e_ns))
l[i]
rings[i]

v <- vulnerability(h)

pdf("lehist.pdf")
oldpar <- par(mar=c(2.1,4.1,2,1))
hist(le,las=1
     ,cex.axis=1.2,las=1,cex.lab=1.2
     ,breaks=50
     ,col='grey'
     ,main="Local efficiency"
     ,ylab='Stations'
     ,xlab="")
par(oldpar)
dev.off()

pdf("vhist.pdf")
oldpar <- par(mar=c(2.1,4.1,2,1))
hist(vv,las=1
     ,cex.axis=1.2,las=1,cex.lab=1.2
     ,breaks=40
     ,col='grey'
     ,main="Individual Vulnerability"
     ,ylab='Stations'
     ,xlab="")
par(oldpar)
dev.off()

pdf("nsnet.pdf")
vv <- (v-min(v))/diff(range(v))
set.seed(3)
par(mar=rep(0,4),oma=rep(0,4))
plot(h,vertex.label=NA
     , vertex.size=2 + 2*vv
     , vertex.color=nsgeel
     , edge.color=nsrblauw
     , vertex.frame.color=nsrblauw)
dev.off()


information_content(h)

       