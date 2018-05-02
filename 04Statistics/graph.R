
suppressPackageStartupMessages({
  library(igraph)
})

edges <- read.csv("02tidy/20180419_railway-edges.csv",stringsAsFactors = FALSE)
nodes <- read.csv("02tidy/20180419_railway_nodes.csv",stringsAsFactors = FALSE)


g <- graph_from_data_frame(edges, directed=FALSE, vertices=nodes)
par(mar=c(0,0,0,0),oma=c(0,0,0,0))

#png("ns.png")


plot(g, vertex.label=NA,vertex.size=1)
#dev.off()




