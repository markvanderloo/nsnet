
suppressPackageStartupMessages({
  library(igraph)
})

edges <- read.csv("02tidy/20180412_railway-edges.csv",stringsAsFactors = FALSE)
nodes <- read.csv("02tidy/20180412_railway_nodes.csv",stringsAsFactors = FALSE)

nodes[which(!nodes$code %in% c(edges$end,edges$start)),]

dim(edges)
dim(edges)

g <- graph_from_data_frame(edges, directed=FALSE, vertices=nodes)
plot(g, vertex.label=NA,vertex.size=1)


