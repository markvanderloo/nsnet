
#' efficiency of a graph, according to Latora (2001)
#' @param g a graph
#'
efficiency <- function(g){
  n <- length(V(g))
  if (n==1) return(0)
  nd <- igraph::distance_table(g)$res
  d <- seq_along(nd)
  N <- n*(n-1)
  2*sum(nd/d)/N
}

#' local efficiency of a graph, according to Latora (2001)
#' @param g, a graph
local_efficiency <- function(g){
  sapply(V(g),function(node){
    h <- induced_subgraph(g,c(neighbors(g,node)))
    efficiency(h)
  })
}

#' Network vulnerability per node, according to Gol'dshtein (2004) and 
#' Latora et al (2005).
#' @param g a graph
vulnerability <- function(g){
  e <- efficiency(g)
  nodes <- V(g)
  sapply(seq_along(nodes), function(i){
    h <- delete_edges(g, incident_edges(g,nodes[i])[[1]])
    1-efficiency(h)/e
  })
}

harmonic <- function(n){
  sum(1/seq_len(n))
}

#' efficiency of the line graph
#' @param n number of nodes (can be a vector of values)
line_efficiency <- function(n){
  sapply(n, function(ni) 2*harmonic(ni-1)/(ni-1) - 2/ni)
}

circle_efficiency <- function(n){
  ce <- function(x){
    h <- if (x%%2==0){
      2*harmonic(x/2-1)/(x-1) + 2/(x*(x-1))
    } else {
      2*harmonic((x-1)/2)/(x-1)
    }
  }
  sapply(n,ce)
}
  
#' Topological information content
#' @param g a graph
#' 
#' @details
#' The topological information content is defined as
#' the logarithm of the size of the automorphism group to the base of 2.
#' 
information_content <- function(g){
  log2(as.numeric(igraph::automorphisms(h)$group_size))
}


# return a vector of graph characterizations
measures <- function(g){
  c(
    global_efficiency = efficiency(g)
    , local_efficiency = mean(local_efficiency(g))
    , vulnerability = max(vulnerability(g))
    , topological_informatioin_content = information_content(g)
  )
}









