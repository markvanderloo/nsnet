
#' efficiency of a graph, according to Latora (2001)
#' @param g a graph
#'
efficiency <- function(g){
  nd <- igraph::distance_table(g)$res
  d <- seq_along(nd)
  n <- length(V(g))
  N <- n*(n-1)
  2*sum(nd/d)/N
}

#' local efficiency of a graph, according to Latora (2001)
#' @param g, a graph
local_efficiency <- function(g){
  sapply(V(g),function(node){
    h <- induced_subgraph(g,c(node,neighbors(g,node)))
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
    h <- induced_subgraph(g, nodes[-i])
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


#' efficacy of a graph
#' @param a graph
#' 
#' @details:
#' The efficacy of a graph is the sum over inverse shortest path lengths,
#' multiplied with the number of shortest paths, divided by n(n-1). 
#' (van der Loo, 2018)
#' 
efficacy <- function(g){
  nodes <- seq_along(V(g))
  n <- length(nodes)
  out <- sapply(nodes[-1],function(i){
    a <- sapply(seq_len(i-1), function(j){
      L <- igraph::all_shortest_paths(g,from=i, to=j)
      mu <- length(L$res)
      len <- length(L$res[[1]])-1L
      c(mu=mu,len=len)
    })
    sum(a[1,]/a[2,])
  })
  2*sum(out)/(n*(n-1))
}



local_efficacy <- function(g){
  sapply(V(g),function(node){
    h <- induced_subgraph(g,c(node,neighbors(g,node)))
    efficacy(h)
  })
}

# return a vector of graph characterizations
measures <- function(g){
  c(
    global_efficiency = efficiency(g)
    , local_efficiency = mean(local_efficiency(g))
    , global_efficacy = efficacy(g)
    , local_efficacy  = mean(local_efficacy)
    , vulnerability = max(vulnerability(g))
    , topological_informatioin_content = information_content(g)
  )
}

measures(h)








