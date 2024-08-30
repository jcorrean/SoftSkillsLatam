library(igraph)
library(network)
load("ARG.RData")#bn6
load("BRA.RData")#bn4
load("CHL.RData")#bn5
load("COL.RData")#bn1
load("ECU.RData")#bn3
load("MEX.RData")#bn8
load("URU.RData")#bn7
load("VEN.RData")#bn2
rm(list=setdiff(ls(), c("bn1","bn2","bn3","bn4","bn5","bn6","bn7","bn8")))

transform_to_network <- function(bn) {
  if (inherits(bn, "bn.fit")) {
    # For bn.fit objects
    BiM <- as_biadjacency_matrix(bn, types = V(bn)$type, names = TRUE)
  } else if (inherits(bn, "igraph")) {
    # For igraph objects (assuming bn is a bipartite graph)
    BiM <- as_biadjacency_matrix(bn, types = V(bn)$type, names = TRUE)
  } else {
    stop("Input object must be a bn.fit or igraph object.")
  }
  
  # Convert to network object
  network(BiM,
          directed = FALSE,
          hyper = FALSE,
          loops = FALSE,
          multiple = FALSE,
          bipartite = TRUE)
}

bn_list <- list(bn1, bn2, bn3, bn4, bn5, bn6, bn7, bn8)
network_list <- lapply(bn_list, transform_to_network)

Colombia <- network_list[1]
summary(bn1)
get.vertex.attribute(Colombia, "vertex.names")



library(igraph)
BiM <- as_biadjacency_matrix(bn6, types = V(bn6)$type, names = TRUE)
library(network)
red <- network(BiM, 
               directed = FALSE, 
               hyper = FALSE, 
               loops = FALSE, 
               multiple = FALSE, 
               bipartite = TRUE)
is.network(red)
red
sna::gden(red)
