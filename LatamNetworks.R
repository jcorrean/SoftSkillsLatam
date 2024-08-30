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

BiM1 <- as_biadjacency_matrix(bn1, types = V(bn1)$type, names = TRUE)
BiM2 <- as_biadjacency_matrix(bn2, types = V(bn2)$type, names = TRUE)
BiM3 <- as_biadjacency_matrix(bn3, types = V(bn3)$type, names = TRUE)
BiM4 <- as_biadjacency_matrix(bn4, types = V(bn4)$type, names = TRUE)
BiM5 <- as_biadjacency_matrix(bn5, types = V(bn5)$type, names = TRUE)
BiM6 <- as_biadjacency_matrix(bn6, types = V(bn6)$type, names = TRUE)
BiM7 <- as_biadjacency_matrix(bn7, types = V(bn7)$type, names = TRUE)
BiM8 <- as_biadjacency_matrix(bn8, types = V(bn8)$type, names = TRUE)

red1 <- network(BiM1, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
red2 <- network(BiM2, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
red3 <- network(BiM3, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
red4 <- network(BiM4, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
red5 <- network(BiM5, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
red6 <- network(BiM6, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
red7 <- network(BiM7, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
red8 <- network(BiM8, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)

class(red1)
class(red2)
class(red3)
class(red4)
class(red5)
class(red6)
class(red7)
class(red8)
get.vertex.attribute(red1, "vertex.names")
