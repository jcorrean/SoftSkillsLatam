library(igraph)
library(network)
load("ARG.RData")#bn6 Argentina (369)
load("BRA.RData")#bn4 Brazil (441)
load("CHL.RData")#bn5 Chile (181)
load("COL.RData")#bn1 Colombia (204)
load("ECU.RData")#bn3 Ecuador (370)
load("MEX.RData")#bn8 Mexico (112)
load("URU.RData")#bn7 Uruguay (114)
load("VEN.RData")#bn2 Venezuela (119)
load("CORI.RData")#bn9 Costa Rica (59)
rm(list=setdiff(ls(), c("bn1","bn2","bn3","bn4","bn5","bn6","bn7","bn8","bn9")))

BiM1 <- as_biadjacency_matrix(bn1, types = V(bn1)$type, names = TRUE)
BiM2 <- as_biadjacency_matrix(bn2, types = V(bn2)$type, names = TRUE)
BiM3 <- as_biadjacency_matrix(bn3, types = V(bn3)$type, names = TRUE)
BiM4 <- as_biadjacency_matrix(bn4, types = V(bn4)$type, names = TRUE)
BiM5 <- as_biadjacency_matrix(bn5, types = V(bn5)$type, names = TRUE)
BiM6 <- as_biadjacency_matrix(bn6, types = V(bn6)$type, names = TRUE)
BiM7 <- as_biadjacency_matrix(bn7, types = V(bn7)$type, names = TRUE)
BiM8 <- as_biadjacency_matrix(bn8, types = V(bn8)$type, names = TRUE)
BiM9 <- as_biadjacency_matrix(bn9, types = V(bn9)$type, names = TRUE)

TotalPrograms <- ncol(BiM1) +
  ncol(BiM2) +
  ncol(BiM3) +
  ncol(BiM4) +
  ncol(BiM5) +
  ncol(BiM6) +
  ncol(BiM7) +
  ncol(BiM8) +
  ncol(BiM9)
TotalPrograms

Colombia <- network(BiM1, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Venezuela <- network(BiM2, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Ecuador <- network(BiM3, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Brazil <- network(BiM4, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Chile <- network(BiM5, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Argentina <- network(BiM6, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Uruguay <- network(BiM7, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Mexico <- network(BiM8, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
CostaRica <- network(BiM9, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)


class(red1)
class(red2)
class(red3)
class(red4)
class(red5)
class(red6)
class(red7)
class(red8)
class(red9)
network::get.vertex.attribute(red9, "vertex.names")
