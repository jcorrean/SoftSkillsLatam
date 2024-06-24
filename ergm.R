library(ergm)
library(network)
pave <- as.network(t(IMEX2), matrix.type = 'incidence', bipartite = TRUE)
pave$iel
pave$gal
