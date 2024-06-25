# Ejemplo de cómo simular una red aleatoria bipartita
# con probabilidad de conexión igual a la observada para Argentina

library(igraph)
g <- sample_bipartite(43, 369, p = 0.0668)
V(g)$shape <- ifelse(V(g)$type, "circle", "square")
V(g)$color <- ifelse(V(g)$type, "red", "blue")
V(g)$size <- 2

# To visualize the graph
plot(g, vertex.label = NA, main = "Random Bipartite Graph")

library(ergm)
library(network)
pave <- as.network(t(IMEX2), matrix.type = 'incidence', bipartite = TRUE)
pave$iel
pave$gal

