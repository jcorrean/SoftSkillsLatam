# Ejemplo de cómo simular una red aleatoria bipartita
# con probabilidad de conexión entre nodos
# igual a la observada para Argentina

# Paso 1 ---- (Cargar la Red)
load("ARG.RData")

# Paso 2 ---- (Calcular la probabilidad de conexión entre nodos)
# Para ello hay que contar el número de celdas en la matriz
# que son diferentes de cero, pues una celda con cero
# indica que la habilidad (skill) no está explícitamente
# incluida en el programa académico.

# El total de conexiones posibles entre nodos en el caso de
# Argentina es
ncol(IARG2) * nrow(IARG2)
# El total de conexiones observadas es
sum(IARG2 != 0)

# Así pues, la probabilidad de conexiones es
sum(IARG2 != 0) / (ncol(IARG2) * nrow(IARG2))

# Paso 3 ---- (Caracterizar propiedades de la red)

# The graph density for the Argentinian academic offering
# is 
igraph::edge_density(bn6)
igraph::betweenness(bn6)
igraph::edge_betweenness(bn6)
mean(igraph::edge_betweenness(bn6))
sd(igraph::edge_betweenness(bn6))


library(igraph)
g <- sample_bipartite(43, 369, p = 0.0668)
V(g)$shape <- ifelse(V(g)$type, "circle", "square")
V(g)$color <- ifelse(V(g)$type, "red", "blue")
E(g)$color <- "lightgray"
V(g)$size <- 2

# To visualize the graph
plot(g, vertex.label = NA, main = "Random Bipartite Graph")

igraph::edge_density(g)






library(ergm)
library(network)
pave <- as.network(t(IMEX2), matrix.type = 'incidence', bipartite = TRUE)
pave$iel
pave$gal

