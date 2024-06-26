# Ejemplo de cómo simular una red aleatoria bipartita
# con probabilidad de conexión entre nodos
# igual a la observada para Argentina

# Paso 1 (Cargar la Red) ----

load("ARG.RData")

# Paso 2 (Calcular la probabilidad de conexión entre nodos) ----
# Para ello hay que contar el número de celdas en la red bipartita
# representada en forma de matriz con columnas que representan
# las habilidades y filas que representan los programas de 
# de postgrado. Para ello, hay que contar las celdas
# que son diferentes de cero, pues una celda con cero
# indica que la habilidad (skill) no está explícitamente
# incluida en el programa académico.

# El total de conexiones posibles entre nodos en el caso de
# Argentina es
ncol(IARG2) * nrow(IARG2)
# El total de conexiones observadas es
sum(IARG2 != 0)

# Así pues, la probabilidad de conexiones es
probabilidad_empirica = sum(IARG2 != 0) / (ncol(IARG2) * nrow(IARG2))
probabilidad_empirica

# Paso 3 (Caracterizar propiedades de la red) ----
library(igraph)
# Para ello hay que usar la red bipartita construida 
# con igraph para calcular los siguientes estadísticos
edge_density(bn6)
degree(bn6)
betweenness(bn6)
eigen_centrality(bn6)
edge_betweenness(bn6)
mean(edge_betweenness(bn6))
sd(edge_betweenness(bn6))

# Paso 4 ----
# Generar red aleatoria con igual probabilidad de conexión que la red observada

library(igraph)
g <- sample_bipartite(43, 369, p = probabilidad_empirica)
V(g)$shape <- ifelse(V(g)$type, "circle", "square")
V(g)$color <- ifelse(V(g)$type, "red", "blue")
E(g)$color <- "lightgray"
V(g)$size <- 2

# To visualize the graph
plot(g, vertex.label = NA, main = "Random Bipartite Graph")

edge_density(g)
edge_density(bn6)
mean(edge_betweenness(g))
mean(edge_betweenness(bn6))
sd(edge_betweenness(g))
sd(edge_betweenness(bn6))




library(ergm)
library(network)
pave <- as.network(t(IMEX2), matrix.type = 'incidence', bipartite = TRUE)
pave$iel
pave$gal

