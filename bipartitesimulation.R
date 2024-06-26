library(igraph)

# Generate a random bipartite graph
# Example: Creating a random bipartite graph with 10 nodes in the first partition and 5 nodes in the second partition
# Using the gnp model with a connection probability of 0.1
g <- sample_bipartite(10, 5, p = 0.1)
V(g)$shape <- ifelse(V(g)$type, "circle", "square")
V(g)$color <- ifelse(V(g)$type, "#CE1127", "#0A3A7E")

# To visualize the graph
plot(g, vertex.label = NA, main = "Random Bipartite Graph")



# Conteo de ceros para definir la probabilidad de conexión entre 
# nodos
# total de celdas en la matrix - total de ceros / total de celdas
# celdas no nulas / total de celdas. Veamos con el caso de Venezuela
load("VEN.RData")
sum(IMVEN2 == 0)
ncol(IMVEN2)
nrow(IMVEN2)
# Para el caso de Venezuela con 119 programas y 42 skills
(ncol(IMVEN2)*nrow(IMVEN2)-sum(IMVEN2 == 0))/(nrow(IMVEN2)*ncol(IMVEN2))

