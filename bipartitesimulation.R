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
# celdas no nulas / total de celdas
sum(IMEX2 == 0)
ncol(IMEX2)
nrow(IMEX2)
# Para el caso de México con 112 programas y 40 skills
(40*112-sum(IMEX2 == 0))/(40*112)

