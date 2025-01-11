library(igraph)
library(network)
library(intergraph)

load("Results/Argentina.RData")

library(igraph)
bnARG <- graph_from_biadjacency_matrix(t(Matriz), directed = FALSE)
EdgeListAR <- as_edgelist(bnARG)
edges_args <- data.frame()
for (i in 1:nrow(Matriz)) {
  for (j in 1:ncol(Matriz)) {
    if (Matriz[i, j] > 0) { # Only include edges where there's a connection
      edges_args <- rbind(edges_args, data.frame(
        Source = paste0("ARG_", rownames(Matriz)[i]),
        Target = colnames(Matriz)[j],
        Weight = Matriz[i, j], # Store the weight
        Country = "Argentina"
      ))
    }
  }
}

bnARG <- graph_from_data_frame(edges_args, directed = FALSE)
bipartite_mapping(bnARG)
V(bnARG)$type <- bipartite_mapping(bnARG)$type
V(bnARG)$shape <- ifelse(V(bnARG)$type, "circle", "square")
V(bnARG)$label.cex <- ifelse(V(bnARG)$type, 0.5, 1)
V(bnARG)$size <- sqrt(igraph::degree(bnARG))
E(bnARG)$color <- "lightgrey"
E(bnARG)$weight <- edges_args$Weight

edges_args <- data.frame()
for (i in 1:nrow(Matriz)) {
  for (j in 1:ncol(Matriz)) {
    if (Matriz[i, j] > 0) {
      edges_args <- rbind(edges_args, data.frame(
        Source = paste0("ARG_", rownames(Matriz)[i]),
        Target = colnames(Matriz)[j],
        Weight = Matriz[i, j],
        Country = "Argentina"
      ))
    }
  }
}

# Igraph creation (for comparison and visualization if needed)
bnARG <- graph_from_data_frame(edges_args, directed = FALSE)
bipartite_mapping(bnARG)
V(bnARG)$type <- bipartite_mapping(bnARG)$type
V(bnARG)$shape <- ifelse(V(bnARG)$type, "circle", "square")
V(bnARG)$label.cex <- ifelse(V(bnARG)$type, 0.5, 1)
V(bnARG)$size <- sqrt(igraph::degree(bnARG))
E(bnARG)$color <- "lightgrey"
E(bnARG)$weight <- edges_args$Weight # Correctly setting weights in igraph

# Network creation with weights
verticesARG <- nrow(Matriz) + ncol(Matriz)
Argentina <- network.initialize(verticesARG, directed = FALSE, bipartite = TRUE)

# Create the bipartite adjacency matrix for the network object
pave <- matrix(0, nrow = nrow(Matriz), ncol = ncol(Matriz))
for(i in 1:nrow(edges_args)){
  row_index <- which(rownames(Matriz) == sub("ARG_", "", edges_args$Source[i]))
  col_index <- which(colnames(Matriz) == edges_args$Target[i])
  pave[row_index, col_index] <- 1
}
network.bipartite(pave, Argentina)

# Add edge weights to the network object
for (i in 1:nrow(edges_args)) {
  source_vertex <- which(network::get.vertex.attribute(Argentina, "vertex.names") == edges_args$Source[i])
  target_vertex <- which(network::get.vertex.attribute(Argentina, "vertex.names") == edges_args$Target[i])
  if (length(source_vertex) > 0 && length(target_vertex) > 0) {
    set.edge.attribute(Argentina, "weight", i, edges_args$Weight[i])
  }
}


#Verify the weights
list.edge.attributes(Argentina)
edge_attr(Argentina, "weight")

Argentina
