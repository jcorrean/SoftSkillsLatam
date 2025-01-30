load("~/Documents/GitHub/SoftSkillsLatam/Matriz.RData")
edges_args <- data.frame()
for (i in 1:nrow(Matriz)) {
  for (j in 1:ncol(Matriz)) {
    if (Matriz[i, j] > 0) { # Only include edges where there's a connection
      edges_args <- rbind(edges_args, data.frame(
        docs = rownames(Matriz)[i],
        features = colnames(Matriz)[j],
        Frequency = Matriz[i, j], # Store the weight
        Country = "Argentina"
      ))
    }
  }
}


library(igraph)
bnARG <- graph_from_biadjacency_matrix(Matriz, directed = FALSE)
V(bnARG)$type <- bipartite_mapping(bnARG)$type
V(bnARG)$shape <- ifelse(V(bnARG)$type, "circle", "square")
V(bnARG)$color <- ifelse(V(bnARG)$type, "red", "blue4")
V(bnARG)$degree <- igraph::degree(bnARG)
V(bnARG)$closeness <- igraph::closeness(bnARG)
V(bnARG)$betweenness <- igraph::betweenness(bnARG)
V(bnARG)$Eigenvector <- igraph::eigen_centrality(bnARG)$vector
E(bnARG)$Frequency <- edges_args$Frequency
E(bnARG)$color <- "lightgrey"
igraph::edge_attr_names(bnARG)
igraph::edge_attr(bnARG)
igraph::vertex.attributes(bnARG)$name

ProgramsARG <- data.frame(Node = igraph::vertex.attributes(bnARG)$name,
                          Degree = V(bnARG)$degree,
                          Closeness = V(bnARG)$closeness,
                          Betweenness = V(bnARG)$betweenness,
                          Eigenvector = V(bnARG)$Eigenvector)
ProgramsARG <- ProgramsARG[order(-ProgramsARG$Eigenvector), ]
ProgramsARG <- mutate(ProgramsARG, 
                      Partition = ifelse(
                        grepl("text", Node), "Program", "Skill"))
ProgramsARG$Country <- "Argentina"

library(gtools)
ProgramsARG$Node <- factor(ProgramsARG$Node, levels = mixedsort(unique(ProgramsARG$Node)))
ProgramsARG <- ProgramsARG[order(ProgramsARG$Node), ]
P.ARG <- ProgramsARG[order(ProgramsARG$Partition), ]

library(intergraph)
pave2 <- asDF(bnARG)
pave2$edges
pave2$vertexes
pave3 <- asNetwork(pave2$edges, directed = FALSE, pave2$vertexes)
attrmap()

library(network)
Argentina <- asNetwork(pave2$edges, directed = FALSE, pave2$vertexes)
summary(Argentina)
Argentina
# 2. Get the edges as a data frame (this is the KEY)
edges_df <- as.data.frame(as.matrix.network(Argentina, matrix.type = "edgelist"))
colnames(edges_df) <- c("tail", "head") 
edge_values <- numeric(nrow(edges_df)) #Initialize a vector to store the values
for (i in 1:nrow(edges_df)) {
  tail_node <- edges_df$tail[i]
  head_node <- edges_df$head[i]
  
  # Adjust indices for bipartite structure
  if (tail_node <= 514) { # Node from the first partition
    row_index <- tail_node
    col_index <- head_node - 514 # Node from the second partition
  } else { #Node from the second partition
    row_index <- head_node
    col_index <- tail_node - 514
  }
  
  edge_values[i] <- Matriz[row_index, col_index]
  
}
set.edge.attribute(Argentina, "Frequency", edge_values)
Argentina
list.edge.attributes(Argentina)
get.edge.attribute(Argentina, "Frequency")


SizeARG <- network::network.size(Argentina)
DensityARG <- network::network.density(Argentina)
ClusteringARG <- tnet::reinforcement_tm(t(Matriz))
# también podría usar C4 como indicador de clustering
# llamado como "reinforcing"
set.network.attribute(Argentina, "Size", SizeARG)
set.network.attribute(Argentina, "Density", DensityARG)
set.network.attribute(Argentina, "Clustering", ClusteringARG)
set.network.attribute(Argentina, "Country", "Argentina")
set.network.attribute(Argentina, "Level", "All")
set.network.attribute(Argentina, "OECD", FALSE)
Program <- c(ARGTexts$Program, rep(NA, ncol(Matriz)))
BrochureLength <- c(ARGTexts$Tokens, rep(NA, ncol(Matriz)))
network::set.vertex.attribute(Argentina, "Program", Program)
network::set.vertex.attribute(Argentina, "Brochure.Length", BrochureLength)

network::get.vertex.attribute(Argentina, "vertex.names")
network::get.vertex.attribute(Argentina, "Program")
network::get.vertex.attribute(Argentina, "Brochure.Length")

Argentina

# Assign centralities as vertex attributes in the NETWORK object
network::set.vertex.attribute(Argentina, "Centrality", P.ARG$Degree)
network::set.vertex.attribute(Argentina, "Closeness", P.ARG$Closeness)
network::set.vertex.attribute(Argentina, "Betweenness", P.ARG$Betweennes)
network::set.vertex.attribute(Argentina, "Eigenvector", P.ARG$Eigenvector)

# Verify
network::get.vertex.attribute(Argentina, "vertex.names")
network::get.vertex.attribute(Argentina, "Centrality")
network::get.vertex.attribute(Argentina, "Closeness")
network::get.vertex.attribute(Argentina, "Betweenness")
network::get.vertex.attribute(Argentina, "Eigenvector")

get.edgeIDs(Argentina, 1,519)
get.edgeIDs(Argentina, 2,518)
network::get.edge.value(Argentina, "Frequency")

