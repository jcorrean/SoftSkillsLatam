load("~/Documents/GitHub/SoftSkillsLatam/Matriz.RData")
edges_args <- data.frame()
for (i in 1:nrow(Matriz)) {
  for (j in 1:ncol(Matriz)) {
    if (Matriz[i, j] >= 0) { # Only include edges where there's a connection
      edges_args <- rbind(edges_args, data.frame(
        actor = rownames(Matriz)[i],
        event = colnames(Matriz)[j],
        Frequency = Matriz[i, j] # Store the weight
        #Country = "Argentina"
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
vertices <- edges_args[edges_args$Frequency > 0, ]
E(bnARG)$Frequency <- vertices$Frequency
#E(bnARG)$color <- "lightgrey"
igraph::edge_attr_names(bnARG)
igraph::edge_attr(bnARG)
igraph::vertex.attributes(bnARG)$name
adj_matrix <- as_adjacency_matrix(bnARG)
str(adj_matrix)

ProgramsARG <- data.frame(node.id = igraph::vertex.attributes(bnARG)$name,
                          Degree = V(bnARG)$degree,
                          Closeness = V(bnARG)$closeness,
                          Betweenness = V(bnARG)$betweenness,
                          Eigenvector = V(bnARG)$Eigenvector,
                          Program = c(ARGTexts$Program, rep(NA, 10)),
                          Brochure.Length = c(ARGTexts$Tokens, rep(NA, 10)),
                          is_actor = c(rep(TRUE, 514), rep(FALSE, 10)))

EDGES <- edges_args[edges_args$Frequency > 0, ]


library(network)
Argentina <- as.network(EDGES, 
                     directed = FALSE,
                     vertices = ProgramsARG,
                     bipartite = TRUE)
network::set.edge.attribute(Argentina, "Frequency", as.numeric(EDGES$Frequency))


Argentina
SizeARG <- network::network.size(Argentina)
get.edge.attribute(Argentina, "Frequency")
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
network::set.edge.attribute(Argentina, "Frequency", EDGES$Frequency)
print(network::get.edge.attribute(Argentina, "Frequency"))
