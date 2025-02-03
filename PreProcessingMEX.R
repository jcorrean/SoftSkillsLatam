load("~/Documents/GitHub/SoftSkillsLatam/Matrices/MatrizMEX.RData")
edges_args <- data.frame()
for (i in 1:nrow(Matriz)) {
  for (j in 1:ncol(Matriz)) {
    if (Matriz[i, j] >= 1) { 
      edges_args <- rbind(edges_args, data.frame(
        Program = rownames(Matriz)[i],
        Skill = colnames(Matriz)[j],
        #Program.Skill = Matriz[i, j] > 0,
        Frequency = Matriz[i, j]
      ))
    }
  }
}

#isolated_programs <- which(rowSums(Matriz == 0) == ncol(Matriz))
#isolated_programs
#Matriz[rownames(Matriz) %in% isolated_programs, ]


library(igraph)
bnMEX <- graph_from_biadjacency_matrix(Matriz, directed = FALSE)
V(bnMEX)$type <- bipartite_mapping(bnMEX)$type
V(bnMEX)$shape <- ifelse(V(bnMEX)$type, "circle", "square")
V(bnMEX)$color <- ifelse(V(bnMEX)$type, "red", "blue4")
V(bnMEX)$degree <- igraph::degree(bnMEX)
V(bnMEX)$closeness <- igraph::closeness(bnMEX)
V(bnMEX)$betweenness <- igraph::betweenness(bnMEX)
V(bnMEX)$Eigenvector <- igraph::eigen_centrality(bnMEX)$vector
vertices <- edges_args[edges_args$Frequency >= 1, ]
E(bnMEX)$Frequency <- vertices$Frequency
#E(bnMEX)$color <- "lightgrey"
igraph::edge_attr_names(bnMEX)
igraph::edge_attr(bnMEX)
igraph::vertex.attributes(bnMEX)$name

ProgramsMEX <- data.frame(vertex.names = igraph::vertex.attributes(bnMEX)$name,
                         is_actor = c(rep(TRUE, nrow(Matriz)), rep(FALSE, 10)),
                         node.type = c(rep("Program", nrow(Matriz)), rep("Skill", 10)),
                         Degree = V(bnMEX)$degree,
                         Closeness = V(bnMEX)$closeness,
                         Betweenness = V(bnMEX)$betweenness,
                         Eigenvector = V(bnMEX)$Eigenvector,
                         Program = c(MEXTexts$Program, rep(NA, 10)),
                         Brochure.Length = c(MEXTexts$Tokens, rep(NA, 10)))


library(network)
Mexico <- as.network(edges_args, 
                      directed = FALSE, 
                      bipartite = TRUE, 
                      bipartite_col = "is_actor", 
                      vertices = ProgramsMEX)
class(Mexico)
Mexico
network::list.edge.attributes(Mexico)
network::delete.edge.attribute(Mexico, "na")
network::list.edge.attributes(Mexico)

print(summary(network::get.edge.attribute(Mexico, "Frequency")))

get.edge.attribute(Mexico, "Frequency")

SizeARG <- network::network.size(Mexico)
DensityARG <- network::network.density(Mexico)
ClusteringARG <- tnet::reinforcement_tm(t(Matriz))
# también podría usar C4 como indicador de clustering
# llamado como "reinforcing"
set.network.attribute(Mexico, "Size", SizeARG)
set.network.attribute(Mexico, "Density", DensityARG)
set.network.attribute(Mexico, "Clustering", ClusteringARG)
set.network.attribute(Mexico, "Country", "Mexico")
set.network.attribute(Mexico, "Level", "All")
set.network.attribute(Mexico, "OECD", TRUE)

network::get.vertex.attribute(Mexico, "vertex.names")
network::get.vertex.attribute(Mexico, "Program")
network::get.vertex.attribute(Mexico, "Brochure.Length")

Mexico
rm(list=setdiff(ls(), c("Mexico")))
saveRDS(Mexico, file = "NetworkData/Mexico.RDS")
readRDS(file = "NetworkData/Mexico.RDS")
