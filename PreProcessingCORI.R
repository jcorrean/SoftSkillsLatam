load("~/Documents/GitHub/SoftSkillsLatam/Matrices/MatrizCORI.RData")
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
bnCR <- graph_from_biadjacency_matrix(Matriz, directed = FALSE)
V(bnCR)$type <- bipartite_mapping(bnCR)$type
V(bnCR)$shape <- ifelse(V(bnCR)$type, "circle", "square")
V(bnCR)$color <- ifelse(V(bnCR)$type, "red", "blue4")
V(bnCR)$degree <- igraph::degree(bnCR)
V(bnCR)$closeness <- igraph::closeness(bnCR)
V(bnCR)$betweenness <- igraph::betweenness(bnCR)
V(bnCR)$Eigenvector <- igraph::eigen_centrality(bnCR)$vector
vertices <- edges_args[edges_args$Frequency >= 1, ]
E(bnCR)$Frequency <- vertices$Frequency
#E(bnCR)$color <- "lightgrey"
igraph::edge_attr_names(bnCR)
igraph::edge_attr(bnCR)
igraph::vertex.attributes(bnCR)$name

ProgramsCR <- data.frame(vertex.names = igraph::vertex.attributes(bnCR)$name,
                          is_actor = c(rep(TRUE, nrow(Matriz)), rep(FALSE, 10)),
                          node.type = c(rep("Program", nrow(Matriz)), rep("Skill", 10)),
                          Degree = V(bnCR)$degree,
                          Closeness = V(bnCR)$closeness,
                          Betweenness = V(bnCR)$betweenness,
                          Eigenvector = V(bnCR)$Eigenvector,
                          Program = c(CORITexts$Program, rep(NA, 10)),
                          Brochure.Length = c(CORITexts$Tokens, rep(NA, 10)))


library(network)
CostaRica <- as.network(edges_args, 
                       directed = FALSE, 
                       bipartite = TRUE, 
                       bipartite_col = "is_actor", 
                       vertices = ProgramsCR)
class(CostaRica)
CostaRica
network::list.edge.attributes(CostaRica)
network::delete.edge.attribute(CostaRica, "na")
network::list.edge.attributes(CostaRica)

print(summary(network::get.edge.attribute(CostaRica, "Frequency")))

get.edge.attribute(CostaRica, "Frequency")

SizeARG <- network::network.size(CostaRica)
DensityARG <- network::network.density(CostaRica)
ClusteringARG <- tnet::reinforcement_tm(t(Matriz))
# también podría usar C4 como indicador de clustering
# llamado como "reinforcing"
set.network.attribute(CostaRica, "Size", SizeARG)
set.network.attribute(CostaRica, "Density", DensityARG)
set.network.attribute(CostaRica, "Clustering", ClusteringARG)
set.network.attribute(CostaRica, "Country", "CostaRica")
set.network.attribute(CostaRica, "Level", "All")
set.network.attribute(CostaRica, "OECD", TRUE)

network::get.vertex.attribute(CostaRica, "vertex.names")
network::get.vertex.attribute(CostaRica, "Program")
network::get.vertex.attribute(CostaRica, "Brochure.Length")

CostaRica
rm(list=setdiff(ls(), c("CostaRica")))
saveRDS(CostaRica, file = "NetworkData/CostaRica.RDS")
