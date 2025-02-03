load("~/Documents/GitHub/SoftSkillsLatam/Matrices/MatrizCOL.RData")
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
bnCOL <- graph_from_biadjacency_matrix(Matriz, directed = FALSE)
V(bnCOL)$type <- bipartite_mapping(bnCOL)$type
V(bnCOL)$shape <- ifelse(V(bnCOL)$type, "circle", "square")
V(bnCOL)$color <- ifelse(V(bnCOL)$type, "red", "blue4")
V(bnCOL)$degree <- igraph::degree(bnCOL)
V(bnCOL)$closeness <- igraph::closeness(bnCOL)
V(bnCOL)$betweenness <- igraph::betweenness(bnCOL)
V(bnCOL)$Eigenvector <- igraph::eigen_centrality(bnCOL)$vector
vertices <- edges_args[edges_args$Frequency >= 1, ]
E(bnCOL)$Frequency <- vertices$Frequency
#E(bnCOL)$color <- "lightgrey"
igraph::edge_attr_names(bnCOL)
igraph::edge_attr(bnCOL)
igraph::vertex.attributes(bnCOL)$name

ProgramsCOL <- data.frame(vertex.names = igraph::vertex.attributes(bnCOL)$name,
                          is_actor = c(rep(TRUE, nrow(Matriz)), rep(FALSE, 10)),
                          node.type = c(rep("Program", nrow(Matriz)), rep("Skill", 10)),
                          Degree = V(bnCOL)$degree,
                          Closeness = V(bnCOL)$closeness,
                          Betweenness = V(bnCOL)$betweenness,
                          Eigenvector = V(bnCOL)$Eigenvector,
                          Program = c(COLTexts$Program, rep(NA, 10)),
                          Brochure.Length = c(COLTexts$Tokens, rep(NA, 10)))


library(network)
Colombia <- as.network(edges_args, 
                    directed = FALSE, 
                    bipartite = TRUE, 
                    bipartite_col = "is_actor", 
                    vertices = ProgramsCOL)
class(Colombia)
Colombia
network::list.edge.attributes(Colombia)
network::delete.edge.attribute(Colombia, "na")
network::list.edge.attributes(Colombia)

print(summary(network::get.edge.attribute(Colombia, "Frequency")))

get.edge.attribute(Colombia, "Frequency")

SizeARG <- network::network.size(Colombia)
DensityARG <- network::network.density(Colombia)
ClusteringARG <- tnet::reinforcement_tm(t(Matriz))
# también podría usar C4 como indicador de clustering
# llamado como "reinforcing"
set.network.attribute(Colombia, "Size", SizeARG)
set.network.attribute(Colombia, "Density", DensityARG)
set.network.attribute(Colombia, "Clustering", ClusteringARG)
set.network.attribute(Colombia, "Country", "Colombia")
set.network.attribute(Colombia, "Level", "All")
set.network.attribute(Colombia, "OECD", TRUE)

network::get.vertex.attribute(Colombia, "vertex.names")
network::get.vertex.attribute(Colombia, "Program")
network::get.vertex.attribute(Colombia, "Brochure.Length")

Colombia
rm(list=setdiff(ls(), c("Colombia")))
saveRDS(Colombia, file = "NetworkData/Colombia.RDS")
