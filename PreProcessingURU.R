load("~/Documents/GitHub/SoftSkillsLatam/Matrices/MatrizURU.RData")
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
bnURU <- graph_from_biadjacency_matrix(Matriz, directed = FALSE)
V(bnURU)$type <- bipartite_mapping(bnURU)$type
V(bnURU)$shape <- ifelse(V(bnURU)$type, "circle", "square")
V(bnURU)$color <- ifelse(V(bnURU)$type, "red", "blue4")
V(bnURU)$degree <- igraph::degree(bnURU)
V(bnURU)$closeness <- igraph::closeness(bnURU)
V(bnURU)$betweenness <- igraph::betweenness(bnURU)
V(bnURU)$Eigenvector <- igraph::eigen_centrality(bnURU)$vector
vertices <- edges_args[edges_args$Frequency >= 1, ]
E(bnURU)$Frequency <- vertices$Frequency
#E(bnURU)$color <- "lightgrey"
igraph::edge_attr_names(bnURU)
igraph::edge_attr(bnURU)
igraph::vertex.attributes(bnURU)$name

ProgramsURU <- data.frame(vertex.names = igraph::vertex.attributes(bnURU)$name,
                          is_actor = c(rep(TRUE, nrow(Matriz)), rep(FALSE, 10)),
                          node.type = c(rep("Program", nrow(Matriz)), rep("Skill", 10)),
                          Degree = V(bnURU)$degree,
                          Closeness = V(bnURU)$closeness,
                          Betweenness = V(bnURU)$betweenness,
                          Eigenvector = V(bnURU)$Eigenvector,
                          Program = c(URUTexts$Program, rep(NA, 10)),
                          Brochure.Length = c(URUTexts$Tokens, rep(NA, 10)))


library(network)
Uruguay <- as.network(edges_args, 
                     directed = FALSE, 
                     bipartite = TRUE, 
                     bipartite_col = "is_actor", 
                     vertices = ProgramsURU)
class(Uruguay)
Uruguay
network::list.edge.attributes(Uruguay)
network::list.edge.attributes(Uruguay)

print(summary(network::get.edge.attribute(Uruguay, "Frequency")))

get.edge.attribute(Uruguay, "Frequency")

SizeARG <- network::network.size(Uruguay)
DensityARG <- network::network.density(Uruguay)
ClusteringARG <- tnet::reinforcement_tm(t(Matriz))
# también podría usar C4 como indicador de clustering
# llamado como "reinforcing"
set.network.attribute(Uruguay, "Size", SizeARG)
set.network.attribute(Uruguay, "Density", DensityARG)
set.network.attribute(Uruguay, "Clustering", ClusteringARG)
set.network.attribute(Uruguay, "Country", "Uruguay")
set.network.attribute(Uruguay, "Level", "All")
set.network.attribute(Uruguay, "OECD", FALSE)

network::get.vertex.attribute(Uruguay, "vertex.names")
network::get.vertex.attribute(Uruguay, "Program")
network::get.vertex.attribute(Uruguay, "Brochure.Length")

Uruguay
rm(list=setdiff(ls(), c("Uruguay")))
saveRDS(Uruguay, file = "NetworkData/Uruguay.RDS")
