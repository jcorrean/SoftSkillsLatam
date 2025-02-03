load("~/Documents/GitHub/SoftSkillsLatam/Matrices/MatrizVEN.RData")
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
bnVEN <- graph_from_biadjacency_matrix(Matriz, directed = FALSE)
V(bnVEN)$type <- bipartite_mapping(bnVEN)$type
V(bnVEN)$shape <- ifelse(V(bnVEN)$type, "circle", "square")
V(bnVEN)$color <- ifelse(V(bnVEN)$type, "red", "blue4")
V(bnVEN)$degree <- igraph::degree(bnVEN)
V(bnVEN)$closeness <- igraph::closeness(bnVEN)
V(bnVEN)$betweenness <- igraph::betweenness(bnVEN)
V(bnVEN)$Eigenvector <- igraph::eigen_centrality(bnVEN)$vector
vertices <- edges_args[edges_args$Frequency >= 1, ]
E(bnVEN)$Frequency <- vertices$Frequency
#E(bnVEN)$color <- "lightgrey"
igraph::edge_attr_names(bnVEN)
igraph::edge_attr(bnVEN)
igraph::vertex.attributes(bnVEN)$name

ProgramsVEN <- data.frame(vertex.names = igraph::vertex.attributes(bnVEN)$name,
                          is_actor = c(rep(TRUE, nrow(Matriz)), rep(FALSE, 10)),
                          node.type = c(rep("Program", nrow(Matriz)), rep("Skill", 10)),
                          Degree = V(bnVEN)$degree,
                          Closeness = V(bnVEN)$closeness,
                          Betweenness = V(bnVEN)$betweenness,
                          Eigenvector = V(bnVEN)$Eigenvector,
                          Program = c(VENTexts$Program, rep(NA, 10)),
                          Brochure.Length = c(VENTexts$Tokens, rep(NA, 10)))


library(network)
Venezuela <- as.network(edges_args, 
                      directed = FALSE, 
                      bipartite = TRUE, 
                      bipartite_col = "is_actor", 
                      vertices = ProgramsVEN)
class(Venezuela)
Venezuela
network::list.edge.attributes(Venezuela)
network::delete.edge.attribute(Venezuela, "na")
network::list.edge.attributes(Venezuela)

print(summary(network::get.edge.attribute(Venezuela, "Frequency")))

get.edge.attribute(Venezuela, "Frequency")

SizeARG <- network::network.size(Venezuela)
DensityARG <- network::network.density(Venezuela)
ClusteringARG <- tnet::reinforcement_tm(t(Matriz))
# también podría usar C4 como indicador de clustering
# llamado como "reinforcing"
set.network.attribute(Venezuela, "Size", SizeARG)
set.network.attribute(Venezuela, "Density", DensityARG)
set.network.attribute(Venezuela, "Clustering", ClusteringARG)
set.network.attribute(Venezuela, "Country", "Venezuela")
set.network.attribute(Venezuela, "Level", "All")
set.network.attribute(Venezuela, "OECD", FALSE)

network::get.vertex.attribute(Venezuela, "vertex.names")
network::get.vertex.attribute(Venezuela, "Program")
network::get.vertex.attribute(Venezuela, "Brochure.Length")

Venezuela
rm(list=setdiff(ls(), c("Venezuela")))
saveRDS(Venezuela, file = "NetworkData/Venezuela.RDS")
