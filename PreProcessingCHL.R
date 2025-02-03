load("~/Documents/GitHub/SoftSkillsLatam/Matrices/MatrizCHL.RData")
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
bnCHL <- graph_from_biadjacency_matrix(Matriz, directed = FALSE)
V(bnCHL)$type <- bipartite_mapping(bnCHL)$type
V(bnCHL)$shape <- ifelse(V(bnCHL)$type, "circle", "square")
V(bnCHL)$color <- ifelse(V(bnCHL)$type, "red", "blue4")
V(bnCHL)$degree <- igraph::degree(bnCHL)
V(bnCHL)$closeness <- igraph::closeness(bnCHL)
V(bnCHL)$betweenness <- igraph::betweenness(bnCHL)
V(bnCHL)$Eigenvector <- igraph::eigen_centrality(bnCHL)$vector
vertices <- edges_args[edges_args$Frequency >= 1, ]
E(bnCHL)$Frequency <- vertices$Frequency
#E(bnCHL)$color <- "lightgrey"
igraph::edge_attr_names(bnCHL)
igraph::edge_attr(bnCHL)
igraph::vertex.attributes(bnCHL)$name

ProgramsCHL <- data.frame(vertex.names = igraph::vertex.attributes(bnCHL)$name,
                          is_actor = c(rep(TRUE, nrow(Matriz)), rep(FALSE, 10)),
                          node.type = c(rep("Program", nrow(Matriz)), rep("Skill", 10)),
                          Degree = V(bnCHL)$degree,
                          Closeness = V(bnCHL)$closeness,
                          Betweenness = V(bnCHL)$betweenness,
                          Eigenvector = V(bnCHL)$Eigenvector,
                          Program = c(CHLTexts$Program, rep(NA, 10)),
                          Brochure.Length = c(CHLTexts$Tokens, rep(NA, 10)))


library(network)
Chile <- as.network(edges_args, 
                        directed = FALSE, 
                        bipartite = TRUE, 
                        bipartite_col = "is_actor", 
                        vertices = ProgramsCHL)
class(Chile)
Chile
network::list.edge.attributes(Chile)
network::delete.edge.attribute(Chile, "na")
network::list.edge.attributes(Chile)

print(summary(network::get.edge.attribute(Chile, "Frequency")))

get.edge.attribute(Chile, "Frequency")

SizeARG <- network::network.size(Chile)
DensityARG <- network::network.density(Chile)
ClusteringARG <- tnet::reinforcement_tm(t(Matriz))
# también podría usar C4 como indicador de clustering
# llamado como "reinforcing"
set.network.attribute(Chile, "Size", SizeARG)
set.network.attribute(Chile, "Density", DensityARG)
set.network.attribute(Chile, "Clustering", ClusteringARG)
set.network.attribute(Chile, "Country", "Chile")
set.network.attribute(Chile, "Level", "All")
set.network.attribute(Chile, "OECD", TRUE)

network::get.vertex.attribute(Chile, "vertex.names")
network::get.vertex.attribute(Chile, "Program")
network::get.vertex.attribute(Chile, "Brochure.Length")

Chile
rm(list=setdiff(ls(), c("Chile")))
saveRDS(Chile, file = "NetworkData/Chile.RDS")
