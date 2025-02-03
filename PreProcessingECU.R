load("~/Documents/GitHub/SoftSkillsLatam/Matrices/MatrizECU.RData")
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
bnECU <- graph_from_biadjacency_matrix(Matriz, directed = FALSE)
V(bnECU)$type <- bipartite_mapping(bnECU)$type
V(bnECU)$shape <- ifelse(V(bnECU)$type, "circle", "square")
V(bnECU)$color <- ifelse(V(bnECU)$type, "red", "blue4")
V(bnECU)$degree <- igraph::degree(bnECU)
V(bnECU)$closeness <- igraph::closeness(bnECU)
V(bnECU)$betweenness <- igraph::betweenness(bnECU)
V(bnECU)$Eigenvector <- igraph::eigen_centrality(bnECU)$vector
vertices <- edges_args[edges_args$Frequency >= 1, ]
E(bnECU)$Frequency <- vertices$Frequency
#E(bnECU)$color <- "lightgrey"
igraph::edge_attr_names(bnECU)
igraph::edge_attr(bnECU)
igraph::vertex.attributes(bnECU)$name

ProgramsCR <- data.frame(vertex.names = igraph::vertex.attributes(bnECU)$name,
                         is_actor = c(rep(TRUE, nrow(Matriz)), rep(FALSE, 10)),
                         node.type = c(rep("Program", nrow(Matriz)), rep("Skill", 10)),
                         Degree = V(bnECU)$degree,
                         Closeness = V(bnECU)$closeness,
                         Betweenness = V(bnECU)$betweenness,
                         Eigenvector = V(bnECU)$Eigenvector,
                         Program = c(ECUTexts$Program, rep(NA, 10)),
                         Brochure.Length = c(ECUTexts$Tokens, rep(NA, 10)))


library(network)
Ecuador <- as.network(edges_args, 
                        directed = FALSE, 
                        bipartite = TRUE, 
                        bipartite_col = "is_actor", 
                        vertices = ProgramsCR)
class(Ecuador)
Ecuador
network::list.edge.attributes(Ecuador)
network::delete.edge.attribute(Ecuador, "na")
network::list.edge.attributes(Ecuador)

print(summary(network::get.edge.attribute(Ecuador, "Frequency")))

get.edge.attribute(Ecuador, "Frequency")

SizeARG <- network::network.size(Ecuador)
DensityARG <- network::network.density(Ecuador)
ClusteringARG <- tnet::reinforcement_tm(t(Matriz))
# también podría usar C4 como indicador de clustering
# llamado como "reinforcing"
set.network.attribute(Ecuador, "Size", SizeARG)
set.network.attribute(Ecuador, "Density", DensityARG)
set.network.attribute(Ecuador, "Clustering", ClusteringARG)
set.network.attribute(Ecuador, "Country", "Ecuador")
set.network.attribute(Ecuador, "Level", "All")
set.network.attribute(Ecuador, "OECD", FALSE)

network::get.vertex.attribute(Ecuador, "vertex.names")
network::get.vertex.attribute(Ecuador, "Program")
network::get.vertex.attribute(Ecuador, "Brochure.Length")

Ecuador
rm(list=setdiff(ls(), c("Ecuador")))
saveRDS(Ecuador, file = "NetworkData/Ecuador.RDS")
