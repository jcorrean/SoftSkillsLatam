load("~/Documents/GitHub/SoftSkillsLatam/Matrices/MatrizBRA.RData")
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
bnBRA <- graph_from_biadjacency_matrix(Matriz, directed = FALSE)
V(bnBRA)$type <- bipartite_mapping(bnBRA)$type
V(bnBRA)$shape <- ifelse(V(bnBRA)$type, "circle", "square")
V(bnBRA)$color <- ifelse(V(bnBRA)$type, "red", "blue4")
V(bnBRA)$degree <- igraph::degree(bnBRA)
V(bnBRA)$closeness <- igraph::closeness(bnBRA)
V(bnBRA)$betweenness <- igraph::betweenness(bnBRA)
V(bnBRA)$Eigenvector <- igraph::eigen_centrality(bnBRA)$vector
vertices <- edges_args[edges_args$Frequency >= 1, ]
E(bnBRA)$Frequency <- vertices$Frequency
#E(bnBRA)$color <- "lightgrey"
igraph::edge_attr_names(bnBRA)
igraph::edge_attr(bnBRA)
igraph::vertex.attributes(bnBRA)$name

ProgramsBRA <- data.frame(vertex.names = igraph::vertex.attributes(bnBRA)$name,
                          is_actor = c(rep(TRUE, nrow(Matriz)), rep(FALSE, 10)),
                          node.type = c(rep("Program", nrow(Matriz)), rep("Skill", 10)),
                          Degree = V(bnBRA)$degree,
                          Closeness = V(bnBRA)$closeness,
                          Betweenness = V(bnBRA)$betweenness,
                          Eigenvector = V(bnBRA)$Eigenvector,
                          Program = c(BRATexts$Program, rep(NA, 10)),
                          Brochure.Length = c(BRATexts$Tokens, rep(NA, 10)))


library(network)
Brazil <- as.network(edges_args, 
                        directed = FALSE, 
                        bipartite = TRUE, 
                        bipartite_col = "is_actor", 
                        vertices = ProgramsBRA)
class(Brazil)
Brazil
network::list.edge.attributes(Brazil)
network::delete.edge.attribute(Brazil, "na")
network::list.edge.attributes(Brazil)

print(summary(network::get.edge.attribute(Brazil, "Frequency")))

get.edge.attribute(Brazil, "Frequency")

SizeARG <- network::network.size(Brazil)
DensityARG <- network::network.density(Brazil)
ClusteringARG <- tnet::reinforcement_tm(t(Matriz))
# también podría usar C4 como indicador de clustering
# llamado como "reinforcing"
set.network.attribute(Brazil, "Size", SizeARG)
set.network.attribute(Brazil, "Density", DensityARG)
set.network.attribute(Brazil, "Clustering", ClusteringARG)
set.network.attribute(Brazil, "Country", "Brazil")
set.network.attribute(Brazil, "Level", "All")
set.network.attribute(Brazil, "OECD", FALSE)

network::get.vertex.attribute(Brazil, "vertex.names")
network::get.vertex.attribute(Brazil, "Program")
network::get.vertex.attribute(Brazil, "Brochure.Length")

Brazil
rm(list=setdiff(ls(), c("Brazil")))
saveRDS(Brazil, file = "NetworkData/Brazil.RDS")
