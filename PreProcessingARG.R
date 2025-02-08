load("~/Documents/GitHub/SoftSkillsLatam/Matrices/MatrizARG.RData")
edges_args <- data.frame()
for (i in 1:nrow(Matriz)) {
  for (j in 1:ncol(Matriz)) {
    if (Matriz[i, j] >= 1) { 
      edges_args <- rbind(edges_args, data.frame(
        Program = rownames(Matriz)[i],
        Skill = colnames(Matriz)[j],
        #Program.Skill = Matriz[i, j] > 0,
        Frequency = Matriz[i, j],
        stringsAsFactors = FALSE
      ))
    }
  }
}

library(igraph)
bnARG <- graph_from_biadjacency_matrix(Matriz, directed = FALSE, weighted = TRUE)
V(bnARG)$type <- bipartite_mapping(bnARG)$type
V(bnARG)$shape <- ifelse(V(bnARG)$type, "circle", "square")
V(bnARG)$color <- ifelse(V(bnARG)$type, "red", "blue4")
V(bnARG)$degree <- igraph::degree(bnARG)
V(bnARG)$closeness <- igraph::closeness(bnARG)
V(bnARG)$betweenness <- igraph::betweenness(bnARG)
V(bnARG)$Eigenvector <- igraph::eigen_centrality(bnARG)$vector
#E(bnARG)$color <- "lightgrey"
igraph::edge_attr_names(bnARG)
igraph::edge_attr(bnARG)
NODES <-  data.frame(nodes = 1:length(igraph::vertex.attributes(bnARG)$name))
igraph::edge_density(bnARG)
bnARG
pave <- igraph::as_biadjacency_matrix(bnARG, names = TRUE)
pave2 <- igraph::as_edgelist(bnARG, names = FALSE)
pave3 <- as.matrix(igraph::as_adjacency_matrix(bnARG))
pave4 <- as_long_data_frame(bnARG)

ProgramsARG <- data.frame(vertex.names = NODES$nodes,
                          is_actor = c(rep(TRUE, 514), rep(FALSE, 10)),
                          node.type = c(rep("Program", 514), rep("Skill", 10)),
                          Degree = V(bnARG)$degree,
                          Closeness = V(bnARG)$closeness,
                          Betweenness = V(bnARG)$betweenness,
                          Eigenvector = V(bnARG)$Eigenvector,
                          Program = c(ARGTexts$Program, rep(NA, 10)),
                          Brochure.Length = c(ARGTexts$Tokens, rep(NA, 10)))
ProgramsARG$is_actor[ProgramsARG$Degree == 0] <- FALSE
length(ProgramsARG$is_actor[ProgramsARG$Degree > 0])


library(network)
Argentina <- network(pave, 
                 loops = FALSE, 
                 directed = FALSE, 
                 bipartite = TRUE)
Argentina
network::list.edge.attributes(Argentina)
network::list.vertex.attributes(Argentina)
get.vertex.attribute(Argentina, "vertex.names")

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

network::get.vertex.attribute(Argentina, "vertex.names")
network::get.vertex.attribute(Argentina, "Program")
network::get.vertex.attribute(Argentina, "Brochure.Length")

Argentina
rm(list=setdiff(ls(), c("Argentina")))
saveRDS(Argentina, file = "NetworkData/Argentina.RDS")
