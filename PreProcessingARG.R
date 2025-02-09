load("~/Documents/GitHub/SoftSkillsLatam/Matrices/MatrizARG.RData")
rm(list=setdiff(ls(), c("Matriz", "ARGTexts")))
library(igraph)
bnARG <- graph_from_biadjacency_matrix(Matriz, directed = FALSE, weighted = TRUE)
V(bnARG)$type <- bipartite_mapping(bnARG)$type
V(bnARG)$shape <- ifelse(V(bnARG)$type, "circle", "square")
V(bnARG)$color <- ifelse(V(bnARG)$type, "red", "blue4")
V(bnARG)$degree <- igraph::degree(bnARG)
V(bnARG)$closeness <- igraph::closeness(bnARG)
V(bnARG)$betweenness <- igraph::betweenness(bnARG)
V(bnARG)$Eigenvector <- igraph::eigen_centrality(bnARG)$vector
Edgelist <- data.frame(as_long_data_frame(bnARG), stringsAsFactors = FALSE)
Edgelist$from <- Edgelist$from_name
Edgelist$to <- Edgelist$to_name
Edgelist <- Edgelist[1:3]

ProgramsARG <- data.frame(vertex.names = V(bnARG)$name,
                          is_actor = c(rep(TRUE, 514), rep(FALSE, 10)),
                          node.type = c(rep("Program", 514), rep("Skill", 10)),
                          Degree = V(bnARG)$degree,
                          Closeness = V(bnARG)$closeness,
                          Betweenness = V(bnARG)$betweenness,
                          Eigenvector = V(bnARG)$Eigenvector,
                          Program = c(ARGTexts$Program, rep(NA, 10)),
                          Brochure.Length = c(ARGTexts$Tokens, rep(NA, 10)))

library(network)
Argentina <- as.network(Matriz,
                   loops = FALSE,
                   directed = FALSE,
                   bipartite = TRUE,  # First 3 nodes belong to partition 1
                   vertices = ProgramsARG)

Argentina
vergacion <- network::as.edgelist(Argentina)
network::list.edge.attributes(Argentina)
delete.edge.attribute(Argentina, "na")
set.edge.value(Argentina, "Frequency", Edgelist$weight)
get.edge.attribute(Argentina, "Frequency")
Argentina
network::list.vertex.attributes(Argentina)
delete.vertex.attribute(Argentina, "na")
network::list.vertex.attributes(Argentina)
network::list.edge.attributes(Argentina)


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
