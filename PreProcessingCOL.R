load("~/Documents/GitHub/SoftSkillsLatam/Matrices/MatrizCOL.RData")
rm(list=setdiff(ls(), c("Matriz", "COLTexts")))
library(igraph)
bnCOL <- graph_from_biadjacency_matrix(Matriz, directed = FALSE, weighted = TRUE)
V(bnCOL)$type <- bipartite_mapping(bnCOL)$type
V(bnCOL)$shape <- ifelse(V(bnCOL)$type, "circle", "square")
V(bnCOL)$color <- ifelse(V(bnCOL)$type, "red", "blue4")
V(bnCOL)$degree <- igraph::degree(bnCOL)
V(bnCOL)$closeness <- igraph::closeness(bnCOL)
V(bnCOL)$betweenness <- igraph::betweenness(bnCOL)
V(bnCOL)$Eigenvector <- igraph::eigen_centrality(bnCOL)$vector
Edgelist <- data.frame(as_long_data_frame(bnCOL), stringsAsFactors = FALSE)
Edgelist$from <- Edgelist$from_name
Edgelist$to <- Edgelist$to_name
Edgelist <- Edgelist[1:3]

ProgramsCOL <- data.frame(vertex.names = V(bnCOL)$name,
                          is_actor = c(rep(TRUE, nrow(Matriz)), rep(FALSE, 10)),
                          node.type = c(rep("Program", nrow(Matriz)), rep("Skill", 10)),
                          Degree = V(bnCOL)$degree,
                          Closeness = V(bnCOL)$closeness,
                          Betweenness = V(bnCOL)$betweenness,
                          Eigenvector = V(bnCOL)$Eigenvector,
                          Program = c(COLTexts$Program, rep(NA, 10)),
                          Brochure.Length = c(COLTexts$Tokens, rep(NA, 10)))

library(network)
Colombia <- as.network(Matriz,
                       loops = FALSE,
                       directed = FALSE,
                       bipartite = TRUE,
                       vertices = ProgramsCOL)
Colombia
network::list.vertex.attributes(Colombia)
#delete.vertex.attribute(Colombia, "na")
network::list.edge.attributes(Colombia)
#delete.edge.attribute(Colombia, "na")
Colombia
Colombia %v% "Degree" <- ProgramsCOL$Degree
Colombia %v% "Eigenvector.centrality" <- ProgramsCOL$Eigenvector
Colombia %v% "Brochure.Length" <-  ProgramsCOL$Brochure.Length
Colombia %v% "Program" <- ProgramsCOL$Program
Colombia %e% "Freq" <- Edgelist$weight
get.edge.attribute(Colombia, "Freq")
summary(get.edge.attribute(Colombia, "Freq"))
summary(Edgelist$weight)

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
