load("~/Documents/GitHub/SoftSkillsLatam/Matrices/MatrizCORI.RData")
rm(list=setdiff(ls(), c("Matriz", "CORITexts")))
library(igraph)
bnCR <- graph_from_biadjacency_matrix(Matriz, directed = FALSE, weighted = TRUE)
V(bnCR)$type <- bipartite_mapping(bnCR)$type
V(bnCR)$shape <- ifelse(V(bnCR)$type, "circle", "square")
V(bnCR)$color <- ifelse(V(bnCR)$type, "red", "blue4")
V(bnCR)$degree <- igraph::degree(bnCR)
V(bnCR)$closeness <- igraph::closeness(bnCR)
V(bnCR)$betweenness <- igraph::betweenness(bnCR)
V(bnCR)$Eigenvector <- igraph::eigen_centrality(bnCR)$vector
Edgelist <- data.frame(as_long_data_frame(bnCR), stringsAsFactors = FALSE)
Edgelist$from <- Edgelist$from_name
Edgelist$to <- Edgelist$to_name
Edgelist <- Edgelist[1:3]

ProgramsCR <- data.frame(vertex.names = V(bnCR)$name,
                          is_actor = c(rep(TRUE, nrow(Matriz)), rep(FALSE, 10)),
                          node.type = c(rep("Program", nrow(Matriz)), rep("Skill", 10)),
                          Degree = V(bnCR)$degree,
                          Closeness = V(bnCR)$closeness,
                          Betweenness = V(bnCR)$betweenness,
                          Eigenvector = V(bnCR)$Eigenvector,
                          Program = c(CORITexts$Program, rep(NA, 10)),
                          Brochure.Length = c(CORITexts$Tokens, rep(NA, 10)))

library(network)
CostaRica <- as.network(Matriz,
                        loops = FALSE,
                        directed = FALSE,
                        bipartite = TRUE,
                        vertices = ProgramsCR)
CostaRica
network::list.vertex.attributes(CostaRica)
#delete.vertex.attribute(CostaRica, "na")
network::list.edge.attributes(CostaRica)
#delete.edge.attribute(CostaRica, "na")
CostaRica
CostaRica %v% "Degree" <- ProgramsCR$Degree
CostaRica %v% "Eigenvector.centrality" <- ProgramsCR$Eigenvector
CostaRica %v% "Brochure.Length" <-  ProgramsCR$Brochure.Length
CostaRica %v% "Program" <- ProgramsCR$Program
CostaRica %v% "Country" <- "CostaRica"
CostaRica %e% "Freq" <- Edgelist$weight
get.edge.attribute(CostaRica, "Freq")
summary(get.edge.attribute(CostaRica, "Freq"))
summary(Edgelist$weight)

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
