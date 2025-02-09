load("~/Documents/GitHub/SoftSkillsLatam/Matrices/MatrizURU.RData")
rm(list=setdiff(ls(), c("Matriz", "URUTexts")))
library(igraph)
bnURU <- graph_from_biadjacency_matrix(Matriz, directed = FALSE, weighted = TRUE)
V(bnURU)$type <- bipartite_mapping(bnURU)$type
V(bnURU)$shape <- ifelse(V(bnURU)$type, "circle", "square")
V(bnURU)$color <- ifelse(V(bnURU)$type, "red", "blue4")
V(bnURU)$degree <- igraph::degree(bnURU)
V(bnURU)$closeness <- igraph::closeness(bnURU)
V(bnURU)$betweenness <- igraph::betweenness(bnURU)
V(bnURU)$Eigenvector <- igraph::eigen_centrality(bnURU)$vector
Edgelist <- data.frame(as_long_data_frame(bnURU), stringsAsFactors = FALSE)
Edgelist$from <- Edgelist$from_name
Edgelist$to <- Edgelist$to_name
Edgelist <- Edgelist[1:3]

ProgramsURU <- data.frame(vertex.names = V(bnURU)$name,
                          is_actor = c(rep(TRUE, nrow(Matriz)), rep(FALSE, 10)),
                          node.type = c(rep("Program", nrow(Matriz)), rep("Skill", 10)),
                          Degree = V(bnURU)$degree,
                          Closeness = V(bnURU)$closeness,
                          Betweenness = V(bnURU)$betweenness,
                          Eigenvector = V(bnURU)$Eigenvector,
                          Program = c(URUTexts$Program, rep(NA, 10)),
                          Brochure.Length = c(URUTexts$Tokens, rep(NA, 10)))


library(network)
Uruguay <- as.network(Matriz,
                      loops = FALSE,
                      directed = FALSE,
                      bipartite = TRUE,
                     vertices = ProgramsURU)
Uruguay
network::list.vertex.attributes(Uruguay)
delete.vertex.attribute(Uruguay, "na")
network::list.edge.attributes(Uruguay)
delete.edge.attribute(Uruguay, "na")
Uruguay
Uruguay %v% "Degree" <- ProgramsURU$Degree
Uruguay %v% "Eigenvector.centrality" <- ProgramsURU$Eigenvector
Uruguay %v% "Brochure.Length" <-  ProgramsURU$Brochure.Length
Uruguay %v% "Program" <- ProgramsURU$Program
Uruguay %e% "Freq" <- Edgelist$weight
get.edge.attribute(Uruguay, "Freq")
summary(get.edge.attribute(Uruguay, "Freq"))
summary(Edgelist$weight)

Uruguay

network::list.vertex.attributes(Uruguay)
get.vertex.attribute(Uruguay, "vertex.names")
network::list.edge.attributes(Uruguay)

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
