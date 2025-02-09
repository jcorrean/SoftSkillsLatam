load("~/Documents/GitHub/SoftSkillsLatam/Matrices/MatrizVEN.RData")
rm(list=setdiff(ls(), c("Matriz", "VENTexts")))
library(igraph)
bnVEN <- graph_from_biadjacency_matrix(Matriz, directed = FALSE, weighted = TRUE)
V(bnVEN)$type <- bipartite_mapping(bnVEN)$type
V(bnVEN)$shape <- ifelse(V(bnVEN)$type, "circle", "square")
V(bnVEN)$color <- ifelse(V(bnVEN)$type, "red", "blue4")
V(bnVEN)$degree <- igraph::degree(bnVEN)
V(bnVEN)$closeness <- igraph::closeness(bnVEN)
V(bnVEN)$betweenness <- igraph::betweenness(bnVEN)
V(bnVEN)$Eigenvector <- igraph::eigen_centrality(bnVEN)$vector
Edgelist <- data.frame(as_long_data_frame(bnVEN), stringsAsFactors = FALSE)
Edgelist$from <- Edgelist$from_name
Edgelist$to <- Edgelist$to_name
Edgelist <- Edgelist[1:3]

ProgramsVEN <- data.frame(vertex.names = V(bnVEN)$name,
                          is_actor = c(rep(TRUE, nrow(Matriz)), rep(FALSE, 10)),
                          node.type = c(rep("Program", nrow(Matriz)), rep("Skill", 10)),
                          Degree = V(bnVEN)$degree,
                          Closeness = V(bnVEN)$closeness,
                          Betweenness = V(bnVEN)$betweenness,
                          Eigenvector = V(bnVEN)$Eigenvector,
                          Program = c(VENTexts$Program, rep(NA, 10)),
                          Brochure.Length = c(VENTexts$Tokens, rep(NA, 10)))

library(network)
Venezuela <- as.network(Matriz, 
                        loops = FALSE,
                      directed = FALSE, 
                      bipartite = TRUE,
                      vertices = ProgramsVEN)
Venezuela
network::list.vertex.attributes(Venezuela)
#delete.vertex.attribute(Venezuela, "na")
network::list.edge.attributes(Venezuela)
#delete.edge.attribute(Venezuela, "na")
Venezuela
Venezuela %v% "Degree" <- ProgramsVEN$Degree
Venezuela %v% "Eigenvector.centrality" <- ProgramsVEN$Eigenvector
Venezuela %v% "Brochure.Length" <-  ProgramsVEN$Brochure.Length
Venezuela %v% "Program" <- ProgramsVEN$Program
Venezuela %e% "Freq" <- Edgelist$weight
get.edge.attribute(Venezuela, "Freq")
summary(get.edge.attribute(Venezuela, "Freq"))
summary(Edgelist$weight)

Venezuela

network::list.vertex.attributes(Venezuela)
get.vertex.attribute(Venezuela, "vertex.names")
network::list.edge.attributes(Venezuela)

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
