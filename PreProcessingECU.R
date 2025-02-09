load("~/Documents/GitHub/SoftSkillsLatam/Matrices/MatrizECU.RData")
rm(list=setdiff(ls(), c("Matriz", "ECUTexts")))
library(igraph)
bnECU <- graph_from_biadjacency_matrix(Matriz, directed = FALSE, weighted = TRUE)
V(bnECU)$type <- bipartite_mapping(bnECU)$type
V(bnECU)$shape <- ifelse(V(bnECU)$type, "circle", "square")
V(bnECU)$color <- ifelse(V(bnECU)$type, "red", "blue4")
V(bnECU)$degree <- igraph::degree(bnECU)
V(bnECU)$closeness <- igraph::closeness(bnECU)
V(bnECU)$betweenness <- igraph::betweenness(bnECU)
V(bnECU)$Eigenvector <- igraph::eigen_centrality(bnECU)$vector
Edgelist <- data.frame(as_long_data_frame(bnECU), stringsAsFactors = FALSE)
Edgelist$from <- Edgelist$from_name
Edgelist$to <- Edgelist$to_name
Edgelist <- Edgelist[1:3]

ProgramsECU <- data.frame(vertex.names = V(bnECU)$name,
                         is_actor = c(rep(TRUE, nrow(Matriz)), rep(FALSE, 10)),
                         node.type = c(rep("Program", nrow(Matriz)), rep("Skill", 10)),
                         Degree = V(bnECU)$degree,
                         Closeness = V(bnECU)$closeness,
                         Betweenness = V(bnECU)$betweenness,
                         Eigenvector = V(bnECU)$Eigenvector,
                         Program = c(ECUTexts$Program, rep(NA, 10)),
                         Brochure.Length = c(ECUTexts$Tokens, rep(NA, 10)))


library(network)
Ecuador <- as.network(Matriz,
                      loops = FALSE,
                      directed = FALSE,
                      bipartite = TRUE,
                      vertices = ProgramsECU)
Ecuador
network::list.vertex.attributes(Ecuador)
#delete.vertex.attribute(Ecuador, "na")
network::list.edge.attributes(Ecuador)
#delete.edge.attribute(Ecuador, "na")
Ecuador
Ecuador %v% "Degree" <- ProgramsECU$Degree
Ecuador %v% "Eigenvector.centrality" <- ProgramsECU$Eigenvector
Ecuador %v% "Brochure.Length" <-  ProgramsECU$Brochure.Length
Ecuador %v% "Program" <- ProgramsECU$Program
Ecuador %e% "Freq" <- Edgelist$weight
get.edge.attribute(Ecuador, "Freq")
summary(get.edge.attribute(Ecuador, "Freq"))
summary(Edgelist$weight)

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
