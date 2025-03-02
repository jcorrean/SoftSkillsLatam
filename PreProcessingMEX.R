load("~/Documents/GitHub/SoftSkillsLatam/Matrices/MatrizMEX.RData")
rm(list=setdiff(ls(), c("Matriz", "MEXTexts")))
library(igraph)
bnMEX <- graph_from_biadjacency_matrix(Matriz, directed = FALSE, weighted = TRUE)
V(bnMEX)$type <- bipartite_mapping(bnMEX)$type
V(bnMEX)$shape <- ifelse(V(bnMEX)$type, "circle", "square")
V(bnMEX)$color <- ifelse(V(bnMEX)$type, "red", "blue4")
V(bnMEX)$degree <- igraph::degree(bnMEX)
V(bnMEX)$closeness <- igraph::closeness(bnMEX)
V(bnMEX)$betweenness <- igraph::betweenness(bnMEX)
V(bnMEX)$Eigenvector <- igraph::eigen_centrality(bnMEX)$vector
Edgelist <- data.frame(as_long_data_frame(bnMEX), stringsAsFactors = FALSE)
Edgelist$from <- Edgelist$from_name
Edgelist$to <- Edgelist$to_name
Edgelist <- Edgelist[1:3]

ProgramsMEX <- data.frame(vertex.names = V(bnMEX)$name,
                         is_actor = c(rep(TRUE, nrow(Matriz)), rep(FALSE, 10)),
                         node.type = c(rep("Program", nrow(Matriz)), rep("Skill", 10)),
                         Degree = V(bnMEX)$degree,
                         Closeness = V(bnMEX)$closeness,
                         Betweenness = V(bnMEX)$betweenness,
                         Eigenvector = V(bnMEX)$Eigenvector,
                         Program = c(MEXTexts$Program, rep(NA, 10)),
                         Brochure.Length = c(MEXTexts$Tokens, rep(NA, 10)))

library(network)
Mexico <- as.network(Matriz,
                     loops = FALSE,
                     directed = FALSE,
                     bipartite = TRUE,
                     vertices = ProgramsMEX)
Mexico
network::list.vertex.attributes(Mexico)
#delete.vertex.attribute(Mexico, "na")
network::list.edge.attributes(Mexico)
#delete.edge.attribute(Mexico, "na")
Mexico
Mexico %v% "Degree" <- ProgramsMEX$Degree
Mexico %v% "Eigenvector.centrality" <- ProgramsMEX$Eigenvector
Mexico %v% "Brochure.Length" <-  ProgramsMEX$Brochure.Length
Mexico %v% "Program" <- ProgramsMEX$Program
Mexico %v% "Country" <- "Mexico"
Mexico %e% "Freq" <- Edgelist$weight
get.edge.attribute(Mexico, "Freq")
summary(get.edge.attribute(Mexico, "Freq"))
summary(Edgelist$weight)

SizeARG <- network::network.size(Mexico)
DensityARG <- network::network.density(Mexico)
ClusteringARG <- tnet::reinforcement_tm(t(Matriz))
# también podría usar C4 como indicador de clustering
# llamado como "reinforcing"
set.network.attribute(Mexico, "Size", SizeARG)
set.network.attribute(Mexico, "Density", DensityARG)
set.network.attribute(Mexico, "Clustering", ClusteringARG)
set.network.attribute(Mexico, "Country", "Mexico")
set.network.attribute(Mexico, "Level", "All")
set.network.attribute(Mexico, "OECD", TRUE)

network::get.vertex.attribute(Mexico, "vertex.names")
network::get.vertex.attribute(Mexico, "Program")
network::get.vertex.attribute(Mexico, "Brochure.Length")

Mexico
rm(list=setdiff(ls(), c("Mexico")))
saveRDS(Mexico, file = "NetworkData/Mexico.RDS")

