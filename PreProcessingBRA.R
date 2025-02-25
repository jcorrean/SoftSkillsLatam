load("~/Documents/GitHub/SoftSkillsLatam/Matrices/MatrizBRA.RData")
rm(list=setdiff(ls(), c("Matriz", "BRATexts")))
library(igraph)
bnBRA <- graph_from_biadjacency_matrix(Matriz, directed = FALSE, weighted = TRUE)
V(bnBRA)$type <- bipartite_mapping(bnBRA)$type
V(bnBRA)$shape <- ifelse(V(bnBRA)$type, "circle", "square")
V(bnBRA)$color <- ifelse(V(bnBRA)$type, "red", "blue4")
V(bnBRA)$degree <- igraph::degree(bnBRA)
V(bnBRA)$closeness <- igraph::closeness(bnBRA)
V(bnBRA)$betweenness <- igraph::betweenness(bnBRA)
V(bnBRA)$Eigenvector <- igraph::eigen_centrality(bnBRA)$vector
Edgelist <- data.frame(as_long_data_frame(bnBRA), stringsAsFactors = FALSE)
Edgelist$from <- Edgelist$from_name
Edgelist$to <- Edgelist$to_name
Edgelist <- Edgelist[1:3]

ProgramsBRA <- data.frame(vertex.names = V(bnBRA)$name,
                          is_actor = c(rep(TRUE, nrow(Matriz)), rep(FALSE, 10)),
                          node.type = c(rep("Program", nrow(Matriz)), rep("Skill", 10)),
                          Degree = V(bnBRA)$degree,
                          Closeness = V(bnBRA)$closeness,
                          Betweenness = V(bnBRA)$betweenness,
                          Eigenvector = V(bnBRA)$Eigenvector,
                          Program = c(BRATexts$Program, rep(NA, 10)),
                          Brochure.Length = c(BRATexts$Tokens, rep(NA, 10)))


library(network)
Brazil <- as.network(Matriz,
                     loops = FALSE,
                     directed = FALSE,
                     bipartite = TRUE,
                     vertices = ProgramsBRA)
Brazil
network::list.vertex.attributes(Brazil)
#delete.vertex.attribute(Brazil, "na")
network::list.edge.attributes(Brazil)
#delete.edge.attribute(Brazil, "na")
Brazil
Brazil %v% "Degree" <- ProgramsBRA$Degree
Brazil %v% "Eigenvector.centrality" <- ProgramsBRA$Eigenvector
Brazil %v% "Brochure.Length" <-  ProgramsBRA$Brochure.Length
Brazil %v% "Program" <- ProgramsBRA$Program
Brazil %e% "Freq" <- Edgelist$weight
network::get.edge.attribute(Brazil, "Freq")
summary(network::get.edge.attribute(Brazil, "Freq"))
summary(Edgelist$weight)

Brazil

network::list.vertex.attributes(Brazil)
network::get.vertex.attribute(Brazil, "vertex.names")
network::list.edge.attributes(Brazil)

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
