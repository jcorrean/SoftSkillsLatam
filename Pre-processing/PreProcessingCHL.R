load("~/Documents/GitHub/SoftSkillsLatam/Matrices/MatrizCHL.RData")
rm(list=setdiff(ls(), c("Matriz", "CHLTexts")))
library(igraph)
bnCHL <- graph_from_biadjacency_matrix(Matriz, directed = FALSE, weighted = TRUE)
V(bnCHL)$type <- bipartite_mapping(bnCHL)$type
V(bnCHL)$shape <- ifelse(V(bnCHL)$type, "circle", "square")
V(bnCHL)$color <- ifelse(V(bnCHL)$type, "red", "blue4")
V(bnCHL)$degree <- igraph::degree(bnCHL)
V(bnCHL)$closeness <- igraph::closeness(bnCHL)
V(bnCHL)$betweenness <- igraph::betweenness(bnCHL)
V(bnCHL)$Eigenvector <- igraph::eigen_centrality(bnCHL)$vector
Edgelist <- data.frame(as_long_data_frame(bnCHL), stringsAsFactors = FALSE)
Edgelist$from <- Edgelist$from_name
Edgelist$to <- Edgelist$to_name
Edgelist <- Edgelist[1:3]

ProgramsCHL <- data.frame(vertex.names = V(bnCHL)$name,
                          is_actor = c(rep(TRUE, nrow(Matriz)), rep(FALSE, 10)),
                          node.type = c(rep("Program", nrow(Matriz)), rep("Skill", 10)),
                          Degree = V(bnCHL)$degree,
                          Closeness = V(bnCHL)$closeness,
                          Betweenness = V(bnCHL)$betweenness,
                          Eigenvector = V(bnCHL)$Eigenvector,
                          Program = c(CHLTexts$Program, rep(NA, 10)),
                          Brochure.Length = c(CHLTexts$Tokens, rep(NA, 10)))

library(network)
Chile <- as.network(Matriz,
                    loops = FALSE,
                    directed = FALSE,
                    bipartite = TRUE,
                    vertices = ProgramsCHL)
Chile
network::list.vertex.attributes(Chile)
#delete.vertex.attribute(Chile, "na")
network::list.edge.attributes(Chile)
#delete.edge.attribute(Chile, "na")
Chile
Chile %v% "Degree" <- ProgramsCHL$Degree
Chile %v% "Eigenvector.centrality" <- ProgramsCHL$Eigenvector
Chile %v% "Brochure.Length" <-  ProgramsCHL$Brochure.Length
Chile %v% "Program" <- ProgramsCHL$Program
Chile %v% "Country" <- "Chile"
Chile %e% "Freq" <- Edgelist$weight
get.edge.attribute(Chile, "Freq")
summary(get.edge.attribute(Chile, "Freq"))
summary(Edgelist$weight)


SizeARG <- network::network.size(Chile)
DensityARG <- network::network.density(Chile)
ClusteringARG <- tnet::reinforcement_tm(t(Matriz))
# también podría usar C4 como indicador de clustering
# llamado como "reinforcing"
set.network.attribute(Chile, "Size", SizeARG)
set.network.attribute(Chile, "Density", DensityARG)
set.network.attribute(Chile, "Clustering", ClusteringARG)
set.network.attribute(Chile, "Country", "Chile")
set.network.attribute(Chile, "Level", "All")
set.network.attribute(Chile, "OECD", TRUE)

network::get.vertex.attribute(Chile, "vertex.names")
network::get.vertex.attribute(Chile, "Program")
network::get.vertex.attribute(Chile, "Brochure.Length")

Chile
SkillsCHL <- ProgramsCHL %>% tail(., n =10)
SkillsCHL$country <- "Chile"
save(SkillsCHL, file = "Curated_Data/SkillsCHL.RData")
rm(list=setdiff(ls(), c("Chile")))
saveRDS(Chile, file = "NetworkData/Chile.RDS")
