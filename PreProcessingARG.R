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
igraph::vertex.attributes(bnARG)$name
igraph::edge_density(bnARG)
pave <- igraph::as_biadjacency_matrix(bnARG, names = TRUE)
pave2 <- igraph::as_edgelist(bnARG, names = FALSE)
pave3 <- as.matrix(igraph::as_adjacency_matrix(bnARG, names = TRUE))

V(bnARG)$name
ProgramsARG <- data.frame(vertex.names = igraph::vertex.attributes(bnARG)$name,
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
Messi <- network(pave3, 
                 loops = TRUE, 
                 directed = FALSE, 
                 bipartite = 514,
                 vertices = ProgramsARG)
Messi
edge_ids <- as.matrix.network.incidence(Messi)
edges <- as.matrix.network.edgelist(Messi)

network::list.edge.attributes(valued)

as.matrix(valued, attrname = "Frequency")

Messi %e% 

Messi <- network.initialize(igraph::vcount(bnARG),
                            directed = FALSE,
                            hyper = FALSE,
                            loops = FALSE,
                            multiple = FALSE,
                            bipartite = length(ProgramsARG$is_actor[ProgramsARG$Degree > 0])-10
)
Messi
valuedNet <- set.edge.value(Matriz, "Frequency")
set.edge.value(Messi, "Frequency", pave3)
network::get.edgeIDs(Messi, 1)
network::set.edge.value(Messi, value = E(bnARG)$Frequency, e = pave2, v = V(bnARG)$name)

Verga <-network.bipartite(pave2, 
                          Messi, 
                          ignore.eval = FALSE,
                          names.eval = "Frequency")

Argentina <- as.network(edges_args,
                        matrix.type = "bipartite",
                        directed = FALSE, 
                        bipartite = TRUE, 
                        bipartite_col = "is_actor", 
                        vertices = ProgramsARG,
                        ignore.eval = FALSE)
class(Argentina)
Argentina
network::set.edge.attribute(Argentina, "Frequency", edges_args$Frequency)
Argentina
network::list.edge.attributes(Argentina)
print(summary(network::get.edge.attribute(Argentina, "Frequency")))

network::get.edge.attribute(Argentina, "Frequency")

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
