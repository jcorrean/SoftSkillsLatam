load("~/Documents/GitHub/SoftSkillsLatam/Matriz.RData")
edges_args <- data.frame()
for (i in 1:nrow(Matriz)) {
  for (j in 1:ncol(Matriz)) {
    if (Matriz[i, j] > 0) { 
      edges_args <- rbind(edges_args, data.frame(
        Program = rownames(Matriz)[i],
        Skill = colnames(Matriz)[j],
        Program.Skill = Matriz[i, j] > 0,
        Frequency = Matriz[i, j]
      ))
    }
  }
}

isolated_programs <- which(rowSums(Matriz == 0) == ncol(Matriz))
Matriz[rownames(Matriz) %in% isolated_programs, ]


library(igraph)
bnARG <- graph_from_biadjacency_matrix(Matriz, directed = FALSE)
V(bnARG)$type <- bipartite_mapping(bnARG)$type
V(bnARG)$shape <- ifelse(V(bnARG)$type, "circle", "square")
V(bnARG)$color <- ifelse(V(bnARG)$type, "red", "blue4")
V(bnARG)$degree <- igraph::degree(bnARG)
V(bnARG)$closeness <- igraph::closeness(bnARG)
V(bnARG)$betweenness <- igraph::betweenness(bnARG)
V(bnARG)$Eigenvector <- igraph::eigen_centrality(bnARG)$vector
vertices <- edges_args[edges_args$Frequency > 0, ]
E(bnARG)$Frequency <- vertices$Frequency
#E(bnARG)$color <- "lightgrey"
igraph::edge_attr_names(bnARG)
igraph::edge_attr(bnARG)
igraph::vertex.attributes(bnARG)$name
adj_matrix <- as_adjacency_matrix(bnARG)
str(adj_matrix)

ProgramsARG <- data.frame(vertex.names = igraph::vertex.attributes(bnARG)$name,
                          node.type = c(rep("Program", 514), rep("Skill", 10)),
                          Degree = V(bnARG)$degree,
                          Closeness = V(bnARG)$closeness,
                          Betweenness = V(bnARG)$betweenness,
                          Eigenvector = V(bnARG)$Eigenvector,
                          Program = c(ARGTexts$Program, rep(NA, 10)),
                          Brochure.Length = c(ARGTexts$Tokens, rep(NA, 10)))

library(network)
Argentina <- network::network(
  edges_args,
  directed = FALSE,
  vertices = ProgramsARG,
  bipartite = TRUE,
  matrix.type = "edgelist",
  ignore.eval = FALSE  # Ensure edge attributes are parsed
)
network::set.network.attribute(Argentina, "bipartite", 514)
network::delete.edge.attribute(Argentina, "na")
network::get.network.attribute(Argentina, "bipartite") 
network::list.edge.attributes(Argentina)
Argentina
network::set.network.attribute(Argentina, "bipartite", 514)
network::list.edge.attributes(Argentina)  # Should include "Frequency"

network::set.edge.attribute(Argentina, "Frequency", as.numeric(edges_args$Frequency))
network::set.edge.attribute(Argentina, "Program.Skill", as.logical(edges_args$Program.Skill))
if ("na" %in% network::list.edge.attributes(Argentina)) {
  network::delete.edge.attribute(Argentina, "na")
}

print(network::list.edge.attributes(Argentina))
print(summary(network::get.edge.attribute(Argentina, "Frequency")))
Argentina

network::list.edge.attributes(Argentina)

network::set.edge.attribute(Argentina, "Frequency", as.numeric(edges_args$Frequency))
network::delete.edge.attribute(Argentina, "na")
print(summary(network::get.edge.attribute(Argentina, "Frequency")))

Argentina

print(dim(EDGES))  # Should show (1574, 3) with "actor", "event", "Frequency"
print(head(EDGES))

get.edge.attribute(Argentina, "Frequency")

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
Program <- c(ARGTexts$Program, rep(NA, ncol(Matriz)))
BrochureLength <- c(ARGTexts$Tokens, rep(NA, ncol(Matriz)))
network::set.vertex.attribute(Argentina, "Program", Program)
network::set.vertex.attribute(Argentina, "Brochure.Length", BrochureLength)

network::get.vertex.attribute(Argentina, "vertex.names")
network::get.vertex.attribute(Argentina, "Program")
network::get.vertex.attribute(Argentina, "Brochure.Length")

Argentina
network::set.edge.attribute(Argentina, "Frequency", EDGES$Frequency)
print(network::get.edge.attribute(Argentina, "Frequency"))
