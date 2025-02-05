load("~/Documents/GitHub/SoftSkillsLatam/Matrices/MatrizARG.RData")
rm(list=setdiff(ls(), c("Matriz", "ARGTexts")))
# Primero, debo crear la matriz de adyacencia completa
full <- matrix(0, nrow = 524, ncol = 524)
# Vamos a adjudicarle los nombres de filas y columnas
rownames(full) <- c(rownames(Matriz), colnames(Matriz))
colnames(full) <- c(rownames(Matriz), colnames(Matriz))
# Luego, copio la parte superior derecha, así
full[1:514, 515:524] <- Matriz
# Ahora, copio la parte inferior izquierda, así
full[515:524, 1:514] <- t(Matriz)
nrow(full)
ncol(full)
# La matriz "full" tiene 524x524
dim(full)

# Pero sé que hay filas y columnas que debo eliminar
# porque hay programas que no se conectan con habilidades
rows_to_keep <- rowSums(full != 0) > 0
cols_to_keep <- colSums(full != 0) > 0
# De esta manera creo la matriz ajustada
full.adj <- full[rows_to_keep, cols_to_keep]
dim(full.adj)

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


bnARG
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

library(intergraph)
pave <- asNetwork(bnARG)
pave

if (!is.null(V(bnARG)$type)) {
  
  # Convertir a network
  pave <- asNetwork(bnARG)
  
  # Establecer la propiedad de bipartito
  pave %v% "bipartite" <- as.integer(V(bnARG)$type)
  
  # Confirmar que el objeto es bipartito
  pave %n% "bipartite" <- TRUE
}

print(pave)
# Ahora, paso a crear la matriz ajustada

library(network)
Argentina <-network(full.adj,loops=TRUE,directed=FALSE, bipartite = 504,
                    ignore.eval=FALSE,names.eval='Freq')
Argentina
list.edge.attributes(Argentina)
get.edge.attribute(Argentina, "Freq")
get.edgeIDs()
lista <- as.edgelist(Argentina)
lista2 <- as.matrix.network(Argentina, store.eid = TRUE)
lista3 <- tibble::as_tibble(Argentina, store.eid = TRUE)

Argentina <- set.edge.value(Argentina,'Freq',full.adj)
Argentina


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
pave3 <- as.matrix(igraph::as_adjacency_matrix(bnARG))

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
