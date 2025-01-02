library(readtext)
MEX <- readtext("Mexico")
MEX$doc_id <- gsub("\\.pdf$|\\.docx$", "", MEX$doc_id)

library(dplyr)
MEX <- mutate(MEX, 
              Program = ifelse(
                grepl("Doctorado", text), "Doctorado",
                ifelse(grepl("Maestría|Magíster en|MAGISTER EN", text), "Maestría", 
                       "Especialización")))
Programas <- data.frame(table(MEX$Program))
colnames(Programas)[1] <- "Programa" 
colnames(Programas)[2] <- "Total"

library(tidyverse)
MEX <- MEX %>%
  mutate(University.Code = str_extract(doc_id, "^\\d+"))

library(quanteda)
TextsMEX <- corpus(MEX$text)
docvars(TextsMEX, "Program") <- MEX$Program
docvars(TextsMEX, "Country") <- "Mexico"
head(summary(TextsMEX), 10)
MEXTexts <- data.frame(summary(TextsMEX, length(TextsMEX)))
MEXSpec <- corpus_subset(TextsMEX, Program == "Especialización")
MEXMS <- corpus_subset(TextsMEX, Program == "Maestría")
MEXPhD <- corpus_subset(TextsMEX, Program == "Doctorado")

Dictionary <- dictionary(list(
  active_listening = c("escucha*", "pregunta*", "cuestiona*", "entend*", "comprend*", "silencio"),
  mathematics = c("matemática", "resolver problemas matemáticos", "cálculos", "calcular"),
  reading_comprehension = c("lectura", "leer", "oraciones", "párrafo*", "textos", "documento*"),
  science = c("ciencia*", "científic*", "método*", "resolver problemas", "investiga*"),
  speaking = c("oratoria", "comunica*"),
  writing = c("escrib*", "redact*", "escrito"),
  active_learning = c("implicaciones", "comprende", "decisión", "futur*", "nueva información"),
  critical_thinking = c("crítico", "pensamiento crítico", "lógic*", "razona*"),
  learning_strategy = c("aprend*", "enseñ*", "instrucci*"),
  monitoring = c("auto-evalu*",  "reflexi*", "desempeñ", "ejecución")
))
ProgramsMEX <- tokens(TextsMEX, 
                      remove_numbers = TRUE, 
                      remove_punct = TRUE, 
                      remove_url = TRUE, 
                      remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

MEX_Spec <- tokens(MEXSpec, 
                   remove_numbers = TRUE, 
                   remove_punct = TRUE, 
                   remove_url = TRUE, 
                   remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

MEX_MS <- tokens(MEXMS, 
                 remove_numbers = TRUE, 
                 remove_punct = TRUE, 
                 remove_url = TRUE, 
                 remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

MEX_PhD <- tokens(MEXPhD, 
                  remove_numbers = TRUE, 
                  remove_punct = TRUE, 
                  remove_url = TRUE, 
                  remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

MatrizMEXSPEC <- as.matrix(t(MEX_Spec))
MatrizMEXMS <- as.matrix(t(MEX_MS))
MatrizMEXPHD <- as.matrix(t(MEX_PhD))
ProgramsMEX
Matriz <- as.matrix(t(ProgramsMEX))
rowSums(Matriz)


library(igraph)
bnMEX <- graph_from_biadjacency_matrix(t(Matriz), directed = FALSE)
EdgeListMEX <- as_edgelist(bnMEX)
edges_mx <- data.frame(
  Source = paste0("MEX_", EdgeListMEX[, 1]),
  Target = EdgeListMEX[, 2],
  Country = "Mexico"
)
bnMEX <- graph_from_data_frame(edges_mx, directed = TRUE)
V(bnMEX)$type <- bipartite_mapping(bnMEX)$type
V(bnMEX)$shape <- ifelse(V(bnMEX)$type, "circle", "square")
V(bnMEX)$label.cex <- ifelse(V(bnMEX)$type, 0.5, 1)
V(bnMEX)$size <- sqrt(igraph::degree(bnMEX))
E(bnMEX)$color <- "lightgrey"
png(filename = "f1.png", width = 10, height = 8, units = "in", res = 300)
plot(bnMEX, vertex.label = NA, layout = layout_as_bipartite, edge.arrow.size = 0.5,edge.arrow.width = 0.5)
dev.off()
ProgramsMEX <- data.frame(Degree = igraph::degree(bnMEX),
                          Closeness = igraph::closeness(bnMEX),
                          Betweennes = igraph::betweenness(bnMEX),
                          Eigen = igraph::eigen_centrality(bnMEX))
ProgramsMEX <- ProgramsMEX[ -c(5:25) ]
rownames(ProgramsMEX)
ProgramsMEX$SS <- rownames(ProgramsMEX)
ProgramsMEX <- ProgramsMEX[order(-ProgramsMEX$Degree), ]
#ProgramsMEX <- ProgramsMEX[!grepl("text", ProgramsMEX$SS), ]
colnames(ProgramsMEX)[4] <- "Eigenvector"
ProgramsMEX$Node <- rownames(ProgramsMEX)
ProgramsMEX <- mutate(ProgramsMEX, 
                      Partition = ifelse(
                        grepl("text", Node), "Program", "Skill"))
ProgramsMEX$Country <- "Mexico"

psych::describeBy(ProgramsMEX$Eigenvector, group = ProgramsMEX$Partition, mat = TRUE, digits = 2)

library(network)
verticesMEX <- nrow(Matriz) + ncol(Matriz)
g <- network.initialize(verticesMEX, directed = TRUE, bipartite = TRUE)
pave <- network.bipartite(Matriz, g)
Mexico <- network(pave, directed = TRUE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Mexico
SizeMEX <- network::network.size(Mexico)
DensityMEX <- network::network.density(Mexico)
ClusteringMEX <- tnet::clustering_tm(Matriz)
set.network.attribute(Mexico, "Size", SizeMEX)
set.network.attribute(Mexico, "Density", DensityMEX)
set.network.attribute(Mexico, "Clustering", ClusteringMEX)
set.network.attribute(Mexico, "Country", "Mexico")
set.network.attribute(Mexico, "Level", "All")
Mexico

bnMEX1 <- graph_from_biadjacency_matrix(t(MatrizMEXSPEC), directed = FALSE)
EdgeListMEX1 <- as_edgelist(bnMEX1)
edges_mx1 <- data.frame(
  Source = paste0("MEX_", EdgeListMEX1[, 1]),
  Target = EdgeListMEX1[, 2],
  Country = "Mexico"
)
bnMEX1 <- graph_from_data_frame(edges_mx1, directed = TRUE)
V(bnMEX1)$type <- bipartite_mapping(bnMEX1)$type
V(bnMEX1)$shape <- ifelse(V(bnMEX1)$type, "circle", "square")
V(bnMEX1)$label.cex <- ifelse(V(bnMEX1)$type, 0.5, 1)
V(bnMEX1)$size <- sqrt(igraph::degree(bnMEX1))
E(bnMEX1)$color <- "lightgrey"
png(filename = "f2.png", width = 10, height = 8, units = "in", res = 300)
plot(bnMEX1, vertex.label = NA, layout = layout_as_bipartite, edge.arrow.size = 0.5,edge.arrow.width = 0.5)
dev.off()
ProgramsMEX1 <- data.frame(Degree = igraph::degree(bnMEX1),
                           Closeness = igraph::closeness(bnMEX1),
                           Betweennes = igraph::betweenness(bnMEX1),
                           Eigen = igraph::eigen_centrality(bnMEX1))
ProgramsMEX1 <- ProgramsMEX1[ -c(5:25) ]
rownames(ProgramsMEX1)
ProgramsMEX1$SS <- rownames(ProgramsMEX1)
ProgramsMEX1 <- ProgramsMEX1[order(-ProgramsMEX1$Degree), ]
#ProgramsMEX1 <- ProgramsMEX1[!grepl("text", ProgramsMEX1$SS), ]
colnames(ProgramsMEX1)[4] <- "Eigenvector"
ProgramsMEX1$Node <- rownames(ProgramsMEX1)
ProgramsMEX1 <- mutate(ProgramsMEX1, 
                       Partition = ifelse(
                         grepl("text", Node), "Program", "Skill"))
ProgramsMEX1$Country <- "Mexico"
ProgramsMEX1$Level <- "Specialization"

psych::describeBy(ProgramsMEX1$Eigenvector, group = ProgramsMEX1$Partition, mat = TRUE, digits = 2)
library(network)
verticesMEXSPEC <- nrow(MatrizMEXSPEC) + ncol(MatrizMEXSPEC)
g1 <- network.initialize(verticesMEXSPEC, directed = TRUE, bipartite = TRUE)
pave1 <- network.bipartite(MatrizMEXSPEC, g1)
Mexico1 <- network(pave1, directed = TRUE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Mexico1
SizeMEX1 <- network::network.size(Mexico1)
DensityMEX1 <- network::network.density(Mexico1)
ClusteringMEX1 <- tnet::clustering_tm(MatrizMEXSPEC)
set.network.attribute(Mexico1, "Size", SizeMEX1)
set.network.attribute(Mexico1, "Density", DensityMEX1)
set.network.attribute(Mexico1, "Clustering", ClusteringMEX1)
set.network.attribute(Mexico1, "Country", "Mexico")
set.network.attribute(Mexico1, "Level", "Specialization")
Mexico1

bnMEX2 <- graph_from_biadjacency_matrix(t(MatrizMEXMS), directed = FALSE)
EdgeListMEX2 <- as_edgelist(bnMEX2)
edges_mx2 <- data.frame(
  Source = paste0("MEX_", EdgeListMEX2[, 1]),
  Target = EdgeListMEX2[, 2],
  Country = "Mexico"
)
bnMEX2 <- graph_from_data_frame(edges_mx2, directed = TRUE)
V(bnMEX2)$type <- bipartite_mapping(bnMEX2)$type
V(bnMEX2)$shape <- ifelse(V(bnMEX2)$type, "circle", "square")
V(bnMEX2)$label.cex <- ifelse(V(bnMEX2)$type, 0.5, 1)
V(bnMEX2)$size <- sqrt(igraph::degree(bnMEX2))
E(bnMEX2)$color <- "lightgrey"
png(filename = "f3.png", width = 10, height = 8, units = "in", res = 300)
plot(bnMEX2, vertex.label = NA, layout = layout_as_bipartite, edge.arrow.size = 0.5,edge.arrow.width = 0.5)
dev.off()
ProgramsMEX2 <- data.frame(Degree = igraph::degree(bnMEX2),
                           Closeness = igraph::closeness(bnMEX2),
                           Betweennes = igraph::betweenness(bnMEX2),
                           Eigen = igraph::eigen_centrality(bnMEX2))
ProgramsMEX2 <- ProgramsMEX2[ -c(5:25) ]
rownames(ProgramsMEX2)
ProgramsMEX2$SS <- rownames(ProgramsMEX2)
ProgramsMEX2 <- ProgramsMEX2[order(-ProgramsMEX2$Degree), ]
#ProgramsMEX2 <- ProgramsMEX2[!grepl("text", ProgramsMEX2$SS), ]
colnames(ProgramsMEX2)[4] <- "Eigenvector"
ProgramsMEX2$Node <- rownames(ProgramsMEX2)
ProgramsMEX2 <- mutate(ProgramsMEX2, 
                       Partition = ifelse(
                         grepl("text", Node), "Program", "Skill"))
ProgramsMEX2$Country <- "Mexico"
ProgramsMEX2$Level <- "Master"
psych::describeBy(ProgramsMEX2$Eigenvector, group = ProgramsMEX2$Partition, mat = TRUE, digits = 2)
library(network)
verticesMEXMS <- nrow(MatrizMEXMS) + ncol(MatrizMEXMS)
g2 <- network.initialize(verticesMEXMS, directed = TRUE, bipartite = TRUE)
pave2 <- network.bipartite(MatrizMEXMS, g2)
Mexico2 <- network(pave2, directed = TRUE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Mexico2
SizeMEX2 <- network::network.size(Mexico2)
DensityMEX2 <- network::network.density(Mexico2)
ClusteringMEX2 <- tnet::clustering_tm(MatrizMEXMS)
set.network.attribute(Mexico2, "Size", SizeMEX2)
set.network.attribute(Mexico2, "Density", DensityMEX2)
set.network.attribute(Mexico2, "Clustering", ClusteringMEX2)
set.network.attribute(Mexico2, "Country", "Mexico")
set.network.attribute(Mexico2, "Level", "Master")
Mexico2

bnMEX3 <- graph_from_biadjacency_matrix(t(MatrizMEXPHD), directed = FALSE)
EdgeListMEX3 <- as_edgelist(bnMEX3)
edges_mx3 <- data.frame(
  Source = paste0("MEX_", EdgeListMEX3[, 1]),
  Target = EdgeListMEX3[, 2],
  Country = "Mexico"
)
bnMEX3 <- graph_from_data_frame(edges_mx3, directed = TRUE)
V(bnMEX3)$type <- bipartite_mapping(bnMEX3)$type
V(bnMEX3)$shape <- ifelse(V(bnMEX3)$type, "circle", "square")
V(bnMEX3)$label.cex <- ifelse(V(bnMEX3)$type, 0.5, 1)
V(bnMEX3)$size <- sqrt(igraph::degree(bnMEX3))
E(bnMEX3)$color <- "lightgrey"
png(filename = "f4.png", width = 10, height = 8, units = "in", res = 300)
plot(bnMEX3, vertex.label = NA, layout = layout_as_bipartite, edge.arrow.size = 0.5,edge.arrow.width = 0.5)
dev.off()
ProgramsMEX3 <- data.frame(Degree = igraph::degree(bnMEX3),
                           Closeness = igraph::closeness(bnMEX3),
                           Betweennes = igraph::betweenness(bnMEX3),
                           Eigen = igraph::eigen_centrality(bnMEX3))
ProgramsMEX3 <- ProgramsMEX3[ -c(5:25) ]
rownames(ProgramsMEX3)
ProgramsMEX3$SS <- rownames(ProgramsMEX3)
ProgramsMEX3 <- ProgramsMEX3[order(-ProgramsMEX3$Degree), ]
#ProgramsMEX3 <- ProgramsMEX3[!grepl("text", ProgramsMEX3$SS), ]
colnames(ProgramsMEX3)[4] <- "Eigenvector"
ProgramsMEX3$Node <- rownames(ProgramsMEX3)
ProgramsMEX3 <- mutate(ProgramsMEX3, 
                       Partition = ifelse(
                         grepl("text", Node), "Program", "Skill"))
ProgramsMEX3$Country <- "Mexico"
ProgramsMEX3$Level <- "PhD"

psych::describeBy(ProgramsMEX3$Eigenvector, group = ProgramsMEX3$Partition, mat = TRUE, digits = 2)
verticesMEXPHD <- nrow(MatrizMEXPHD) + ncol(MatrizMEXPHD)
g3 <- network.initialize(verticesMEXPHD, directed = TRUE, bipartite = TRUE)
pave3 <- network.bipartite(MatrizMEXPHD, g3)
Mexico3 <- network(MatrizMEXPHD, directed = TRUE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Mexico3
SizeMEX3 <- network::network.size(Mexico3)
DensityMEX3 <- network::network.density(Mexico3)
ClusteringMEX3 <- tnet::clustering_tm(MatrizMEXPHD)
set.network.attribute(Mexico3, "Size", SizeMEX3)
set.network.attribute(Mexico3, "Density", DensityMEX3)
set.network.attribute(Mexico3, "Clustering", ClusteringMEX3)
set.network.attribute(Mexico3, "Country", "Mexico")
set.network.attribute(Mexico3, "Level", "PhD")
Mexico3

MatrizMEXSPEC <- as.matrix(t(MEX_Spec))
MatrizMEXMS <- as.matrix(t(MEX_MS))
MatrizMEXPHD <- as.matrix(t(MEX_PhD))

save.image("~/Documents/GitHub/SoftSkillsLatam/Results/Mexico.RData")

