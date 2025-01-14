library(readtext)
COL <- readtext("Colombia")
COL$doc_id <- gsub("\\.pdf$|\\.docx$", "", COL$doc_id)

library(dplyr)
COL <- mutate(COL, 
              Program = ifelse(
                grepl("Doctorado en", text), "Doctorado",
                ifelse(grepl("Maestría|Magíster en|MAGISTER EN", text), "Maestría", 
                       "Especialización")))
Programas <- data.frame(table(COL$Program))
colnames(Programas)[1] <- "Programa" 
colnames(Programas)[2] <- "Total"

library(tidyverse)
COL <- COL %>%
  mutate(University.Code = str_extract(doc_id, "^\\d+"))

library(quanteda)
TextsCOL <- corpus(COL$text)
docvars(TextsCOL, "Program") <- COL$Program
docvars(TextsCOL, "Country") <- "Colombia"
head(summary(TextsCOL), 10)
COLTexts <- data.frame(summary(TextsCOL, length(TextsCOL)))
COLSpec <- corpus_subset(TextsCOL, Program == "Especialización")
COLMS <- corpus_subset(TextsCOL, Program == "Maestría")
COLPhD <- corpus_subset(TextsCOL, Program == "Doctorado")

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
ProgramsCOL <- tokens(TextsCOL, 
                       remove_numbers = TRUE, 
                       remove_punct = TRUE, 
                       remove_url = TRUE, 
                       remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

COL_Spec <- tokens(COLSpec, 
                   remove_numbers = TRUE, 
                   remove_punct = TRUE, 
                   remove_url = TRUE, 
                   remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

COL_MS <- tokens(COLMS, 
                 remove_numbers = TRUE, 
                 remove_punct = TRUE, 
                 remove_url = TRUE, 
                 remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

COL_PhD <- tokens(COLPhD, 
                  remove_numbers = TRUE, 
                  remove_punct = TRUE, 
                  remove_url = TRUE, 
                  remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

MatrizCOSPEC <- as.matrix(COL_Spec)
MatrizCOMS <- as.matrix(COL_MS)
MatrizCOPHD <- as.matrix(COL_PhD)
ProgramsCOL
Matriz <- as.matrix(ProgramsCOL)
rowSums(Matriz)
library(network)
Colombia <- as.network(Matriz, matrix.type = "adjacency", directed = FALSE, bipartite = TRUE)
Colombia1 <- as.network(MatrizCOSPEC, matrix.type = "adjacency", directed = FALSE, bipartite = TRUE)
Colombia2 <- as.network(MatrizCOMS, matrix.type = "adjacency", directed = FALSE, bipartite = TRUE)
Colombia3 <- as.network(MatrizCOPHD, matrix.type = "adjacency", directed = FALSE, bipartite = TRUE)
SizeCOL <- network::network.size(Colombia)
DensityCOL <- network::network.density(Colombia)
ClusteringCOL <- tnet::clustering_tm(t(Matriz))
set.network.attribute(Colombia, "Size", SizeCOL)
set.network.attribute(Colombia, "Density", DensityCOL)
set.network.attribute(Colombia, "Clustering", ClusteringCOL)
set.network.attribute(Colombia, "Country", "Colombia")
set.network.attribute(Colombia, "Level", "All")
set.network.attribute(Colombia, "OECD", TRUE)
Colombia
library(igraph)
bnCOL <- graph_from_biadjacency_matrix(t(Matriz), directed = FALSE)
EdgeListCOL <- as_edgelist(bnCOL)
edges_col <- data.frame(
  Source = paste0("COL_", EdgeListCOL[, 1]),
  Target = EdgeListCOL[, 2],
  Country = "Colombia"
)
bnCOL <- graph_from_data_frame(edges_col, directed = F)
bipartite_mapping(bnCOL)
V(bnCOL)$type <- bipartite_mapping(bnCOL)$type
V(bnCOL)$shape <- ifelse(V(bnCOL)$type, "circle", "square")
V(bnCOL)$label.cex <- ifelse(V(bnCOL)$type, 0.5, 1)
V(bnCOL)$size <- sqrt(igraph::degree(bnCOL))
E(bnCOL)$color <- "lightgrey"
plot(bnCOL, vertex.label = NA, layout = layout_as_bipartite)
ProgramsCOL <- data.frame(Degree = igraph::degree(bnCOL),
                           Closeness = igraph::closeness(bnCOL),
                           Betweennes = igraph::betweenness(bnCOL),
                           Eigen = igraph::eigen_centrality(bnCOL))
ProgramsCOL <- ProgramsCOL[ -c(5:25) ]
rownames(ProgramsCOL)
ProgramsCOL$SS <- rownames(ProgramsCOL)
ProgramsCOL <- ProgramsCOL[order(-ProgramsCOL$Degree), ]
#ProgramsCOL <- ProgramsCOL[!grepl("text", ProgramsCOL$SS), ]
colnames(ProgramsCOL)[4] <- "Eigenvector"
ProgramsCOL$Node <- rownames(ProgramsCOL)
ProgramsCOL <- mutate(ProgramsCOL, 
                       Partition = ifelse(
                         grepl("text", Node), "Program", "Skill"))
ProgramsCOL$Country <- "Colombia"

psych::describeBy(ProgramsCOL$Eigenvector, group = ProgramsCOL$Partition, mat = TRUE, digits = 2)

library(network)
verticesCOL <- nrow(Matriz) + ncol(Matriz)
g <- network.initialize(verticesCOL, directed = F, bipartite = TRUE)
pave <- network.bipartite(Matriz, g)
Colombia <- network(pave, directed = F, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Colombia
SizeCOL <- network::network.size(Colombia)
DensityCOL <- network::network.density(Colombia)
ClusteringCOL <- tnet::clustering_tm(Matriz)
set.network.attribute(Colombia, "Size", SizeCOL)
set.network.attribute(Colombia, "Density", DensityCOL)
set.network.attribute(Colombia, "Clustering", ClusteringCOL)
set.network.attribute(Colombia, "Country", "Colombia")
set.network.attribute(Colombia, "Level", "All")
set.network.attribute(Colombia, "OECD", TRUE)
Colombia

bnCOL1 <- graph_from_biadjacency_matrix(t(MatrizCOSPEC), directed = FALSE)
EdgeListCOL1 <- as_edgelist(bnCOL1)
edges_col1 <- data.frame(
  Source = paste0("COL_", EdgeListCOL1[, 1]),
  Target = EdgeListCOL1[, 2],
  Country = "Colombia"
)
bnCOL1 <- graph_from_data_frame(edges_col1, directed = F)
bipartite_mapping(bnCOL1)
V(bnCOL1)$type <- bipartite_mapping(bnCOL1)$type
V(bnCOL1)$shape <- ifelse(V(bnCOL1)$type, "circle", "square")
V(bnCOL1)$label.cex <- ifelse(V(bnCOL1)$type, 0.5, 1)
V(bnCOL1)$size <- sqrt(igraph::degree(bnCOL1))
E(bnCOL1)$color <- "lightgrey"
png(filename = "f1.png", width = 10, height = 8, units = "in", res = 300)
plot(bnCOL1, vertex.label = NA, layout = layout_as_bipartite, edge.arrow.size = 0.5,edge.arrow.width = 0.5)
dev.off()
ProgramsCOL1 <- data.frame(Degree = igraph::degree(bnCOL1),
                           Closeness = igraph::closeness(bnCOL1),
                           Betweennes = igraph::betweenness(bnCOL1),
                           Eigen = igraph::eigen_centrality(bnCOL1))
ProgramsCOL1 <- ProgramsCOL1[ -c(5:25) ]
rownames(ProgramsCOL1)
ProgramsCOL1$SS <- rownames(ProgramsCOL1)
ProgramsCOL1 <- ProgramsCOL1[order(-ProgramsCOL1$Degree), ]
#ProgramsVEN <- ProgramsVEN[!grepl("text", ProgramsVEN$SS), ]
colnames(ProgramsCOL1)[4] <- "Eigenvector"
ProgramsCOL1$Node <- rownames(ProgramsCOL1)
ProgramsCOL1 <- mutate(ProgramsCOL1, 
                       Partition = ifelse(
                         grepl("text", Node), "Program", "Skill"))
ProgramsCOL1$Country <- "Colombia"
ProgramsCOL1$Level <- "Specialization"

psych::describeBy(ProgramsCOL1$Eigenvector, group = ProgramsCOL1$Partition, mat = TRUE, digits = 2)
library(network)
verticesCOLSPEC <- nrow(MatrizCOSPEC) + ncol(MatrizCOSPEC)
g1 <- network.initialize(verticesCOLSPEC, directed = F, bipartite = TRUE)
pave1 <- network.bipartite(MatrizCOSPEC, g1)
Colombia1 <- network(pave1, directed = F, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Colombia1
SizeCO1 <- network::network.size(Colombia1)
DensityCO1 <- network::network.density(Colombia1)
ClusteringCO1 <- tnet::clustering_tm(MatrizCOSPEC)
set.network.attribute(Colombia1, "Size", SizeCO1)
set.network.attribute(Colombia1, "Density", DensityCO1)
set.network.attribute(Colombia1, "Clustering", ClusteringCO1)
set.network.attribute(Colombia1, "Country", "Colombia")
set.network.attribute(Colombia1, "Level", "Specialization")
Colombia1

library(igraph)
bnCOL2 <- graph_from_biadjacency_matrix(t(MatrizCOMS), directed = FALSE)
EdgeListCOL2 <- as_edgelist(bnCOL2)
edges_col2 <- data.frame(
  Source = paste0("COL_", EdgeListCOL2[, 1]),
  Target = EdgeListCOL2[, 2],
  Country = "Colombia"
)
bnCOL2 <- graph_from_data_frame(edges_col2, directed = F)
V(bnCOL2)$type <- bipartite_mapping(bnCOL2)$type
V(bnCOL2)$shape <- ifelse(V(bnCOL2)$type, "circle", "square")
V(bnCOL2)$label.cex <- ifelse(V(bnCOL2)$type, 0.5, 1)
V(bnCOL2)$size <- sqrt(igraph::degree(bnCOL2))
E(bnCOL2)$color <- "lightgrey"
png(filename = "f2.png", width = 10, height = 8, units = "in", res = 300)
plot(bnCOL2, vertex.label = NA, layout = layout_as_bipartite, edge.arrow.size = 0.5,edge.arrow.width = 0.5)
dev.off()
ProgramsCOL2 <- data.frame(Degree = igraph::degree(bnCOL2),
                           Closeness = igraph::closeness(bnCOL2),
                           Betweennes = igraph::betweenness(bnCOL2),
                           Eigen = igraph::eigen_centrality(bnCOL2))
ProgramsCOL2 <- ProgramsCOL2[ -c(5:25) ]
rownames(ProgramsCOL2)
ProgramsCOL2$SS <- rownames(ProgramsCOL2)
ProgramsCOL2 <- ProgramsCOL2[order(-ProgramsCOL2$Degree), ]
#ProgramsCOL2 <- ProgramsCOL2[!grepl("text", ProgramsCOL2$SS), ]
colnames(ProgramsCOL2)[4] <- "Eigenvector"
ProgramsCOL2$Node <- rownames(ProgramsCOL2)
ProgramsCOL2 <- mutate(ProgramsCOL2, 
                       Partition = ifelse(
                         grepl("text", Node), "Program", "Skill"))
ProgramsCOL2$Country <- "Colombia"
ProgramsCOL2$Level <- "Master"

psych::describeBy(ProgramsCOL2$Eigenvector, group = ProgramsCOL2$Partition, mat = TRUE, digits = 2)
library(network)
verticesCOLMS <- nrow(MatrizCOMS) + ncol(MatrizCOMS)
g2 <- network.initialize(verticesCOLMS, directed = F, bipartite = TRUE)
pave2 <- network.bipartite(MatrizCOMS, g2)
Colombia2 <- network(pave2, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Colombia2
SizeCO2 <- network::network.size(Colombia2)
DensityCO2 <- network::network.density(Colombia2)
ClusteringCO2 <- tnet::clustering_tm(MatrizCOMS)
set.network.attribute(Colombia2, "Size", SizeCO2)
set.network.attribute(Colombia2, "Density", DensityCO2)
set.network.attribute(Colombia2, "Clustering", ClusteringCO2)
set.network.attribute(Colombia2, "Country", "Colombia")
set.network.attribute(Colombia2, "Level", "Master")
Colombia2

library(igraph)
bnCOL3 <- graph_from_biadjacency_matrix(t(MatrizCOPHD), directed = FALSE)
EdgeListCOL3 <- as_edgelist(bnCOL3)
edges_col3 <- data.frame(
  Source = paste0("COL_", EdgeListCOL3[, 1]),
  Target = EdgeListCOL3[, 2],
  Country = "Colombia"
)
bnCOL3 <- graph_from_data_frame(edges_col3, directed = F)
bipartite_mapping(bnCOL3)
V(bnCOL3)$type <- bipartite_mapping(bnCOL3)$type
V(bnCOL3)$shape <- ifelse(V(bnCOL3)$type, "circle", "square")
V(bnCOL3)$label.cex <- ifelse(V(bnCOL3)$type, 0.5, 1)
V(bnCOL3)$size <- sqrt(igraph::degree(bnCOL3))
E(bnCOL3)$color <- "lightgrey"
png(filename = "f3.png", width = 10, height = 8, units = "in", res = 300)
plot(bnCOL3, vertex.label = NA, layout = layout_as_bipartite, edge.arrow.size = 0.5,edge.arrow.width = 0.5)
dev.off()

ProgramsCOL3 <- data.frame(Degree = igraph::degree(bnCOL3),
                           Closeness = igraph::closeness(bnCOL3),
                           Betweennes = igraph::betweenness(bnCOL3),
                           Eigen = igraph::eigen_centrality(bnCOL3))
ProgramsCOL3 <- ProgramsCOL3[ -c(5:25) ]
rownames(ProgramsCOL3)
ProgramsCOL3$SS <- rownames(ProgramsCOL3)
ProgramsCOL3 <- ProgramsCOL3[order(-ProgramsCOL3$Degree), ]
#ProgramsCOL3 <- ProgramsCOL3[!grepl("text", ProgramsCOL3$SS), ]
colnames(ProgramsCOL3)[4] <- "Eigenvector"
ProgramsCOL3$Node <- rownames(ProgramsCOL3)
ProgramsCOL3 <- mutate(ProgramsCOL3, 
                       Partition = ifelse(
                         grepl("text", Node), "Program", "Skill"))
ProgramsCOL3$Country <- "Colombia"
ProgramsCOL3$Level <- "PhD"


psych::describeBy(ProgramsCOL3$Eigenvector, group = ProgramsCOL3$Partition, mat = TRUE, digits = 2)
library(network)
verticesCOLPHD <- nrow(MatrizCOPHD) + ncol(MatrizCOPHD)
g3 <- network.initialize(verticesCOLPHD, directed = F, bipartite = TRUE)
pave3 <- network.bipartite(MatrizCOPHD, g3)
Colombia3 <- network(pave3, directed = F, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Colombia3
SizeCOL3 <- network::network.size(Colombia3)
DensityCOL3 <- network::network.density(Colombia3)
ClusteringCOL3 <- tnet::clustering_tm(MatrizCOPHD)
set.network.attribute(Colombia3, "Size", SizeCOL3)
set.network.attribute(Colombia3, "Density", DensityCOL3)
set.network.attribute(Colombia3, "Clustering", ClusteringCOL3)
set.network.attribute(Colombia3, "Country", "Colombia")
set.network.attribute(Colombia3, "Level", "PhD")
Colombia3

MatrizCOSPEC <- as.matrix(t(COL_Spec))
MatrizCOMS <- as.matrix(t(COL_MS))
MatrizCOPHD <- as.matrix(t(COL_PhD))

save.image("~/Documents/GitHub/SoftSkillsLatam/Results/Colombia.RData")
