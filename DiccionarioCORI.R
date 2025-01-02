library(readtext)
CORI <- readtext("Costa_Rica")
CORI$doc_id <- gsub("\\.pdf$|\\.docx$", "", CORI$doc_id)

library(dplyr)
CORI <- mutate(CORI, 
               Program = ifelse(
                 grepl("Doctorado en", text), "Doctorado",
                 ifelse(grepl("ESPECIALIDAD|ESPECIALIZACIÓN|Especialidad", text), "Especialización", 
                        "Maestría")))
Programas <- data.frame(table(CORI$Program))
colnames(Programas)[1] <- "Programa" 
colnames(Programas)[2] <- "Total"

library(tidyverse)
CORI <- CORI %>%
  mutate(University.Code = str_extract(doc_id, "^\\d+"))

library(quanteda)
TextsCORI <- corpus(CORI$text)
docvars(TextsCORI, "Program") <- CORI$Program
docvars(TextsCORI, "Country") <- "Costa Rica"
tail(summary(TextsCORI), 10)
CORITexts <- data.frame(summary(TextsCORI, length(TextsCORI)))
CORISpec <- corpus_subset(TextsCORI, Program == "Especialización")
CORIMS <- corpus_subset(TextsCORI, Program == "Maestría")
CORIPhD <- corpus_subset(TextsCORI, Program == "Doctorado")

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
ProgramsCORI <- tokens(TextsCORI, 
                      remove_numbers = TRUE, 
                      remove_punct = TRUE, 
                      remove_url = TRUE, 
                      remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

CORI_Spec <- tokens(CORISpec, 
                   remove_numbers = TRUE, 
                   remove_punct = TRUE, 
                   remove_url = TRUE, 
                   remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

CORI_MS <- tokens(CORIMS, 
                 remove_numbers = TRUE, 
                 remove_punct = TRUE, 
                 remove_url = TRUE, 
                 remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

CORI_PhD <- tokens(CORIPhD, 
                  remove_numbers = TRUE, 
                  remove_punct = TRUE, 
                  remove_url = TRUE, 
                  remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

MatrizCORISPEC <- as.matrix(t(CORI_Spec))
MatrizCORIMS <- as.matrix(t(CORI_MS))
MatrizCORIPHD <- as.matrix(t(CORI_PhD))
ProgramsCORI
Matriz <- as.matrix(t(ProgramsCORI))
rowSums(Matriz)
library(igraph)
bnCORI <- graph_from_biadjacency_matrix(t(Matriz), directed = FALSE)
EdgeListCORI <- as_edgelist(bnCORI)
edges_cr <- data.frame(
  Source = paste0("CR_", EdgeListCORI[, 1]),
  Target = EdgeListCORI[, 2],
  Country = "Costa Rica"
)
bnCORI <- graph_from_data_frame(edges_cr, directed = TRUE)
bipartite_mapping(bnCORI)
V(bnCORI)$type <- bipartite_mapping(bnCORI)$type
V(bnCORI)$shape <- ifelse(V(bnCORI)$type, "circle", "square")
V(bnCORI)$label.cex <- ifelse(V(bnCORI)$type, 0.5, 1)
V(bnCORI)$size <- sqrt(igraph::degree(bnCORI))
E(bnCORI)$color <- "lightgrey"
png(filename = "f1.png", width = 10, height = 8, units = "in", res = 300)
plot(bnCORI, vertex.label = NA, layout = layout_as_bipartite, edge.arrow.size = 0.5,edge.arrow.width = 0.5)
dev.off()
ProgramsCORI <- data.frame(Degree = igraph::degree(bnCORI),
                          Closeness = igraph::closeness(bnCORI),
                          Betweennes = igraph::betweenness(bnCORI),
                          Eigen = igraph::eigen_centrality(bnCORI))
ProgramsCORI <- ProgramsCORI[ -c(5:25) ]
rownames(ProgramsCORI)
ProgramsCORI$SS <- rownames(ProgramsCORI)
ProgramsCORI <- ProgramsCORI[order(-ProgramsCORI$Degree), ]
#ProgramsCORI <- ProgramsCORI[!grepl("text", ProgramsCORI$SS), ]
colnames(ProgramsCORI)[4] <- "Eigenvector"
ProgramsCORI$Node <- rownames(ProgramsCORI)
ProgramsCORI <- mutate(ProgramsCORI, 
                      Partition = ifelse(
                        grepl("text", Node), "Program", "Skill"))
ProgramsCORI$Country <- "Costa Rica"

psych::describeBy(ProgramsCORI$Eigenvector, group = ProgramsCORI$Partition, mat = TRUE, digits = 2)

library(network)
verticesCORI <- nrow(Matriz) + ncol(Matriz)
g <- network.initialize(verticesCORI, directed = TRUE, bipartite = TRUE)
pave <- network.bipartite(Matriz, g)
CostaRica <- network(pave, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
CostaRica
SizeCR <- network::network.size(CostaRica)
DensityCR <- network::network.density(CostaRica)
ClusteringCR <- tnet::clustering_tm(Matriz)
set.network.attribute(CostaRica, "Size", SizeCR)
set.network.attribute(CostaRica, "Density", DensityCR)
set.network.attribute(CostaRica, "Clustering", ClusteringCR)
set.network.attribute(CostaRica, "Country", "Costa Rica")
set.network.attribute(CostaRica, "Level", "All")
CostaRica

bnCORI1 <- graph_from_biadjacency_matrix(t(MatrizCORISPEC), directed = FALSE)
EdgeListCORI1 <- as_edgelist(bnCORI1)
edges_cr1 <- data.frame(
  Source = paste0("CR_", EdgeListCORI1[, 1]),
  Target = EdgeListCORI1[, 2],
  Country = "Costa Rica"
)
bnCORI1 <- graph_from_data_frame(edges_cr1, directed = TRUE)
V(bnCORI1)$type <- bipartite_mapping(bnCORI1)$type
V(bnCORI1)$shape <- ifelse(V(bnCORI1)$type, "circle", "square")
V(bnCORI1)$label.cex <- ifelse(V(bnCORI1)$type, 0.5, 1)
V(bnCORI1)$size <- sqrt(igraph::degree(bnCORI1))
E(bnCORI1)$color <- "lightgrey"
png(filename = "f2.png", width = 10, height = 8, units = "in", res = 300)
plot(bnCORI1, vertex.label = NA, layout = layout_as_bipartite, edge.arrow.size = 0.5,edge.arrow.width = 0.5)
dev.off()
ProgramsCORI1 <- data.frame(Degree = igraph::degree(bnCORI1),
                           Closeness = igraph::closeness(bnCORI1),
                           Betweennes = igraph::betweenness(bnCORI1),
                           Eigen = igraph::eigen_centrality(bnCORI1))
ProgramsCORI1 <- ProgramsCORI1[ -c(5:25) ]
rownames(ProgramsCORI1)
ProgramsCORI1$SS <- rownames(ProgramsCORI1)
ProgramsCORI1 <- ProgramsCORI1[order(-ProgramsCORI1$Degree), ]
#ProgramsVEN <- ProgramsVEN[!grepl("text", ProgramsVEN$SS), ]
colnames(ProgramsCORI1)[4] <- "Eigenvector"
ProgramsCORI1$Node <- rownames(ProgramsCORI1)
ProgramsCORI1 <- mutate(ProgramsCORI1, 
                       Partition = ifelse(
                         grepl("text", Node), "Program", "Skill"))
ProgramsCORI1$Country <- "Costa Rica"
ProgramsCORI1$Level <- "Specialization"

psych::describeBy(ProgramsCORI1$Eigenvector, group = ProgramsCORI1$Partition, mat = TRUE, digits = 2)
library(network)
verticesCORISPEC <- nrow(MatrizCORISPEC) + ncol(MatrizCORISPEC)
g1 <- network.initialize(verticesCORISPEC, directed = TRUE, bipartite = TRUE)
pave1 <- network.bipartite(MatrizCORISPEC, g1)
CostaRica1 <- network(pave1, directed = TRUE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
CostaRica1
SizeCORI1 <- network::network.size(CostaRica1)
DensityCORI1 <- network::network.density(CostaRica1)
ClusteringCORI1 <- tnet::clustering_tm(MatrizCORISPEC)
set.network.attribute(CostaRica1, "Size", SizeCORI1)
set.network.attribute(CostaRica1, "Density", DensityCORI1)
set.network.attribute(CostaRica1, "Clustering", ClusteringCORI1)
set.network.attribute(CostaRica1, "Country", "Costa Rica")
set.network.attribute(CostaRica1, "Level", "Specialization")
CostaRica1

bnCORI2 <- graph_from_biadjacency_matrix(t(MatrizCORIMS), directed = FALSE)
EdgeListCORI2 <- as_edgelist(bnCORI2)
edges_cr2 <- data.frame(
  Source = paste0("CR_", EdgeListCORI2[, 1]),
  Target = EdgeListCORI2[, 2],
  Country = "Costa Rica"
)
bnCORI2 <- graph_from_data_frame(edges_cr2, directed = TRUE)
V(bnCORI2)$type <- bipartite_mapping(bnCORI2)$type
V(bnCORI2)$shape <- ifelse(V(bnCORI2)$type, "circle", "square")
V(bnCORI2)$label.cex <- ifelse(V(bnCORI2)$type, 0.5, 1)
V(bnCORI2)$size <- sqrt(igraph::degree(bnCORI2))
E(bnCORI2)$color <- "lightgrey"
png(filename = "f3.png", width = 10, height = 8, units = "in", res = 300)
plot(bnCORI2, vertex.label = NA, layout = layout_as_bipartite, edge.arrow.size = 0.5,edge.arrow.width = 0.5)
dev.off()
ProgramsCORI2 <- data.frame(Degree = igraph::degree(bnCORI2),
                           Closeness = igraph::closeness(bnCORI2),
                           Betweennes = igraph::betweenness(bnCORI2),
                           Eigen = igraph::eigen_centrality(bnCORI2))
ProgramsCORI2 <- ProgramsCORI2[ -c(5:25) ]
rownames(ProgramsCORI2)
ProgramsCORI2$SS <- rownames(ProgramsCORI2)
ProgramsCORI2 <- ProgramsCORI2[order(-ProgramsCORI2$Degree), ]
#ProgramsCORI2 <- ProgramsCORI2[!grepl("text", ProgramsCORI2$SS), ]
colnames(ProgramsCORI2)[4] <- "Eigenvector"
ProgramsCORI2$Node <- rownames(ProgramsCORI2)
ProgramsCORI2 <- mutate(ProgramsCORI2, 
                       Partition = ifelse(
                         grepl("text", Node), "Program", "Skill"))
ProgramsCORI2$Country <- "Costa Rica"
ProgramsCORI2$Level <- "Master"
psych::describeBy(ProgramsCORI2$Eigenvector, group = ProgramsCORI2$Partition, mat = TRUE, digits = 2)
library(network)
verticesCORIMS <- nrow(MatrizCORIMS) + ncol(MatrizCORIMS)
g2 <- network.initialize(verticesCORIMS, directed = TRUE, bipartite = TRUE)
pave2 <- network.bipartite(MatrizCORIMS, g2)
CostaRica2 <- network(pave2, directed = TRUE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
CostaRica2
SizeCORI2 <- network::network.size(CostaRica2)
DensityCORI2 <- network::network.density(CostaRica2)
ClusteringCORI2 <- tnet::clustering_tm(MatrizCORIMS)
set.network.attribute(CostaRica2, "Size", SizeCORI2)
set.network.attribute(CostaRica2, "Density", DensityCORI2)
set.network.attribute(CostaRica2, "Clustering", ClusteringCORI2)
set.network.attribute(CostaRica2, "Country", "Costa Rica")
set.network.attribute(CostaRica2, "Level", "Master")
CostaRica2

bnCORI3 <- graph_from_biadjacency_matrix(t(MatrizCORIPHD), directed = FALSE)
EdgeListCORI3 <- as_edgelist(bnCORI3)
edges_cr3 <- data.frame(
  Source = paste0("CR_", EdgeListCORI3[, 1]),
  Target = EdgeListCORI3[, 2],
  Country = "CostaRica"
)
bnCORI3 <- graph_from_data_frame(edges_cr3, directed = TRUE)
V(bnCORI3)$type <- bipartite_mapping(bnCORI3)$type
V(bnCORI3)$shape <- ifelse(V(bnCORI3)$type, "circle", "square")
V(bnCORI3)$label.cex <- ifelse(V(bnCORI3)$type, 0.5, 1)
V(bnCORI3)$size <- sqrt(igraph::degree(bnCORI3))
E(bnCORI3)$color <- "lightgrey"
png(filename = "f4.png", width = 10, height = 8, units = "in", res = 300)
plot(bnCORI3, vertex.label = NA, layout = layout_as_bipartite, edge.arrow.size = 0.5,edge.arrow.width = 0.5)
dev.off()
ProgramsCORI3 <- data.frame(Degree = igraph::degree(bnCORI3),
                           Closeness = igraph::closeness(bnCORI3),
                           Betweennes = igraph::betweenness(bnCORI3),
                           Eigen = igraph::eigen_centrality(bnCORI3))
ProgramsCORI3 <- ProgramsCORI3[ -c(5:25) ]
rownames(ProgramsCORI3)
ProgramsCORI3$SS <- rownames(ProgramsCORI3)
ProgramsCORI3 <- ProgramsCORI3[order(-ProgramsCORI3$Degree), ]
#ProgramsCORI3 <- ProgramsCORI3[!grepl("text", ProgramsCORI3$SS), ]
colnames(ProgramsCORI3)[4] <- "Eigenvector"
ProgramsCORI3$Node <- rownames(ProgramsCORI3)
ProgramsCORI3 <- mutate(ProgramsCORI3, 
                       Partition = ifelse(
                         grepl("text", Node), "Program", "Skill"))
ProgramsCORI3$Country <- "Costa Rica"
ProgramsCORI3$Level <- "PhD"


psych::describeBy(ProgramsCORI3$Eigenvector, group = ProgramsCORI3$Partition, mat = TRUE, digits = 2)
library(network)
verticesCORIPHD <- nrow(MatrizCORIPHD) + ncol(MatrizCORIPHD)
g3 <- network.initialize(verticesCORIPHD, directed = TRUE, bipartite = TRUE)
pave3 <- network.bipartite(MatrizCORIPHD, g3)
CostaRica3 <- network(pave3, directed = TRUE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
CostaRica3
SizeCORI3 <- network::network.size(CostaRica3)
DensityCORI3 <- network::network.density(CostaRica3)
ClusteringCORI3 <- tnet::clustering_tm(MatrizCORIPHD)
set.network.attribute(CostaRica3, "Size", SizeCORI3)
set.network.attribute(CostaRica3, "Density", DensityCORI3)
set.network.attribute(CostaRica3, "Clustering", ClusteringCORI3)
set.network.attribute(CostaRica3, "Country", "Costa Rica")
set.network.attribute(CostaRica3, "Level", "PhD")
CostaRica3

MatrizCORISPEC <- as.matrix(t(CORI_Spec))
MatrizCORIMS <- as.matrix(t(CORI_MS))
MatrizCORIPHD <- as.matrix(t(CORI_PhD))

save.image("~/Documents/GitHub/SoftSkillsLatam/Results/CostaRica.RData")

