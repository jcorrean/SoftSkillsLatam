library(readtext)
ECU <- readtext("Ecuador")
ECU$doc_id <- gsub("\\.pdf$|\\.docx$", "", ECU$doc_id)

library(dplyr)
ECU <- mutate(ECU, 
              Program = ifelse(
                grepl("Doctorado en", text), "Doctorado",
                ifelse(grepl("Maestría|Magíster en|MAGISTER EN", text), "Maestría", 
                       "Especialización")))
Programas <- data.frame(table(ECU$Program))
colnames(Programas)[1] <- "Programa" 
colnames(Programas)[2] <- "Total"

library(tidyverse)
ECU <- ECU %>%
  mutate(University.Code = str_extract(doc_id, "^\\d+"))

library(quanteda)
TextsECU <- corpus(ECU$text)
docvars(TextsECU, "Program") <- ECU$Program
docvars(TextsECU, "Country") <- "Ecuador"
head(summary(TextsECU), 10)
ECUTexts <- data.frame(summary(TextsECU, length(TextsECU)))
ECUSpec <- corpus_subset(TextsECU, Program == "Especialización")
ECUMS <- corpus_subset(TextsECU, Program == "Maestría")
ECUPhD <- corpus_subset(TextsECU, Program == "Doctorado")

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
ProgramsECU <- tokens(TextsECU, 
                      remove_numbers = TRUE, 
                      remove_punct = TRUE, 
                      remove_url = TRUE, 
                      remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

ECU_Spec <- tokens(ECUSpec, 
                    remove_numbers = TRUE, 
                    remove_punct = TRUE, 
                    remove_url = TRUE, 
                    remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

ECU_MS <- tokens(ECUMS, 
                  remove_numbers = TRUE, 
                  remove_punct = TRUE, 
                  remove_url = TRUE, 
                  remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

ECU_PhD <- tokens(ECUPhD, 
                   remove_numbers = TRUE, 
                   remove_punct = TRUE, 
                   remove_url = TRUE, 
                   remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

MatrizECUSPEC <- as.matrix(ECU_Spec)
MatrizECUMS <- as.matrix(ECU_MS)
MatrizECUPHD <- as.matrix(ECU_PhD)

ProgramsECU
Matriz <- as.matrix(ProgramsECU)
rowSums(Matriz)
library(network)
Ecuador <- as.network(Matriz, matrix.type = "adjacency", directed = FALSE, bipartite = TRUE)
Ecuador1 <- as.network(MatrizECUSPEC, matrix.type = "adjacency", directed = FALSE, bipartite = TRUE)
Ecuador2 <- as.network(MatrizECUMS, matrix.type = "adjacency", directed = FALSE, bipartite = TRUE)
Ecuador3 <- as.network(MatrizECUPHD, matrix.type = "adjacency", directed = FALSE, bipartite = TRUE)
SizeECU <- network::network.size(Ecuador)
DensityECU <- network::network.density(Ecuador)
ClusteringECU <- tnet::clustering_tm(t(Matriz))
set.network.attribute(Ecuador, "Size", SizeECU)
set.network.attribute(Ecuador, "Density", DensityECU)
set.network.attribute(Ecuador, "Clustering", ClusteringECU)
set.network.attribute(Ecuador, "Country", "Ecuador")
set.network.attribute(Ecuador, "Level", "All")
set.network.attribute(Ecuador, "OECD", FALSE)
library(igraph)
bnECU <- graph_from_biadjacency_matrix(Matriz, directed = FALSE)
edges_ecu <- data.frame()
for (i in 1:nrow(Matriz)) {
  for (j in 1:ncol(Matriz)) {
    if (Matriz[i, j] > 0) { # Only include edges where there's a connection
      edges_ecu <- rbind(edges_ecu, data.frame(
        Source = paste0("ECU_", rownames(Matriz)[i]),
        Target = colnames(Matriz)[j],
        Weight = Matriz[i, j], # Store the weight
        Country = "Ecuador"
      ))
    }
  }
}
bnECU <- graph_from_data_frame(edges_ecu, directed = F)
V(bnECU)$type <- bipartite_mapping(bnECU)$type
V(bnECU)$shape <- ifelse(V(bnECU)$type, "circle", "square")
V(bnECU)$label.cex <- ifelse(V(bnECU)$type, 0.5, 1)
V(bnECU)$size <- sqrt(igraph::degree(bnECU))
E(bnECU)$color <- "lightgrey"
E(bnECU)$weight <- edges_ecu$Weight
network::set.edge.attribute(Ecuador, "Frecuencia", edges_ecu$Weight)
Frecuencias <- as.sociomatrix(Ecuador, attrname = "Frecuencia")
network::list.edge.attributes(Ecuador)
Ecuador
png(filename = "f1.png", width = 10, height = 8, units = "in", res = 300)
plot(bnECU, vertex.label = NA, layout = layout_as_bipartite, edge.arrow.size = 0.5,edge.arrow.width = 0.5)
dev.off()
ProgramsECU <- data.frame(Degree = igraph::degree(bnECU),
                          Closeness = igraph::closeness(bnECU),
                          Betweennes = igraph::betweenness(bnECU),
                          Eigen = igraph::eigen_centrality(bnECU))
ProgramsECU <- ProgramsECU[ -c(5:25) ]
rownames(ProgramsECU)
ProgramsECU$SS <- rownames(ProgramsECU)
ProgramsECU <- ProgramsECU[order(-ProgramsECU$Degree), ]
#ProgramsECU <- ProgramsECU[!grepl("text", ProgramsECU$SS), ]
colnames(ProgramsECU)[4] <- "Eigenvector"
ProgramsECU$Node <- rownames(ProgramsECU)
ProgramsECU <- mutate(ProgramsECU, 
                       Partition = ifelse(
                         grepl("text", Node), "Program", "Skill"))
ProgramsECU$Country <- "Ecuador"

psych::describeBy(ProgramsECU$Eigenvector, group = ProgramsECU$Partition, mat = TRUE, digits = 2)

bnECU1 <- graph_from_biadjacency_matrix(t(MatrizECUSPEC), directed = FALSE)
EdgeListECU1 <- as_edgelist(bnECU1)
edges_ec1 <- data.frame(
  Source = paste0("ECU_", EdgeListECU1[, 1]),
  Target = EdgeListECU1[, 2],
  Country = "Ecuador"
)
bnECU1 <- graph_from_data_frame(edges_ec1, directed = F)
V(bnECU1)$type <- bipartite_mapping(bnECU1)$type
V(bnECU1)$shape <- ifelse(V(bnECU1)$type, "circle", "square")
V(bnECU1)$label.cex <- ifelse(V(bnECU1)$type, 0.5, 1)
V(bnECU1)$size <- sqrt(igraph::degree(bnECU1))
E(bnECU1)$color <- "lightgrey"
png(filename = "f2.png", width = 10, height = 8, units = "in", res = 300)
plot(bnECU1, vertex.label = NA, layout = layout_as_bipartite, edge.arrow.size = 0.5,edge.arrow.width = 0.5)
dev.off()
ProgramsECU1 <- data.frame(Degree = igraph::degree(bnECU1),
                           Closeness = igraph::closeness(bnECU1),
                           Betweennes = igraph::betweenness(bnECU1),
                           Eigen = igraph::eigen_centrality(bnECU1))
ProgramsECU1 <- ProgramsECU1[ -c(5:25) ]
rownames(ProgramsECU1)
ProgramsECU1$SS <- rownames(ProgramsECU1)
ProgramsECU1 <- ProgramsECU1[order(-ProgramsECU1$Degree), ]
#ProgramsECU1 <- ProgramsECU1[!grepl("text", ProgramsECU1$SS), ]
colnames(ProgramsECU1)[4] <- "Eigenvector"
ProgramsECU1$Node <- rownames(ProgramsECU1)
ProgramsECU1 <- mutate(ProgramsECU1, 
                       Partition = ifelse(
                         grepl("text", Node), "Program", "Skill"))
ProgramsECU1$Country <- "Ecuador"
ProgramsECU1$Level <- "Specialization"

psych::describeBy(ProgramsECU1$Eigenvector, group = ProgramsECU1$Partition, mat = TRUE, digits = 2)
library(network)
verticesECUSPEC <- nrow(MatrizECUSPEC) + ncol(MatrizECUSPEC)
g1 <- network.initialize(verticesECUSPEC, directed = F, bipartite = TRUE)
pave1 <- network.bipartite(MatrizECUSPEC, g1)
Ecuador1 <- network(pave1, directed = F, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Ecuador1
SizeECU1 <- network::network.size(Ecuador1)
DensityECU1 <- network::network.density(Ecuador1)
ClusteringECU1 <- tnet::clustering_tm(MatrizECUSPEC)
set.network.attribute(Ecuador1, "Size", SizeECU1)
set.network.attribute(Ecuador1, "Density", DensityECU1)
set.network.attribute(Ecuador1, "Clustering", ClusteringECU1)
set.network.attribute(Ecuador1, "Country", "Ecuador")
set.network.attribute(Ecuador1, "Level", "Specialization")
Ecuador1

bnECU2 <- graph_from_biadjacency_matrix(t(MatrizECUMS), directed = FALSE)
EdgeListECU2 <- as_edgelist(bnECU2)
edges_ec2 <- data.frame(
  Source = paste0("ECU_", EdgeListECU2[, 1]),
  Target = EdgeListECU2[, 2],
  Country = "Ecuador"
)
bnECU2 <- graph_from_data_frame(edges_ec2, directed = F)
V(bnECU2)$type <- bipartite_mapping(bnECU2)$type
V(bnECU2)$shape <- ifelse(V(bnECU2)$type, "circle", "square")
V(bnECU2)$label.cex <- ifelse(V(bnECU2)$type, 0.5, 1)
V(bnECU2)$size <- sqrt(igraph::degree(bnECU2))
E(bnECU2)$color <- "lightgrey"
png(filename = "f3.png", width = 10, height = 8, units = "in", res = 300)
plot(bnECU2, vertex.label = NA, layout = layout_as_bipartite, edge.arrow.size = 0.5,edge.arrow.width = 0.5)
dev.off()
ProgramsECU2 <- data.frame(Degree = igraph::degree(bnECU2),
                           Closeness = igraph::closeness(bnECU2),
                           Betweennes = igraph::betweenness(bnECU2),
                           Eigen = igraph::eigen_centrality(bnECU2))
ProgramsECU2 <- ProgramsECU2[ -c(5:25) ]
rownames(ProgramsECU2)
ProgramsECU2$SS <- rownames(ProgramsECU2)
ProgramsECU2 <- ProgramsECU2[order(-ProgramsECU2$Degree), ]
#ProgramsECU2 <- ProgramsECU2[!grepl("text", ProgramsECU2$SS), ]
colnames(ProgramsECU2)[4] <- "Eigenvector"
ProgramsECU2$Node <- rownames(ProgramsECU2)
ProgramsECU2 <- mutate(ProgramsECU2, 
                       Partition = ifelse(
                         grepl("text", Node), "Program", "Skill"))
ProgramsECU2$Country <- "Ecuador"
ProgramsECU2$Level <- "Master"
psych::describeBy(ProgramsECU2$Eigenvector, group = ProgramsECU2$Partition, mat = TRUE, digits = 2)
library(network)
verticesECUMS <- nrow(MatrizECUMS) + ncol(MatrizECUMS)
g2 <- network.initialize(verticesECUMS, directed = F, bipartite = TRUE)
pave2 <- network.bipartite(MatrizECUMS, g2)
Ecuador2 <- network(pave2, directed = F, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Ecuador2
SizeECU2 <- network::network.size(Ecuador2)
DensityECU2 <- network::network.density(Ecuador2)
ClusteringECU2 <- tnet::clustering_tm(MatrizECUMS)
set.network.attribute(Ecuador2, "Size", SizeECU2)
set.network.attribute(Ecuador2, "Density", DensityECU2)
set.network.attribute(Ecuador2, "Clustering", ClusteringECU2)
set.network.attribute(Ecuador2, "Country", "Ecuador")
set.network.attribute(Ecuador2, "Level", "Master")
Ecuador2

bnECU3 <- graph_from_biadjacency_matrix(t(MatrizECUPHD), directed = FALSE)
EdgeListECU3 <- as_edgelist(bnECU3)
edges_ec3 <- data.frame(
  Source = paste0("ECU_", EdgeListECU3[, 1]),
  Target = EdgeListECU3[, 2],
  Country = "Ecuador"
)
bnECU3 <- graph_from_data_frame(edges_ec3, directed = F)
V(bnECU3)$type <- bipartite_mapping(bnECU3)$type
V(bnECU3)$shape <- ifelse(V(bnECU3)$type, "circle", "square")
V(bnECU3)$label.cex <- ifelse(V(bnECU3)$type, 0.5, 1)
V(bnECU3)$size <- sqrt(igraph::degree(bnECU3))
E(bnECU3)$color <- "lightgrey"
png(filename = "f4.png", width = 10, height = 8, units = "in", res = 300)
plot(bnECU3, vertex.label = NA, layout = layout_as_bipartite, edge.arrow.size = 0.5,edge.arrow.width = 0.5)
dev.off()
ProgramsECU3 <- data.frame(Degree = igraph::degree(bnECU3),
                           Closeness = igraph::closeness(bnECU3),
                           Betweennes = igraph::betweenness(bnECU3),
                           Eigen = igraph::eigen_centrality(bnECU3))
ProgramsECU3 <- ProgramsECU3[ -c(5:25) ]
rownames(ProgramsECU3)
ProgramsECU3$SS <- rownames(ProgramsECU3)
ProgramsECU3 <- ProgramsECU3[order(-ProgramsECU3$Degree), ]
#ProgramsECU3 <- ProgramsECU3[!grepl("text", ProgramsECU3$SS), ]
colnames(ProgramsECU3)[4] <- "Eigenvector"
ProgramsECU3$Node <- rownames(ProgramsECU3)
ProgramsECU3 <- mutate(ProgramsECU3, 
                       Partition = ifelse(
                         grepl("text", Node), "Program", "Skill"))
ProgramsECU3$Country <- "Ecuador"
ProgramsECU3$Level <- "PhD"

psych::describeBy(ProgramsECU3$Eigenvector, group = ProgramsECU3$Partition, mat = TRUE, digits = 2)
verticesECUPHD <- nrow(MatrizECUPHD) + ncol(MatrizECUPHD)
g3 <- network.initialize(verticesECUPHD, directed = F, bipartite = TRUE)
pave3 <- network.bipartite(MatrizECUPHD, g3)
Ecuador3 <- network(pave3, directed = F, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Ecuador3
SizeECU3 <- network::network.size(Ecuador3)
DensityECU3 <- network::network.density(Ecuador3)
ClusteringECU3 <- tnet::clustering_tm(MatrizECUPHD)
set.network.attribute(Ecuador3, "Size", SizeECU3)
set.network.attribute(Ecuador3, "Density", DensityECU3)
set.network.attribute(Ecuador3, "Clustering", ClusteringECU3)
set.network.attribute(Ecuador3, "Country", "Ecuador")
set.network.attribute(Ecuador3, "Level", "PhD")
Ecuador3

MatrizECUSPEC <- as.matrix(t(ECU_Spec))
MatrizECUMS <- as.matrix(t(ECU_MS))
MatrizECUPHD <- as.matrix(t(ECU_PhD))

save.image("~/Documents/GitHub/SoftSkillsLatam/Results/Ecuador.RData")

