library(readtext)
ARG <- readtext("Argentina")
ARG$doc_id <- gsub("\\.pdf$|\\.docx$", "", ARG$doc_id)

library(dplyr)
ARG <- mutate(ARG, 
              Program = ifelse(
                grepl("Doctor|Doctorado en", text), "Doctorado",
                ifelse(grepl("Maestría|Magíster en|MAGISTER EN", text), "Maestría", 
                       "Especialización")))
ARG <- mutate(ARG, University = str_extract(doc_id, "^\\d+")) 

library(stringr)
ARG <- ARG %>%
  mutate(University.Code = str_extract(doc_id, "^\\d+")) 

Programas <- data.frame(table(ARG$Program))
colnames(Programas)[1] <- "Programa" 
colnames(Programas)[2] <- "Total"

library(quanteda)
TextsARG <- corpus(ARG$text)
docvars(TextsARG, "Program") <- ARG$Program
docvars(TextsARG, "Country") <- "Argentina"
head(summary(TextsARG), 10)
ARGTexts <- data.frame(summary(TextsARG, length(TextsARG)))
ARGSpec <- corpus_subset(TextsARG, Program == "Especialización")
ARGMS <- corpus_subset(TextsARG, Program == "Maestría")
ARGPhD <- corpus_subset(TextsARG, Program == "Doctorado")

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
ProgramsARG <- tokens(TextsARG, 
                      remove_numbers = TRUE, 
                      remove_punct = TRUE, 
                      remove_url = TRUE, 
                      remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

ARG_Spec <- tokens(ARGSpec, 
                   remove_numbers = TRUE, 
                   remove_punct = TRUE, 
                   remove_url = TRUE, 
                   remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

ARG_MS <- tokens(ARGMS, 
                 remove_numbers = TRUE, 
                 remove_punct = TRUE, 
                 remove_url = TRUE, 
                 remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

ARG_PhD <- tokens(ARGPhD, 
                  remove_numbers = TRUE, 
                  remove_punct = TRUE, 
                  remove_url = TRUE, 
                  remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

MatrizARGSPEC <- as.matrix(t(ARG_Spec))
MatrizARGMS <- as.matrix(t(ARG_MS))
MatrizARGPHD <- as.matrix(t(ARG_PhD))
ProgramsARG
Matriz <- as.matrix(t(ProgramsARG))
rowSums(Matriz)
str(Matriz)

library(igraph)
bnARG <- graph_from_biadjacency_matrix(t(Matriz), directed = FALSE)
EdgeListAR <- as_edgelist(bnARG)
edges_args <- data.frame()
for (i in 1:nrow(Matriz)) {
  for (j in 1:ncol(Matriz)) {
    if (Matriz[i, j] > 0) { # Only include edges where there's a connection
      edges_args <- rbind(edges_args, data.frame(
        Source = paste0("ARG_", rownames(Matriz)[i]),
        Target = colnames(Matriz)[j],
        Weight = Matriz[i, j], # Store the weight
        Country = "Argentina"
      ))
    }
  }
}

bnARG <- graph_from_data_frame(edges_args, directed = FALSE)
bipartite_mapping(bnARG)
V(bnARG)$type <- bipartite_mapping(bnARG)$type
V(bnARG)$shape <- ifelse(V(bnARG)$type, "circle", "square")
V(bnARG)$label.cex <- ifelse(V(bnARG)$type, 0.5, 1)
V(bnARG)$size <- sqrt(igraph::degree(bnARG))
E(bnARG)$color <- "lightgrey"
E(bnARG)$weight <- edges_args$Weight

plot(bnARG, vertex.label = NA, layout = layout_as_bipartite, edge.width=0.2*edges_args$Weight)
ProgramsARG <- data.frame(Degree = igraph::degree(bnARG),
                          Closeness = igraph::closeness(bnARG),
                          Betweennes = igraph::betweenness(bnARG),
                          Eigen = igraph::eigen_centrality(bnARG))
ProgramsARG <- ProgramsARG[ -c(5:25) ]
rownames(ProgramsARG)
ProgramsARG$SS <- rownames(ProgramsARG)
ProgramsARG <- ProgramsARG[order(-ProgramsARG$Degree), ]
#ProgramsARG <- ProgramsARG[!grepl("text", ProgramsARG$SS), ]
colnames(ProgramsARG)[4] <- "Eigenvector"
ProgramsARG$Node <- rownames(ProgramsARG)
ProgramsARG <- mutate(ProgramsARG, 
                      Partition = ifelse(
                        grepl("text", Node), "Program", "Skill"))
ProgramsARG$Country <- "Argentina"

library(psych)
describeBy(ProgramsARG$Eigenvector, group = ProgramsARG$Partition, mat = TRUE, digits = 2)

library(network)
library(intergraph)
netARG <- asNetwork(bnARG)


Argentina <- network(pave, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Argentina
SizeARG <- network::network.size(Argentina)
DensityARG <- network::network.density(Argentina)
ClusteringARG <- tnet::clustering_tm(Matriz)
set.network.attribute(Argentina, "Size", SizeARG)
set.network.attribute(Argentina, "Density", DensityARG)
set.network.attribute(Argentina, "Clustering", ClusteringARG)
set.network.attribute(Argentina, "Country", "Argentina")
set.network.attribute(Argentina, "Level", "All")
set.network.attribute(Argentina, "OECD", FALSE)
Argentina %e% "weight" <- t(Matriz)
Argentina

library(igraph)
bnARG1 <- graph_from_biadjacency_matrix(t(MatrizARGSPEC), directed = FALSE)
EdgeListAR1 <- as_edgelist(bnARG1)
edges_arg1 <- data.frame(
  Source = paste0("ARG_", EdgeListAR1[, 1]),
  Target = EdgeListAR1[, 2],
  Country = "Argentina"
)
bnARG1 <- graph_from_data_frame(edges_arg1, directed = FALSE)
bipartite_mapping(bnARG1)
V(bnARG1)$type <- bipartite_mapping(bnARG1)$type
V(bnARG1)$shape <- ifelse(V(bnARG1)$type, "circle", "square")
V(bnARG1)$label.cex <- ifelse(V(bnARG1)$type, 0.5, 1)
V(bnARG1)$size <- sqrt(igraph::degree(bnARG1))
E(bnARG1)$color <- "lightgrey"
plot(bnARG1, vertex.label = NA, layout = layout_as_bipartite)
ProgramsARG1 <- data.frame(Degree = igraph::degree(bnARG1),
                           Closeness = igraph::closeness(bnARG1),
                           Betweennes = igraph::betweenness(bnARG1),
                           Eigen = igraph::eigen_centrality(bnARG1))
ProgramsARG1 <- ProgramsARG1[ -c(5:25) ]
rownames(ProgramsARG1)
ProgramsARG1$SS <- rownames(ProgramsARG1)
ProgramsARG1 <- ProgramsARG1[order(-ProgramsARG1$Degree), ]
#ProgramsARG1 <- ProgramsARG1[!grepl("text", ProgramsARG1$SS), ]
colnames(ProgramsARG1)[4] <- "Eigenvector"
ProgramsARG1$Node <- rownames(ProgramsARG1)
ProgramsARG1 <- mutate(ProgramsARG1, 
                       Partition = ifelse(
                         grepl("text", Node), "Program", "Skill"))
ProgramsARG1$Country <- "Argentina"
ProgramsARG1$Level <- "Specialization"

library(psych)
describeBy(ProgramsARG1$Eigenvector, group = ProgramsARG1$Partition, mat = TRUE, digits = 2)
library(network)
verticesARGSPEC <- nrow(MatrizARGSPEC) + ncol(MatrizARGSPEC)
g1 <- network.initialize(verticesARGSPEC, directed = FALSE, bipartite = TRUE)
pave1 <- network.bipartite(MatrizARGSPEC, g1)

Argentina1 <- network(pave1, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Argentina1
SizeARG1 <- network::network.size(Argentina1)
DensityARG1 <- network::network.density(Argentina1)
ClusteringARG1 <- tnet::clustering_tm(MatrizARGSPEC)
set.network.attribute(Argentina1, "Size", SizeARG1)
set.network.attribute(Argentina1, "Density", DensityARG1)
set.network.attribute(Argentina1, "Clustering", ClusteringARG1)
set.network.attribute(Argentina1, "Country", "Argentina")
set.network.attribute(Argentina1, "Level", "Specialization")
Argentina1

library(igraph)
bnARG2 <- graph_from_biadjacency_matrix(t(MatrizARGMS), directed = FALSE)
EdgeListAR2 <- as_edgelist(bnARG2)
edges_arg2 <- data.frame(
  Source = paste0("ARG_", EdgeListAR2[, 1]),
  Target = EdgeListAR2[, 2],
  Country = "Argentina"
)
bnARG2 <- graph_from_data_frame(edges_arg2, directed = FALSE)
bipartite_mapping(bnARG2)
V(bnARG2)$type <- bipartite_mapping(bnARG2)$type
V(bnARG2)$shape <- ifelse(V(bnARG2)$type, "circle", "square")
V(bnARG2)$label.cex <- ifelse(V(bnARG2)$type, 0.5, 1)
V(bnARG2)$size <- sqrt(igraph::degree(bnARG2))
E(bnARG2)$color <- "lightgrey"
plot(bnARG2, vertex.label = NA, layout = layout_as_bipartite)
ProgramsARG2 <- data.frame(Degree = igraph::degree(bnARG2),
                           Closeness = igraph::closeness(bnARG2),
                           Betweennes = igraph::betweenness(bnARG2),
                           Eigen = igraph::eigen_centrality(bnARG2))
ProgramsARG2 <- ProgramsARG2[ -c(5:25) ]
rownames(ProgramsARG2)
ProgramsARG2$SS <- rownames(ProgramsARG2)
ProgramsARG2 <- ProgramsARG2[order(-ProgramsARG2$Degree), ]
#ProgramsARG2 <- ProgramsARG2[!grepl("text", ProgramsARG2$SS), ]
colnames(ProgramsARG2)[4] <- "Eigenvector"
ProgramsARG2$Node <- rownames(ProgramsARG2)
ProgramsARG2 <- mutate(ProgramsARG2, 
                       Partition = ifelse(
                         grepl("text", Node), "Program", "Skill"))
ProgramsARG2$Country <- "Argentina"
ProgramsARG2$Level <- "Master"
library(psych)
describeBy(ProgramsARG2$Eigenvector, group = ProgramsARG2$Partition, mat = TRUE, digits = 2)
library(network)
verticesARG2 <- nrow(MatrizARGMS) + ncol(MatrizARGMS)
g2 <- network.initialize(verticesARG2, directed = FALSE, bipartite = TRUE)
pave2 <- network.bipartite(MatrizARGMS, g2)
Argentina2 <- network(pave2, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Argentina2
SizeARG2 <- network::network.size(Argentina2)
DensityARG2 <- network::network.density(Argentina2)
ClusteringARG2 <- tnet::clustering_tm(MatrizARGMS)
set.network.attribute(Argentina2, "Size", SizeARG2)
set.network.attribute(Argentina2, "Density", DensityARG2)
set.network.attribute(Argentina2, "Clustering", ClusteringARG2)
set.network.attribute(Argentina2, "Country", "Argentina")
set.network.attribute(Argentina2, "Level", "Master")
Argentina2

library(igraph)
bnARG3 <- graph_from_biadjacency_matrix(t(MatrizARGPHD), directed = FALSE)
EdgeListAR3 <- as_edgelist(bnARG3)
edges_arg3 <- data.frame(
  Source = paste0("ARG_", EdgeListAR3[, 1]),
  Target = EdgeListAR3[, 2],
  Country = "Argentina"
)
bnARG3 <- graph_from_data_frame(edges_arg3, directed = FALSE)
bipartite_mapping(bnARG3)
V(bnARG3)$type <- bipartite_mapping(bnARG3)$type
V(bnARG3)$shape <- ifelse(V(bnARG3)$type, "circle", "square")
V(bnARG3)$label.cex <- ifelse(V(bnARG3)$type, 0.5, 1)
V(bnARG3)$size <- sqrt(igraph::degree(bnARG3))
E(bnARG3)$color <- "lightgrey"
plot(bnARG3, vertex.label = NA, layout = layout_as_bipartite)

ProgramsARG3 <- data.frame(Degree = igraph::degree(bnARG3),
                           Closeness = igraph::closeness(bnARG3),
                           Betweennes = igraph::betweenness(bnARG3),
                           Eigen = igraph::eigen_centrality(bnARG3))
ProgramsARG3 <- ProgramsARG3[ -c(5:25) ]
rownames(ProgramsARG3)
ProgramsARG3$SS <- rownames(ProgramsARG3)
ProgramsARG3 <- ProgramsARG3[order(-ProgramsARG3$Degree), ]
#ProgramsARG3 <- ProgramsARG3[!grepl("text", ProgramsARG3$SS), ]
colnames(ProgramsARG3)[4] <- "Eigenvector"
ProgramsARG3$Node <- rownames(ProgramsARG3)
ProgramsARG3 <- mutate(ProgramsARG3, 
                       Partition = ifelse(
                         grepl("text", Node), "Program", "Skill"))
ProgramsARG3$Country <- "Argentina"
ProgramsARG3$Level <- "PhD"

library(psych)
describeBy(ProgramsARG3$Eigenvector, group = ProgramsARG3$Partition, mat = TRUE, digits = 2)
library(network)
verticesARG3 <- nrow(MatrizARGPHD) + ncol(MatrizARGPHD)
g3 <- network.initialize(verticesARG3, directed = FALSE, bipartite = TRUE)
pave3 <- network.bipartite(MatrizARGPHD, g3)
Argentina3 <- network(pave3, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Argentina3
SizeARG3 <- network::network.size(Argentina3)
DensityARG3 <- network::network.density(Argentina3)
ClusteringARG3 <- tnet::clustering_tm(MatrizARGPHD)
set.network.attribute(Argentina3, "Size", SizeARG3)
set.network.attribute(Argentina3, "Density", DensityARG3)
set.network.attribute(Argentina3, "Clustering", ClusteringARG3)
set.network.attribute(Argentina3, "Country", "Argentina")
set.network.attribute(Argentina3, "Level", "PhD")
Argentina3

MatrizARGSPEC <- as.matrix(t(ARG_Spec))
MatrizARGMS <- as.matrix(t(ARG_MS))
MatrizARGPHD <- as.matrix(t(ARG_PhD))
save.image("~/Documents/GitHub/SoftSkillsLatam/Results/Argentina.RData")
