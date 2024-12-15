library(readtext)
URU <- readtext("Uruguay")
URU$doc_id <- gsub("\\.pdf$|\\.docx$", "", URU$doc_id)

library(dplyr)
URU <- mutate(URU, 
              Program = ifelse(
                grepl("Doctorado en", text), "Doctorado",
                ifelse(grepl("Maestría|Magíster en|MAGISTER EN", text), "Maestría", 
                       "Especialización")))
Programas <- data.frame(table(URU$Program))
colnames(Programas)[1] <- "Programa" 
colnames(Programas)[2] <- "Total"

library(quanteda)
TextsURU <- corpus(URU$text)
docvars(TextsURU, "Program") <- URU$Program
docvars(TextsURU, "Country") <- "Uruguay"
head(summary(TextsURU), 10)
URUTexts <- data.frame(summary(TextsURU, length(TextsURU)))
URUSpec <- corpus_subset(TextsURU, Program == "Especialización")
URUMS <- corpus_subset(TextsURU, Program == "Maestría")
URUPhD <- corpus_subset(TextsURU, Program == "Doctorado")

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
  monitoring = c("auto-evalu*",  "reflexi*", "desempeñ", "ejMEXción")
))
ProgramsURU <- tokens(TextsURU, 
                      remove_numbers = TRUE, 
                      remove_punct = TRUE, 
                      remove_url = TRUE, 
                      remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

URU_Spec <- tokens(URUSpec, 
                   remove_numbers = TRUE, 
                   remove_punct = TRUE, 
                   remove_url = TRUE, 
                   remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

URU_MS <- tokens(URUMS, 
                 remove_numbers = TRUE, 
                 remove_punct = TRUE, 
                 remove_url = TRUE, 
                 remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

URU_PhD <- tokens(URUPhD, 
                  remove_numbers = TRUE, 
                  remove_punct = TRUE, 
                  remove_url = TRUE, 
                  remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

MatrizURUSPEC <- as.matrix(t(URU_Spec))
MatrizURUMS <- as.matrix(t(URU_MS))
MatrizURUPHD <- as.matrix(t(URU_PhD))
ProgramsURU
Matriz <- as.matrix(t(ProgramsURU))
rowSums(Matriz)

library(igraph)
bnURU <- graph_from_biadjacency_matrix(t(Matriz), directed = FALSE)
EdgeListURU <- as_edgelist(bnURU)
edges_ur <- data.frame(
  Source = paste0("URU_", EdgeListURU[, 1]),
  Target = EdgeListURU[, 2],
  Country = "Uruguay"
)
bipartite_mapping(bnURU)
V(bnURU)$type <- bipartite_mapping(bnURU)$type
V(bnURU)$shape <- ifelse(V(bnURU)$type, "circle", "square")
V(bnURU)$label.cex <- ifelse(V(bnURU)$type, 0.5, 1)
V(bnURU)$size <- sqrt(igraph::degree(bnURU))
E(bnURU)$color <- "lightgrey"

ProgramsURU <- data.frame(Degree = igraph::degree(bnURU),
                          Closeness = igraph::closeness(bnURU),
                          Betweennes = igraph::betweenness(bnURU),
                          Eigen = igraph::eigen_centrality(bnURU))
ProgramsURU <- ProgramsURU[ -c(5:25) ]
rownames(ProgramsURU)
ProgramsURU$SS <- rownames(ProgramsURU)
ProgramsURU <- ProgramsURU[order(-ProgramsURU$Degree), ]
#ProgramsURU <- ProgramsURU[!grepl("text", ProgramsURU$SS), ]
colnames(ProgramsURU)[4] <- "Eigenvector"
ProgramsURU$Node <- rownames(ProgramsURU)
ProgramsURU <- mutate(ProgramsURU, 
                      Partition = ifelse(
                        grepl("text", Node), "Program", "Skill"))
ProgramsURU$Country <- "Uruguay"

psych::describeBy(ProgramsURU$Eigenvector, group = ProgramsURU$Partition, mat = TRUE, digits = 2)
library(network)
Uruguay <- network(Matriz, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Uruguay
SizeURU <- network::network.size(Uruguay)
DensityURU <- network::network.density(Uruguay)
ClusteringURU <- tnet::clustering_tm(Matriz)
set.network.attribute(Uruguay, "Size", SizeURU)
set.network.attribute(Uruguay, "Density", DensityURU)
set.network.attribute(Uruguay, "Clustering", ClusteringURU)
Uruguay

library(igraph)
bnURU1 <- graph_from_biadjacency_matrix(t(MatrizURUSPEC), directed = FALSE)
EdgeListVE <- as_edgelist(bnURU1)
edges_ur1 <- data.frame(
  Source = paste0("URU_", EdgeListVE[, 1]),
  Target = EdgeListVE[, 2],
  Country = "Uruguay"
)
bipartite_mapping(bnURU1)
V(bnURU1)$type <- bipartite_mapping(bnURU1)$type
V(bnURU1)$shape <- ifelse(V(bnURU1)$type, "circle", "square")
V(bnURU1)$label.cex <- ifelse(V(bnURU1)$type, 0.5, 1)
V(bnURU1)$size <- sqrt(igraph::degree(bnURU1))
E(bnURU1)$color <- "lightgrey"

ProgramsURU1 <- data.frame(Degree = igraph::degree(bnURU1),
                           Closeness = igraph::closeness(bnURU1),
                           Betweennes = igraph::betweenness(bnURU1),
                           Eigen = igraph::eigen_centrality(bnURU1))
ProgramsURU1 <- ProgramsURU1[ -c(5:25) ]
rownames(ProgramsURU1)
ProgramsURU1$SS <- rownames(ProgramsURU1)
ProgramsURU1 <- ProgramsURU1[order(-ProgramsURU1$Degree), ]
#ProgramsURU1 <- ProgramsURU1[!grepl("text", ProgramsURU1$SS), ]
colnames(ProgramsURU1)[4] <- "Eigenvector"
ProgramsURU1$Node <- rownames(ProgramsURU1)
ProgramsURU1 <- mutate(ProgramsURU1, 
                       Partition = ifelse(
                         grepl("text", Node), "Program", "Skill"))
ProgramsURU1$Country <- "Uruguay"
ProgramsURU1$Level <- "Specialization"

psych::describeBy(ProgramsURU1$Eigenvector, group = ProgramsURU1$Partition, mat = TRUE, digits = 2)
library(network)
Uruguay1 <- network(MatrizURUSPEC, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Uruguay1
SizeURU1 <- network::network.size(Uruguay1)
DensityURU1 <- network::network.density(Uruguay1)
ClusteringURU1 <- tnet::clustering_tm(MatrizURUSPEC)

library(igraph)
bnURU2 <- graph_from_biadjacency_matrix(t(MatrizURUMS), directed = FALSE)
EdgeListVE <- as_edgelist(bnURU2)
edges_ur2 <- data.frame(
  Source = paste0("URU_", EdgeListVE[, 1]),
  Target = EdgeListVE[, 2],
  Country = "Uruguay"
)
bipartite_mapping(bnURU2)
V(bnURU2)$type <- bipartite_mapping(bnURU2)$type
V(bnURU2)$shape <- ifelse(V(bnURU2)$type, "circle", "square")
V(bnURU2)$label.cex <- ifelse(V(bnURU2)$type, 0.5, 1)
V(bnURU2)$size <- sqrt(igraph::degree(bnURU2))
E(bnURU2)$color <- "lightgrey"

ProgramsURU2 <- data.frame(Degree = igraph::degree(bnURU2),
                           Closeness = igraph::closeness(bnURU2),
                           Betweennes = igraph::betweenness(bnURU2),
                           Eigen = igraph::eigen_centrality(bnURU2))
ProgramsURU2 <- ProgramsURU2[ -c(5:25) ]
rownames(ProgramsURU2)
ProgramsURU2$SS <- rownames(ProgramsURU2)
ProgramsURU2 <- ProgramsURU2[order(-ProgramsURU2$Degree), ]
#ProgramsURU2 <- ProgramsURU2[!grepl("text", ProgramsURU2$SS), ]
colnames(ProgramsURU2)[4] <- "Eigenvector"
ProgramsURU2$Node <- rownames(ProgramsURU2)
ProgramsURU2 <- mutate(ProgramsURU2, 
                       Partition = ifelse(
                         grepl("text", Node), "Program", "Skill"))
ProgramsURU2$Country <- "Uruguay"
ProgramsURU2$Level <- "Master"
psych::describeBy(ProgramsURU2$Eigenvector, group = ProgramsURU2$Partition, mat = TRUE, digits = 2)
library(network)
Uruguay2 <- network(MatrizURUMS, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Uruguay2
SizeURU2 <- network::network.size(Uruguay2)
DensityURU2 <- network::network.density(Uruguay2)
ClusteringURU2 <- tnet::clustering_tm(MatrizURUMS)

library(igraph)
bnURU3 <- graph_from_biadjacency_matrix(t(MatrizURUPHD), directed = FALSE)
EdgeListVE <- as_edgelist(bnURU3)
edges_ur3 <- data.frame(
  Source = paste0("URU_", EdgeListVE[, 1]),
  Target = EdgeListVE[, 2],
  Country = "Uruguay"
)
bipartite_mapping(bnURU3)
V(bnURU3)$type <- bipartite_mapping(bnURU3)$type
V(bnURU3)$shape <- ifelse(V(bnURU3)$type, "circle", "square")
V(bnURU3)$label.cex <- ifelse(V(bnURU3)$type, 0.5, 1)
V(bnURU3)$size <- sqrt(igraph::degree(bnURU3))
E(bnURU3)$color <- "lightgrey"

ProgramsURU3 <- data.frame(Degree = igraph::degree(bnURU3),
                           Closeness = igraph::closeness(bnURU3),
                           Betweennes = igraph::betweenness(bnURU3),
                           Eigen = igraph::eigen_centrality(bnURU3))
ProgramsURU3 <- ProgramsURU3[ -c(5:25) ]
rownames(ProgramsURU3)
ProgramsURU3$SS <- rownames(ProgramsURU3)
ProgramsURU3 <- ProgramsURU3[order(-ProgramsURU3$Degree), ]
#ProgramsURU3 <- ProgramsURU3[!grepl("text", ProgramsURU3$SS), ]
colnames(ProgramsURU3)[4] <- "Eigenvector"
ProgramsURU3$Node <- rownames(ProgramsURU3)
ProgramsURU3 <- mutate(ProgramsURU3, 
                       Partition = ifelse(
                         grepl("text", Node), "Program", "Skill"))
ProgramsURU3$Country <- "Uruguay"
ProgramsURU3$Level <- "PhD"

psych::describeBy(ProgramsURU3$Eigenvector, group = ProgramsURU3$Partition, mat = TRUE, digits = 2)

Uruguay3 <- network(MatrizURUPHD, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Uruguay3
SizeURU3 <- network::network.size(Uruguay3)
DensityURU3 <- network::network.density(Uruguay3)
ClusteringURU3 <- tnet::clustering_tm(MatrizURUPHD)

save.image("~/Documents/GitHub/SoftSkillsLatam/Results/Uruguay.RData")

