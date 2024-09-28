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

MatrizECUSPEC <- as.matrix(t(ECU_Spec))
MatrizECUMS <- as.matrix(t(ECU_MS))
MatrizECUPHD <- as.matrix(t(ECU_PhD))

ProgramsECU
Matriz <- as.matrix(t(ProgramsECU))
rowSums(Matriz)

library(igraph)
bnECU <- graph_from_biadjacency_matrix(t(Matriz), directed = FALSE)
EdgeListECU <- as_edgelist(bnECU)
edges_ec <- data.frame(
  Source = paste0("ECU_", EdgeListECU[, 1]),
  Target = EdgeListECU[, 2],
  Country = "Ecuador"
)
bipartite_mapping(bnECU)
V(bnECU)$type <- bipartite_mapping(bnECU)$type
V(bnECU)$shape <- ifelse(V(bnECU)$type, "circle", "square")
V(bnECU)$label.cex <- ifelse(V(bnECU)$type, 0.5, 1)
V(bnECU)$size <- sqrt(igraph::degree(bnECU))
E(bnECU)$color <- "lightgrey"

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
library(network)
Ecuador <- network(Matriz, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Ecuador
SizeECU <- network::network.size(Ecuador)
DensityECU <- network::network.density(Ecuador)
ClusteringECU <- tnet::clustering_tm(Matriz)

bnECU1 <- graph_from_biadjacency_matrix(t(MatrizECUSPEC), directed = FALSE)
EdgeListVE <- as_edgelist(bnECU1)
edges_ec1 <- data.frame(
  Source = paste0("ECU_", EdgeListVE[, 1]),
  Target = EdgeListVE[, 2],
  Country = "Ecuador"
)
bipartite_mapping(bnECU1)
V(bnECU1)$type <- bipartite_mapping(bnECU1)$type
V(bnECU1)$shape <- ifelse(V(bnECU1)$type, "circle", "square")
V(bnECU1)$label.cex <- ifelse(V(bnECU1)$type, 0.5, 1)
V(bnECU1)$size <- sqrt(igraph::degree(bnECU1))
E(bnECU1)$color <- "lightgrey"

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
Ecuador1 <- network(MatrizECUSPEC, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Ecuador1
SizeECU1 <- network::network.size(Ecuador1)
DensityECU1 <- network::network.density(Ecuador1)
ClusteringECU1 <- tnet::clustering_tm(MatrizECUSPEC)

bnECU2 <- graph_from_biadjacency_matrix(t(MatrizECUMS), directed = FALSE)
EdgeListVE <- as_edgelist(bnECU2)
edges_ec2 <- data.frame(
  Source = paste0("ECU_", EdgeListVE[, 1]),
  Target = EdgeListVE[, 2],
  Country = "Ecuador"
)
bipartite_mapping(bnECU2)
V(bnECU2)$type <- bipartite_mapping(bnECU2)$type
V(bnECU2)$shape <- ifelse(V(bnECU2)$type, "circle", "square")
V(bnECU2)$label.cex <- ifelse(V(bnECU2)$type, 0.5, 1)
V(bnECU2)$size <- sqrt(igraph::degree(bnECU2))
E(bnECU2)$color <- "lightgrey"

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
Ecuador2 <- network(MatrizECUMS, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Ecuador2
SizeECU2 <- network::network.size(Ecuador2)
DensityECU2 <- network::network.density(Ecuador2)
ClusteringECU2 <- tnet::clustering_tm(MatrizECUMS)

bnECU3 <- graph_from_biadjacency_matrix(t(MatrizECUPHD), directed = FALSE)
EdgeListVE <- as_edgelist(bnECU3)
edges_ec3 <- data.frame(
  Source = paste0("ECU_", EdgeListVE[, 1]),
  Target = EdgeListVE[, 2],
  Country = "Ecuador"
)
bipartite_mapping(bnECU3)
V(bnECU3)$type <- bipartite_mapping(bnECU3)$type
V(bnECU3)$shape <- ifelse(V(bnECU3)$type, "circle", "square")
V(bnECU3)$label.cex <- ifelse(V(bnECU3)$type, 0.5, 1)
V(bnECU3)$size <- sqrt(igraph::degree(bnECU3))
E(bnECU3)$color <- "lightgrey"

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

Ecuador3 <- network(MatrizECUPHD, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Ecuador3
SizeECU3 <- network::network.size(Ecuador3)
DensityECU3 <- network::network.density(Ecuador3)
ClusteringECU3 <- tnet::clustering_tm(MatrizECUPHD)

MatrizECUSPEC <- as.matrix(t(ECU_Spec))
MatrizECUMS <- as.matrix(t(ECU_MS))
MatrizECUPHD <- as.matrix(t(ECU_PhD))

save.image("~/Documents/GitHub/SoftSkillsLatam/Results/Ecuador.RData")
