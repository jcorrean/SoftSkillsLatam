library(readtext)
VEN <- readtext("Venezuela")
VEN$doc_id <- gsub("\\.pdf$|\\.docx$", "", VEN$doc_id)

library(dplyr)
VEN <- mutate(VEN, 
              Program = ifelse(
                grepl("Doctorado en", text), "Doctorado",
                ifelse(grepl("Maestría|Magíster en|MAGISTER EN", text), "Maestría", 
                       "Especialización")))
Programas <- data.frame(table(VEN$Program))
colnames(Programas)[1] <- "Programa" 
colnames(Programas)[2] <- "Total"

library(quanteda)
TextsVEN <- corpus(VEN$text)
docvars(TextsVEN, "Program") <- VEN$Program
docvars(TextsVEN, "Country") <- "Venezuela"
head(summary(TextsVEN), 10)
VENTexts <- data.frame(summary(TextsVEN, length(TextsVEN)))
VENSpec <- corpus_subset(TextsVEN, Program == "Especialización")
VENMS <- corpus_subset(TextsVEN, Program == "Maestría")
VENPhD <- corpus_subset(TextsVEN, Program == "Doctorado")

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
ProgramsVEN <- tokens(TextsVEN, 
                      remove_numbers = TRUE, 
                      remove_punct = TRUE, 
                      remove_url = TRUE, 
                      remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

VEN_Spec <- tokens(VENSpec, 
                   remove_numbers = TRUE, 
                   remove_punct = TRUE, 
                   remove_url = TRUE, 
                   remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

VEN_MS <- tokens(VENMS, 
                 remove_numbers = TRUE, 
                 remove_punct = TRUE, 
                 remove_url = TRUE, 
                 remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

VEN_PhD <- tokens(VENPhD, 
                  remove_numbers = TRUE, 
                  remove_punct = TRUE, 
                  remove_url = TRUE, 
                  remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

MatrizVESPEC <- as.matrix(t(VEN_Spec))
MatrizVEMS <- as.matrix(t(VEN_MS))
MatrizVEPHD <- as.matrix(t(VEN_PhD))

ProgramsVEN
Matriz <- as.matrix(t(ProgramsVEN))
rowSums(Matriz)

library(igraph)
bnVEN <- graph_from_biadjacency_matrix(t(Matriz), directed = FALSE)
EdgeListVE <- as_edgelist(bnVEN)
edges_ve <- data.frame(
  Source = paste0("VEN_", EdgeListVE[, 1]),
  Target = EdgeListVE[, 2],
  Country = "Venezuela"
)
bipartite_mapping(bnVEN)
V(bnVEN)$type <- bipartite_mapping(bnVEN)$type
V(bnVEN)$shape <- ifelse(V(bnVEN)$type, "circle", "square")
V(bnVEN)$label.cex <- ifelse(V(bnVEN)$type, 0.5, 1)
V(bnVEN)$size <- sqrt(igraph::degree(bnVEN))
E(bnVEN)$color <- "lightgrey"

ProgramsVEN <- data.frame(Degree = igraph::degree(bnVEN),
                          Closeness = igraph::closeness(bnVEN),
                          Betweennes = igraph::betweenness(bnVEN),
                          Eigen = igraph::eigen_centrality(bnVEN))
ProgramsVEN <- ProgramsVEN[ -c(5:25) ]
rownames(ProgramsVEN)
ProgramsVEN$SS <- rownames(ProgramsVEN)
ProgramsVEN <- ProgramsVEN[order(-ProgramsVEN$Degree), ]
#ProgramsVEN <- ProgramsVEN[!grepl("text", ProgramsVEN$SS), ]
colnames(ProgramsVEN)[4] <- "Eigenvector"
ProgramsVEN$Node <- rownames(ProgramsVEN)
ProgramsVEN <- mutate(ProgramsVEN, 
                       Partition = ifelse(
                         grepl("text", Node), "Program", "Skill"))
ProgramsVEN$Country <- "Venezuela"

library(psych)
describeBy(ProgramsVEN$Eigenvector, group = ProgramsVEN$Partition, mat = TRUE, digits = 2)
library(network)
Venezuela <- network(Matriz, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Venezuela
SizeVE <- network::network.size(Venezuela)
DensityVE <- network::network.density(Venezuela)
ClusteringVE <- tnet::clustering_tm(Matriz)
set.network.attribute(Venezuela, "Size", SizeVE)
set.network.attribute(Venezuela, "Density", DensityVE)
set.network.attribute(Venezuela, "Clustering", ClusteringVE)
Venezuela

bnVEN1 <- graph_from_biadjacency_matrix(t(MatrizVESPEC), directed = FALSE)
EdgeListVE <- as_edgelist(bnVEN1)
edges_ve1 <- data.frame(
  Source = paste0("VEN_", EdgeListVE[, 1]),
  Target = EdgeListVE[, 2],
  Country = "Venezuela"
)
bipartite_mapping(bnVEN1)
V(bnVEN1)$type <- bipartite_mapping(bnVEN1)$type
V(bnVEN1)$shape <- ifelse(V(bnVEN1)$type, "circle", "square")
V(bnVEN1)$label.cex <- ifelse(V(bnVEN1)$type, 0.5, 1)
V(bnVEN1)$size <- sqrt(igraph::degree(bnVEN1))
E(bnVEN1)$color <- "lightgrey"

ProgramsVEN1 <- data.frame(Degree = igraph::degree(bnVEN1),
                          Closeness = igraph::closeness(bnVEN1),
                          Betweennes = igraph::betweenness(bnVEN1),
                          Eigen = igraph::eigen_centrality(bnVEN1))
ProgramsVEN1 <- ProgramsVEN1[ -c(5:25) ]
rownames(ProgramsVEN1)
ProgramsVEN1$SS <- rownames(ProgramsVEN1)
ProgramsVEN1 <- ProgramsVEN1[order(-ProgramsVEN1$Degree), ]
#ProgramsVEN <- ProgramsVEN[!grepl("text", ProgramsVEN$SS), ]
colnames(ProgramsVEN1)[4] <- "Eigenvector"
ProgramsVEN1$Node <- rownames(ProgramsVEN1)
ProgramsVEN1 <- mutate(ProgramsVEN1, 
                                 Partition = ifelse(
                                   grepl("text", Node), "Program", "Skill"))
ProgramsVEN1$Country <- "Venezuela"
ProgramsVEN1$Level <- "Specialization"

library(psych)
describeBy(ProgramsVEN1$Eigenvector, group = ProgramsVEN$Partition, mat = TRUE, digits = 2)
library(network)
Venezuela1 <- network(MatrizVESPEC, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Venezuela1
SizeVE1 <- network::network.size(Venezuela1)
DensityVE1 <- network::network.density(Venezuela1)
ClusteringVE1 <- tnet::clustering_tm(MatrizVESPEC)
set.network.attribute(Venezuela1, "Size", SizeVE1)
set.network.attribute(Venezuela1, "Density", DensityVE1)
set.network.attribute(Venezuela1, "Clustering", ClusteringVE1)
Venezuela1

library(igraph)
bnVEN2 <- graph_from_biadjacency_matrix(t(MatrizVEMS), directed = FALSE)
EdgeListVE <- as_edgelist(bnVEN2)
edges_ve2 <- data.frame(
  Source = paste0("VEN_", EdgeListVE[, 1]),
  Target = EdgeListVE[, 2],
  Country = "Venezuela"
)
bipartite_mapping(bnVEN2)
V(bnVEN2)$type <- bipartite_mapping(bnVEN2)$type
V(bnVEN2)$shape <- ifelse(V(bnVEN2)$type, "circle", "square")
V(bnVEN2)$label.cex <- ifelse(V(bnVEN2)$type, 0.5, 1)
V(bnVEN2)$size <- sqrt(igraph::degree(bnVEN2))
E(bnVEN2)$color <- "lightgrey"

ProgramsVEN2 <- data.frame(Degree = igraph::degree(bnVEN2),
                           Closeness = igraph::closeness(bnVEN2),
                           Betweennes = igraph::betweenness(bnVEN2),
                           Eigen = igraph::eigen_centrality(bnVEN2))
ProgramsVEN2 <- ProgramsVEN2[ -c(5:25) ]
rownames(ProgramsVEN2)
ProgramsVEN2$SS <- rownames(ProgramsVEN2)
ProgramsVEN2 <- ProgramsVEN2[order(-ProgramsVEN2$Degree), ]
#ProgramsVEN2 <- ProgramsVEN2[!grepl("text", ProgramsVEN2$SS), ]
colnames(ProgramsVEN2)[4] <- "Eigenvector"
ProgramsVEN2$Node <- rownames(ProgramsVEN2)
ProgramsVEN2 <- mutate(ProgramsVEN2, 
                       Partition = ifelse(
                         grepl("text", Node), "Program", "Skill"))
ProgramsVEN2$Country <- "Venezuela"
ProgramsVEN2$Level <- "Master"

library(psych)
describeBy(ProgramsVEN2$Eigenvector, group = ProgramsVEN2$Partition, mat = TRUE, digits = 2)
library(network)
Venezuela2 <- network(MatrizVEMS, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Venezuela2
SizeVE2 <- network::network.size(Venezuela2)
DensityVE2 <- network::network.density(Venezuela2)
ClusteringVE2 <- tnet::clustering_tm(MatrizVEMS)
set.network.attribute(Venezuela2, "Size", SizeVE2)
set.network.attribute(Venezuela2, "Density", DensityVE2)
set.network.attribute(Venezuela2, "Clustering", ClusteringVE2)
Venezuela2

library(igraph)
bnVEN3 <- graph_from_biadjacency_matrix(t(MatrizVEPHD), directed = FALSE)
EdgeListVE <- as_edgelist(bnVEN3)
edges_ve3 <- data.frame(
  Source = paste0("VEN_", EdgeListVE[, 1]),
  Target = EdgeListVE[, 2],
  Country = "Venezuela"
)
bipartite_mapping(bnVEN3)
V(bnVEN3)$type <- bipartite_mapping(bnVEN3)$type
V(bnVEN3)$shape <- ifelse(V(bnVEN3)$type, "circle", "square")
V(bnVEN3)$label.cex <- ifelse(V(bnVEN3)$type, 0.5, 1)
V(bnVEN3)$size <- sqrt(igraph::degree(bnVEN3))
E(bnVEN3)$color <- "lightgrey"

ProgramsVEN3 <- data.frame(Degree = igraph::degree(bnVEN3),
                           Closeness = igraph::closeness(bnVEN3),
                           Betweennes = igraph::betweenness(bnVEN3),
                           Eigen = igraph::eigen_centrality(bnVEN3))
ProgramsVEN3 <- ProgramsVEN3[ -c(5:25) ]
rownames(ProgramsVEN3)
ProgramsVEN3$SS <- rownames(ProgramsVEN3)
ProgramsVEN3 <- ProgramsVEN3[order(-ProgramsVEN3$Degree), ]
#ProgramsVEN3 <- ProgramsVEN3[!grepl("text", ProgramsVEN3$SS), ]
colnames(ProgramsVEN3)[4] <- "Eigenvector"
ProgramsVEN3$Node <- rownames(ProgramsVEN3)
ProgramsVEN3 <- mutate(ProgramsVEN3, 
                       Partition = ifelse(
                         grepl("text", Node), "Program", "Skill"))
ProgramsVEN3$Country <- "Venezuela"
ProgramsVEN3$Level <- "PhD"

library(psych)
describeBy(ProgramsVEN3$Eigenvector, group = ProgramsVEN3$Partition, mat = TRUE, digits = 2)
library(network)
Venezuela3 <- network(MatrizVEPHD, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Venezuela3
SizeVE3 <- network::network.size(Venezuela3)
DensityVE3 <- network::network.density(Venezuela3)
ClusteringVE3 <- tnet::clustering_tm(MatrizVEPHD)
set.network.attribute(Venezuela3, "Size", SizeVE3)
set.network.attribute(Venezuela3, "Density", DensityVE3)
set.network.attribute(Venezuela3, "Clustering", ClusteringVE3)
Venezuela3

MatrizVESPEC <- as.matrix(t(VEN_Spec))
MatrizVEMS <- as.matrix(t(VEN_MS))
MatrizVEPHD <- as.matrix(t(VEN_PhD))


save.image("~/Documents/GitHub/SoftSkillsLatam/Results/Venezuela.RData")

