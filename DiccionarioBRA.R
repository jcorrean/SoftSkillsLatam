library(readtext)
BRA <- readtext("Brazil")
BRA$doc_id <- gsub("\\.pdf$|\\.docx$", "", BRA$doc_id)

library(dplyr)
BRA <- mutate(BRA, 
              Program = ifelse(
                grepl("Doutorado", text), "Doctorado",
                ifelse(grepl("Mestrado", text), "Maestría", 
                       "Especialização")))
Programas <- data.frame(table(BRA$Program))
colnames(Programas)[1] <- "Programa" 
colnames(Programas)[2] <- "Total"

library(tidyverse)
BRA <- BRA %>%
  mutate(University.Code = str_extract(doc_id, "^\\d+"))

library(quanteda)
TextsBRA <- corpus(BRA$text)
docvars(TextsBRA, "Program") <- BRA$Program
docvars(TextsBRA, "Country") <- "Brazil"
head(summary(TextsBRA), 10)
BRATexts <- data.frame(summary(TextsBRA, length(TextsBRA)))
BRASpec <- corpus_subset(TextsBRA, Program == "Especialização")
BRAMS <- corpus_subset(TextsBRA, Program == "Maestría")
BRAPhD <- corpus_subset(TextsBRA, Program == "Doctorado")

Dictionary <- dictionary(list(
  active_listening = c("escuta*", "pergunta*", "questiona*", "entende*", "compreende*", "silêncio"),
  mathematics = c("matemática", "resolver problemas matemáticos", "cálculos", "calcular"),
  reading_comprehension = c("leitura", "ler", "orações", "parágrafo*", "textos", "documento*"),
  science = c("ciência*", "científico*", "método*", "resolver problemas", "investiga*"),
  speaking = c("oratória", "comunica*"),
  writing = c("escreve*", "redata*", "escrito"),
  active_learning = c("implicações", "compreende", "decisão", "futuro*", "nova informação"),
  critical_thinking = c("crítico", "pensamento crítico", "lógico*", "racional*"),
  learning_strategy = c("aprend*", "ensina*", "instrução*"),
  monitoring = c("auto-avalia*", "reflexivo*", "desempenho", "execução")
))

ProgramsBRA <- tokens(TextsBRA,
                      remove_numbers = TRUE,
                      remove_punct = TRUE,
                      remove_url = TRUE,
                      remove_symbols = TRUE) %>%
  tokens_remove(stopwords("portuguese")) %>% tokens_lookup(dictionary = Dictionary) %>%
  dfm()
BRA_Spec <- tokens(BRASpec, 
                    remove_numbers = TRUE, 
                    remove_punct = TRUE, 
                    remove_url = TRUE, 
                    remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

BRA_MS <- tokens(BRAMS, 
                  remove_numbers = TRUE, 
                  remove_punct = TRUE, 
                  remove_url = TRUE, 
                  remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

BRA_PhD <- tokens(BRAPhD, 
                   remove_numbers = TRUE, 
                   remove_punct = TRUE, 
                   remove_url = TRUE, 
                   remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

MatrizBRASPEC <- as.matrix(BRA_Spec)
MatrizBRAMS <- as.matrix(BRA_MS)
MatrizBRAPHD <- as.matrix(BRA_PhD)
ProgramsBRA
Matriz <- as.matrix(ProgramsBRA)
rowSums(Matriz)

library(network)
Brazil <- as.network(Matriz, matrix.type = "adjacency", directed = FALSE, bipartite = TRUE)
Brazil1 <- as.network(MatrizBRASPEC, matrix.type = "adjacency", directed = FALSE, bipartite = TRUE)
Brazil2 <- as.network(MatrizBRAMS, matrix.type = "adjacency", directed = FALSE, bipartite = TRUE)
Brazil3 <- as.network(MatrizBRAPHD, matrix.type = "adjacency", directed = FALSE, bipartite = TRUE)
SizeBRA <- network::network.size(Brazil)
DensityBRA <- network::network.density(Brazil)
ClusteringBRA <- tnet::clustering_tm(t(Matriz))
set.network.attribute(Brazil, "Size", SizeBRA)
set.network.attribute(Brazil, "Density", DensityBRA)
set.network.attribute(Brazil, "Clustering", ClusteringBRA)
set.network.attribute(Brazil, "Country", "Brazil")
set.network.attribute(Brazil, "Level", "All")
set.network.attribute(Brazil, "OECD", FALSE)
Brazil

library(igraph)
bnBRA <- graph_from_biadjacency_matrix(t(Matriz), directed = FALSE)
edges_br <- data.frame()
for (i in 1:nrow(Matriz)) {
  for (j in 1:ncol(Matriz)) {
    if (Matriz[i, j] > 0) { # Only include edges where there's a connection
      edges_br <- rbind(edges_br, data.frame(
        Source = paste0("BRA_", rownames(Matriz)[i]),
        Target = colnames(Matriz)[j],
        Weight = Matriz[i, j], # Store the weight
        Country = "Brazil"
      ))
    }
  }
}


bnBRA <- graph_from_data_frame(edges_br, directed = FALSE)
bipartite_mapping(bnBRA)
V(bnBRA)$type <- bipartite_mapping(bnBRA)$type
V(bnBRA)$shape <- ifelse(V(bnBRA)$type, "circle", "square")
V(bnBRA)$label.cex <- ifelse(V(bnBRA)$type, 0.5, 1)
V(bnBRA)$size <- sqrt(igraph::degree(bnBRA))
E(bnBRA)$color <- "lightgrey"
E(bnBRA)$weight <- edges_br$Weight
network::set.edge.attribute(Brazil, "Frecuencia", edges_br$Weight)
Frecuencias <- as.sociomatrix(Brazil, attrname = "Frecuencia")
Brazil

ProgramsBRA <- data.frame(Degree = igraph::degree(bnBRA),
                          Closeness = igraph::closeness(bnBRA),
                          Betweennes = igraph::betweenness(bnBRA),
                          Eigen = igraph::eigen_centrality(bnBRA))
ProgramsBRA <- ProgramsBRA[ -c(5:25) ]
rownames(ProgramsBRA)
ProgramsBRA$SS <- rownames(ProgramsBRA)
ProgramsBRA <- ProgramsBRA[order(-ProgramsBRA$Degree), ]
#ProgramsBRA <- ProgramsBRA[!grepl("text", ProgramsBRA$SS), ]
colnames(ProgramsBRA)[4] <- "Eigenvector"
ProgramsBRA$Node <- rownames(ProgramsBRA)
ProgramsBRA <- mutate(ProgramsBRA, 
                       Partition = ifelse(
                         grepl("text", Node), "Program", "Skill"))
ProgramsBRA$Country <- "Brazil"

library(psych)
describeBy(ProgramsBRA$Eigenvector, group = ProgramsBRA$Partition, mat = TRUE, digits = 2)



bnBRA1 <- graph_from_biadjacency_matrix(t(MatrizBRASPEC), directed = FALSE)
EdgeListBR1 <- as_edgelist(bnBRA1)
edges_br1 <- data.frame(
  Source = paste0("BRA_", EdgeListBR1[, 1]),
  Target = EdgeListBR1[, 2],
  Country = "Brazil"
)
bnBRA1 <- graph_from_data_frame(edges_br1, directed = FALSE)
bipartite_mapping(bnBRA1)
V(bnBRA1)$type <- bipartite_mapping(bnBRA1)$type
V(bnBRA1)$shape <- ifelse(V(bnBRA1)$type, "circle", "square")
V(bnBRA1)$label.cex <- ifelse(V(bnBRA1)$type, 0.5, 1)
V(bnBRA1)$size <- sqrt(igraph::degree(bnBRA1))
E(bnBRA1)$color <- "lightgrey"

ProgramsBRA1 <- data.frame(Degree = igraph::degree(bnBRA1),
                            Closeness = igraph::closeness(bnBRA1),
                            Betweennes = igraph::betweenness(bnBRA1),
                            Eigen = igraph::eigen_centrality(bnBRA1))
ProgramsBRA1 <- ProgramsBRA1[ -c(5:25) ]
rownames(ProgramsBRA1)
ProgramsBRA1$SS <- rownames(ProgramsBRA1)
ProgramsBRA1 <- ProgramsBRA1[order(-ProgramsBRA1$Degree), ]
#ProgramsVEN <- ProgramsVEN[!grepl("text", ProgramsVEN$SS), ]
colnames(ProgramsBRA1)[4] <- "Eigenvector"
ProgramsBRA1$Node <- rownames(ProgramsBRA1)
ProgramsBRA1 <- mutate(ProgramsBRA1, 
                        Partition = ifelse(
                          grepl("text", Node), "Program", "Skill"))
ProgramsBRA1$Country <- "Brazil"
ProgramsBRA1$Level <- "Specialization"

psych::describeBy(ProgramsBRA1$Eigenvector, group = ProgramsBRA1$Partition, mat = TRUE, digits = 2)
library(network)

verticesBRA1 <- nrow(MatrizBRASPEC) + ncol(MatrizBRASPEC)
g1 <- network.initialize(verticesBRA1, directed = FALSE, bipartite = TRUE)
pave1 <- network.bipartite(MatrizBRASPEC, g1)

Brazil1 <- network(pave1, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Brazil1
SizeBRA1 <- network::network.size(Brazil1)
DensityBRA1 <- network::network.density(Brazil1)
ClusteringBRA1 <- tnet::clustering_tm(MatrizBRASPEC)
set.network.attribute(Brazil1, "Size", SizeBRA1)
set.network.attribute(Brazil1, "Density", DensityBRA1)
set.network.attribute(Brazil1, "Clustering", ClusteringBRA1)
set.network.attribute(Brazil1, "Country", "Brazil")
set.network.attribute(Brazil1, "Level", "Specialization")
Brazil1

bnBRA2 <- graph_from_biadjacency_matrix(t(MatrizBRAMS), directed = FALSE)
EdgeListBR2 <- as_edgelist(bnBRA2)
edges_br2 <- data.frame(
  Source = paste0("BR_", EdgeListBR2[, 1]),
  Target = EdgeListBR2[, 2],
  Country = "Brazil"
)
bipartite_mapping(bnBRA2)
V(bnBRA2)$type <- bipartite_mapping(bnBRA2)$type
V(bnBRA2)$shape <- ifelse(V(bnBRA2)$type, "circle", "square")
V(bnBRA2)$label.cex <- ifelse(V(bnBRA2)$type, 0.5, 1)
V(bnBRA2)$size <- sqrt(igraph::degree(bnBRA2))
E(bnBRA2)$color <- "lightgrey"

ProgramsBRA2 <- data.frame(Degree = igraph::degree(bnBRA2),
                            Closeness = igraph::closeness(bnBRA2),
                            Betweennes = igraph::betweenness(bnBRA2),
                            Eigen = igraph::eigen_centrality(bnBRA2))
ProgramsBRA2 <- ProgramsBRA2[ -c(5:25) ]
rownames(ProgramsBRA2)
ProgramsBRA2$SS <- rownames(ProgramsBRA2)
ProgramsBRA2 <- ProgramsBRA2[order(-ProgramsBRA2$Degree), ]
#ProgramsBRA2 <- ProgramsBRA2[!grepl("text", ProgramsBRA2$SS), ]
colnames(ProgramsBRA2)[4] <- "Eigenvector"
ProgramsBRA2$Node <- rownames(ProgramsBRA2)
ProgramsBRA2 <- mutate(ProgramsBRA2, 
                        Partition = ifelse(
                          grepl("text", Node), "Program", "Skill"))
ProgramsBRA2$Country <- "Brazil"
ProgramsBRA2$Level <- "Master"
psych::describeBy(ProgramsBRA2$Eigenvector, group = ProgramsBRA2$Partition, mat = TRUE, digits = 2)
library(network)
verticesBRA2 <- nrow(MatrizBRAMS) + ncol(MatrizBRAMS)
g2 <- network.initialize(verticesBRA2, directed = FALSE, bipartite = TRUE)
pave2 <- network.bipartite(MatrizBRAMS, g2)
Brazil2 <- network(pave2, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Brazil2
SizeBRA2 <- network::network.size(Brazil2)
DensityBRA2 <- network::network.density(Brazil2)
ClusteringBRA2 <- tnet::clustering_tm(MatrizBRASPEC)
set.network.attribute(Brazil2, "Size", SizeBRA2)
set.network.attribute(Brazil2, "Density", DensityBRA2)
set.network.attribute(Brazil2, "Clustering", ClusteringBRA2)
set.network.attribute(Brazil2, "Country", "Brazil")
set.network.attribute(Brazil2, "Level", "Master")
Brazil2

bnBRA3 <- graph_from_biadjacency_matrix(t(MatrizBRAPHD), directed = FALSE)
EdgeListBR3 <- as_edgelist(bnBRA3)
edges_br3 <- data.frame(
  Source = paste0("BRA_", EdgeListBR3[, 1]),
  Target = EdgeListBR3[, 2],
  Country = "Brazil"
)
bipartite_mapping(bnBRA3)
V(bnBRA3)$type <- bipartite_mapping(bnBRA3)$type
V(bnBRA3)$shape <- ifelse(V(bnBRA3)$type, "circle", "square")
V(bnBRA3)$label.cex <- ifelse(V(bnBRA3)$type, 0.5, 1)
V(bnBRA3)$size <- sqrt(igraph::degree(bnBRA3))
E(bnBRA3)$color <- "lightgrey"

ProgramsBRA3 <- data.frame(Degree = igraph::degree(bnBRA3),
                            Closeness = igraph::closeness(bnBRA3),
                            Betweennes = igraph::betweenness(bnBRA3),
                            Eigen = igraph::eigen_centrality(bnBRA3))
ProgramsBRA3 <- ProgramsBRA3[ -c(5:25) ]
rownames(ProgramsBRA3)
ProgramsBRA3$SS <- rownames(ProgramsBRA3)
ProgramsBRA3 <- ProgramsBRA3[order(-ProgramsBRA3$Degree), ]
#ProgramsBRA3 <- ProgramsBRA3[!grepl("text", ProgramsBRA3$SS), ]
colnames(ProgramsBRA3)[4] <- "Eigenvector"
ProgramsBRA3$Node <- rownames(ProgramsBRA3)
ProgramsBRA3 <- mutate(ProgramsBRA3, 
                        Partition = ifelse(
                          grepl("text", Node), "Program", "Skill"))
ProgramsBRA3$Country <- "Brazil"
ProgramsBRA3$Level <- "PhD"


psych::describeBy(ProgramsBRA3$Eigenvector, group = ProgramsBRA3$Partition, mat = TRUE, digits = 2)
library(network)
verticesBRA3 <- nrow(MatrizBRAPHD) + ncol(MatrizBRAPHD)
g3 <- network.initialize(verticesBRA3, directed = FALSE, bipartite = TRUE)
pave3 <- network.bipartite(MatrizBRAPHD, g3)
Brazil3 <- network(pave3, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Brazil3
SizeBRA3 <- network::network.size(Brazil3)
DensityBRA3 <- network::network.density(Brazil3)
ClusteringBRA3 <- tnet::clustering_tm(MatrizBRAPHD)
set.network.attribute(Brazil3, "Size", SizeBRA3)
set.network.attribute(Brazil3, "Density", DensityBRA3)
set.network.attribute(Brazil3, "Clustering", ClusteringBRA3)
set.network.attribute(Brazil3, "Country", "Brazil")
set.network.attribute(Brazil3, "Level", "PhD")
Brazil3

MatrizBRASPEC <- as.matrix(t(BRA_Spec))
MatrizBRAMS <- as.matrix(t(BRA_MS))
MatrizBRAPHD <- as.matrix(t(BRA_PhD))
save.image("~/Documents/GitHub/SoftSkillsLatam/Results/Brazil.RData")
