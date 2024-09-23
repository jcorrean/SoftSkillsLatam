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

MatrizBRASPEC <- as.matrix(t(BRA_Spec))
MatrizBRAMS <- as.matrix(t(BRA_MS))
MatrizBRAPHD <- as.matrix(t(BRA_PhD))
ProgramsBRA
Matriz <- as.matrix(t(ProgramsBRA))
rowSums(Matriz)
library(igraph)
bnBRA <- graph_from_biadjacency_matrix(t(Matriz), directed = FALSE)
EdgeListBR <- as_edgelist(bnBRA)
edges_br <- data.frame(
  Source = paste0("BRA_", EdgeListBR[, 1]),
  Target = EdgeListBR[, 2],
  Country = "Brazil"
)
bipartite_mapping(bnBRA)
V(bnBRA)$type <- bipartite_mapping(bnBRA)$type
V(bnBRA)$shape <- ifelse(V(bnBRA)$type, "circle", "square")
V(bnBRA)$label.cex <- ifelse(V(bnBRA)$type, 0.5, 1)
V(bnBRA)$size <- sqrt(igraph::degree(bnBRA))
E(bnBRA)$color <- "lightgrey"

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


library(network)
Brazil <- network(Matriz, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Brazil
SizeBR <- network::network.size(Brazil)
DensityBR <- network::network.density(Brazil)
ClusteringBR <- tnet::clustering_tm(Matriz)

bnBRA1 <- graph_from_biadjacency_matrix(t(MatrizBRASPEC), directed = FALSE)
EdgeListCOL <- as_edgelist(bnBRA1)
edges_ve <- data.frame(
  Source = paste0("BRA_", EdgeListCOL[, 1]),
  Target = EdgeListCOL[, 2],
  Country = "Costa Rica"
)
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
Brazil1 <- network(MatrizBRASPEC, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Brazil1
SizeBRA1 <- network::network.size(Brazil1)
DensityBRA1 <- network::network.density(Brazil1)
ClusteringBRA1 <- tnet::clustering_tm(MatrizBRASPEC)

bnBRA2 <- graph_from_biadjacency_matrix(t(MatrizBRAMS), directed = FALSE)
EdgeListCOL <- as_edgelist(bnBRA2)
edges_ve <- data.frame(
  Source = paste0("BR_", EdgeListCOL[, 1]),
  Target = EdgeListCOL[, 2],
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
Brazil2 <- network(MatrizBRAMS, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Brazil2
SizeBRA2 <- network::network.size(Brazil2)
DensityBRA2 <- network::network.density(Brazil2)
ClusteringCORI1 <- tnet::clustering_tm(MatrizCORISPEC)

bnCORI3 <- graph_from_biadjacency_matrix(t(MatrizCORIPHD), directed = FALSE)
EdgeListCOL <- as_edgelist(bnCORI3)
edges_ve <- data.frame(
  Source = paste0("CR_", EdgeListCOL[, 1]),
  Target = EdgeListCOL[, 2],
  Country = "CostaRica"
)
bipartite_mapping(bnCORI3)
V(bnCORI3)$type <- bipartite_mapping(bnCORI3)$type
V(bnCORI3)$shape <- ifelse(V(bnCORI3)$type, "circle", "square")
V(bnCORI3)$label.cex <- ifelse(V(bnCORI3)$type, 0.5, 1)
V(bnCORI3)$size <- sqrt(igraph::degree(bnCORI3))
E(bnCORI3)$color <- "lightgrey"

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
CostaRica3 <- network(MatrizCORIPHD, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
CostaRica3
SizeCOL3 <- network::network.size(CostaRica3)
DensityCOL3 <- network::network.density(CostaRica3)
ClusteringCOL3 <- tnet::clustering_tm(MatrizCORIPHD)

MatrizBRASPEC <- as.matrix(t(CORI_Spec))
MatrizBRAMS <- as.matrix(t(CORI_MS))
MatrizCORIPHD <- as.matrix(t(CORI_PhD))
save.image("~/Documents/GitHub/SoftSkillsLatam/Results/Brazil.RData")
