library(readtext)
CHL <- readtext("Chile")
CHL$doc_id <- gsub("\\.pdf$|\\.docx$", "", CHL$doc_id)

library(dplyr)
CHL <- mutate(CHL, 
             Program = ifelse(
               grepl("Doctorado en", text), "Doctorado",
               ifelse(grepl("Maestría|Magíster en|MAGISTER EN", text), "Maestría", 
                      "Especialización")))
Programas <- data.frame(table(CHL$Program))
colnames(Programas)[1] <- "Programa" 
colnames(Programas)[2] <- "Total"

library(tidyverse)
CHL <- CHL %>%
  mutate(University.Code = str_extract(doc_id, "^\\d+"))

library(quanteda)
TextsCHL <- corpus(CHL$text)
docvars(TextsCHL, "Program") <- CHL$Program
docvars(TextsCHL, "Country") <- "Chile"
head(summary(TextsCHL), 10)
CHLTexts <- data.frame(summary(TextsCHL, length(TextsCHL)))
CHLSpec <- corpus_subset(TextsCHL, Program == "Especialización")
CHLMS <- corpus_subset(TextsCHL, Program == "Maestría")
CHLPhD <- corpus_subset(TextsCHL, Program == "Doctorado")

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
ProgramsCHL <- tokens(TextsCHL, 
                      remove_numbers = TRUE, 
                      remove_punct = TRUE, 
                      remove_url = TRUE, 
                      remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

CHL_Spec <- tokens(CHLSpec, 
                    remove_numbers = TRUE, 
                    remove_punct = TRUE, 
                    remove_url = TRUE, 
                    remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

CHL_MS <- tokens(CHLMS, 
                  remove_numbers = TRUE, 
                  remove_punct = TRUE, 
                  remove_url = TRUE, 
                  remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

CHL_PhD <- tokens(CHLPhD, 
                   remove_numbers = TRUE, 
                   remove_punct = TRUE, 
                   remove_url = TRUE, 
                   remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

MatrizCHLSPEC <- as.matrix(CHL_Spec)
MatrizCHLMS <- as.matrix(CHL_MS)
MatrizCHLPHD <- as.matrix(CHL_PhD)
ProgramsCHL
Matriz <- as.matrix(ProgramsCHL)
rowSums(Matriz)

library(network)
Chile <- as.network(Matriz, matrix.type = "adjacency", directed = FALSE, bipartite = TRUE)
Chile1 <- as.network(MatrizCHLSPEC, matrix.type = "adjacency", directed = FALSE, bipartite = TRUE)
Chile2 <- as.network(MatrizCHLMS, matrix.type = "adjacency", directed = FALSE, bipartite = TRUE)
Chile3 <- as.network(MatrizCHLPHD, matrix.type = "adjacency", directed = FALSE, bipartite = TRUE)


SizeCHL <- network::network.size(Chile)
DensityCHL <- network::network.density(Chile)
ClusteringCHL <- tnet::clustering_tm(t(Matriz))
set.network.attribute(Chile, "Size", SizeCHL)
set.network.attribute(Chile, "Density", DensityCHL)
set.network.attribute(Chile, "Clustering", ClusteringCHL)
set.network.attribute(Chile, "Country", "Chile")
set.network.attribute(Chile, "Level", "All")
set.network.attribute(Chile, "OECD", TRUE)
library(igraph)
bnCHL <- graph_from_biadjacency_matrix(Matriz, directed = FALSE)
edges_chl <- data.frame()
for (i in 1:nrow(Matriz)) {
  for (j in 1:ncol(Matriz)) {
    if (Matriz[i, j] > 0) { # Only include edges where there's a connection
      edges_chl <- rbind(edges_chl, data.frame(
        Source = paste0("CHL_", rownames(Matriz)[i]),
        Target = colnames(Matriz)[j],
        Weight = Matriz[i, j], # Store the weight
        Country = "Chile"
      ))
    }
  }
}

bnCHL <- graph_from_data_frame(edges_chl, directed = F)
bipartite_mapping(bnCHL)
V(bnCHL)$type <- bipartite_mapping(bnCHL)$type
V(bnCHL)$shape <- ifelse(V(bnCHL)$type, "circle", "square")
V(bnCHL)$label.cex <- ifelse(V(bnCHL)$type, 0.5, 1)
V(bnCHL)$size <- sqrt(igraph::degree(bnCHL))
E(bnCHL)$color <- "lightgrey"
E(bnCHL)$weight <- edges_chl$Weight
network::set.edge.attribute(Chile, "Frecuencia", edges_chl$Weight)
Frecuencias <- as.sociomatrix(Chile, attrname = "Frecuencia")
Chile
network::list.edge.attributes(Chile)

plot(bnCHL, vertex.label = NA, layout = layout_as_bipartite)

ProgramsCHL <- data.frame(Degree = igraph::degree(bnCHL),
                          Closeness = igraph::closeness(bnCHL),
                          Betweennes = igraph::betweenness(bnCHL),
                          Eigen = igraph::eigen_centrality(bnCHL))
ProgramsCHL <- ProgramsCHL[ -c(5:25) ]
rownames(ProgramsCHL)
ProgramsCHL$SS <- rownames(ProgramsCHL)
ProgramsCHL <- ProgramsCHL[order(-ProgramsCHL$Degree), ]
#ProgramsCHL <- ProgramsCHL[!grepl("text", ProgramsCHL$SS), ]
colnames(ProgramsCHL)[4] <- "Eigenvector"
ProgramsCHL$Node <- rownames(ProgramsCHL)
ProgramsCHL <- mutate(ProgramsCHL, 
                       Partition = ifelse(
                         grepl("text", Node), "Program", "Skill"))
ProgramsCHL$Country <- "Chile"

library(psych)
describeBy(ProgramsCHL$Eigenvector, group = ProgramsCHL$Partition, mat = TRUE, digits = 2)



bnCHL1 <- graph_from_biadjacency_matrix(t(MatrizCHLSPEC), directed = FALSE)
EdgeListCHL1 <- as_edgelist(bnCHL1)
edges_chl1 <- data.frame(
  Source = paste0("CHL_", EdgeListCHL1[, 1]),
  Target = EdgeListCHL1[, 2],
  Country = "Chile"
)
bnCHL1 <- graph_from_data_frame(edges_chl1, directed = F)
bipartite_mapping(bnCHL1)
V(bnCHL1)$type <- bipartite_mapping(bnCHL1)$type
V(bnCHL1)$shape <- ifelse(V(bnCHL1)$type, "circle", "square")
V(bnCHL1)$label.cex <- ifelse(V(bnCHL1)$type, 0.5, 1)
V(bnCHL1)$size <- sqrt(igraph::degree(bnCHL1))
E(bnCHL1)$color <- "lightgrey"
plot(bnCHL1, vertex.label = NA, layout = layout_as_bipartite)

ProgramsCHL1 <- data.frame(Degree = igraph::degree(bnCHL1),
                           Closeness = igraph::closeness(bnCHL1),
                           Betweennes = igraph::betweenness(bnCHL1),
                           Eigen = igraph::eigen_centrality(bnCHL1))
ProgramsCHL1 <- ProgramsCHL1[ -c(5:25) ]
rownames(ProgramsCHL1)
ProgramsCHL1$SS <- rownames(ProgramsCHL1)
ProgramsCHL1 <- ProgramsCHL1[order(-ProgramsCHL1$Degree), ]
#ProgramsCHL1 <- ProgramsCHL1[!grepl("text", ProgramsCHL1$SS), ]
colnames(ProgramsCHL1)[4] <- "Eigenvector"
ProgramsCHL1$Node <- rownames(ProgramsCHL1)
ProgramsCHL1 <- mutate(ProgramsCHL1, 
                       Partition = ifelse(
                         grepl("text", Node), "Program", "Skill"))
ProgramsCHL1$Country <- "Chile"
ProgramsCHL1$Level <- "Specialization"

library(psych)
describeBy(ProgramsCHL1$Eigenvector, group = ProgramsCHL1$Partition, mat = TRUE, digits = 2)
library(network)
verticesCHLSPEC <- nrow(MatrizCHLSPEC) + ncol(MatrizCHLSPEC)
g1 <- network.initialize(verticesCHLSPEC, directed = F, bipartite = TRUE)
pave1 <- network.bipartite(MatrizCHLSPEC, g1)
Chile1 <- network(pave1, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Chile1
SizeCHL1 <- network::network.size(Chile1)
DensityCHL1 <- network::network.density(Chile1)
ClusteringCHL1 <- tnet::clustering_tm(MatrizCHLSPEC)
set.network.attribute(Chile1, "Size", SizeCHL1)
set.network.attribute(Chile1, "Density", DensityCHL1)
set.network.attribute(Chile1, "Clustering", ClusteringCHL1)
set.network.attribute(Chile1, "Country", "Chile")
set.network.attribute(Chile1, "Level", "Specialization")
Chile1

bnCHL2 <- graph_from_biadjacency_matrix(t(MatrizCHLMS), directed = FALSE)
EdgeListCHL2 <- as_edgelist(bnCHL2)
edges_chl2 <- data.frame(
  Source = paste0("CHL_", EdgeListCHL2[, 1]),
  Target = EdgeListCHL2[, 2],
  Country = "Chile"
)
bnCHL2 <- graph_from_data_frame(edges_chl2, directed = F)
bipartite_mapping(bnCHL2)
V(bnCHL2)$type <- bipartite_mapping(bnCHL2)$type
V(bnCHL2)$shape <- ifelse(V(bnCHL2)$type, "circle", "square")
V(bnCHL2)$label.cex <- ifelse(V(bnCHL2)$type, 0.5, 1)
V(bnCHL2)$size <- sqrt(igraph::degree(bnCHL2))
E(bnCHL2)$color <- "lightgrey"
plot(bnCHL2, vertex.label = NA, layout = layout_as_bipartite)

ProgramsCHL2 <- data.frame(Degree = igraph::degree(bnCHL2),
                           Closeness = igraph::closeness(bnCHL2),
                           Betweennes = igraph::betweenness(bnCHL2),
                           Eigen = igraph::eigen_centrality(bnCHL2))
ProgramsCHL2 <- ProgramsCHL2[ -c(5:25) ]
rownames(ProgramsCHL2)
ProgramsCHL2$SS <- rownames(ProgramsCHL2)
ProgramsCHL2 <- ProgramsCHL2[order(-ProgramsCHL2$Degree), ]
#ProgramsCHL2 <- ProgramsCHL2[!grepl("text", ProgramsCHL2$SS), ]
colnames(ProgramsCHL2)[4] <- "Eigenvector"
ProgramsCHL2$Node <- rownames(ProgramsCHL2)
ProgramsCHL2 <- mutate(ProgramsCHL2, 
                       Partition = ifelse(
                         grepl("text", Node), "Program", "Skill"))
ProgramsCHL2$Country <- "Chile"
ProgramsCHL2$Level <- "Master"
psych::describeBy(ProgramsCHL2$Eigenvector, group = ProgramsCHL2$Partition, mat = TRUE, digits = 2)

library(network)
verticesCHL2 <- nrow(MatrizCHLMS) + ncol(MatrizCHLMS)
g2 <- network.initialize(verticesCHL2, directed = F, bipartite = TRUE)
pave2 <- network.bipartite(MatrizCHLMS, g2)
Chile2 <- network(pave2, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Chile2
SizeCHL2 <- network::network.size(Chile2)
DensityCHL2 <- network::network.density(Chile2)
ClusteringCHL2 <- tnet::clustering_tm(MatrizCHLMS)
set.network.attribute(Chile2, "Size", SizeCHL2)
set.network.attribute(Chile2, "Density", DensityCHL2)
set.network.attribute(Chile2, "Clustering", ClusteringCHL2)
set.network.attribute(Chile2, "Country", "Chile")
set.network.attribute(Chile2, "Level", "Master")
Chile2


bnCHL3 <- graph_from_biadjacency_matrix(t(MatrizCHLPHD), directed = FALSE)
EdgeListCHL3 <- as_edgelist(bnCHL3)
edges_chl3 <- data.frame(
  Source = paste0("CHL_", EdgeListCHL3[, 1]),
  Target = EdgeListCHL3[, 2],
  Country = "Chile"
)
bnCHL3 <- graph_from_data_frame(edges_chl3, directed = F)
bipartite_mapping(bnCHL3)
V(bnCHL3)$type <- bipartite_mapping(bnCHL3)$type
V(bnCHL3)$shape <- ifelse(V(bnCHL3)$type, "circle", "square")
V(bnCHL3)$label.cex <- ifelse(V(bnCHL3)$type, 0.5, 1)
V(bnCHL3)$size <- sqrt(igraph::degree(bnCHL3))
E(bnCHL3)$color <- "lightgrey"
plot(bnCHL3, vertex.label = NA, layout = layout_as_bipartite)

ProgramsCHL3 <- data.frame(Degree = igraph::degree(bnCHL3),
                           Closeness = igraph::closeness(bnCHL3),
                           Betweennes = igraph::betweenness(bnCHL3),
                           Eigen = igraph::eigen_centrality(bnCHL3))
ProgramsCHL3 <- ProgramsCHL3[ -c(5:25) ]
rownames(ProgramsCHL3)
ProgramsCHL3$SS <- rownames(ProgramsCHL3)
ProgramsCHL3 <- ProgramsCHL3[order(-ProgramsCHL3$Degree), ]
#ProgramsCHL3 <- ProgramsCHL3[!grepl("text", ProgramsCHL3$SS), ]
colnames(ProgramsCHL3)[4] <- "Eigenvector"
ProgramsCHL3$Node <- rownames(ProgramsCHL3)
ProgramsCHL3 <- mutate(ProgramsCHL3, 
                       Partition = ifelse(
                         grepl("text", Node), "Program", "Skill"))
ProgramsCHL3$Country <- "Chile"
ProgramsCHL3$Level <- "PhD"

psych::describeBy(ProgramsCHL3$Eigenvector, group = ProgramsCHL3$Partition, mat = TRUE, digits = 2)

verticesCHL3 <- nrow(MatrizCHLPHD) + ncol(MatrizCHLPHD)
g3 <- network.initialize(verticesCHL3, directed = F, bipartite = TRUE)
pave3 <- network.bipartite(MatrizCHLPHD, g3)
Chile3 <- network(pave3, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Chile3
SizeCHL3 <- network::network.size(Chile3)
DensityCHL3 <- network::network.density(Chile3)
ClusteringCHL3 <- tnet::clustering_tm(MatrizCHLPHD)
set.network.attribute(Chile3, "Size", SizeCHL3)
set.network.attribute(Chile3, "Density", DensityCHL3)
set.network.attribute(Chile3, "Clustering", ClusteringCHL3)
set.network.attribute(Chile3, "Country", "Chile")
set.network.attribute(Chile3, "Level", "PhD")
Chile3
save.image("~/Documents/GitHub/SoftSkillsLatam/Results/Chile.RData")

