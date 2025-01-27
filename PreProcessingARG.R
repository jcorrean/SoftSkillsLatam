library(readtext)
ARG <- readtext("Argentina")
ARG$doc_id <- gsub("\\.pdf$|\\.docx$", "", ARG$doc_id)

library(dplyr)
ARG <- mutate(ARG, 
              Program = ifelse(
                grepl("Doctor|Doctorado en", text), "Doctorado",
                ifelse(grepl("Maestría|Magíster en|MAGISTER EN", text), "Maestría", 
                       "Especialización")))

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

MatrizARGSPEC <- as.matrix(ARG_Spec)
MatrizARGMS <- as.matrix(ARG_MS)
MatrizARGPHD <- as.matrix(ARG_PhD)
ProgramsARG
Matriz <- as.matrix(ProgramsARG)

library(network)
Argentina <- as.network(Matriz, matrix.type = "adjacency", directed = FALSE, bipartite = TRUE)
SizeARG <- network::network.size(Argentina)
DensityARG <- network::network.density(Argentina)
ClusteringARG <- tnet::clustering_tm(t(Matriz))
# también podría usar C4 como indicador de clustering
# llamado como "reinforcing"
set.network.attribute(Argentina, "Size", SizeARG)
set.network.attribute(Argentina, "Density", DensityARG)
set.network.attribute(Argentina, "Clustering", ClusteringARG)
set.network.attribute(Argentina, "Country", "Argentina")
set.network.attribute(Argentina, "Level", "All")
set.network.attribute(Argentina, "OECD", FALSE)
Program <- c(ARGTexts$Program, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
BrochureLength <- c(ARGTexts$Tokens, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
set.vertex.attribute(Argentina, "Program", Program)
set.vertex.attribute(Argentina, "Brochure.Length", BrochureLength)

get.vertex.attribute(Argentina, "vertex.names")
get.vertex.attribute(Argentina, "Program")

Argentina
library(igraph)
bnARG <- graph_from_biadjacency_matrix(Matriz, directed = FALSE)

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

ProgramsARG <- data.frame(Degree = igraph::degree(bnARG),
                          Closeness = igraph::closeness(bnARG),
                          Betweennes = igraph::betweenness(bnARG),
                          Eigen = igraph::eigen_centrality(bnARG))
ProgramsARG <- ProgramsARG[ -c(5:25) ]
rownames(ProgramsARG)
ProgramsARG$SS <- rownames(ProgramsARG)
ProgramsARG <- ProgramsARG[order(-ProgramsARG$Degree), ]
variable.names(ProgramsARG)
colnames(ProgramsARG)[4] <- "Eigenvector"
ProgramsARG$Node <- rownames(ProgramsARG)
ProgramsARG <- mutate(ProgramsARG, 
                      Partition = ifelse(
                        grepl("text", Node), "Program", "Skill"))
ProgramsARG$Country <- "Argentina"

pave <- ProgramsARG <- ProgramsARG[order(ProgramsARG$Node), ]

library(gtools)

ProgramsARG$Node <- factor(ProgramsARG$Node, levels = mixedsort(unique(ProgramsARG$Node)))
ProgramsARG <- ProgramsARG[order(ProgramsARG$Node), ]
