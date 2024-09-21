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
ProgramsURU$Partition <- "Skill"
ProgramsURU$Partition[c(11:147)] <- "Program"
ProgramsURU$Country <- "Uruguay"

library(psych)
describeBy(ProgramsURU$Eigenvector, group = ProgramsURU$Partition, mat = TRUE, digits = 2)
library(network)
Uruguay <- network(Matriz, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Uruguay
SizeURU <- network::network.size(Uruguay)
DensityURU <- network::network.density(Uruguay)
ClusteringURU <- tnet::clustering_tm(Matriz)
save.image("~/Documents/GitHub/SoftSkillsLatam/Results/Uruguay.RData")
