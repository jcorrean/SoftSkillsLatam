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

ProgramsECU
Matriz <- as.matrix(t(ProgramsECU))
rowSums(Matriz)

library(igraph)
bnECU <- graph_from_biadjacency_matrix(t(Matriz), directed = FALSE)
EdgeListECU <- as_edgelist(bnECU)
edges_cr <- data.frame(
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

library(network)
Ecuador <- network(Matriz, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Ecuador
SizeECU <- network::network.size(Ecuador)
DensityECU <- network::network.density(Ecuador)
ClusteringECU <- tnet::clustering_tm(Matriz)
