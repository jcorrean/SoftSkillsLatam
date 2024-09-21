library(readtext)
MEX <- readtext("Mexico")
MEX$doc_id <- gsub("\\.pdf$|\\.docx$", "", MEX$doc_id)

library(dplyr)
MEX <- mutate(MEX, 
              Program = ifelse(
                grepl("Doctorado en", text), "Doctorado",
                ifelse(grepl("Maestría|Magíster en|MAGISTER EN", text), "Maestría", 
                       "Especialización")))
Programas <- data.frame(table(MEX$Program))
colnames(Programas)[1] <- "Programa" 
colnames(Programas)[2] <- "Total"

library(quanteda)
TextsMEX <- corpus(MEX$text)
docvars(TextsMEX, "Program") <- MEX$Program
docvars(TextsMEX, "Country") <- "Mexico"
head(summary(TextsMEX), 10)
MEXTexts <- data.frame(summary(TextsMEX, length(TextsMEX)))
MEXSpec <- corpus_subset(TextsMEX, Program == "Especialización")
MEXMS <- corpus_subset(TextsMEX, Program == "Maestría")
MEXPhD <- corpus_subset(TextsMEX, Program == "Doctorado")

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
ProgramsMEX <- tokens(TextsMEX, 
                      remove_numbers = TRUE, 
                      remove_punct = TRUE, 
                      remove_url = TRUE, 
                      remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

ProgramsMEX
Matriz <- as.matrix(t(ProgramsMEX))
rowSums(Matriz)

library(igraph)
bnMEX <- graph_from_biadjacency_matrix(t(Matriz), directed = FALSE)
EdgeListMEX <- as_edgelist(bnMEX)
edges_mx <- data.frame(
  Source = paste0("MEX_", EdgeListMEX[, 1]),
  Target = EdgeListMEX[, 2],
  Country = "Mexico"
)
bipartite_mapping(bnMEX)
V(bnMEX)$type <- bipartite_mapping(bnMEX)$type
V(bnMEX)$shape <- ifelse(V(bnMEX)$type, "circle", "square")
V(bnMEX)$label.cex <- ifelse(V(bnMEX)$type, 0.5, 1)
V(bnMEX)$size <- sqrt(igraph::degree(bnMEX))
E(bnMEX)$color <- "lightgrey"

ProgramsMEX <- data.frame(Degree = igraph::degree(bnMEX),
                          Closeness = igraph::closeness(bnMEX),
                          Betweennes = igraph::betweenness(bnMEX),
                          Eigen = igraph::eigen_centrality(bnMEX))
ProgramsMEX <- ProgramsMEX[ -c(5:25) ]
rownames(ProgramsMEX)
ProgramsMEX$SS <- rownames(ProgramsMEX)
ProgramsMEX <- ProgramsMEX[order(-ProgramsMEX$Degree), ]
#ProgramsMEX <- ProgramsMEX[!grepl("text", ProgramsMEX$SS), ]
colnames(ProgramsMEX)[4] <- "Eigenvector"
ProgramsMEX$Node <- rownames(ProgramsMEX)
ProgramsMEX$Partition <- "Skill"
ProgramsMEX$Partition[c(11:563)] <- "Program"
ProgramsMEX$Country <- "Mexico"

library(psych)
describeBy(ProgramsMEX$Eigenvector, group = ProgramsMEX$Partition, mat = TRUE, digits = 2)

library(network)
Mexico <- network(Matriz, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Mexico
SizeMEX <- network::network.size(Mexico)
DensityMEX <- network::network.density(Mexico)
ClusteringMEX <- tnet::clustering_tm(Matriz)
save.image("~/Documents/GitHub/SoftSkillsLatam/Results/Mexico.RData")