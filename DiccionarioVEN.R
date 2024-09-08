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

ProgramsVEN
Matriz <- as.matrix(t(ProgramsVEN))
rowSums(Matriz)

library(igraph)
bnVEN <- graph_from_biadjacency_matrix(t(Matriz), directed = FALSE)
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

library(network)
Venezuela <- network(Matriz, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Venezuela
network::network.size(Venezuela)
network::network.density(Venezuela)
tnet::clustering_tm(Matriz)
