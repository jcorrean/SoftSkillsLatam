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

library(quanteda)
TextsCHL <- corpus(CHL$text)
docvars(TextsCHL, "Program") <- CHL$Program
docvars(TextsCHL, "Country") <- "Chile"
head(summary(TextsCHL), 10)
CHLTexts <- data.frame(summary(TextsCHL, length(TextsCHL)))


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

ProgramsCHL
Matriz <- as.matrix(t(ProgramsCHL))
rowSums(Matriz)

library(igraph)
bnCHL <- graph_from_biadjacency_matrix(t(Matriz), directed = FALSE)
EdgeListCHL <- as_edgelist(bnCHL)
edges_chl <- data.frame(
  Source = paste0("CHL_", EdgeListCHL[, 1]),
  Target = EdgeListCHL[, 2],
  Country = "Chile"
)

bipartite_mapping(bnCHL)
V(bnCHL)$type <- bipartite_mapping(bnCHL)$type
V(bnCHL)$shape <- ifelse(V(bnCHL)$type, "circle", "square")
V(bnCHL)$label.cex <- ifelse(V(bnCHL)$type, 0.5, 1)
V(bnCHL)$size <- sqrt(igraph::degree(bnCHL))
E(bnCHL)$color <- "lightgrey"

ProgramsCHL <- data.frame(Degree = igraph::degree(bnCHL),
                          Closeness = igraph::closeness(bnCHL),
                          Betweennes = igraph::betweenness(bnCHL),
                          Eigen = igraph::eigen_centrality(bnCHL))
ProgramsCHL <- ProgramsCHL[ -c(5:25) ]
rownames(ProgramsCHL)
ProgramsCHL$SS <- rownames(ProgramsCHL)
ProgramsCHL <- ProgramsCHL[order(-ProgramsCHL$Degree), ]
#ProgramsCHL <- ProgramsCHL[!grepl("text", ProgramsCHL$SS), ]
ProgramsCHL <- ProgramsCHL[1:4]
colnames(ProgramsCHL)[4] <- "Eigenvector"

library(network)
Chile <- network(Matriz, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Chile
SizeCHL <- network::network.size(Chile)
DensityCHL <- network::network.density(Chile)
ClusteringCHL <- tnet::clustering_tm(Matriz)
