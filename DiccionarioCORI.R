library(readtext)
CORI <- readtext("Costa_Rica")
CORI$doc_id <- gsub("\\.pdf$|\\.docx$", "", CORI$doc_id)

library(dplyr)
CORI <- mutate(CORI, 
               Program = ifelse(
                 grepl("Doctorado en", text), "Doctorado",
                 ifelse(grepl("Maestría|Magíster en|MAGISTER EN", text), "Maestría", 
                        "Especialización")))
Programas <- data.frame(table(CORI$Program))
colnames(Programas)[1] <- "Programa" 
colnames(Programas)[2] <- "Total"

library(quanteda)
TextsCORI <- corpus(CORI$text)
docvars(TextsCORI, "Program") <- CORI$Program
docvars(TextsCORI, "Country") <- "Costa Rica"
head(summary(TextsCORI), 10)
CORITexts <- data.frame(summary(TextsCORI, length(TextsCORI)))


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
ProgramsCORI <- tokens(TextsCORI, 
                      remove_numbers = TRUE, 
                      remove_punct = TRUE, 
                      remove_url = TRUE, 
                      remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

ProgramsCORI
Matriz <- as.matrix(t(ProgramsCORI))
rowSums(Matriz)

library(igraph)
bnCORI <- graph_from_biadjacency_matrix(t(Matriz), directed = FALSE)
EdgeListCORI <- as_edgelist(bnCORI)
edges_cr <- data.frame(
  Source = paste0("CR_", EdgeListCORI[, 1]),
  Target = EdgeListCORI[, 2],
  Country = "Costa Rica"
)
bipartite_mapping(bnCORI)
V(bnCORI)$type <- bipartite_mapping(bnCORI)$type
V(bnCORI)$shape <- ifelse(V(bnCORI)$type, "circle", "square")
V(bnCORI)$label.cex <- ifelse(V(bnCORI)$type, 0.5, 1)
V(bnCORI)$size <- sqrt(igraph::degree(bnCORI))
E(bnCORI)$color <- "lightgrey"

ProgramsCORI <- data.frame(Degree = igraph::degree(bnCORI),
                          Closeness = igraph::closeness(bnCORI),
                          Betweennes = igraph::betweenness(bnCORI),
                          Eigen = igraph::eigen_centrality(bnCORI))
ProgramsCORI <- ProgramsCORI[ -c(5:25) ]
rownames(ProgramsCORI)
ProgramsCORI$SS <- rownames(ProgramsCORI)
ProgramsCORI <- ProgramsCORI[order(-ProgramsCORI$Degree), ]
#ProgramsCORI <- ProgramsCORI[!grepl("text", ProgramsCORI$SS), ]
colnames(ProgramsCORI)[4] <- "Eigenvector"
ProgramsCORI$Node <- rownames(ProgramsCORI)
ProgramsCORI$Partition <- "Skill"
ProgramsCORI$Partition[c(11:130)] <- "Program"
ProgramsCORI$Country <- "Costa Rica"

library(psych)
describeBy(ProgramsCORI$Degree, group = ProgramsCORI$Partition, mat = TRUE, digits = 2)

library(network)
CostaRica <- network(Matriz, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
CostaRica
SizeCR <- network::network.size(CostaRica)
DensityCR <- network::network.density(CostaRica)
ClusteringCR <- tnet::clustering_tm(Matriz)
save.image("~/Documents/GitHub/SoftSkillsLatam/Results/CostaRica.RData")