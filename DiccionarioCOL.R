library(readtext)
COL <- readtext("Colombia")
COL$doc_id <- gsub("\\.pdf$|\\.docx$", "", COL$doc_id)

library(dplyr)
COL <- mutate(COL, 
              Program = ifelse(
                grepl("Doctorado en", text), "Doctorado",
                ifelse(grepl("Maestría|Magíster en|MAGISTER EN", text), "Maestría", 
                       "Especialización")))
Programas <- data.frame(table(COL$Program))
colnames(Programas)[1] <- "Programa" 
colnames(Programas)[2] <- "Total"

library(quanteda)
TextsCOL <- corpus(COL$text)
docvars(TextsCOL, "Program") <- COL$Program
docvars(TextsCOL, "Country") <- "Colombia"
head(summary(TextsCOL), 10)
COLTexts <- data.frame(summary(TextsCOL, length(TextsCOL)))
COLSpec <- corpus_subset(TextsCOL, Program == "Especialización")
COLMS <- corpus_subset(TextsCOL, Program == "Maestría")
COLPhD <- corpus_subset(TextsCOL, Program == "Doctorado")

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
ProgramsCOL <- tokens(TextsCOL, 
                       remove_numbers = TRUE, 
                       remove_punct = TRUE, 
                       remove_url = TRUE, 
                       remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish")) |> tokens_lookup(dictionary = Dictionary) |>
  dfm()

ProgramsCOL
Matriz <- as.matrix(t(ProgramsCOL))
rowSums(Matriz)

library(igraph)
bnCOL <- graph_from_biadjacency_matrix(t(Matriz), directed = FALSE)
EdgeListCOL <- as_edgelist(bnCOL)
edges_col <- data.frame(
  Source = paste0("COL_", EdgeListCOL[, 1]),
  Target = EdgeListCOL[, 2],
  Country = "Colombia"
)
bipartite_mapping(bnCOL)
V(bnCOL)$type <- bipartite_mapping(bnCOL)$type
V(bnCOL)$shape <- ifelse(V(bnCOL)$type, "circle", "square")
V(bnCOL)$label.cex <- ifelse(V(bnCOL)$type, 0.5, 1)
V(bnCOL)$size <- sqrt(igraph::degree(bnCOL))
E(bnCOL)$color <- "lightgrey"

ProgramsCOL <- data.frame(Degree = igraph::degree(bnCOL),
                           Closeness = igraph::closeness(bnCOL),
                           Betweennes = igraph::betweenness(bnCOL),
                           Eigen = igraph::eigen_centrality(bnCOL))
ProgramsCOL <- ProgramsCOL[ -c(5:25) ]
rownames(ProgramsCOL)
ProgramsCOL$SS <- rownames(ProgramsCOL)
ProgramsCOL <- ProgramsCOL[order(-ProgramsCOL$Degree), ]
#ProgramsCOL <- ProgramsCOL[!grepl("text", ProgramsCOL$SS), ]
colnames(ProgramsCOL)[4] <- "Eigenvector"
ProgramsCOL$Node <- rownames(ProgramsCOL)
ProgramsCOL$Partition <- "Skill"
ProgramsCOL$Partition[c(11:240)] <- "Program"
ProgramsCOL$Country <- "Colombia"

library(psych)
describeBy(ProgramsCOL$Eigenvector, group = ProgramsCOL$Partition, mat = TRUE, digits = 2)


library(network)
Colombia <- network(Matriz, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Colombia
SizeCOL <- network::network.size(Colombia)
SizeCOL <- network::network.density(Colombia)
SizeCOL <- tnet::clustering_tm(Matriz)
save.image("~/Documents/GitHub/SoftSkillsLatam/Results/Colombia.RData")
