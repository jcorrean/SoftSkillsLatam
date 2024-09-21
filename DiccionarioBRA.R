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

ProgramsBRA
Matriz <- as.matrix(t(ProgramsBRA))
DataBrazil <- data.frame(t(Matriz))
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
ProgramsBRA$Partition <- "Skill"
ProgramsBRA$Partition[c(11:932)] <- "Program"
ProgramsBRA$Country <- "Brazil"

library(psych)
describeBy(ProgramsBRA$Eigenvector, group = ProgramsBRA$Partition, mat = TRUE, digits = 2)


library(network)
Brazil <- network(Matriz, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = TRUE)
Brazil
SizeBR <- network::network.size(Brazil)
DensityBR <- network::network.density(Brazil)
ClusteringBR <- tnet::clustering_tm(Matriz)
save.image("~/Documents/GitHub/SoftSkillsLatam/Results/Brazil.RData")
