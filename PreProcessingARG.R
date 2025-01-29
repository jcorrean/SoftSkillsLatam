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

library(igraph)
bnARG <- graph_from_biadjacency_matrix(Matriz, directed = FALSE)
edges_args <- data.frame()
for (i in 1:nrow(Matriz)) {
  for (j in 1:ncol(Matriz)) {
    if (Matriz[i, j] > 0) { # Only include edges where there's a connection
      edges_args <- rbind(edges_args, data.frame(
        docs = rownames(Matriz)[i],
        features = colnames(Matriz)[j],
        Frequency = Matriz[i, j], # Store the weight
        Country = "Argentina"
      ))
    }
  }
}

#bnARG <- graph_from_data_frame(edges_args, directed = FALSE)
#bipartite_mapping(bnARG)
V(bnARG)$type <- bipartite_mapping(bnARG)$type
V(bnARG)$shape <- ifelse(V(bnARG)$type, "circle", "square")
V(bnARG)$color <- ifelse(V(bnARG)$type, "red", "blue4")
V(bnARG)$size <- sqrt(igraph::degree(bnARG))
E(bnARG)$color <- "lightgrey"
E(bnARG)$Frequency <- edges_args$Frequency
igraph::edge_attr_names(bnARG)
igraph::edge_attr(bnARG)

edge_list_igraph <- as_edgelist(bnARG, names = TRUE)


ProgramsARG <- data.frame(Degree = igraph::degree(bnARG),
                          Closeness = igraph::closeness(bnARG),
                          Betweenness = igraph::betweenness(bnARG),
                          Eigen = igraph::eigen_centrality(bnARG)$vector)
rownames(ProgramsARG)
ProgramsARG <- ProgramsARG[order(-ProgramsARG$Degree), ]
variable.names(ProgramsARG)
colnames(ProgramsARG)[4] <- "Eigenvector"
ProgramsARG$Node <- rownames(ProgramsARG)
ProgramsARG <- mutate(ProgramsARG, 
                      Partition = ifelse(
                        grepl("text", Node), "Program", "Skill"))
ProgramsARG$Country <- "Argentina"

library(gtools)
ProgramsARG$Node <- factor(ProgramsARG$Node, levels = mixedsort(unique(ProgramsARG$Node)))
ProgramsARG <- ProgramsARG[order(ProgramsARG$Node), ]
P.ARG <- ProgramsARG[order(ProgramsARG$Partition), ]

library(intergraph)


library(network)
Argentina <- network.initialize(524, directed = FALSE, hyper = FALSE, loops = FALSE, multiple = FALSE, bipartite = 514)
Argentina
Argentina <- network.bipartite(matriz,
                               Argentina,
                               ignore.eval = FALSE,
                               names.eval = "Frequency")
Argentina
list.edge.attributes(Argentina)
Argentina
set.edge.value(Argentina, "Frequency", matriz)
print(Argentina)
list.edge.attributes(Argentina)
rm(Argentina)

SizeARG <- network::network.size(Argentina)
DensityARG <- network::network.density(Argentina)
ClusteringARG <- tnet::reinforcement_tm(t(Matriz))
# también podría usar C4 como indicador de clustering
# llamado como "reinforcing"
set.network.attribute(Argentina, "Size", SizeARG)
set.network.attribute(Argentina, "Density", DensityARG)
set.network.attribute(Argentina, "Clustering", ClusteringARG)
set.network.attribute(Argentina, "Country", "Argentina")
set.network.attribute(Argentina, "Level", "All")
set.network.attribute(Argentina, "OECD", FALSE)
Program <- c(ARGTexts$Program, rep(NA, ncol(Matriz)))
BrochureLength <- c(ARGTexts$Tokens, rep(NA, ncol(Matriz)))
network::set.vertex.attribute(Argentina, "Program", Program)
network::set.vertex.attribute(Argentina, "Brochure.Length", BrochureLength)

network::get.vertex.attribute(Argentina, "vertex.names")
network::get.vertex.attribute(Argentina, "Program")
network::get.vertex.attribute(Argentina, "Brochure.Length")

Argentina

# Assign centralities as vertex attributes in the NETWORK object
network::set.vertex.attribute(Argentina, "Centrality", P.ARG$Degree)
network::set.vertex.attribute(Argentina, "Closeness", P.ARG$Closeness)
network::set.vertex.attribute(Argentina, "Betweenness", P.ARG$Betweennes)
network::set.vertex.attribute(Argentina, "Eigenvector", P.ARG$Eigenvector)

# Verify
network::get.vertex.attribute(Argentina, "vertex.names")
network::get.vertex.attribute(Argentina, "Centrality")
network::get.vertex.attribute(Argentina, "Closeness")
network::get.vertex.attribute(Argentina, "Betweenness")
network::get.vertex.attribute(Argentina, "Eigenvector")

get.edgeIDs(Argentina, 1,519)
get.edgeIDs(Argentina, 2,518)
get.edgeI

network::get.edge.value(Argentina, "Frequency")

Argentina <- network::set.edge.value(Argentina, "Frequency", )
Argentina
network::list.edge.attributes(Argentina)
Argentina2 <- network.edgelist(edges_args[1:3], Argentina, ignore.eval = FALSE)
Argentina2

network::get.edge.attribute(Argentina, "Frequency") # Returns all edge weights
Argentina["text1", "science"] 
Argentina["text4", "science"] 
