library(readtext)
ARG <- readtext("Argentina")
ARG$doc_id <- gsub("\\.pdf$|\\.docx$", "", ARG$doc_id)

library(dplyr)
ARG <- mutate(ARG, 
              Program = ifelse(
                grepl("Doctorado en", text), "Doctorado",
                ifelse(grepl("Maestría|Magíster en|MAGISTER EN", text), "Maestría", 
                       "Especialización")))
Programas <- data.frame(table(ARG$Program))
colnames(Programas)[1] <- "Programa" 
colnames(Programas)[2] <- "Total"

library(ggplot2)
Programas$fraction = Programas$Total / sum(Programas$Total)
Programas$ymax = cumsum(Programas$fraction)
Programas$ymin = c(0, head(Programas$ymax, n=-1))
Programas$ymax <- cumsum(Programas$fraction)
Programas$ymin <- c(0, head(Programas$ymax, n=-1))
Programas$labelPosition <- (Programas$ymax + Programas$ymin) / 2
Programas$label <- paste0(Programas$Programa, "\n total: ", Programas$Total)
ggplot(Programas, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Programa)) +
  geom_rect() +
  geom_text(x=2, aes(y=labelPosition, label=label, color=Programa), size=5) + 
  scale_fill_manual(values=c("Doctorado"="#FCE300", "Maestría"="#0A3A7E", "Especialización"="#CE1127")) + 
  scale_color_manual(values=c("Doctorado"="black", "Maestría"="black", "Especialización"="black")) + # Custom text colors
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none") + 
  ggtitle("Argentina")

library(quanteda)
TextsARG <- corpus(ARG$text)
docvars(TextsARG, "Program") <- ARG$Program
docvars(TextsARG, "Country") <- "Argentina"
head(summary(TextsARG), 10)

ProgramsARG <- tokens(TextsARG, 
                     remove_numbers = TRUE, 
                     remove_punct = TRUE, 
                     remove_url = TRUE, 
                     remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish"))


s1 <- data.frame(kwic(ProgramsARG, pattern = c(phrase("escucha activamente"),
                                               phrase("comprende"),
                                               phrase("silencio"),
                                               phrase("verbalmente"),
                                            phrase("entiende la situación"),
                                            phrase("plantea preguntas"),
                                            phrase("seguir"),
                                            phrase("dar seguimiento"),
                                            phrase("se cuestiona"),
                                            phrase("se pregunta"),
                                            phrase("atiende"),
                                            phrase("interrumpir"),
                                            phrase("no interrumpe"),
                                            phrase("discute"),
                                            phrase("prestar atención"),
                                            phrase("oye"),
                                            phrase("voz"),
                                            phrase("escuchar"),
                                            phrase("oyendo"),
                                            phrase("se toma tiempo para entender"),
                                            phrase("escuchando"))))




s2 <- data.frame(kwic(ProgramsARG, pattern = phrase("oyendo")))
s3 <- data.frame(kwic(ProgramsARG, pattern = "comunicar"))
s4 <- data.frame(kwic(ProgramsARG, pattern = "creatividad"))
s5 <- data.frame(kwic(ProgramsARG, pattern = "paciencia"))
s6 <- data.frame(kwic(ProgramsARG, pattern = "crear"))
s7 <- data.frame(kwic(ProgramsARG, pattern = "liderar"))
s8 <- data.frame(kwic(ProgramsARG, pattern = "resolver"))
s9 <- data.frame(kwic(ProgramsARG, pattern = "comprometer"))
s10 <- data.frame(kwic(ProgramsARG, pattern = "comprometerse"))


df_list <- mget(paste0("s", 1:10))
SS <- do.call(rbind, df_list)
SS$Skill <- rownames(SS)
SS$Skill <- gsub("\\..*", "", SS$Skill)

SkillsARG <- data.frame(Country = "Argentina",
                        SkillCode = c(paste0("s", 1:50)),
                        Skill = c("pensamiento crítico",
                                  "solucionar problemas",
                                  "comunicar",
                                  "creatividad",
                                  "paciencia",
                                  "crear",
                                  "liderar",
                                  "resolver",
                                  "comprometer",
                                  "comprometerse",
                                  "gestionar",
                                  "reflexionar",
                                  "controlar",
                                  "ético",
                                  "tolerar",
                                  "argumentar",
                                  "conflictos",
                                  "negociar",
                                  "comprender",
                                  "equipos",
                                  "planificar",
                                  "generar",
                                  "empatía",
                                  "compartir",
                                  "analizar",
                                  "reconocer",
                                  "orientar",
                                  "respetar",
                                  "motivar",
                                  "cooperar",
                                  "fortalecer",
                                  "impulsar",
                                  "acercar",
                                  "ayudar",
                                  "cambiar",
                                  "apreciar",
                                  "dirigir",
                                  "fomentar",
                                  "interactuar",
                                  "identificar",
                                  "competir",
                                  "manifestar",
                                  "responsable",
                                  "evaluar",
                                  "innovar",
                                  "decidir",
                                  "tomar decisiones",
                                  "flexibilidad",
                                  "persua*",
                                  "convencer"))

networkARG <- SS[c(8,1)]
head(networkARG, 3)

library(igraph)
bn6 <- graph_from_data_frame(networkARG,directed=FALSE)
bipartite_mapping(bn6)
V(bn6)$type <- bipartite_mapping(bn6)$type
V(bn6)$shape <- ifelse(V(bn6)$type, "circle", "square")
V(bn6)$label.cex <- ifelse(V(bn6)$type, 0.5, 1)
V(bn6)$size <- sqrt(igraph::degree(bn6))
E(bn6)$color <- "lightgrey"

bn6.pr <- bipartite_projection(bn6)
Terms <- bn6.pr$proj2

pave <- data.frame(igraph::betweenness(Terms))

centrality_scores <- igraph::eigen_centrality(Terms)
centrality_scores <- centrality_scores$vector

color_palette <- colorRampPalette(c("#CE1127", "white", "#0A3A7E"))(length(unique(centrality_scores)))

# Assign ARGors to nodes based on their normalized centrality scores
node_colors <- color_palette[rank(centrality_scores)]

# Plot the network with node ARGors based on centrality
set.seed(915)


plot(Terms, vertex.label.color = "black", vertex.label.cex = 0.8, vertex.color = node_colors, vertex.size = 15, edge.width = 0.5, edge.color = "lightgray", layout = layout_as_star, main = "")

BNARG <- graph.data.frame(networkARG, directed = FALSE)
ProgramsARG <- data.frame(Degree = igraph::degree(BNARG),
                         Closeness = igraph::closeness(BNARG),
                         Betweennes = igraph::betweenness(BNARG),
                         Eigen = igraph::eigen_centrality(BNARG))
ProgramsARG <- ProgramsARG[ -c(5:25) ]
rownames(ProgramsARG)
ProgramsARG$SS <- rownames(ProgramsARG)
ProgramsARG <- ProgramsARG[order(ProgramsARG$SS), ]
ProgramsARG <- ProgramsARG[grepl("s", ProgramsARG$SS), ]
ProgramsARG <- ProgramsARG[1:4]
colnames(ProgramsARG)[4] <- "Eigenvector"

save.image("~/Documents/GitHub/SoftSkillsLatam/ARG.RData")
