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
  ggtitle("ECUADOR")

library(quanteda)
TextsEC <- corpus(ECU$text)
docvars(TextsEC, "Program") <- ECU$Program
head(summary(TextsEC), 10)

ProgramsEC <- tokens(TextsEC, 
                     remove_numbers = TRUE, 
                     remove_punct = TRUE, 
                     remove_url = TRUE, 
                     remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish"))

s1 <- data.frame(kwic(ProgramsEC, pattern = phrase("pensamiento crítico")))
s2 <- data.frame(kwic(ProgramsEC, pattern = phrase("solucionar problemas")))
s3 <- data.frame(kwic(ProgramsEC, pattern = "comunicar"))
s4 <- data.frame(kwic(ProgramsEC, pattern = "creatividad"))
s5 <- data.frame(kwic(ProgramsEC, pattern = "paciencia"))
s6 <- data.frame(kwic(ProgramsEC, pattern = "crear"))
s7 <- data.frame(kwic(ProgramsEC, pattern = "liderar"))
s8 <- data.frame(kwic(ProgramsEC, pattern = "resolver"))
s9 <- data.frame(kwic(ProgramsEC, pattern = "comprometer"))
s10 <- data.frame(kwic(ProgramsEC, pattern = "comprometerse"))
s11 <- data.frame(kwic(ProgramsEC, pattern = "gestionar"))
s12 <- data.frame(kwic(ProgramsEC, pattern = "reflexionar"))
s13 <- data.frame(kwic(ProgramsEC, pattern = "controlar"))
s14 <- data.frame(kwic(ProgramsEC, pattern = "ético"))
s15 <- data.frame(kwic(ProgramsEC, pattern = "tolerar"))
s16 <- data.frame(kwic(ProgramsEC, pattern = "argumentar"))
s17 <- data.frame(kwic(ProgramsEC, pattern = "conflictos"))
s18 <- data.frame(kwic(ProgramsEC, pattern = "negociar"))
s19 <- data.frame(kwic(ProgramsEC, pattern = "comprender"))
s20 <- data.frame(kwic(ProgramsEC, pattern = "equipos"))
s21 <- data.frame(kwic(ProgramsEC, pattern = "planificar"))
s22 <- data.frame(kwic(ProgramsEC, pattern = "generar"))
s23 <- data.frame(kwic(ProgramsEC, pattern = "empatía"))
s24 <- data.frame(kwic(ProgramsEC, pattern = "compartir"))
s25 <- data.frame(kwic(ProgramsEC, pattern = "analizar"))
s26 <- data.frame(kwic(ProgramsEC, pattern = "reconocer"))
s27 <- data.frame(kwic(ProgramsEC, pattern = "orientar"))
s28 <- data.frame(kwic(ProgramsEC, pattern = "respetar"))
s29 <- data.frame(kwic(ProgramsEC, pattern = "motivar"))
s30 <- data.frame(kwic(ProgramsEC, pattern = "cooperar"))
s31 <- data.frame(kwic(ProgramsEC, pattern = "fortalecer"))
s32 <- data.frame(kwic(ProgramsEC, pattern = "impulsar"))
s33 <- data.frame(kwic(ProgramsEC, pattern = "acercar"))
s34 <- data.frame(kwic(ProgramsEC, pattern = "ayudar"))
s35 <- data.frame(kwic(ProgramsEC, pattern = "cambiar"))
s36 <- data.frame(kwic(ProgramsEC, pattern = "apreciar"))
s37 <- data.frame(kwic(ProgramsEC, pattern = "dirigir"))
s38 <- data.frame(kwic(ProgramsEC, pattern = "fomentar"))
s39 <- data.frame(kwic(ProgramsEC, pattern = "interactuar"))
s40 <- data.frame(kwic(ProgramsEC, pattern = "identificar"))
s41 <- data.frame(kwic(ProgramsEC, pattern = "competir"))
s42 <- data.frame(kwic(ProgramsEC, pattern = "manifestar"))
s43 <- data.frame(kwic(ProgramsEC, pattern = "responsable"))
s44 <- data.frame(kwic(ProgramsEC, pattern = "evaluar"))
s45 <- data.frame(kwic(ProgramsEC, pattern = "innovar"))
s46 <- data.frame(kwic(ProgramsEC, pattern = "decidir"))
s47 <- data.frame(kwic(ProgramsEC, pattern = phrase("tomar decisiones")))
s48 <- data.frame(kwic(ProgramsEC, pattern = "flexibilidad"))
s49 <- data.frame(kwic(ProgramsEC, pattern = "persua*"))
s50 <- data.frame(kwic(ProgramsEC, pattern = "convencer"))

df_list <- mget(paste0("s", 1:50))
SS <- do.call(rbind, df_list)
SS$Skill <- rownames(SS)
SS$Skill <- gsub("\\..*", "", SS$Skill)

SkillsEC <- data.frame(Country = "Ecuador",
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

networkEC <- SS[c(1,8)]
head(networkEC, 3)

library(igraph)
bn3 <- graph.data.frame(networkEC,directed=FALSE)
bipartite_mapping(bn3)
V(bn3)$type <- bipartite_mapping(bn3)$type
V(bn3)$shape <- ifelse(V(bn3)$type, "circle", "square")
V(bn3)$label.cex <- ifelse(V(bn3)$type, 0.5, 1)
V(bn3)$size <- sqrt(igraph::degree(bn3))
E(bn3)$color <- "lightgrey"

bn3.pr <- bipartite.projection(bn3)
Terms <- bn3.pr$proj2

pave <- data.frame(igraph::betweenness(Terms))

centrality_scores <- igraph::eigen_centrality(Terms)
centrality_scores <- centrality_scores$vector

color_palette <- colorRampPalette(c("#CE1127", "white", "#0A3A7E"))(length(unique(centrality_scores)))

# Assign ECUors to nodes based on their normalized centrality scores
node_colors <- color_palette[rank(centrality_scores)]

# Plot the network with node ECUors based on centrality
set.seed(915)


plot(Terms, vertex.label.color = "black", vertex.label.cex = 0.8, vertex.color = node_colors, vertex.size = 15, edge.width = 0.5, edge.color = "lightgray", layout = layout_as_star, main = "")

BNEC <- graph.data.frame(networkEC, directed = FALSE)
ProgramsEC <- data.frame(Degree = igraph::degree(BNEC),
                         Closeness = igraph::closeness(BNEC),
                         Betweennes = igraph::betweenness(BNEC),
                         Eigen = igraph::eigen_centrality(BNEC))
ProgramsEC <- ProgramsEC[ -c(5:25) ]
rownames(ProgramsEC)
ProgramsEC$SS <- rownames(ProgramsEC)
ProgramsEC <- ProgramsEC[order(ProgramsEC$SS), ]
ProgramsEC <- ProgramsEC[grepl("s", ProgramsEC$SS), ]
ProgramsEC <- ProgramsEC[1:4]
colnames(ProgramsEC)[4] <- "Eigenvector"

save.image("~/Documents/GitHub/SoftSkillsLatam/ECU.RData")