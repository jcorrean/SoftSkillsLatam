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
  ggtitle("Venezuela")

library(quanteda)
TextsVEN <- corpus(VEN$text)
docvars(TextsVEN, "Program") <- VEN$Program
docvars(TextsVEN, "Country") <- "Venezuela"
docvars(TextsVEN, "doc_id") <- VEN$doc_id
head(summary(TextsVEN), 3)
ProgIdentificados <- data.frame(summary(TextsVEN, length(TextsVEN)))

ProgramsVEN <- tokens(TextsVEN, 
                      remove_numbers = TRUE, 
                      remove_punct = TRUE, 
                      remove_url = TRUE, 
                      remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish"))

s1 <- data.frame(kwic(ProgramsVEN, pattern = phrase("pensamiento crítico")))
s2 <- data.frame(kwic(ProgramsVEN, pattern = phrase("solucionar problemas")))
s3 <- data.frame(kwic(ProgramsVEN, pattern = "comunicar"))
s4 <- data.frame(kwic(ProgramsVEN, pattern = "creatividad"))
s5 <- data.frame(kwic(ProgramsVEN, pattern = "paciencia"))
s6 <- data.frame(kwic(ProgramsVEN, pattern = "crear"))
s7 <- data.frame(kwic(ProgramsVEN, pattern = "liderar"))
s8 <- data.frame(kwic(ProgramsVEN, pattern = "resolver"))
s9 <- data.frame(kwic(ProgramsVEN, pattern = "comprometer"))
s10 <- data.frame(kwic(ProgramsVEN, pattern = "comprometerse"))
s11 <- data.frame(kwic(ProgramsVEN, pattern = "gestionar"))
s12 <- data.frame(kwic(ProgramsVEN, pattern = "reflexionar"))
s13 <- data.frame(kwic(ProgramsVEN, pattern = "controlar"))
s14 <- data.frame(kwic(ProgramsVEN, pattern = "ético"))
s15 <- data.frame(kwic(ProgramsVEN, pattern = "tolerar"))
s16 <- data.frame(kwic(ProgramsVEN, pattern = "argumentar"))
s17 <- data.frame(kwic(ProgramsVEN, pattern = "conflictos"))
s18 <- data.frame(kwic(ProgramsVEN, pattern = "negociar"))
s19 <- data.frame(kwic(ProgramsVEN, pattern = "comprender"))
s20 <- data.frame(kwic(ProgramsVEN, pattern = "equipos"))
s21 <- data.frame(kwic(ProgramsVEN, pattern = "planificar"))
s22 <- data.frame(kwic(ProgramsVEN, pattern = "generar"))
s23 <- data.frame(kwic(ProgramsVEN, pattern = "empatía"))
s24 <- data.frame(kwic(ProgramsVEN, pattern = "compartir"))
s25 <- data.frame(kwic(ProgramsVEN, pattern = "analizar"))
s26 <- data.frame(kwic(ProgramsVEN, pattern = "reconocer"))
s27 <- data.frame(kwic(ProgramsVEN, pattern = "orientar"))
s28 <- data.frame(kwic(ProgramsVEN, pattern = "respetar"))
s29 <- data.frame(kwic(ProgramsVEN, pattern = "motivar"))
s30 <- data.frame(kwic(ProgramsVEN, pattern = "cooperar"))
s31 <- data.frame(kwic(ProgramsVEN, pattern = "fortalecer"))
s32 <- data.frame(kwic(ProgramsVEN, pattern = "impulsar"))
s33 <- data.frame(kwic(ProgramsVEN, pattern = "acercar"))
s34 <- data.frame(kwic(ProgramsVEN, pattern = "ayudar"))
s35 <- data.frame(kwic(ProgramsVEN, pattern = "cambiar"))
s36 <- data.frame(kwic(ProgramsVEN, pattern = "apreciar"))
s37 <- data.frame(kwic(ProgramsVEN, pattern = "dirigir"))
s38 <- data.frame(kwic(ProgramsVEN, pattern = "fomentar"))
s39 <- data.frame(kwic(ProgramsVEN, pattern = "interactuar"))
s40 <- data.frame(kwic(ProgramsVEN, pattern = "identificar"))
s41 <- data.frame(kwic(ProgramsVEN, pattern = "competir"))
s42 <- data.frame(kwic(ProgramsVEN, pattern = "manifestar"))
s43 <- data.frame(kwic(ProgramsVEN, pattern = "responsable"))
s44 <- data.frame(kwic(ProgramsVEN, pattern = "evaluar"))
s45 <- data.frame(kwic(ProgramsVEN, pattern = "innovar"))
s46 <- data.frame(kwic(ProgramsVEN, pattern = "decidir"))
s47 <- data.frame(kwic(ProgramsVEN, pattern = phrase("tomar decisiones")))
s48 <- data.frame(kwic(ProgramsVEN, pattern = "flexibilidad"))
s49 <- data.frame(kwic(ProgramsVEN, pattern = "persua*"))
s50 <- data.frame(kwic(ProgramsVEN, pattern = "convencer"))

df_list <- mget(paste0("s", 1:50))
SS <- do.call(rbind, df_list)
SS$Skill <- rownames(SS)
SS$Skill <- gsub("\\..*", "", SS$Skill)

SkillsVEN <- data.frame(Country = "Venezuela",
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

networkVEN <- SS[c(8,1)]
head(networkVEN, 3)

library(igraph)
bn2 <- graph.data.frame(networkVEN,directed=FALSE)
bipartite_mapping(bn2)
V(bn2)$type <- bipartite_mapping(bn2)$type
V(bn2)$shape <- ifelse(V(bn2)$type, "circle", "square")
V(bn2)$label.cex <- ifelse(V(bn2)$type, 0.5, 1)
V(bn2)$size <- sqrt(igraph::degree(bn2))
E(bn2)$color <- "lightgrey"

bn2.pr <- bipartite_projection(bn2)
Terms <- bn2.pr$proj2

pave <- data.frame(igraph::betweenness(Terms))

centrality_scores <- igraph::eigen_centrality(Terms)
centrality_scores <- centrality_scores$vector

color_palette <- colorRampPalette(c("#CE1127", "white", "#0A3A7E"))(length(unique(centrality_scores)))

# Assign colors to nodes based on their normalized centrality scores
node_colors <- color_palette[rank(centrality_scores)]

# Plot the network with node colors based on centrality
set.seed(915)


plot(Terms, vertex.label.color = "black", vertex.label.cex = 0.8, vertex.color = node_colors, vertex.size = 15, edge.width = 0.5, edge.color = "lightgray", layout = layout_as_star, main = "")

BNVEN <- graph.data.frame(networkVEN, directed = FALSE)
ProgramsVEN <- data.frame(Degree = igraph::degree(BNVEN),
                          Closeness = igraph::closeness(BNVEN),
                          Betweennes = igraph::betweenness(BNVEN),
                          Eigen = igraph::eigen_centrality(BNVEN))
ProgramsVEN <- ProgramsVEN[ -c(5:25) ]
rownames(ProgramsVEN)
ProgramsVEN$SS <- rownames(ProgramsVEN)
ProgramsVEN <- ProgramsVEN[order(ProgramsVEN$SS), ]
ProgramsVEN <- ProgramsVEN[grepl("s", ProgramsVEN$SS), ]
ProgramsVEN <- ProgramsVEN[1:4]
colnames(ProgramsVEN)[4] <- "Eigenvector"

save.image("~/Documents/GitHub/SoftSkillsLatam/VEN.RData")
