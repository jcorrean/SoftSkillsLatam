library(readtext)
URU <- readtext("Uruguay")
URU$doc_id <- gsub("\\.pdf$|\\.docx$", "", URU$doc_id)

library(dplyr)
URU <- mutate(URU, 
              Program = ifelse(
                grepl("Doctorado en", text), "Doctorado",
                ifelse(grepl("Maestría|Magíster en|MAGISTER EN", text), "Maestría", 
                       "Especialización")))
Programas <- data.frame(table(URU$Program))
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
  ggtitle("Uruguay")

library(quanteda)
TextsURU <- corpus(URU$text)
docvars(TextsURU, "Program") <- URU$Program
docvars(TextsURU, "Country") <- "Uruguay"
head(summary(TextsURU), 10)

ProgramsURU <- tokens(TextsURU, 
                      remove_numbers = TRUE, 
                      remove_punct = TRUE, 
                      remove_url = TRUE, 
                      remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish"))

s1 <- data.frame(kwic(ProgramsURU, pattern = phrase("pensamiento crítico")))
s2 <- data.frame(kwic(ProgramsURU, pattern = phrase("solucionar problemas")))
s3 <- data.frame(kwic(ProgramsURU, pattern = "comunicar"))
s4 <- data.frame(kwic(ProgramsURU, pattern = "creatividad"))
s5 <- data.frame(kwic(ProgramsURU, pattern = "paciencia"))
s6 <- data.frame(kwic(ProgramsURU, pattern = "crear"))
s7 <- data.frame(kwic(ProgramsURU, pattern = "liderar"))
s8 <- data.frame(kwic(ProgramsURU, pattern = "resolver"))
s9 <- data.frame(kwic(ProgramsURU, pattern = "comprometer"))
s10 <- data.frame(kwic(ProgramsURU, pattern = "comprometerse"))
s11 <- data.frame(kwic(ProgramsURU, pattern = "gestionar"))
s12 <- data.frame(kwic(ProgramsURU, pattern = "reflexionar"))
s13 <- data.frame(kwic(ProgramsURU, pattern = "controlar"))
s14 <- data.frame(kwic(ProgramsURU, pattern = "ético"))
s15 <- data.frame(kwic(ProgramsURU, pattern = "tolerar"))
s16 <- data.frame(kwic(ProgramsURU, pattern = "URUumentar"))
s17 <- data.frame(kwic(ProgramsURU, pattern = "conflictos"))
s18 <- data.frame(kwic(ProgramsURU, pattern = "negociar"))
s19 <- data.frame(kwic(ProgramsURU, pattern = "comprender"))
s20 <- data.frame(kwic(ProgramsURU, pattern = "equipos"))
s21 <- data.frame(kwic(ProgramsURU, pattern = "planificar"))
s22 <- data.frame(kwic(ProgramsURU, pattern = "generar"))
s23 <- data.frame(kwic(ProgramsURU, pattern = "empatía"))
s24 <- data.frame(kwic(ProgramsURU, pattern = "compartir"))
s25 <- data.frame(kwic(ProgramsURU, pattern = "analizar"))
s26 <- data.frame(kwic(ProgramsURU, pattern = "reconocer"))
s27 <- data.frame(kwic(ProgramsURU, pattern = "orientar"))
s28 <- data.frame(kwic(ProgramsURU, pattern = "respetar"))
s29 <- data.frame(kwic(ProgramsURU, pattern = "motivar"))
s30 <- data.frame(kwic(ProgramsURU, pattern = "cooperar"))
s31 <- data.frame(kwic(ProgramsURU, pattern = "fortalecer"))
s32 <- data.frame(kwic(ProgramsURU, pattern = "impulsar"))
s33 <- data.frame(kwic(ProgramsURU, pattern = "acercar"))
s34 <- data.frame(kwic(ProgramsURU, pattern = "ayudar"))
s35 <- data.frame(kwic(ProgramsURU, pattern = "cambiar"))
s36 <- data.frame(kwic(ProgramsURU, pattern = "apreciar"))
s37 <- data.frame(kwic(ProgramsURU, pattern = "dirigir"))
s38 <- data.frame(kwic(ProgramsURU, pattern = "fomentar"))
s39 <- data.frame(kwic(ProgramsURU, pattern = "interactuar"))
s40 <- data.frame(kwic(ProgramsURU, pattern = "identificar"))
s41 <- data.frame(kwic(ProgramsURU, pattern = "competir"))
s42 <- data.frame(kwic(ProgramsURU, pattern = "manifestar"))
s43 <- data.frame(kwic(ProgramsURU, pattern = "responsable"))
s44 <- data.frame(kwic(ProgramsURU, pattern = "evaluar"))
s45 <- data.frame(kwic(ProgramsURU, pattern = "innovar"))
s46 <- data.frame(kwic(ProgramsURU, pattern = "decidir"))
s47 <- data.frame(kwic(ProgramsURU, pattern = phrase("tomar decisiones")))
s48 <- data.frame(kwic(ProgramsURU, pattern = "flexibilidad"))
s49 <- data.frame(kwic(ProgramsURU, pattern = "persua*"))
s50 <- data.frame(kwic(ProgramsURU, pattern = "convencer"))

df_list <- mget(paste0("s", 1:50))
SS <- do.call(rbind, df_list)
SS$Skill <- rownames(SS)
SS$Skill <- gsub("\\..*", "", SS$Skill)

SkillsURU <- data.frame(Country = "Uruguay",
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
                                  "URUumentar",
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

networkURU <- SS[c(8,1)]
head(networkURU, 3)

library(igraph)
bn7 <- graph.data.frame(networkURU,directed=FALSE)
bipartite_mapping(bn7)
V(bn7)$type <- bipartite_mapping(bn7)$type
V(bn7)$shape <- ifelse(V(bn7)$type, "circle", "square")
V(bn7)$label.cex <- ifelse(V(bn7)$type, 0.5, 1)
V(bn7)$size <- sqrt(igraph::degree(bn7))
E(bn7)$color <- "lightgrey"

bn7.pr <- bipartite_projection(bn7)
Terms <- bn7.pr$proj2

pave <- data.frame(igraph::betweenness(Terms))

centrality_scores <- igraph::eigen_centrality(Terms)
centrality_scores <- centrality_scores$vector

color_palette <- colorRampPalette(c("#CE1127", "white", "#0A3A7E"))(length(unique(centrality_scores)))

# Assign URUors to nodes based on their normalized centrality scores
node_colors <- color_palette[rank(centrality_scores)]

# Plot the network with node URUors based on centrality
set.seed(915)


plot(Terms, vertex.label.color = "black", vertex.label.cex = 0.8, vertex.color = node_colors, vertex.size = 15, edge.width = 0.5, edge.color = "lightgray", layout = layout_as_star, main = "")

BNURU <- graph.data.frame(networkURU, directed = FALSE)
ProgramsURU <- data.frame(Degree = igraph::degree(BNURU),
                          Closeness = igraph::closeness(BNURU),
                          Betweennes = igraph::betweenness(BNURU),
                          Eigen = igraph::eigen_centrality(BNURU))
ProgramsURU <- ProgramsURU[ -c(5:25) ]
rownames(ProgramsURU)
ProgramsURU$SS <- rownames(ProgramsURU)
ProgramsURU <- ProgramsURU[order(ProgramsURU$SS), ]
ProgramsURU <- ProgramsURU[grepl("s", ProgramsURU$SS), ]
ProgramsURU <- ProgramsURU[1:4]
colnames(ProgramsURU)[4] <- "Eigenvector"

save.image("~/Documents/GitHub/SoftSkillsLatam/URU.RData")
