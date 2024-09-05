library(readtext)
CORI <- readtext("Costa_Rica")
CORI$doc_id <- gsub("\\.pdf$|\\.docx$", "", COL$doc_id)

library(dplyr)
CORI <- mutate(CORI, 
              Program = ifelse(
                grepl("Doctorado en", text), "Doctorado",
                ifelse(grepl("Maestría|Magíster en|MAGISTER EN", text), "Maestría", 
                       "Especialización")))
Programas <- data.frame(table(CORI$Program))
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
  ggtitle("COSTA RICA")

library(quanteda)
TextsCR <- corpus(CORI$text)
docvars(TextsCR, "Program") <- CORI$Program
head(summary(TextsCR), 10)

ProgramsCR <- tokens(TextsCR, 
                     remove_numbers = TRUE, 
                     remove_punct = TRUE, 
                     remove_url = TRUE, 
                     remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish"))

s1 <- data.frame(kwic(ProgramsCR, pattern = phrase("pensamiento crítico")))
s2 <- data.frame(kwic(ProgramsCR, pattern = phrase("solucionar problemas")))
s3 <- data.frame(kwic(ProgramsCR, pattern = "comunicar"))
s4 <- data.frame(kwic(ProgramsCR, pattern = "creatividad"))
s5 <- data.frame(kwic(ProgramsCR, pattern = "paciencia"))
s6 <- data.frame(kwic(ProgramsCR, pattern = "crear"))
s7 <- data.frame(kwic(ProgramsCR, pattern = "liderar"))
s8 <- data.frame(kwic(ProgramsCR, pattern = "resolver"))
s9 <- data.frame(kwic(ProgramsCR, pattern = "comprometer"))
s10 <- data.frame(kwic(ProgramsCR, pattern = "comprometerse"))
s11 <- data.frame(kwic(ProgramsCR, pattern = "gestionar"))
s12 <- data.frame(kwic(ProgramsCR, pattern = "reflexionar"))
s13 <- data.frame(kwic(ProgramsCR, pattern = "controlar"))
s14 <- data.frame(kwic(ProgramsCR, pattern = "ético"))
s15 <- data.frame(kwic(ProgramsCR, pattern = "tolerar"))
s16 <- data.frame(kwic(ProgramsCR, pattern = "argumentar"))
s17 <- data.frame(kwic(ProgramsCR, pattern = "conflictos"))
s18 <- data.frame(kwic(ProgramsCR, pattern = "negociar"))
s19 <- data.frame(kwic(ProgramsCR, pattern = "comprender"))
s20 <- data.frame(kwic(ProgramsCR, pattern = "equipos"))
s21 <- data.frame(kwic(ProgramsCR, pattern = "planificar"))
s22 <- data.frame(kwic(ProgramsCR, pattern = "generar"))
s23 <- data.frame(kwic(ProgramsCR, pattern = "empatía"))
s24 <- data.frame(kwic(ProgramsCR, pattern = "compartir"))
s25 <- data.frame(kwic(ProgramsCR, pattern = "analizar"))
s26 <- data.frame(kwic(ProgramsCR, pattern = "reconocer"))
s27 <- data.frame(kwic(ProgramsCR, pattern = "orientar"))
s28 <- data.frame(kwic(ProgramsCR, pattern = "respetar"))
s29 <- data.frame(kwic(ProgramsCR, pattern = "motivar"))
s30 <- data.frame(kwic(ProgramsCR, pattern = "cooperar"))
s31 <- data.frame(kwic(ProgramsCR, pattern = "fortalecer"))
s32 <- data.frame(kwic(ProgramsCR, pattern = "impulsar"))
s33 <- data.frame(kwic(ProgramsCR, pattern = "acercar"))
s34 <- data.frame(kwic(ProgramsCR, pattern = "ayudar"))
s35 <- data.frame(kwic(ProgramsCR, pattern = "cambiar"))
s36 <- data.frame(kwic(ProgramsCR, pattern = "apreciar"))
s37 <- data.frame(kwic(ProgramsCR, pattern = "dirigir"))
s38 <- data.frame(kwic(ProgramsCR, pattern = "fomentar"))
s39 <- data.frame(kwic(ProgramsCR, pattern = "interactuar"))
s40 <- data.frame(kwic(ProgramsCR, pattern = "identificar"))
s41 <- data.frame(kwic(ProgramsCR, pattern = "competir"))
s42 <- data.frame(kwic(ProgramsCR, pattern = "manifestar"))
s43 <- data.frame(kwic(ProgramsCR, pattern = "responsable"))
s44 <- data.frame(kwic(ProgramsCR, pattern = "evaluar"))
s45 <- data.frame(kwic(ProgramsCR, pattern = "innovar"))
s46 <- data.frame(kwic(ProgramsCR, pattern = "decidir"))
s47 <- data.frame(kwic(ProgramsCR, pattern = phrase("tomar decisiones")))
s48 <- data.frame(kwic(ProgramsCR, pattern = "flexibilidad"))
s49 <- data.frame(kwic(ProgramsCR, pattern = "persua*"))
s50 <- data.frame(kwic(ProgramsCR, pattern = "convencer"))

df_list <- mget(paste0("s", 1:50))
SS <- do.call(rbind, df_list)
SS$Skill <- rownames(SS)
SS$Skill <- gsub("\\..*", "", SS$Skill)

SkillsCR <- data.frame(Country = "Costa Rica",
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

networkCR <- SS[c(8,1)]
head(networkCR, 3)

library(igraph)
bn9 <- graph_from_data_frame(networkCR,directed=FALSE)
bipartite_mapping(bn9)
V(bn9)$type <- bipartite_mapping(bn9)$type
V(bn9)$shape <- ifelse(V(bn9)$type, "circle", "square")
V(bn9)$label.cex <- ifelse(V(bn9)$type, 0.5, 1)
V(bn9)$size <- sqrt(igraph::degree(bn9))
E(bn9)$color <- "lightgrey"

bn9.pr <- bipartite_projection(bn9)
Terms <- bn9.pr$proj2

pave <- data.frame(igraph::betweenness(Terms))

centrality_scores <- igraph::eigen_centrality(Terms)
centrality_scores <- centrality_scores$vector

color_palette <- colorRampPalette(c("#CE1127", "white", "#0A3A7E"))(length(unique(centrality_scores)))

# Assign colors to nodes based on their normalized centrality scores
node_colors <- color_palette[rank(centrality_scores)]

# Plot the network with node colors based on centrality
set.seed(915)


plot(Terms, vertex.label.color = "black", vertex.label.cex = 0.8, vertex.color = node_colors, vertex.size = 15, edge.width = 0.5, edge.color = "lightgray", layout = layout_as_star, main = "")

BNCR <- graph.data.frame(networkCR, directed = FALSE)
ProgramsCR <- data.frame(Degree = igraph::degree(BNCR),
                         Closeness = igraph::closeness(BNCR),
                         Betweennes = igraph::betweenness(BNCR),
                         Eigen = igraph::eigen_centrality(BNCR))
ProgramsCR <- ProgramsCR[ -c(5:25) ]
rownames(ProgramsCR)
ProgramsCR$SS <- rownames(ProgramsCR)
ProgramsCR <- ProgramsCR[order(ProgramsCR$SS), ]
ProgramsCR <- ProgramsCR[grepl("s", ProgramsCR$SS), ]
ProgramsCR <- ProgramsCR[1:4]
colnames(ProgramsCR)[4] <- "Eigenvector"

save.image("~/Documents/GitHub/SoftSkillsLatam/CORI.RData")
