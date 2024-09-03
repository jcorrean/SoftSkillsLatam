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
  scale_fill_manual(values=c("Doctorado"="#FCE300", "Maestría"="#0A3A7E", "Especialização"="#CE1127")) + 
  scale_color_manual(values=c("Doctorado"="black", "Maestría"="black", "Especialização"="black")) + # Custom text colors
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none") + 
  ggtitle("BRAZIL")

library(quanteda)
TextsBRA <- corpus(BRA$text)
docvars(TextsBRA, "Program") <- BRA$Program
head(summary(TextsBRA), 10)

ProgramsBRA <- tokens(TextsBRA, 
                     remove_numbers = TRUE, 
                     remove_punct = TRUE, 
                     remove_url = TRUE, 
                     remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("portuguese"))

s1 <- data.frame(kwic(ProgramsBRA, pattern = phrase("pensamento crítico")))
s2 <- data.frame(kwic(ProgramsBRA, pattern = phrase("solução problemas")))
s3 <- data.frame(kwic(ProgramsBRA, pattern = "comunicar"))
s4 <- data.frame(kwic(ProgramsBRA, pattern = "criatividade"))
s5 <- data.frame(kwic(ProgramsBRA, pattern = "tranqüilidade"))# paciencia
s6 <- data.frame(kwic(ProgramsBRA, pattern = "criar"))
s7 <- data.frame(kwic(ProgramsBRA, pattern = "liderar"))
s8 <- data.frame(kwic(ProgramsBRA, pattern = "resolver"))
s9 <- data.frame(kwic(ProgramsBRA, pattern = "comprometer"))
s10 <- data.frame(kwic(ProgramsBRA, pattern = "comprometer-se"))
s11 <- data.frame(kwic(ProgramsBRA, pattern = "gerenciar"))
s12 <- data.frame(kwic(ProgramsBRA, pattern = "refletir"))
s13 <- data.frame(kwic(ProgramsBRA, pattern = "verificar"))
s14 <- data.frame(kwic(ProgramsBRA, pattern = "ético"))
s15 <- data.frame(kwic(ProgramsBRA, pattern = "suportar")) # tolerar
s16 <- data.frame(kwic(ProgramsBRA, pattern = "discutir")) 
s17 <- data.frame(kwic(ProgramsBRA, pattern = "conflitos"))
s18 <- data.frame(kwic(ProgramsBRA, pattern = "negociação")) # negociar
s19 <- data.frame(kwic(ProgramsBRA, pattern = "entender"))
s20 <- data.frame(kwic(ProgramsBRA, pattern = "equipes"))
s21 <- data.frame(kwic(ProgramsBRA, pattern = "planejar"))
s22 <- data.frame(kwic(ProgramsBRA, pattern = "efetuar"))  # generar
s23 <- data.frame(kwic(ProgramsBRA, pattern = "empatia"))   
s24 <- data.frame(kwic(ProgramsBRA, pattern = "compartilhar")) # compartir
s25 <- data.frame(kwic(ProgramsBRA, pattern = "analisar"))
s26 <- data.frame(kwic(ProgramsBRA, pattern = "reconhecer"))
s27 <- data.frame(kwic(ProgramsBRA, pattern = "dirigir"))  # orientar
s28 <- data.frame(kwic(ProgramsBRA, pattern = "respeito"))
s29 <- data.frame(kwic(ProgramsBRA, pattern = "incentivar")) # motivar
s30 <- data.frame(kwic(ProgramsBRA, pattern = "colaborar")) # cooperar
s31 <- data.frame(kwic(ProgramsBRA, pattern = "fortalecer"))
s32 <- data.frame(kwic(ProgramsBRA, pattern = "impulsionar")) # impulsar
s33 <- data.frame(kwic(ProgramsBRA, pattern = "aproximar"))
s34 <- data.frame(kwic(ProgramsBRA, pattern = "ajudar"))
s35 <- data.frame(kwic(ProgramsBRA, pattern = "transformar")) # mudar
s36 <- data.frame(kwic(ProgramsBRA, pattern = "gostar"))  # apreciar
s37 <- data.frame(kwic(ProgramsBRA, pattern = "conduzir"))
s38 <- data.frame(kwic(ProgramsBRA, pattern = "fomentar"))
s39 <- data.frame(kwic(ProgramsBRA, pattern = "interagir"))
s40 <- data.frame(kwic(ProgramsBRA, pattern = "identificar"))
s41 <- data.frame(kwic(ProgramsBRA, pattern = "concorrer")) # competir
s42 <- data.frame(kwic(ProgramsBRA, pattern = "demonstrar")) # manifestar
s43 <- data.frame(kwic(ProgramsBRA, pattern = "responsável"))
s44 <- data.frame(kwic(ProgramsBRA, pattern = "avaliar")) # evaluar
s45 <- data.frame(kwic(ProgramsBRA, pattern = "inovar"))
s46 <- data.frame(kwic(ProgramsBRA, pattern = "definir")) # decidir
s47 <- data.frame(kwic(ProgramsBRA, pattern = phrase("tomada decisões")))
s48 <- data.frame(kwic(ProgramsBRA, pattern = "flexibilidade"))
s49 <- data.frame(kwic(ProgramsBRA, pattern = "aconselhar")) # persuadir
s50 <- data.frame(kwic(ProgramsBRA, pattern = "incentivar")) # convencer

df_list <- mget(paste0("s", 1:50))
SS <- do.call(rbind, df_list)
SS$Skill <- rownames(SS)
SS$Skill <- gsub("\\..*", "", SS$Skill)

SkillsBRA <- data.frame(Country = "Brazil",
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

networkBRA <- SS[c(8,1)]
head(networkBRA, 3)

library(igraph)
bn4 <- graph_from_data_frame(networkBRA,directed=FALSE)
bipartite_mapping(bn4)
V(bn4)$type <- bipartite_mapping(bn4)$type
V(bn4)$shape <- ifelse(V(bn4)$type, "circle", "square")
V(bn4)$label.cex <- ifelse(V(bn4)$type, 0.5, 1)
V(bn4)$size <- sqrt(igraph::degree(bn4))
E(bn4)$color <- "lightgrey"

bn4.pr <- bipartite_projection(bn4)
Terms <- bn4.pr$proj2

pave <- data.frame(igraph::betweenness(Terms))

centrality_scores <- igraph::eigen_centrality(Terms)
centrality_scores <- centrality_scores$vector

color_palette <- colorRampPalette(c("#CE1127", "white", "#0A3A7E"))(length(unique(centrality_scores)))

# Assign colors to nodes based on their normalized centrality scores
node_colors <- color_palette[rank(centrality_scores)]

# Plot the network with node colors based on centrality
set.seed(915)


plot(Terms, vertex.label.color = "black", vertex.label.cex = 0.8, vertex.color = node_colors, vertex.size = 15, edge.width = 0.5, edge.color = "lightgray", layout = layout_as_star, main = "")

BNBRA <- graph.data.frame(networkBRA, directed = FALSE)
ProgramsBRA <- data.frame(Degree = igraph::degree(BNBRA),
                         Closeness = igraph::closeness(BNBRA),
                         Betweennes = igraph::betweenness(BNBRA),
                         Eigen = igraph::eigen_centrality(BNBRA))
ProgramsBRA <- ProgramsBRA[ -c(5:25) ]
rownames(ProgramsBRA)
ProgramsBRA$SS <- rownames(ProgramsBRA)
ProgramsBRA <- ProgramsBRA[order(ProgramsBRA$SS), ]
ProgramsBRA <- ProgramsBRA[grepl("s", ProgramsBRA$SS), ]
ProgramsBRA <- ProgramsBRA[1:4]
colnames(ProgramsBRA)[4] <- "Eigenvector"
save.image("~/Documents/GitHub/SoftSkillsLatam/BRA.RData")
