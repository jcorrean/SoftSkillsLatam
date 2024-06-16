library(readtext)
MEX <- readtext("Mexico")
MEX$doc_id <- gsub("\\.pdf$|\\.docx$", "", MEX$doc_id)

library(dplyr)
MEX <- mutate(MEX, 
              Program = ifelse(
                grepl("Doctorado en", text), "Doctorado",
                ifelse(grepl("Maestría|Magíster en|MAGISTER EN", text), "Maestría", 
                       "Especialización")))
Programas <- data.frame(table(MEX$Program))
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
  ggtitle("MEXICO")

library(quanteda)
TextsMEX <- corpus(MEX$text)
docvars(TextsMEX, "Program") <- MEX$Program
head(summary(TextsMEX), 10)

ProgramsMEX <- tokens(TextsMEX, 
                     remove_numbers = TRUE, 
                     remove_punct = TRUE, 
                     remove_url = TRUE, 
                     remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish"))

s1 <- data.frame(kwic(ProgramsMEX, pattern = phrase("pensamiento crítico")))
s2 <- data.frame(kwic(ProgramsMEX, pattern = phrase("solucionar problemas")))
s3 <- data.frame(kwic(ProgramsMEX, pattern = "comunicar"))
s4 <- data.frame(kwic(ProgramsMEX, pattern = "creatividad"))
s5 <- data.frame(kwic(ProgramsMEX, pattern = "paciencia"))
s6 <- data.frame(kwic(ProgramsMEX, pattern = "crear"))
s7 <- data.frame(kwic(ProgramsMEX, pattern = "liderar"))
s8 <- data.frame(kwic(ProgramsMEX, pattern = "resolver"))
s9 <- data.frame(kwic(ProgramsMEX, pattern = "comprometer"))
s10 <- data.frame(kwic(ProgramsMEX, pattern = "comprometerse"))
s11 <- data.frame(kwic(ProgramsMEX, pattern = "gestionar"))
s12 <- data.frame(kwic(ProgramsMEX, pattern = "reflexionar"))
s13 <- data.frame(kwic(ProgramsMEX, pattern = "controlar"))
s14 <- data.frame(kwic(ProgramsMEX, pattern = "ético"))
s15 <- data.frame(kwic(ProgramsMEX, pattern = "tolerar"))
s16 <- data.frame(kwic(ProgramsMEX, pattern = "argumentar"))
s17 <- data.frame(kwic(ProgramsMEX, pattern = "conflictos"))
s18 <- data.frame(kwic(ProgramsMEX, pattern = "negociar"))
s19 <- data.frame(kwic(ProgramsMEX, pattern = "comprender"))
s20 <- data.frame(kwic(ProgramsMEX, pattern = "equipos"))
s21 <- data.frame(kwic(ProgramsMEX, pattern = "planificar"))
s22 <- data.frame(kwic(ProgramsMEX, pattern = "generar"))
s23 <- data.frame(kwic(ProgramsMEX, pattern = "empatía"))
s24 <- data.frame(kwic(ProgramsMEX, pattern = "compartir"))
s25 <- data.frame(kwic(ProgramsMEX, pattern = "analizar"))
s26 <- data.frame(kwic(ProgramsMEX, pattern = "reconocer"))
s27 <- data.frame(kwic(ProgramsMEX, pattern = "orientar"))
s28 <- data.frame(kwic(ProgramsMEX, pattern = "respetar"))
s29 <- data.frame(kwic(ProgramsMEX, pattern = "motivar"))
s30 <- data.frame(kwic(ProgramsMEX, pattern = "cooperar"))
s31 <- data.frame(kwic(ProgramsMEX, pattern = "fortalecer"))
s32 <- data.frame(kwic(ProgramsMEX, pattern = "impulsar"))
s33 <- data.frame(kwic(ProgramsMEX, pattern = "acercar"))
s34 <- data.frame(kwic(ProgramsMEX, pattern = "ayudar"))
s35 <- data.frame(kwic(ProgramsMEX, pattern = "cambiar"))
s36 <- data.frame(kwic(ProgramsMEX, pattern = "apreciar"))
s37 <- data.frame(kwic(ProgramsMEX, pattern = "dirigir"))
s38 <- data.frame(kwic(ProgramsMEX, pattern = "fomentar"))
s39 <- data.frame(kwic(ProgramsMEX, pattern = "interactuar"))
s40 <- data.frame(kwic(ProgramsMEX, pattern = "identificar"))
s41 <- data.frame(kwic(ProgramsMEX, pattern = "competir"))
s42 <- data.frame(kwic(ProgramsMEX, pattern = "manifestar"))
s43 <- data.frame(kwic(ProgramsMEX, pattern = "responsable"))
s44 <- data.frame(kwic(ProgramsMEX, pattern = "evaluar"))
s45 <- data.frame(kwic(ProgramsMEX, pattern = "innovar"))
s46 <- data.frame(kwic(ProgramsMEX, pattern = "decidir"))
s47 <- data.frame(kwic(ProgramsMEX, pattern = phrase("tomar decisiones")))
s48 <- data.frame(kwic(ProgramsMEX, pattern = "flexibilidad"))
s49 <- data.frame(kwic(ProgramsMEX, pattern = "persua*"))
s50 <- data.frame(kwic(ProgramsMEX, pattern = "convencer"))

df_list <- mget(paste0("s", 1:50))
SS <- do.call(rbind, df_list)
SS$Skill <- rownames(SS)
SS$Skill <- gsub("\\..*", "", SS$Skill)

SkillsMEX <- data.frame(Country = "Mexico",
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

networkMEX <- SS[c(1,8)]
head(networkMEX, 3)

library(igraph)
bn4 <- graph.data.frame(networkMEX,directed=FALSE)
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

# Assign MEXors to nodes based on their normalized centrality scores
node_colors <- color_palette[rank(centrality_scores)]

# Plot the network with node MEXors based on centrality
set.seed(915)


plot(Terms, vertex.label.color = "black", vertex.label.cex = 0.8, vertex.color = node_colors, vertex.size = 15, edge.width = 0.5, edge.color = "lightgray", layout = layout_as_star, main = "")

BNMEX <- graph.data.frame(networkMEX, directed = FALSE)
ProgramsMEX <- data.frame(Degree = igraph::degree(BNMEX),
                         Closeness = igraph::closeness(BNMEX),
                         Betweennes = igraph::betweenness(BNMEX),
                         Eigen = igraph::eigen_centrality(BNMEX))
ProgramsMEX <- ProgramsMEX[ -c(5:25) ]
rownames(ProgramsMEX)
ProgramsMEX$SS <- rownames(ProgramsMEX)
ProgramsMEX <- ProgramsMEX[order(ProgramsMEX$SS), ]
ProgramsMEX <- ProgramsMEX[grepl("s", ProgramsMEX$SS), ]
ProgramsMEX <- ProgramsMEX[1:4]
colnames(ProgramsMEX)[4] <- "Eigenvector"



library(psych)

pairs.panels(ProgramsMEX, 
             method = "spearman", 
             hist.col = "#0A3A7E",
             density = TRUE,  
             ellipses = TRUE,
             pch = 15,
             cex = 1,
             cex.axis = 1.8,
             cex.labels = 1.5,
             lwd = 2,
             rug = TRUE,
             stars = TRUE, 
             main = "Mexico")

IMEX <- as_biadjacency_matrix(BNMEX, names = TRUE, sparse = TRUE, types = bipartite_mapping(BNMEX)$type)
IMEX2 <- as.matrix(IMEX)

rownames(ProgramsMEX)[order(ProgramsMEX$Eigenvector, decreasing = TRUE)]
selected_columns <- head(rownames(ProgramsMEX)[order(ProgramsMEX$Eigenvector, decreasing = TRUE)], 10)
# Let's pick the most important soft skills
# as per their eigenvector centrality

current_column_names <- colnames(IMEX2)

# Subset the matrix by column names
IMEX3 <- IMEX2[, selected_columns, drop = FALSE]

current_column_names <- colnames(IMEX3)


# Create a vector to hold the mapped skill names
mapped_skill_names <- character(length(current_column_names))


for (i in seq_along(current_column_names)) {
  
  skill_index <- match(current_column_names[i], SkillsMEX$SkillCode)
  
  # If a match is found, map the skill code to its name
  if (!is.na(skill_index)) {
    mapped_skill_names[i] <- SkillsMEX$Skill[skill_index]
  } else {
    # Handle unmatched column names as needed
    # For example, assign a default value or leave blank
    mapped_skill_names[i] <- "Unmatched"
  }
}

# Replace the column names of IM3 with the mapped skill names
colnames(IMEX3) <- mapped_skill_names
colnames(IMEX3)

library(bipartite)
plotweb(IMEX3, method = "normal", 
        col.high = "#006847", 
        bor.col.high = "#006847",
        col.low = "#CE1125", 
        bor.col.low = "#CE1125",
        col.interaction = "grey90",
        bor.col.interaction = "grey90",
        low.lablength = 0,
        text.rot = 80,
        high.y = 1,
        ybig = 0.8,
        labsize = 2)
