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
docvars(TextsMEX, "Country") <- "Mexico"
head(summary(TextsMEX), 10)

ProgramsMX <- tokens(TextsMEX, 
                     remove_numbers = TRUE, 
                     remove_punct = TRUE, 
                     remove_url = TRUE, 
                     remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish"))

s1 <- data.frame(kwic(ProgramsMX, pattern = phrase("pensamiento crítico")))
s2 <- data.frame(kwic(ProgramsMX, pattern = phrase("solucionar problemas")))
s3 <- data.frame(kwic(ProgramsMX, pattern = "comunicar"))
s4 <- data.frame(kwic(ProgramsMX, pattern = "creatividad"))
s5 <- data.frame(kwic(ProgramsMX, pattern = "paciencia"))
s6 <- data.frame(kwic(ProgramsMX, pattern = "crear"))
s7 <- data.frame(kwic(ProgramsMX, pattern = "liderar"))
s8 <- data.frame(kwic(ProgramsMX, pattern = "resolver"))
s9 <- data.frame(kwic(ProgramsMX, pattern = "comprometer"))
s10 <- data.frame(kwic(ProgramsMX, pattern = "comprometerse"))
s11 <- data.frame(kwic(ProgramsMX, pattern = "gestionar"))
s12 <- data.frame(kwic(ProgramsMX, pattern = "reflexionar"))
s13 <- data.frame(kwic(ProgramsMX, pattern = "controlar"))
s14 <- data.frame(kwic(ProgramsMX, pattern = "ético"))
s15 <- data.frame(kwic(ProgramsMX, pattern = "tolerar"))
s16 <- data.frame(kwic(ProgramsMX, pattern = "argumentar"))
s17 <- data.frame(kwic(ProgramsMX, pattern = "conflictos"))
s18 <- data.frame(kwic(ProgramsMX, pattern = "negociar"))
s19 <- data.frame(kwic(ProgramsMX, pattern = "comprender"))
s20 <- data.frame(kwic(ProgramsMX, pattern = "equipos"))
s21 <- data.frame(kwic(ProgramsMX, pattern = "planificar"))
s22 <- data.frame(kwic(ProgramsMX, pattern = "generar"))
s23 <- data.frame(kwic(ProgramsMX, pattern = "empatía"))
s24 <- data.frame(kwic(ProgramsMX, pattern = "compartir"))
s25 <- data.frame(kwic(ProgramsMX, pattern = "analizar"))
s26 <- data.frame(kwic(ProgramsMX, pattern = "reconocer"))
s27 <- data.frame(kwic(ProgramsMX, pattern = "orientar"))
s28 <- data.frame(kwic(ProgramsMX, pattern = "respetar"))
s29 <- data.frame(kwic(ProgramsMX, pattern = "motivar"))
s30 <- data.frame(kwic(ProgramsMX, pattern = "cooperar"))
s31 <- data.frame(kwic(ProgramsMX, pattern = "fortalecer"))
s32 <- data.frame(kwic(ProgramsMX, pattern = "impulsar"))
s33 <- data.frame(kwic(ProgramsMX, pattern = "acercar"))
s34 <- data.frame(kwic(ProgramsMX, pattern = "ayudar"))
s35 <- data.frame(kwic(ProgramsMX, pattern = "cambiar"))
s36 <- data.frame(kwic(ProgramsMX, pattern = "apreciar"))
s37 <- data.frame(kwic(ProgramsMX, pattern = "dirigir"))
s38 <- data.frame(kwic(ProgramsMX, pattern = "fomentar"))
s39 <- data.frame(kwic(ProgramsMX, pattern = "interactuar"))
s40 <- data.frame(kwic(ProgramsMX, pattern = "identificar"))
s41 <- data.frame(kwic(ProgramsMX, pattern = "competir"))
s42 <- data.frame(kwic(ProgramsMX, pattern = "manifestar"))
s43 <- data.frame(kwic(ProgramsMX, pattern = "responsable"))
s44 <- data.frame(kwic(ProgramsMX, pattern = "evaluar"))
s45 <- data.frame(kwic(ProgramsMX, pattern = "innovar"))
s46 <- data.frame(kwic(ProgramsMX, pattern = "decidir"))
s47 <- data.frame(kwic(ProgramsMX, pattern = phrase("tomar decisiones")))
s48 <- data.frame(kwic(ProgramsMX, pattern = "flexibilidad"))
s49 <- data.frame(kwic(ProgramsMX, pattern = "persua*"))
s50 <- data.frame(kwic(ProgramsMX, pattern = "convencer"))

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
bn8 <- graph.data.frame(networkMEX,directed=FALSE)
bipartite_mapping(bn8)
V(bn8)$type <- bipartite_mapping(bn8)$type
V(bn8)$shape <- ifelse(V(bn8)$type, "circle", "square")
V(bn8)$label.cex <- ifelse(V(bn8)$type, 0.5, 1)
V(bn8)$size <- sqrt(igraph::degree(bn8))
E(bn8)$color <- "lightgrey"

bn8.pr <- bipartite_projection(bn8)
Terms <- bn8.pr$proj2

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
ProgramsMX <- data.frame(Degree = igraph::degree(BNMEX),
                         Closeness = igraph::closeness(BNMEX),
                         Betweennes = igraph::betweenness(BNMEX),
                         Eigen = igraph::eigen_centrality(BNMEX))
ProgramsMX <- ProgramsMX[ -c(5:25) ]
rownames(ProgramsMX)
ProgramsMX$SS <- rownames(ProgramsMX)
ProgramsMX <- ProgramsMX[order(ProgramsMX$SS), ]
ProgramsMX <- ProgramsMX[grepl("s", ProgramsMX$SS), ]
ProgramsMX <- ProgramsMX[1:4]
colnames(ProgramsMX)[4] <- "Eigenvector"



library(psych)

pairs.panels(ProgramsMX, 
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

rownames(ProgramsMX)[order(ProgramsMX$Eigenvector, decreasing = TRUE)]
selected_columns <- head(rownames(ProgramsMX)[order(ProgramsMX$Eigenvector, decreasing = TRUE)], 10)
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
