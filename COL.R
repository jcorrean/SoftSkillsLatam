library(readtext)
COL <- readtext("Colombia")
COL$doc_id <- gsub("\\.pdf$|\\.docx$", "", COL$doc_id)

library(dplyr)
COL <- mutate(COL, 
             Program = ifelse(
               grepl("Doctorado en", text), "Doctorado",
               ifelse(grepl("Maestría|Magíster en|MAGISTER EN", text), "Maestría", 
                      "Especialización")))
Programas <- data.frame(table(COL$Program))
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
  ggtitle("COLOMBIA")

library(quanteda)
TextsCO <- corpus(COL$text)
docvars(TextsCO, "Program") <- COL$Program
head(summary(TextsCO), 10)

ProgramsCO <- tokens(TextsCO, 
                   remove_numbers = TRUE, 
                   remove_punct = TRUE, 
                   remove_url = TRUE, 
                   remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish"))

s1 <- data.frame(kwic(ProgramsCO, pattern = phrase("pensamiento crítico")))
s2 <- data.frame(kwic(ProgramsCO, pattern = phrase("solucionar problemas")))
s3 <- data.frame(kwic(ProgramsCO, pattern = "comunicar"))
s4 <- data.frame(kwic(ProgramsCO, pattern = "creatividad"))
s5 <- data.frame(kwic(ProgramsCO, pattern = "paciencia"))
s6 <- data.frame(kwic(ProgramsCO, pattern = "crear"))
s7 <- data.frame(kwic(ProgramsCO, pattern = "liderar"))
s8 <- data.frame(kwic(ProgramsCO, pattern = "resolver"))
s9 <- data.frame(kwic(ProgramsCO, pattern = "comprometer"))
s10 <- data.frame(kwic(ProgramsCO, pattern = "comprometerse"))
s11 <- data.frame(kwic(ProgramsCO, pattern = "gestionar"))
s12 <- data.frame(kwic(ProgramsCO, pattern = "reflexionar"))
s13 <- data.frame(kwic(ProgramsCO, pattern = "controlar"))
s14 <- data.frame(kwic(ProgramsCO, pattern = "ético"))
s15 <- data.frame(kwic(ProgramsCO, pattern = "tolerar"))
s16 <- data.frame(kwic(ProgramsCO, pattern = "argumentar"))
s17 <- data.frame(kwic(ProgramsCO, pattern = "conflictos"))
s18 <- data.frame(kwic(ProgramsCO, pattern = "negociar"))
s19 <- data.frame(kwic(ProgramsCO, pattern = "comprender"))
s20 <- data.frame(kwic(ProgramsCO, pattern = "equipos"))
s21 <- data.frame(kwic(ProgramsCO, pattern = "planificar"))
s22 <- data.frame(kwic(ProgramsCO, pattern = "generar"))
s23 <- data.frame(kwic(ProgramsCO, pattern = "empatía"))
s24 <- data.frame(kwic(ProgramsCO, pattern = "compartir"))
s25 <- data.frame(kwic(ProgramsCO, pattern = "analizar"))
s26 <- data.frame(kwic(ProgramsCO, pattern = "reconocer"))
s27 <- data.frame(kwic(ProgramsCO, pattern = "orientar"))
s28 <- data.frame(kwic(ProgramsCO, pattern = "respetar"))
s29 <- data.frame(kwic(ProgramsCO, pattern = "motivar"))
s30 <- data.frame(kwic(ProgramsCO, pattern = "cooperar"))
s31 <- data.frame(kwic(ProgramsCO, pattern = "fortalecer"))
s32 <- data.frame(kwic(ProgramsCO, pattern = "impulsar"))
s33 <- data.frame(kwic(ProgramsCO, pattern = "acercar"))
s34 <- data.frame(kwic(ProgramsCO, pattern = "ayudar"))
s35 <- data.frame(kwic(ProgramsCO, pattern = "cambiar"))
s36 <- data.frame(kwic(ProgramsCO, pattern = "apreciar"))
s37 <- data.frame(kwic(ProgramsCO, pattern = "dirigir"))
s38 <- data.frame(kwic(ProgramsCO, pattern = "fomentar"))
s39 <- data.frame(kwic(ProgramsCO, pattern = "interactuar"))
s40 <- data.frame(kwic(ProgramsCO, pattern = "identificar"))
s41 <- data.frame(kwic(ProgramsCO, pattern = "competir"))
s42 <- data.frame(kwic(ProgramsCO, pattern = "manifestar"))
s43 <- data.frame(kwic(ProgramsCO, pattern = "responsable"))
s44 <- data.frame(kwic(ProgramsCO, pattern = "evaluar"))
s45 <- data.frame(kwic(ProgramsCO, pattern = "innovar"))
s46 <- data.frame(kwic(ProgramsCO, pattern = "decidir"))
s47 <- data.frame(kwic(ProgramsCO, pattern = phrase("tomar decisiones")))
s48 <- data.frame(kwic(ProgramsCO, pattern = "flexibilidad"))
s49 <- data.frame(kwic(ProgramsCO, pattern = "persua*"))
s50 <- data.frame(kwic(ProgramsCO, pattern = "convencer"))

df_list <- mget(paste0("s", 1:50))
SS <- do.call(rbind, df_list)
SS$Skill <- rownames(SS)
SS$Skill <- gsub("\\..*", "", SS$Skill)

SkillsCO <- data.frame(Country = "Colombia",
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

networkCO <- SS[c(1,8)]
head(networkCO, 3)

library(igraph)
bn1 <- graph.data.frame(networkCO,directed=FALSE)
bipartite_mapping(bn1)
V(bn1)$type <- bipartite_mapping(bn1)$type
V(bn1)$shape <- ifelse(V(bn1)$type, "circle", "square")
V(bn1)$label.cex <- ifelse(V(bn1)$type, 0.5, 1)
V(bn1)$size <- sqrt(igraph::degree(bn1))
E(bn1)$color <- "lightgrey"

bn1.pr <- bipartite.projection(bn1)
Terms <- bn1.pr$proj2

pave <- data.frame(igraph::betweenness(Terms))

centrality_scores <- igraph::eigen_centrality(Terms)
centrality_scores <- centrality_scores$vector

color_palette <- colorRampPalette(c("#CE1127", "white", "#0A3A7E"))(length(unique(centrality_scores)))

# Assign colors to nodes based on their normalized centrality scores
node_colors <- color_palette[rank(centrality_scores)]

# Plot the network with node colors based on centrality
set.seed(915)


plot(Terms, vertex.label.color = "black", vertex.label.cex = 0.8, vertex.color = node_colors, vertex.size = 15, edge.width = 0.5, edge.color = "lightgray", layout = layout_as_star, main = "")

BNCO <- graph.data.frame(networkCO, directed = FALSE)
ProgramsCO <- data.frame(Degree = igraph::degree(BNCO),
                       Closeness = igraph::closeness(BNCO),
                       Betweennes = igraph::betweenness(BNCO),
                       Eigen = igraph::eigen_centrality(BNCO))
ProgramsCO <- ProgramsCO[ -c(5:25) ]
rownames(ProgramsCO)
ProgramsCO$SS <- rownames(ProgramsCO)
ProgramsCO <- ProgramsCO[order(ProgramsCO$SS), ]
ProgramsCO <- ProgramsCO[grepl("s", ProgramsCO$SS), ]
ProgramsCO <- ProgramsCO[1:4]
colnames(ProgramsCO)[4] <- "Eigenvector"



library(psych)

pairs.panels(ProgramsCO, 
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
             main = "Colombia")

IMCO <- as_incidence_matrix(BNCO, names = TRUE, sparse = TRUE, types = bipartite_mapping(BNCO)$type)
IMCO2 <- as.matrix(IMCO)

rownames(ProgramsCO)[order(ProgramsCO$Eigenvector, decreasing = TRUE)]
selected_columns <- head(rownames(ProgramsCO)[order(ProgramsCO$Eigenvector, decreasing = TRUE)], 10)
# Let's pick the most important soft skills
# as per their eigenvector centrality

current_column_names <- colnames(IMCO2)

# Subset the matrix by column names
IMCO3 <- IMCO2[, selected_columns, drop = FALSE]

current_column_names <- colnames(IMCO3)


# Create a vector to hold the mapped skill names
mapped_skill_names <- character(length(current_column_names))


for (i in seq_along(current_column_names)) {
  
  skill_index <- match(current_column_names[i], SkillsCO$SkillCode)
  
  # If a match is found, map the skill code to its name
  if (!is.na(skill_index)) {
    mapped_skill_names[i] <- SkillsCO$Skill[skill_index]
  } else {
    # Handle unmatched column names as needed
    # For example, assign a default value or leave blank
    mapped_skill_names[i] <- "Unmatched"
  }
}

# Replace the column names of IM3 with the mapped skill names
colnames(IMCO3) <- mapped_skill_names
colnames(IMCO3)

library(bipartite)
plotweb(IMCO3, method = "normal", 
        col.high = "#FCD116", 
        bor.col.high = "#FCD116",
        col.low = "#CE1127", 
        bor.col.low = "#CE1127",
        col.interaction = "grey90",
        bor.col.interaction = "grey90",
        low.lablength = 0,
        text.rot = 80,
        high.y = 1,
        ybig = 0.8,
        labsize = 2)
