library(readtext)
CL <- readtext("Chile")
CL$doc_id <- gsub("\\.pdf$|\\.docx$", "", CL$doc_id)

library(dplyr)
CL <- mutate(CL, 
             Program = ifelse(
               grepl("Doctorado en", text), "Doctorado",
               ifelse(grepl("Maestría|Magíster en|MAGISTER EN", text), "Maestría", 
                      "Especialización")))
Programas <- data.frame(table(CL$Program))
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
  ggtitle("CHILE")

library(quanteda)
TextsCL <- corpus(CL$text)
docvars(TextsCL, "Program") <- CL$Program
head(summary(TextsCL), 10)

ProgramsCL <- tokens(TextsCL, 
                      remove_numbers = TRUE, 
                      remove_punct = TRUE, 
                      remove_url = TRUE, 
                      remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish"))

s1 <- data.frame(kwic(ProgramsCL, pattern = phrase("pensamiento crítico")))
s2 <- data.frame(kwic(ProgramsCL, pattern = phrase("solucionar problemas")))
s3 <- data.frame(kwic(ProgramsCL, pattern = "comunicar"))
s4 <- data.frame(kwic(ProgramsCL, pattern = "creatividad"))
s5 <- data.frame(kwic(ProgramsCL, pattern = "paciencia"))
s6 <- data.frame(kwic(ProgramsCL, pattern = "crear"))
s7 <- data.frame(kwic(ProgramsCL, pattern = "liderar"))
s8 <- data.frame(kwic(ProgramsCL, pattern = "resolver"))
s9 <- data.frame(kwic(ProgramsCL, pattern = "comprometer"))
s10 <- data.frame(kwic(ProgramsCL, pattern = "comprometerse"))
s11 <- data.frame(kwic(ProgramsCL, pattern = "gestionar"))
s12 <- data.frame(kwic(ProgramsCL, pattern = "reflexionar"))
s13 <- data.frame(kwic(ProgramsCL, pattern = "controlar"))
s14 <- data.frame(kwic(ProgramsCL, pattern = "ético"))
s15 <- data.frame(kwic(ProgramsCL, pattern = "tolerar"))
s16 <- data.frame(kwic(ProgramsCL, pattern = "argumentar"))
s17 <- data.frame(kwic(ProgramsCL, pattern = "conflictos"))
s18 <- data.frame(kwic(ProgramsCL, pattern = "negociar"))
s19 <- data.frame(kwic(ProgramsCL, pattern = "comprender"))
s20 <- data.frame(kwic(ProgramsCL, pattern = "equipos"))
s21 <- data.frame(kwic(ProgramsCL, pattern = "planificar"))
s22 <- data.frame(kwic(ProgramsCL, pattern = "generar"))
s23 <- data.frame(kwic(ProgramsCL, pattern = "empatía"))
s24 <- data.frame(kwic(ProgramsCL, pattern = "compartir"))
s25 <- data.frame(kwic(ProgramsCL, pattern = "analizar"))
s26 <- data.frame(kwic(ProgramsCL, pattern = "reconocer"))
s27 <- data.frame(kwic(ProgramsCL, pattern = "orientar"))
s28 <- data.frame(kwic(ProgramsCL, pattern = "respetar"))
s29 <- data.frame(kwic(ProgramsCL, pattern = "motivar"))
s30 <- data.frame(kwic(ProgramsCL, pattern = "cooperar"))
s31 <- data.frame(kwic(ProgramsCL, pattern = "fortalecer"))
s32 <- data.frame(kwic(ProgramsCL, pattern = "impulsar"))
s33 <- data.frame(kwic(ProgramsCL, pattern = "acercar"))
s34 <- data.frame(kwic(ProgramsCL, pattern = "ayudar"))
s35 <- data.frame(kwic(ProgramsCL, pattern = "cambiar"))
s36 <- data.frame(kwic(ProgramsCL, pattern = "apreciar"))
s37 <- data.frame(kwic(ProgramsCL, pattern = "dirigir"))
s38 <- data.frame(kwic(ProgramsCL, pattern = "fomentar"))
s39 <- data.frame(kwic(ProgramsCL, pattern = "interactuar"))
s40 <- data.frame(kwic(ProgramsCL, pattern = "identificar"))
s41 <- data.frame(kwic(ProgramsCL, pattern = "competir"))
s42 <- data.frame(kwic(ProgramsCL, pattern = "manifestar"))
s43 <- data.frame(kwic(ProgramsCL, pattern = "responsable"))
s44 <- data.frame(kwic(ProgramsCL, pattern = "evaluar"))
s45 <- data.frame(kwic(ProgramsCL, pattern = "innovar"))
s46 <- data.frame(kwic(ProgramsCL, pattern = "decidir"))
s47 <- data.frame(kwic(ProgramsCL, pattern = phrase("tomar decisiones")))
s48 <- data.frame(kwic(ProgramsCL, pattern = "flexibilidad"))
s49 <- data.frame(kwic(ProgramsCL, pattern = "persua*"))
s50 <- data.frame(kwic(ProgramsCL, pattern = "convencer"))

df_list <- mget(paste0("s", 1:50))
SS <- do.call(rbind, df_list)
SS$Skill <- rownames(SS)
SS$Skill <- gsub("\\..*", "", SS$Skill)

SkillsCL <- data.frame(Country = "Chile",
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

networkCL <- SS[c(1,8)]
head(networkCL, 3)

library(igraph)
bn5 <- graph.data.frame(networkCL,directed=FALSE)
bipartite_mapping(bn5)
V(bn5)$type <- bipartite_mapping(bn5)$type
V(bn5)$shape <- ifelse(V(bn5)$type, "circle", "square")
V(bn5)$label.cex <- ifelse(V(bn5)$type, 0.5, 1)
V(bn5)$size <- sqrt(igraph::degree(bn5))
E(bn5)$color <- "lightgrey"

bn5.pr <- bipartite_projection(bn5)
Terms <- bn5.pr$proj2

pave <- data.frame(igraph::betweenness(Terms))

centrality_scores <- igraph::eigen_centrality(Terms)
centrality_scores <- centrality_scores$vector

color_palette <- colorRampPalette(c("#CE1127", "white", "#0A3A7E"))(length(unique(centrality_scores)))

# Assign colors to nodes based on their normalized centrality scores
node_colors <- color_palette[rank(centrality_scores)]

# Plot the network with node colors based on centrality
set.seed(915)


plot(Terms, vertex.label.color = "black", vertex.label.cex = 0.8, vertex.color = node_colors, vertex.size = 15, edge.width = 0.5, edge.color = "lightgray", layout = layout_as_star, main = "")

BNCL <- graph.data.frame(networkCL, directed = FALSE)
ProgramsCL <- data.frame(Degree = igraph::degree(BNCL),
                          Closeness = igraph::closeness(BNCL),
                          Betweennes = igraph::betweenness(BNCL),
                          Eigen = igraph::eigen_centrality(BNCL))
ProgramsCL <- ProgramsCL[ -c(5:25) ]
rownames(ProgramsCL)
ProgramsCL$SS <- rownames(ProgramsCL)
ProgramsCL <- ProgramsCL[order(ProgramsCL$SS), ]
ProgramsCL <- ProgramsCL[grepl("s", ProgramsCL$SS), ]
ProgramsCL <- ProgramsCL[1:4]
colnames(ProgramsCL)[4] <- "Eigenvector"



library(psych)

pairs.panels(ProgramsCL, 
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
             main = "Chile")

IMCL <- as_biadjacency_matrix(BNCL, names = TRUE, sparse = TRUE, types = bipartite_mapping(BNCL)$type)
IMCL2 <- as.matrix(IMCL)

rownames(ProgramsCL)[order(ProgramsCL$Eigenvector, decreasing = TRUE)]
selected_columns <- head(rownames(ProgramsCL)[order(ProgramsCL$Eigenvector, decreasing = TRUE)], 10)
# Let's pick the most important soft skills
# as per their eigenvector centrality

current_column_names <- colnames(IMCL2)

# Subset the matrix by column names
IMCL3 <- IMCL2[, selected_columns, drop = FALSE]

current_column_names <- colnames(IMCL3)


# Create a vector to hold the mapped skill names
mapped_skill_names <- character(length(current_column_names))


for (i in seq_along(current_column_names)) {
  
  skill_index <- match(current_column_names[i], SkillsCL$SkillCode)
  
  # If a match is found, map the skill code to its name
  if (!is.na(skill_index)) {
    mapped_skill_names[i] <- SkillsCL$Skill[skill_index]
  } else {
    # Handle unmatched column names as needed
    # For example, assign a default value or leave blank
    mapped_skill_names[i] <- "Unmatched"
  }
}

# Replace the column names of IM3 with the mapped skill names
colnames(IMCL3) <- mapped_skill_names
colnames(IMCL3)

library(bipartite)
plotweb(IMCL3, method = "normal", 
        col.high = "#0032A0", 
        bor.col.high = "#0032A0",
        col.low = "#DA291C", 
        bor.col.low = "#DA291C",
        col.interaction = "grey90",
        bor.col.interaction = "grey90",
        low.lablength = 0,
        text.rot = 80,
        high.y = 1,
        ybig = 0.8,
        labsize = 2)

