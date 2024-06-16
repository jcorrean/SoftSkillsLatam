library(readtext)
VE <- readtext("Venezuela")
VE$doc_id <- gsub("\\.pdf$|\\.docx$", "", VE$doc_id)

library(dplyr)
VE <- mutate(VE, 
             Program = ifelse(
               grepl("Doctorado en", text), "Doctorado",
               ifelse(grepl("Maestría|Magíster en|MAGISTER EN", text), "Maestría", 
                      "Especialización")))
Programas <- data.frame(table(VE$Program))
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
  ggtitle("VENEZUELA")

library(quanteda)
TextsVE <- corpus(VE$text)
docvars(TextsVE, "Program") <- VE$Program
head(summary(TextsVE), 10)

Programs <- tokens(TextsVE, 
                   remove_numbers = TRUE, 
                   remove_punct = TRUE, 
                   remove_url = TRUE, 
                   remove_symbols = TRUE) %>%  
  tokens_remove(stopwords("spanish"))

s1 <- data.frame(kwic(Programs, pattern = phrase("pensamiento crítico")))
s2 <- data.frame(kwic(Programs, pattern = phrase("solucionar problemas")))
s3 <- data.frame(kwic(Programs, pattern = "comunicar"))
s4 <- data.frame(kwic(Programs, pattern = "creatividad"))
s5 <- data.frame(kwic(Programs, pattern = "paciencia"))
s6 <- data.frame(kwic(Programs, pattern = "crear"))
s7 <- data.frame(kwic(Programs, pattern = "liderar"))
s8 <- data.frame(kwic(Programs, pattern = "resolver"))
s9 <- data.frame(kwic(Programs, pattern = "comprometer"))
s10 <- data.frame(kwic(Programs, pattern = "comprometerse"))
s11 <- data.frame(kwic(Programs, pattern = "gestionar"))
s12 <- data.frame(kwic(Programs, pattern = "reflexionar"))
s13 <- data.frame(kwic(Programs, pattern = "controlar"))
s14 <- data.frame(kwic(Programs, pattern = "ético"))
s15 <- data.frame(kwic(Programs, pattern = "tolerar"))
s16 <- data.frame(kwic(Programs, pattern = "argumentar"))
s17 <- data.frame(kwic(Programs, pattern = "conflictos"))
s18 <- data.frame(kwic(Programs, pattern = "negociar"))
s19 <- data.frame(kwic(Programs, pattern = "comprender"))
s20 <- data.frame(kwic(Programs, pattern = "equipos"))
s21 <- data.frame(kwic(Programs, pattern = "planificar"))
s22 <- data.frame(kwic(Programs, pattern = "generar"))
s23 <- data.frame(kwic(Programs, pattern = "empatía"))
s24 <- data.frame(kwic(Programs, pattern = "compartir"))
s25 <- data.frame(kwic(Programs, pattern = "analizar"))
s26 <- data.frame(kwic(Programs, pattern = "reconocer"))
s27 <- data.frame(kwic(Programs, pattern = "orientar"))
s28 <- data.frame(kwic(Programs, pattern = "respetar"))
s29 <- data.frame(kwic(Programs, pattern = "motivar"))
s30 <- data.frame(kwic(Programs, pattern = "cooperar"))
s31 <- data.frame(kwic(Programs, pattern = "fortalecer"))
s32 <- data.frame(kwic(Programs, pattern = "impulsar"))
s33 <- data.frame(kwic(Programs, pattern = "acercar"))
s34 <- data.frame(kwic(Programs, pattern = "ayudar"))
s35 <- data.frame(kwic(Programs, pattern = "cambiar"))
s36 <- data.frame(kwic(Programs, pattern = "apreciar"))
s37 <- data.frame(kwic(Programs, pattern = "dirigir"))
s38 <- data.frame(kwic(Programs, pattern = "fomentar"))
s39 <- data.frame(kwic(Programs, pattern = "interactuar"))
s40 <- data.frame(kwic(Programs, pattern = "identificar"))
s41 <- data.frame(kwic(Programs, pattern = "competir"))
s42 <- data.frame(kwic(Programs, pattern = "manifestar"))
s43 <- data.frame(kwic(Programs, pattern = "responsable"))
s44 <- data.frame(kwic(Programs, pattern = "evaluar"))
s45 <- data.frame(kwic(Programs, pattern = "innovar"))
s46 <- data.frame(kwic(Programs, pattern = "decidir"))
s47 <- data.frame(kwic(Programs, pattern = phrase("tomar decisiones")))
s48 <- data.frame(kwic(Programs, pattern = "flexibilidad"))
s49 <- data.frame(kwic(Programs, pattern = "persua*"))
s50 <- data.frame(kwic(Programs, pattern = "convencer"))

df_list <- mget(paste0("s", 1:50))
SS <- do.call(rbind, df_list)
SS$Skill <- rownames(SS)
SS$Skill <- gsub("\\..*", "", SS$Skill)

SkillsVE <- data.frame(Country = "Venezuela",
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

networkVE <- SS[c(1,8)]
head(networkVE, 3)

library(igraph)
bn2 <- graph.data.frame(networkVE,directed=FALSE)
bipartite_mapping(bn2)
V(bn2)$type <- bipartite_mapping(bn2)$type
V(bn2)$shape <- ifelse(V(bn2)$type, "circle", "square")
V(bn2)$label.cex <- ifelse(V(bn2)$type, 0.5, 1)
V(bn2)$size <- sqrt(igraph::degree(bn2))
E(bn2)$color <- "lightgrey"

bn2.pr <- bipartite.projection(bn2)
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

BNA <- graph.data.frame(networkVE, directed = FALSE)
Programs <- data.frame(Degree = igraph::degree(BNA),
                       Closeness = igraph::closeness(BNA),
                       Betweennes = igraph::betweenness(BNA),
                       Eigen = igraph::eigen_centrality(BNA))
Programs <- Programs[ -c(5:25) ]
rownames(Programs)
Programs$SS <- rownames(Programs)
Programs <- Programs[order(Programs$SS), ]
Programs <- Programs[grepl("s", Programs$SS), ]
Programs <- Programs[1:4]
colnames(Programs)[4] <- "Eigenvector"



library(psych)

pairs.panels(Programs, 
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
             main = "Venezuela")

IM <- as_incidence_matrix(BNA, names = TRUE, sparse = TRUE, types = bipartite_mapping(BNA)$type)
IM2 <- as.matrix(IM)

rownames(Programs)[order(Programs$Eigenvector, decreasing = TRUE)]
selected_columns <- head(rownames(Programs)[order(Programs$Eigenvector, decreasing = TRUE)], 10)
# Let's pick the most important soft skills
# as per their eigenvector centrality

current_column_names <- colnames(IM2)

# Subset the matrix by column names
IM3 <- IM2[, selected_columns, drop = FALSE]

current_column_names <- colnames(IM3)


# Create a vector to hold the mapped skill names
mapped_skill_names <- character(length(current_column_names))


for (i in seq_along(current_column_names)) {
  
  skill_index <- match(current_column_names[i], SkillsVE$SkillCode)
  
  # If a match is found, map the skill code to its name
  if (!is.na(skill_index)) {
    mapped_skill_names[i] <- SkillsVE$Skill[skill_index]
  } else {
    # Handle unmatched column names as needed
    # For example, assign a default value or leave blank
    mapped_skill_names[i] <- "Unmatched"
  }
}

# Replace the column names of IM3 with the mapped skill names
colnames(IM3) <- mapped_skill_names
colnames(IM3)

library(bipartite)
plotweb(IM3, method = "normal", 
        col.high = "#0A3A7E", 
        bor.col.high = "#0A3A7E",
        col.low = "#CE1127", 
        bor.col.low = "#CE1127",
        col.interaction = "grey90",
        bor.col.interaction = "grey90",
        low.lablength = 0,
        text.rot = 80,
        high.y = 1,
        ybig = 0.8,
        labsize = 2)


