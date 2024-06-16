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

Programs <- tokens(TextsCO, 
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

