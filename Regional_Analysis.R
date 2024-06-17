load("MEX.RData")
load("CHL.RData")
load("COL.RData")
load("VEN.RData")
load("ECU.RData")
rm(list=setdiff(ls(), c("ProgramsMX" = "ProgramsCL", "ProgramsCO", "ProgramsVE", "ProgramsEC")))
ProgramsCL$Country <- "Chile"
ProgramsCL$Skill <- rownames(ProgramsCL)
ProgramsCO$Country <- "Colombia"
ProgramsCO$Skill <- rownames(ProgramsCO)
ProgramsEC$Country <- "Ecuador"
ProgramsEC$Skill <- rownames(ProgramsEC)
ProgramsVE$Country <- "Venezuela"
ProgramsVE$Skill <- rownames(ProgramsVE)
ProgramsMX$Country <- "Mexico"
ProgramsMX$Skill <- rownames(ProgramsMX)
df <- mget(ls(pattern = "Programs"))
SS <- do.call(rbind, df)

library(dplyr)
SS <- SS %>% mutate(Skill = recode(Skill,
                             "s1" = "pensamiento crítico", 
                             "s2" = "solucionar problemas", 
                             "s3" = "comunicar",
                             "s4" = "creatividad",
                             "s5" = "paciencia",
                             "s6" = "crear",
                             "s7" = "liderar",
                             "s8" = "resolver", 
                             "s9" = "comprometer",
                             "s10" = "comprometerse",
                             "s11" = "gestionar",
                             "s12" = "reflexionar",
                             "s13" = "controlar",
                             "s14" = "ético",
                             "s15" = "tolerar",
                             "s16" = "argumentar",
                             "s17" = "conflictos",
                             "s18" = "negociar",
                             "s19" = "comprender", 
                             "s20" = "equipos",
                             "s21" = "planificar",
                             "s22" = "generar",
                             "s23" = "empatía",
                             "s24" = "compartir",
                             "s25" = "analizar",
                             "s26" = "reconocer",
                             "s27" = "orientar",
                             "s28" = "respetar",
                             "s29" = "motivar",
                             "s30" = "cooperar",
                             "s31" = "fortalecer",
                             "s32" = "impulsar",
                             "s33" = "acercar",
                             "s34" = "ayudar",
                             "s35" = "cambiar",
                             "s36" = "apreciar",
                             "s37" = "dirigir",
                             "s38" = "fomentar",
                             "s39" = "interactuar",
                             "s40" = "identificar",
                             "s41" = "competir",
                             "s42" = "manifestar",
                             "s43" = "responsable",
                             "s44" = "evaluar",
                             "s45" = "innovar",
                             "s46" = "decidir",
                             "s47" = "tomar decisiones",
                             "s48" = "flexibilidad",
                             "s49" = "persua*",
                             "s50" = "convencer"))
rm(list=setdiff(ls(), "SS"))
   
