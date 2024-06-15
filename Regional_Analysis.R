library(readtext)
textos <- readtext("Venezuela")
textos$doc_id <- gsub("\\.pdf$|\\.docx$", "", textos$doc_id)

library(dplyr)
textos <- mutate(textos, 
                 Program = ifelse(
                   grepl("Doctorado en", text), "Doctorado",
                   ifelse(grepl("Maestría", text), "Maestría", 
                          "Especialización")))
Programas <- data.frame(table(textos$Program))
colnames(Programas)[1] <- "Programa" 
colnames(Programas)[2] <- "Total"
