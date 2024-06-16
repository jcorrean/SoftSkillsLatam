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

