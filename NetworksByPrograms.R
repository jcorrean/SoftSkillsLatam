load("Results/Argentina.RData")
load("Results/Brazil.RData")
load("Results/Chile.RData")
load("Results/Colombia.RData")
load("Results/CostaRica.RData")
load("Results/Ecuador.RData")
load("Results/Mexico.RData")
load("Results/Uruguay.RData")
load("Results/Venezuela.RData")

rm(list=setdiff(ls(), c("ProgramsARG1", "ProgramsARG2", "ProgramsARG3",
                        "ProgramsBRA1", "ProgramsBRA2", "ProgramsBRA3",
                        "ProgramsCHL1", "ProgramsCHL2", "ProgramsCHL3",
                        "ProgramsCOL1", "ProgramsCOL2", "ProgramsCOL3",
                        "ProgramsCORI1", "ProgramsCORI2", "ProgramsCORI3",
                        "ProgramsECU1", "ProgramsECU2", "ProgramsECU3",
                        "ProgramsMEX1", "ProgramsMEX2", "ProgramsMEX3",
                        "ProgramsURU1", "ProgramsURU2", "ProgramsURU3",
                        "ProgramsVEN1", "ProgramsVEN2", "ProgramsVEN3")))

AllPrograms <- do.call(rbind, list(ProgramsARG1,
                                   ProgramsARG2,
                                   ProgramsARG3,
                                   ProgramsBRA1,
                                   ProgramsBRA2,
                                   ProgramsBRA3,
                                   ProgramsCHL1,
                                   ProgramsCHL2,
                                   ProgramsCHL3,
                                   ProgramsCOL1,
                                   ProgramsCOL2,
                                   ProgramsCOL3,
                                   ProgramsCORI1,
                                   ProgramsCORI2,
                                   ProgramsCORI3,
                                   ProgramsECU1,
                                   ProgramsECU2,
                                   ProgramsECU3,
                                   ProgramsMEX1,
                                   ProgramsMEX2,
                                   ProgramsMEX3,
                                   ProgramsURU1,
                                   ProgramsURU2,
                                   ProgramsURU3,
                                   ProgramsVEN1,
                                   ProgramsVEN2,
                                   ProgramsVEN3))
library(dplyr)
result <- AllPrograms %>%
  group_by(Partition,Country,Level) %>%
  summarize(
    N = length(Node),
    Mean = mean(Eigenvector),
    SD = sd(Eigenvector)
  )
result  
psych::describeBy(result$Mean, group = result$Partition, mat = TRUE, digits = 3)

library(ggplot2)

ggplot(AllPrograms, aes(x=Level, y=Eigenvector, fill = Partition)) + 
  geom_boxplot() + 
  scale_x_discrete(limits = c("Specialization", "Master", "PhD")) +
  facet_wrap(~Country) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"))+
  xlab("") + ylab("Eigenvector centrality degree") +
  scale_fill_manual(values = c("Skill" = "#09419e", "Program" = "#FFFFFF"))
