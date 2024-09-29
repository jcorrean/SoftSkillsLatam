load("Results/Argentina.RData")
load("Results/Brazil.RData")
load("Results/Chile.RData")
load("Results/Colombia.RData")
load("Results/CostaRica.RData")
load("Results/Ecuador.RData")
load("Results/Mexico.RData")
load("Results/Uruguay.RData")
load("Results/Venezuela.RData")

rm(list=setdiff(ls(), c("ProgramsARG",
                        "ProgramsBRA",
                        "ProgramsCHL",
                        "ProgramsCOL",
                        "ProgramsCORI",
                        "ProgramsECU",
                        "ProgramsMEX",
                        "ProgramsURU",
                        "ProgramsVEN")))
AllCountries <- do.call(rbind, list(ProgramsARG,
                                   ProgramsBRA,
                                   ProgramsCHL,
                                   ProgramsCOL,
                                   ProgramsCORI,
                                   ProgramsECU,
                                   ProgramsMEX,
                                   ProgramsURU,
                                   ProgramsVEN))
library(dplyr)
result <- AllCountries %>%
  group_by(Partition,Country) %>%
  summarize(
    N = length(Node),
    Mean = mean(Eigenvector),
    SD = sd(Eigenvector)
  )
result

