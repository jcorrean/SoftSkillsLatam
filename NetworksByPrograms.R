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

Specializations <- do.call(rbind, list(ProgramsARG1,
                                       ProgramsBRA1,
                                       ProgramsCHL1,
                                       ProgramsCOL1,
                                       ProgramsCORI1,
                                       ProgramsECU1,
                                       ProgramsMEX1,
                                       ProgramsURU1,
                                       ProgramsVEN1))
Masters <- do.call(rbind, list(ProgramsARG2,
                               ProgramsBRA2,
                               ProgramsCHL2,
                               ProgramsCOL2,
                               ProgramsCORI2,
                               ProgramsECU2,
                               ProgramsMEX2,
                               ProgramsURU2,
                               ProgramsVEN2))
PhD <- do.call(rbind, list(ProgramsARG3,
                           ProgramsBRA3,
                           ProgramsCHL3,
                           ProgramsCOL3,
                           ProgramsCORI3,
                           ProgramsECU3,
                           ProgramsMEX3,
                           ProgramsURU3,
                           ProgramsVEN3))
