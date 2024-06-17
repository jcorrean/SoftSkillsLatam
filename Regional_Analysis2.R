load("MEX.RData")
load("CHL.RData")
load("COL.RData")
load("VEN.RData")
load("ECU.RData")

rm(list=setdiff(ls(), c("IMCL3", "IMCO3", "IMEC3", "IM3", "IMEX3")))
SKILLS <- data.frame(Chile = colSums(IMCL3),
                    Colombia = colSums(IMCO3),
                    Ecuador = colSums(IMEC3),
                    Mexico = colSums(IMEX3), 
                    Venezuela = colSums(IM3))
write.csv(SKILLS, file = "MatrizTop10.csv")
