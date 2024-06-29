library(igraph)
load("VEN.RData")

library(tnet)
net <- cbind(
  i=c(1,1,2,2,2,3,3,4,5,5,6),
  p=c(1,2,1,3,4,2,3,4,3,5,5),
  w=c(3,5,6,1,2,6,2,1,3,1,2))
class(net)
class(networkMEX)
networkMEX <- as.matrix(networkMEX)
class(networkMEX)
clustering_local_tm(t(IMEX2))
row.names(t(IMEX2))

clustering_tm(t(IMEX2))

projected <- projecting_tm(net, method = "Newman")
projected2 <- projecting_tm(net, method = "sum")
projected
projected2

library(dBlockmodeling)
data("nyt")
clustering_tm(t(nyt))
clustering_local_tm(t(nyt))
resmed <- tmklm(t(nyt),RC = 8,CC = 8,TLIMIT = 1)
resmed
table(resmed$RP)
resmed$objval


res <- tmklm(nyt,RC = 2,CC = 1,TLIMIT = 1)
resmed <- tmklmed(nyt,RC = 4,CC = 3,TLIMIT = 1)
res
table(res$RP)
table(resmed$RP)
resmed$objval

