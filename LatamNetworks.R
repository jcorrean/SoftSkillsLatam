
load("Results/Argentina.RData")
load("Results/Brazil.RData")
load("Results/Chile.RData")
load("Results/Colombia.RData")
load("Results/CostaRica.RData")
load("Results/Ecuador.RData")
load("Results/Mexico.RData")
load("Results/Uruguay.RData")
load("Results/Venezuela.RData")

RegionNetwork <- do.call(rbind, list(edges_ar, 
                                     edges_br, 
                                     edges_chl, 
                                     edges_col, 
                                     edges_cr, 
                                     edges_ec, 
                                     edges_mx, 
                                     edges_ur, 
                                     edges_ve))
rm(list=setdiff(ls(), c("RegionNetwork")))
