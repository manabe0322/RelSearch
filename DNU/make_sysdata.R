#Default values of mutation rates for autosomal STRs
myuStrDefault <- rep(0, 21)
names(myuStrDefault) <- c("D3S1358", "vWA", "D16S539", "CSF1PO", "TPOX",
                          "D8S1179", "D21S11", "D18S51",
                          "D2S441", "D19S433", "TH01", "FGA",
                          "D22S1045", "D5S818", "D13S317", "D7S820", "SE33",
                          "D10S1248", "D1S1656", "D12S391", "D2S1338")
myuStrDefault[1] <- 0.001474647
myuStrDefault[2] <- 0.002858327
myuStrDefault[3] <- 0.001479789
myuStrDefault[4] <- 0.002240583
myuStrDefault[5] <- 0.000227
myuStrDefault[6] <- 0.001433812
myuStrDefault[7] <- 0.001130039
myuStrDefault[8] <- 0.001588339
myuStrDefault[9] <- 0.001521043 #mean of other loci
myuStrDefault[10] <- 0.001069792
myuStrDefault[11] <- 0.0000922
myuStrDefault[12] <- 0.002602109
myuStrDefault[13] <- 0.001521043 #mean of other loci
myuStrDefault[14] <- 0.00184855
myuStrDefault[15] <- 0.001574558
myuStrDefault[16] <- 0.001179836
myuStrDefault[17] <- 0.001521043
myuStrDefault[18] <- 0.001521043 #mean of other loci
myuStrDefault[19] <- 0.001521043 #mean of other loci
myuStrDefault[20] <- 0.001521043 #mean of other loci
myuStrDefault[21] <- 0.001130039

#Default probabilities of IBD
probIBDDefault <- matrix(0, 5, 3)
rownames(probIBDDefault) <- c("direct", "parent-child", "sibling", "2nd-degree", "3rd-degree")
colnames(probIBDDefault) <- c("Pr_IBD2", "Pr_IBD1", "Pr_IBD0")
probIBDDefault[1, ] <- c(1, 0, 0)
probIBDDefault[2, ] <- c(0, 1, 0)
probIBDDefault[3, ] <- c(0.25, 0.5, 0.25)
probIBDDefault[4, ] <- c(0, 0.5, 0.5)
probIBDDefault[5, ] <- c(0, 0.25, 0.75)

usethis::use_data(myuStrDefault, probIBDDefault,internal = TRUE)
