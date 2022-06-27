# Calculate mutational step between query alleles and reference alleles (testthat)
calcMuStep <- function(qAl, rAl){
  nqAl <- length(qAl)
  nrAl <- length(rAl)
  diff <- rep(0, nqAl * nrAl)
  for(j in 1:nrAl){
    diff[(nqAl * (j - 1) + 1):(nqAl * j)] <- qAl - rAl[j]
  }
  muStep <- min(setdiff(abs(diff), 0))
  return(muStep)
}

# Matching query and reference Y haplotypes (testthat)
matchY <- function(qHap, rHap){
  nL <- length(qHap)
  judgeMat <- matrix(0, 3, nL + 1)
  for(i in 1:nL){
    qAl <- qHap[i]
    qAl <- as.numeric(strsplit(qAl, ", ")[[1]])
    rAl <- rHap[i]
    rAl <- as.numeric(strsplit(rAl, ", ")[[1]])
    #mismatch or not
    judgeMat[1, i] <- !setequal(qAl, rAl)
    #ignore or not
    judgeMat[2, i] <- (judgeMat[1, i] == 1) && (length(qAl) == 0)
    #mutation step
    if((judgeMat[1, i] == 1) && (judgeMat[2, i] == 0)){
      judgeMat[3, i] <- calcMuStep(qAl, rAl)
    }
  }
  judgeMat[1, nL + 1] <- sum(judgeMat[1, ])
  judgeMat[2, nL + 1] <- sum(judgeMat[2, ])
  return(judgeMat)
}
