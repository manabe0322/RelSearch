# Setting allele frequencies
set_af <- function(qData, rData, afInput, maf){
  nL <- ncol(afInput) - 1
  nameAl <- afInput[, 1]
  nameL <- colnames(afInput)[-1]
  afList <- afAlList <- list()
  for(i in 1:nL){
    alleleQ <- unique(as.numeric(qData[, c(2 * i - 1, 2 * i)]))
    alleleR <- unique(as.numeric(rData[, c(2 * i - 1, 2 * i)]))
    alleleObs <- sort(unique(c(alleleQ, alleleR)))
    af <- afInput[, i + 1]
    posAl <- !is.na(af)
    af <- af[posAl]
    afAl <- nameAl[posAl]
    posUnobs <- which(is.element(alleleObs, afAl) == FALSE)
    if(length(posUnobs) > 0){
      afAl <- c(afAl, alleleObs[posUnobs])
      af <- c(af, rep(maf, length(posUnobs)))
    }
    afList[[i]] <- af
    afAlList[[i]] <- afAl
  }
  names(afList) <- nameL
  names(afAlList) <- nameL
  return(list(afList, afAlList))
}

# Calculation of the average probability of exclusion (testthat)
calc_ape <- function(af){
  sigma1 <- sum(af^2 * (1 - af)^2)
  nAl <- length(af)
  posAf <- combn(1:nAl, 2)
  sigma2 <- 0
  for(i in 1:ncol(posAf)){
    heteroAf <- af[posAf[, i]]
    sigma2 <- sigma2 + 2 * prod(heteroAf) * (1 - sum(heteroAf))^2
  }
  return(sum(sigma1 + sigma2))
}
