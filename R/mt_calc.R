# Matching query and reference haplotypes of mtDNA (testthat)
matchMt <- function(qHap, qRan, rHap, rRan){
  posMtQR <- extPosMtQR(qRan, rRan)
  lenShare <- length(posMtQR)
  qHap <- strsplit(qHap, " ")[[1]]
  rHap <- strsplit(rHap, " ")[[1]]
  if(lenShare > 0){
    shareRange <- makeShareRange(posMtQR)
    qHap <- qHap[is.element(round(parse_number(qHap), 0), posMtQR)]
    rHap <- rHap[is.element(round(parse_number(rHap), 0), posMtQR)]
    qrHap <- union(qHap, rHap)
    nMis <- length(setdiff(qrHap, qHap)) + length(setdiff(qrHap, rHap))
  }else{
    nMis <- ""
    shareRange <- ""
  }
  return(c(nMis, shareRange, lenShare))
}
