# Extract positions from range (testthat)
extPosMt <- function(range){
  sepRange <- strsplit(range, " ")[[1]]
  sepRange <- strsplit(sepRange, "-")
  posMt <- numeric(0)
  for(i in 1:length(sepRange)){
    sepRange_1 <- as.numeric(sepRange[[i]])
    posMt <- c(posMt, sepRange_1[1]:sepRange_1[2])
  }
  return(sort(posMt))
}

# Extract shared positions between query range and reference range (testthat)
extPosMtQR <- function(qRan, rRan){
  posMtQ <- extPosMt(qRan)
  posMtR <- extPosMt(rRan)
  posMtQR <- intersect(posMtQ, posMtR)
  return(posMtQR)
}

# Make shared range between query range and reference range (testthat)
makeShareRange <- function(posMtQR){
  lenShare <- length(posMtQR)
  posSep <- which(posMtQR[2:lenShare] - posMtQR[1:(lenShare - 1)] != 1)
  posSep <- c(0, posSep, lenShare)
  nSeg <- length(posSep) - 1
  seg <- rep("", nSeg)
  for(i in 1:nSeg){
    seg[i] <- paste0(posMtQR[posSep[i] + 1], "-", posMtQR[posSep[i + 1]])
  }
  shareRange <- paste(seg, collapse = " ")
  return(shareRange)
}

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
