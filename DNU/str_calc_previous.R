# General calculation of likelihoods for pairwise kinship analysis (testthat)
kinLike <- function(qgt, rgt, afOneL, afAlOneL, probIBD, mutation = FALSE, myuOneL = numeric(0), apeOneL = numeric(0)){
  qgt <- sort(unique(qgt))
  rgt <- sort(unique(rgt))
  k2 <- probIBD[1]
  k1 <- probIBD[2] / 2
  k0 <- probIBD[3]

  if(length(qgt) == 1){
    a <- b <- afOneL[which(afAlOneL == qgt)]
  }else{
    a <- afOneL[which(afAlOneL == qgt[1])]
    b <- afOneL[which(afAlOneL == qgt[2])]
  }
  if(length(rgt) == 1){
    c <- d <- afOneL[which(afAlOneL == rgt)]
  }else{
    c <- afOneL[which(afAlOneL == rgt[1])]
    d <- afOneL[which(afAlOneL == rgt[2])]
  }

  if(!any(is.element(qgt, rgt))){
    if(mutation){
      likeH1 <- myuOneL
      likeH2 <- apeOneL
    }else{
      if(length(qgt) == 1){
        if(length(rgt) == 1){
          likeH1 <- a^2 * c^2 * k0
          likeH2 <- a^2 * c^2
        }else{
          likeH1 <- a^2 * 2 * c * d * k0
          likeH2 <- a^2 * 2 * c * d
        }
      }else{
        if(length(rgt) == 1){
          likeH1 <- 2 * a * b * c^2 * k0
          likeH2 <- 2 * a * b * c^2
        }else{
          likeH1 <- 2 * a * b * 2 * c * d * k0
          likeH2 <- 2 * a * b * 2 * c * d
        }
      }
    }
  }else if(setequal(qgt, rgt)){
    if(length(qgt) == 1){
      if(length(rgt) == 1){
        likeH1 <- c^2 * (k2 + 2 * a * k1 + a^2 * k0)
        likeH2 <- c^2 * a^2
      }else{
        likeH1 <- 2 * c * d * (k2 + 2 * a * k1 + a^2 * k0)
        likeH2 <- 2 * c * d * a^2
      }
    }else{
      if(length(unique(rgt)) == 1){
        likeH1 <- c^2 * (k2 + a * k1 + b * k1 + 2 * a * b * k0)
        likeH2 <- c^2 * 2 * a * b
      }else{
        likeH1 <- 2 * c * d * (k2 + a * k1 + b * k1 + 2 * a * b * k0)
        likeH2 <- 2 * c * d * 2 * a * b
      }
    }
  }else if(length(unique(c(qgt, rgt))) == 3){
    if(is.element(qgt[1], rgt)){
      likeH1 <- 2 * c * d * (b * k1 + 2 * a * b * k0)
      likeH2 <- 2 * c * d * 2 * a * b
    }else{
      likeH1 <- 2 * c * d * (a * k1 + 2 * a * b * k0)
      likeH2 <- 2 * c * d * 2 * a * b
    }
  }else{
    if(length(qgt) == 1){
      likeH1 <- 2 * c * d * (a * k1 + a^2 * k0)
      likeH2 <- 2 * c * d * a^2
    }else{
      if(is.element(qgt[1], rgt)){
        likeH1 <- c^2 * (2 * b * k1 + 2 * a * b * k0)
        likeH2 <- c^2 * 2 * a * b
      }else{
        likeH1 <- c^2 * (2 * a * k1 + 2 * a * b * k0)
        likeH2 <- c^2 * 2 * a * b
      }
    }
  }
  return(c(likeH1, likeH2))
}

# Determine dummy genotypes considering a drop-out allele (testthat)
makeDummyGt <- function(qgt, rgt){
  qgt <- unique(qgt)
  rgt <- unique(rgt)
  if(length(rgt) == 1){
    if(is.element(qgt, rgt)){
      dummyGt <- matrix(c(qgt, 99),
                        ncol = 2, byrow = TRUE)
    }else{
      dummyGt <- matrix(c(qgt, rgt,
                          qgt, 99),
                        ncol = 2, byrow = TRUE)
    }
  }else{
    if(is.element(qgt, rgt)){
      dummyGt <- matrix(c(rgt,
                          qgt, 99),
                        ncol = 2, byrow = TRUE)
    }else{
      dummyGt <- matrix(c(qgt, rgt[1],
                          qgt, rgt[2],
                          qgt, 99),
                        ncol = 2, byrow = TRUE)
    }
  }
  dummyGt <- t(apply(dummyGt, 1, sort))
  return(dummyGt)
}

# Make allele frequencies for dummy genotypes (testthat)
makeDummyAf <- function(dummyGt, afOneL, afAlOneL){
  afAlOneL_dummy <- sort(unique(as.numeric(dummyGt)))
  posAl <- is.element(afAlOneL, afAlOneL_dummy)
  afOneL_dummy <- c(afOneL[posAl], sum(afOneL[!posAl]))
  names(afOneL_dummy) <- afAlOneL_dummy
  return(afOneL_dummy)
}

# Calculation of likelihoods for pairwise kinship analysis considering drop-out (testthat)
kinLikeDrop <- function(qgt, rgt, afOneL, afAlOneL, probIBD, mutation  = FALSE, myuOneL = numeric(0), apeOneL = numeric(0), pd){
  #homozygote (no drop-out)
  like1 <- kinLike2(qgt, rgt, afOneL, afAlOneL, probIBD, mutation, myuOneL, apeOneL)

  #heterozygote (drop-out)
  dummyGt <- makeDummyGt(qgt, rgt)
  afOneL_dummy <- makeDummyAf(dummyGt, afOneL, afAlOneL)
  afAlOneL_dummy <- as.numeric(names(afOneL_dummy))
  like2 <- c(0, 0)
  for(i in 1:nrow(dummyGt)){
    like2 <- like2 + kinLike2(dummyGt[i, ], rgt, afOneL_dummy, afAlOneL_dummy, probIBD, mutation, myuOneL, apeOneL)
  }

  likeH1 <- (1 - pd) * like1[1] + pd * like2[1]
  likeH2 <- (1 - pd) * like1[2] + pd * like2[2]
  return(c(likeH1, likeH2))
}

# Calculation of likelihood ratio for kinship analysis
calcKinLr <- function(query, ref, af, probIBD, mutation = FALSE, myu = numeric(0), dropMethStr = 0, pd = 0){
  nL <- length(query) / 2
  ans <- matrix(0, 3, nL + 1)
  for(i in 1:nL){
    qgt <- as.numeric(query[c(2 * i - 1, 2 * i)])
    qgt <- qgt[!is.na(qgt)]
    rgt <- as.numeric(ref[c(2 * i - 1, 2 * i)])
    rgt <- rgt[!is.na(rgt)]
    afOneL <- af[[i]]
    afAlOneL <- as.numeric(names(afOneL))
    if(mutation){
      myuOneL <- myu[i]
      apeOneL <- calcApe(afOneL)
    }else{
      myuOneL <- 0
      apeOneL <- 0
    }

    #locus drop-out or no information
    if((length(qgt) == 0) || (length(rgt) == 0)){
      ans[, i] <- 1
      #considering drop-out
    }else if(dropMethStr != 0){
      #qgt : heterozygote
      if(length(unique(qgt)) == 2){
        ans[c(1, 2), i] <- kinLike2(qgt, rgt, afOneL, afAlOneL, probIBD, mutation, myuOneL, apeOneL)
        #qgt : homozygote or heterozygote
      }else{
        if(dropMethStr == 1){
          #qgt : heterozygote
          if(length(qgt) == 2){
            ans[c(1, 2), i] <- kinLike2(qgt, rgt, afOneL, afAlOneL, probIBD, mutation, myuOneL, apeOneL)
            #considering drop-out
          }else{
            # myuOneL <- 0
            # apeOneL <- 0
            ans[c(1, 2), i] <- kinLikeDrop(qgt, rgt, afOneL, afAlOneL, probIBD, mutation, myuOneL, apeOneL, pd)
          }
          #considering drop-out
        }else if(dropMethStr == 2){
          # myuOneL <- 0
          # apeOneL <- 0
          ans[c(1, 2), i] <- kinLikeDrop(qgt, rgt, afOneL, afAlOneL, probIBD, mutation, myuOneL, apeOneL, pd)
        }
      }
      #not considering drop-out
    }else{
      ans[c(1, 2), i] <- kinLike2(qgt, rgt, afOneL, afAlOneL, probIBD, mutation, myuOneL, apeOneL)
    }
  }
  ans[1, nL + 1] <- prod(ans[1, 1:nL])
  ans[2, nL + 1] <- prod(ans[2, 1:nL])
  ans[3, ] <- ans[1, ] / ans[2, ]
  rownames(ans) <- c("likeH1", "likeH2", "LR")
  colnames(ans) <- c(names(af), "Total")
  return(ans)
}
