# Setting allele frequencies
set_af <- function(qgt_dt, rgt_dt, af_dt, maf){
  n_l <- ncol(af_dt) - 1
  name_al <- af_dt[, Allele]
  name_l <- setdiff(names(af_dt), "Allele")
  af_list <- af_al_list <- list()
  for(i in 1:n_l){

    # Observed alleles in query or reference database
    obsal <- unique(c(qgt_dt[[2 * i - 1]], qgt_dt[[2 * i]], rgt_dt[[2 * i - 1]], rgt_dt[[2 * i]]))
    obsal <- obsal[!is.na(obsal)]

    # Extract allele frequencies in one locus
    af <- af_dt[[i + 1]]
    pos_al <- !is.na(af)
    af <- af[pos_al]
    af_al <- name_al[pos_al]

    # Add unobserved allele frequencies
    pos_unobs <- which(is.element(obsal, af_al) == FALSE)
    if(length(pos_unobs) > 0){
      af_al <- c(af_al, obsal[pos_unobs])
      af <- c(af, rep(maf, length(pos_unobs)))
    }
    af_list[[i]] <- af
    af_al_list[[i]] <- af_al
  }
  names(af_list) <- name_l
  names(af_al_list) <- name_l
  return(list(af_list, af_al_list))
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
