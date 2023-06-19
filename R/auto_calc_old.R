# Setting allele frequencies
set_af <- function(data_auto_q, data_auto_r, data_auto_af, maf){
  n_l <- ncol(data_auto_af) - 1
  name_al <- data_auto_af[, Allele]
  name_l <- setdiff(names(data_auto_q), c("SampleName", "Relationship"))
  af_list <- af_al_list <- list()
  for(i in 1:n_l){

    # Indices of each database
    pos_q <- which(names(data_auto_q) == name_l[i])
    pos_r <- which(names(data_auto_r) == name_l[i])
    pos_af <- which(names(data_auto_af) == name_l[i])

    # Observed alleles in query or reference database
    obsal <- unique(c(data_auto_q[[pos_q[1]]], data_auto_q[[pos_q[2]]], data_auto_r[[pos_r[1]]], data_auto_r[[pos_r[2]]]))
    obsal <- obsal[!is.na(obsal)]

    # Extract allele frequencies in one locus
    af <- data_auto_af[[pos_af]]
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
