#' check_tree
#'
#' @description The function to check family tree
#' @param persons Names of persons
#' @param sex_all Sex of all persons
#' @param father_all Fathers of all persons
#' @param mother_all Mothers of all persons
#' @param founder_all Information on founder/offspring of all persons
check_tree <- function(persons, sex_all, father_all, mother_all, founder_all){
  sex_id <- rep(0, length(sex_all))
  sex_id[sex_all == "M"] <- 1
  sex_id[sex_all == "F"] <- 2

  pos_founder <- which(founder_all == "Yes")

  father_id <- father_all
  father_id[pos_founder] <- 0

  mother_id <- mother_all
  mother_id[pos_founder] <- 0

  tree <- try(ped(id = persons,
                  fid = father_id,
                  mid = mother_id,
                  sex = sex_id),
              silent = TRUE)

  return(tree)
}

#' make_deg_display
#'
#' @description The function to make a displayed degree
#' @param deg Degree of relationship
#' @param k1 Pr(IBD = 1)
make_deg_display <- function(deg, k1 = -1){
  if(is.na(deg)){
    deg_display <- "unr"
  }else if(deg == 1 && k1 == 1){
    deg_display <- "1st_pc"
  }else if(deg == 1 && k1 == 0.5){
    deg_display <- "1st_sib"
  }else if(is.element(deg, c(11, 12, 13))){
    deg_display <- paste0(deg, "th")
  }else if(deg %% 10 == 1){
    deg_display <- paste0(deg, "st")
  }else if(deg %% 10 == 2){
    deg_display <- paste0(deg, "nd")
  }else if(deg %% 10 == 3){
    deg_display <- paste0(deg, "rd")
  }else{
    deg_display <- paste0(deg, "th")
  }
  return(deg_display)
}
