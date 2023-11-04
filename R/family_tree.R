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

#' judge_paternal
#'
#' @description The function to judge paternal lineage
#' @param p1 The name of person 1
#' @param p2 The name of person 2
#' @param id All names of persons
#' @param fid Fathers of all persons
#' @param sex Sex of all persons
judge_paternal <- function(p1, p2, id, fid, sex){

  # The function to search paternal founder
  search_paternal_founder <- function(pos_start, pos_another){
    id_target <- pos_start
    bool_founder <- FALSE

    while(!bool_founder){
      father <- fid[id_target]

      # Lineal relationship
      if(father == id[pos_another]){
        bool_founder <- TRUE
        result <- "lineal"

      # Reach a founder
      }else if(father == 0){
        bool_founder <- TRUE
        result <- id[id_target]

      }else{
        id_target <- which(id == father)
      }
    }
    return(result)
  }

  pos_p1 <- match(p1, id)
  pos_p2 <- match(p2, id)

  if((sex[pos_p1] == 1) && (sex[pos_p2] == 1)){
    if((fid[pos_p1] == 0) && (fid[pos_p2] == 0)){
      result <- "not paternal"
    }else{
      # Search the paternal founder of p1
      founder_p1 <- search_paternal_founder(pos_p1, pos_p2)

      # Search the paternal founder of p2
      founder_p2 <- search_paternal_founder(pos_p2, pos_p1)

      if((founder_p1 == "lineal") || (founder_p2 == "lineal")){
        result <- "lineal"
      }else if(founder_p1 == founder_p2){
        result <- "collateral"
      }else{
        result <- "not paternal"
      }
    }
  }else{
    result <- "not paternal"
  }
  return(result)
}

#' judge_maternal
#'
#' @description The function to judge maternal lineage
#' @param p1 The name of person 1
#' @param p2 The name of person 2
#' @param id All names of persons
#' @param mid Mothers of all persons
judge_maternal <- function(p1, p2, id, mid){

  # The function to search maternal founder
  search_maternal_founder <- function(pos_start, pos_another){
    id_target <- pos_start
    bool_founder <- FALSE

    while(!bool_founder){
      mother <- mid[id_target]

      # Lineal relationship
      if(mother == id[pos_another]){
        bool_founder <- TRUE
        result <- "lineal"

      # Reach a founder
      }else if(mother == 0){
        bool_founder <- TRUE
        result <- id[id_target]

      }else{
        id_target <- which(id == mother)
      }
    }
    return(result)
  }

  pos_p1 <- match(p1, id)
  pos_p2 <- match(p2, id)

  if((mid[pos_p1] == 0) && (mid[pos_p2] == 0)){
    result <- "not maternal"
  }else{
    # Search the maternal founder of p1
    founder_p1 <- search_maternal_founder(pos_p1, pos_p2)

    # Search the maternal founder of p2
    founder_p2 <- search_maternal_founder(pos_p2, pos_p1)

    if((founder_p1 == "lineal") || (founder_p2 == "lineal")){
      result <- "lineal"
    }else if(founder_p1 == founder_p2){
      result <- "collateral"
    }else{
      result <- "not maternal"
    }
  }
  return(result)
}
