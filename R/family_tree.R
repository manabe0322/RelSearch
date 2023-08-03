#####################################
# The function to check family tree #
#####################################

check_tree <- function(persons, sex_all, father_all, mother_all, founder_all){

  # create sex IDs
  sex_id <- rep(0, length(sex_all))
  sex_id[sex_all == "M"] <- 1
  sex_id[sex_all == "F"] <- 2

  # Index of founders
  pos_founder <- which(founder_all == "Yes")

  # Create father IDs
  father_id <- father_all
  father_id[pos_founder] <- 0

  # Create mother IDs
  mother_id <- mother_all
  mother_id[pos_founder] <- 0

  # Define a family tree
  tree <- try(ped(id = persons,
                  fid = father_id,
                  mid = mother_id,
                  sex = sex_id),
              silent = TRUE)

  return(tree)
}


###########################################
# The function to make a displayed degree #
###########################################

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
