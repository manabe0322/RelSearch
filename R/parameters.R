#' create_dt_criteria
#'
#' @description The function to create the data.table for criteria
#' @param path_pack Package path
create_dt_criteria <- function(path_pack){
  fn_par <- list.files(paste0(path_pack, "/extdata/parameters"))

  if(is.element("criteria.csv", fn_par)){
    dt_criteria <- fread(paste0(path_pack, "/extdata/parameters/criteria.csv"))
  }else{
    dt_criteria <- data.table(Criteria = c("min_lr_auto", "max_mismatch_y", "max_ignore_y", "max_mustep_y", "max_mismatch_mt", "min_share_mt"),
                              Value = c(100, 2, 10, 2, 1, 300))
    write.csv(dt_criteria, paste0(path_pack, "/extdata/parameters/criteria.csv"), row.names = FALSE)
  }

  return(dt_criteria)
}

#' create_dt_rel
#'
#' @description The function to create the data.table for information on relationships
#' @param path_pack Package path
create_dt_rel <- function(path_pack){
  fn_par <- list.files(paste0(path_pack, "/extdata/parameters"))

  if(is.element("rel.csv", fn_par)){
    dt_rel <- fread(paste0(path_pack, "/extdata/parameters/rel.csv"))
  }else{
    dt_rel <- data.table(Name_relationship = c("parent-child", "sibling", "grandparent-grandchild", "uncle-nephew", "cousin"),
                         Degree = c("1st_pc", "1st_sib", "2nd", "2nd", "3rd"),
                         Pr_IBD2 = c(0, 0.25, 0, 0, 0),
                         Pr_IBD1 = c(1, 0.5, 0.5, 0.5, 0.25),
                         Pr_IBD0 = c(0, 0.25, 0.5, 0.5, 0.75))
    write.csv(dt_rel, paste0(path_pack, "/extdata/parameters/rel.csv"), row.names = FALSE)
  }

  return(dt_rel)
}

#' create_dt_myu
#'
#' @description The function to create the data.table for mutation rates
#' @param path_pack Package path
create_dt_myu <- function(path_pack){
  fn_par <- list.files(paste0(path_pack, "/extdata/parameters"))

  if(is.element("myu.csv", fn_par)){
    dt_myu <- fread(paste0(path_pack, "/extdata/parameters/myu.csv"))
  }else{
    dt_myu <- data.table(Marker = c("D3S1358", "vWA", "D16S539", "CSF1PO", "TPOX",
                                    "D8S1179", "D21S11", "D18S51",
                                    "D2S441", "D19S433", "TH01", "FGA",
                                    "D22S1045", "D5S818", "D13S317", "D7S820", "SE33",
                                    "D10S1248", "D1S1656", "D12S391", "D2S1338"),
                         Myu = c(0.001474647, 0.002858327, 0.001479789, 0.002240583, 0.000227000,
                                 0.001433812, 0.001130039, 0.001588339,
                                 0.001521043, 0.001069792, 0.000092200, 0.002602109,
                                 0.001521043, 0.001848550, 0.001574558, 0.001179836, 0.001521043,
                                 0.001521043, 0.001521043, 0.001521043, 0.001130039))
    write.csv(dt_myu, paste0(path_pack, "/extdata/parameters/myu.csv"), row.names = FALSE)
  }

  return(dt_myu)
}

#' create_dt_par_auto
#'
#' @description The function to create the data.table for parameters of autosomal STR
#' @param path_pack Package path
create_dt_par_auto <- function(path_pack){
  fn_par <- list.files(paste0(path_pack, "/extdata/parameters"))

  if(is.element("par_auto.csv", fn_par)){
    dt_par_auto <- fread(paste0(path_pack, "/extdata/parameters/par_auto.csv"))
  }else{
    dt_par_auto <- data.table(Parameter = c("maf", "meth_d", "pd"),
                              Value = c(0.001, 1, 0.5))
    write.csv(dt_par_auto, paste0(path_pack, "/extdata/parameters/par_auto.csv"), row.names = FALSE)
  }

  return(dt_par_auto)
}
