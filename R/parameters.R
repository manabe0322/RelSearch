#' create_dt_criteria
#'
#' @description The function to create the data.table for criteria
#' @param path_pack Package path
create_dt_criteria <- function(path_pack, init = TRUE){
  fn_par <- list.files(paste0(path_pack, "/extdata/parameters"))

  if(init && is.element("criteria.csv", fn_par)){
    dt_criteria <- fread(paste0(path_pack, "/extdata/parameters/criteria.csv"))

    col_char <- c("Criteria")
    options(warn = -1)
    dt_criteria[, (col_char) := lapply(.SD, as.character), .SDcols = col_char]
    options(warn = 0)

    col_numeric <- c("Value")
    options(warn = -1)
    dt_criteria[, (col_numeric) := lapply(.SD, as.numeric), .SDcols = col_numeric]
    options(warn = 0)
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
#' @param init Whether initial creation or not
create_dt_rel <- function(path_pack, init = TRUE){
  fn_par <- list.files(paste0(path_pack, "/extdata/parameters"))

  if(init && is.element("rel.csv", fn_par)){
    dt_rel <- fread(paste0(path_pack, "/extdata/parameters/rel.csv"))

    col_char <- c("Relationship", "Victim", "Reference", "Paternal", "Maternal", "Tree_persons", "Tree_sexes", "Tree_fathers", "Tree_mothers", "Tree_founders")
    options(warn = -1)
    dt_rel[, (col_char) := lapply(.SD, as.character), .SDcols = col_char]
    options(warn = 0)

    col_numeric <- c("Pr_IBD2", "Pr_IBD1", "Pr_IBD0")
    options(warn = -1)
    dt_rel[, (col_numeric) := lapply(.SD, as.numeric), .SDcols = col_numeric]
    options(warn = 0)
  }else{
    victim <- c("Father", "Father", "Mother", "Mother", "Son", "Son", "Daughter", "Daughter",
                "Brother", "Brother", "Sister", "Sister",
                "Paternal-uncle", "Paternal-uncle", "Maternal-uncle", "Maternal-uncle",
                "Paternal-aunt", "Paternal-aunt", "Maternal-aunt", "Maternal-aunt",
                "Nephew", "Niece", "Nephew", "Niece",
                "Nephew", "Niece", "Nephew", "Niece",
                "Paternal-grandfather", "Paternal-grandfather", "Maternal-grandfather", "Maternal-grandfather",
                "Paternal-grandmother", "Paternal-grandmother", "Maternal-grandmother", "Maternal-grandmother",
                "Grandson", "Granddaughter", "Grandson", "Granddaughter",
                "Grandson", "Granddaughter", "Grandson", "Granddaughter")

    reference <- c("Son", "Daughter", "Son", "Daughter", "Father", "Mother", "Father", "Mother",
                   "Brother", "Sister", "Brother", "Sister",
                   "Nephew", "Niece", "Nephew", "Niece",
                   "Nephew", "Niece", "Nephew", "Niece",
                   "Paternal-uncle", "Paternal-uncle", "Maternal-uncle", "Maternal-uncle",
                   "Paternal-aunt", "Paternal-aunt", "Maternal-aunt", "Maternal-aunt",
                   "Grandson", "Granddaughter", "Grandson", "Granddaughter",
                   "Grandson", "Granddaughter", "Grandson", "Granddaughter",
                   "Paternal-grandfather", "Paternal-grandfather", "Maternal-grandfather", "Maternal-grandfather",
                   "Paternal-grandmother", "Paternal-grandmother", "Maternal-grandmother", "Maternal-grandmother")

    name <- mapply(paste, victim, reference, sep = "_")

    pibd2 <- c(0, 0, 0, 0, 0, 0, 0, 0,
               0.25, 0.25, 0.25, 0.25,
               0, 0, 0, 0,
               0, 0, 0, 0,
               0, 0, 0, 0,
               0, 0, 0, 0,
               0, 0, 0, 0,
               0, 0, 0, 0,
               0, 0, 0, 0,
               0, 0, 0, 0)

    pibd1 <- c(1, 1, 1, 1, 1, 1, 1, 1,
               0.5, 0.5, 0.5, 0.5,
               0.5, 0.5, 0.5, 0.5,
               0.5, 0.5, 0.5, 0.5,
               0.5, 0.5, 0.5, 0.5,
               0.5, 0.5, 0.5, 0.5,
               0.5, 0.5, 0.5, 0.5,
               0.5, 0.5, 0.5, 0.5,
               0.5, 0.5, 0.5, 0.5,
               0.5, 0.5, 0.5, 0.5)

    pibd0 <- c(0, 0, 0, 0, 0, 0, 0, 0,
               0.25, 0.25, 0.25, 0.25,
               0.5, 0.5, 0.5, 0.5,
               0.5, 0.5, 0.5, 0.5,
               0.5, 0.5, 0.5, 0.5,
               0.5, 0.5, 0.5, 0.5,
               0.5, 0.5, 0.5, 0.5,
               0.5, 0.5, 0.5, 0.5,
               0.5, 0.5, 0.5, 0.5,
               0.5, 0.5, 0.5, 0.5)

    paternal <- c("Yes", "No", "No", "No", "Yes", "No", "No", "No",
                  "Yes", "No", "No", "No",
                  "Yes", "No", "No", "No",
                  "No", "No", "No", "No",
                  "Yes", "No", "No", "No",
                  "No", "No", "No", "No",
                  "Yes", "No", "No", "No",
                  "No", "No", "No", "No",
                  "Yes", "No", "No", "No",
                  "No", "No", "No", "No")

    maternal <- c("No", "No", "Yes", "Yes", "No", "Yes", "No", "Yes",
                  "Yes", "Yes", "Yes", "Yes",
                  "No", "No", "Yes", "Yes",
                  "No", "No", "Yes", "Yes",
                  "No", "No", "Yes", "Yes",
                  "No", "No", "Yes", "Yes",
                  "No", "No", "No", "No",
                  "No", "No", "Yes", "Yes",
                  "No", "No", "No", "No",
                  "No", "No", "Yes", "Yes")

    tree_persons <- c("Victim, Ref, UK1", "Victim, Ref, UK1", "Victim, Ref, UK1", "Victim, Ref, UK1", "Victim, Ref, UK1", "Victim, Ref, UK1", "Victim, Ref, UK1", "Victim, Ref, UK1",
                      "Victim, Ref, UK1, UK2", "Victim, Ref, UK1, UK2", "Victim, Ref, UK1, UK2", "Victim, Ref, UK1, UK2",
                      "Victim, Ref, UK1, UK2, UK3, UK4", "Victim, Ref, UK1, UK2, UK3, UK4", "Victim, Ref, UK1, UK2, UK3, UK4", "Victim, Ref, UK1, UK2, UK3, UK4",
                      "Victim, Ref, UK1, UK2, UK3, UK4", "Victim, Ref, UK1, UK2, UK3, UK4", "Victim, Ref, UK1, UK2, UK3, UK4", "Victim, Ref, UK1, UK2, UK3, UK4",
                      "Victim, Ref, UK1, UK2, UK3, UK4", "Victim, Ref, UK1, UK2, UK3, UK4", "Victim, Ref, UK1, UK2, UK3, UK4", "Victim, Ref, UK1, UK2, UK3, UK4",
                      "Victim, Ref, UK1, UK2, UK3, UK4", "Victim, Ref, UK1, UK2, UK3, UK4", "Victim, Ref, UK1, UK2, UK3, UK4", "Victim, Ref, UK1, UK2, UK3, UK4",
                      "Victim, Ref, UK1, UK2, UK3", "Victim, Ref, UK1, UK2, UK3", "Victim, Ref, UK1, UK2, UK3", "Victim, Ref, UK1, UK2, UK3",
                      "Victim, Ref, UK1, UK2, UK3", "Victim, Ref, UK1, UK2, UK3", "Victim, Ref, UK1, UK2, UK3", "Victim, Ref, UK1, UK2, UK3",
                      "Victim, Ref, UK1, UK2, UK3", "Victim, Ref, UK1, UK2, UK3", "Victim, Ref, UK1, UK2, UK3", "Victim, Ref, UK1, UK2, UK3",
                      "Victim, Ref, UK1, UK2, UK3", "Victim, Ref, UK1, UK2, UK3", "Victim, Ref, UK1, UK2, UK3", "Victim, Ref, UK1, UK2, UK3")

    tree_sexes <- c("M, M, F", "M, F, F", "F, M, M", "F, F, M", "M, M, F", "M, F, M", "F, M, F", "F, F, M",
                    "M, M, M, F", "M, F, M, F", "F, M, M, F", "F, F, M, F",
                    "M, M, M, F, M, F", "M, F, M, F, M, F", "M, M, M, F, F, M", "M, F, M, F, F, M",
                    "F, M, M, F, M, F", "F, F, M, F, M, F", "F, M, M, F, F, M", "F, F, M, F, F, M",
                    "M, M, M, F, M, F", "F, M, M, F, M, F", "M, M, M, F, F, M", "F, M, M, F, F, M",
                    "M, F, M, F, M, F", "F, F, M, F, M, F", "M, F, M, F, F, M", "F, F, M, F, F, M",
                    "M, M, F, M, F", "M, F, F, M, F", "M, M, F, F, M", "M, F, F, F, M",
                    "F, M, M, M, F", "F, F, M, M, F", "F, M, M, F, M", "F, F, M, F, M",
                    "M, M, F, M, F", "F, M, F, M, F", "M, M, F, F, M", "F, M, F, F, M",
                    "M, F, M, M, F", "F, F, M, M, F", "M, F, M, F, M", "F, F, M, F, M")

    tree_fathers <- c("0, Victim, 0", "0, Victim, 0", "0, UK1, 0", "0, UK1, 0", "Ref, 0, 0", "UK1, 0, 0", "Ref, 0, 0", "UK1, 0, 0",
                      "UK1, UK1, 0, 0", "UK1, UK1, 0, 0", "UK1, UK1, 0, 0", "UK1, UK1, 0, 0",
                      "UK1, UK3, 0, 0, UK1, 0", "UK1, UK3, 0, 0, UK1, 0", "UK1, UK4, 0, 0, UK1, 0", "UK1, UK4, 0, 0, UK1, 0",
                      "UK1, UK3, 0, 0, UK1, 0", "UK1, UK3, 0, 0, UK1, 0", "UK1, UK4, 0, 0, UK1, 0", "UK1, UK4, 0, 0, UK1, 0",
                      "UK3, UK1, 0, 0, UK1, 0", "UK3, UK1, 0, 0, UK1, 0", "UK4, UK1, 0, 0, UK1, 0", "UK4, UK1, 0, 0, UK1, 0",
                      "UK3, UK1, 0, 0, UK1, 0", "UK3, UK1, 0, 0, UK1, 0", "UK4, UK1, 0, 0, UK1, 0", "UK4, UK1, 0, 0, UK1, 0",
                      "0, UK2, 0, Victim, 0", "0, UK2, 0, Victim, 0", "0, UK3, 0, Victim, 0", "0, UK3, 0, Victim, 0",
                      "0, UK2, 0, UK1, 0", "0, UK2, 0, UK1, 0", "0, UK3, 0, UK1, 0", "0, UK3, 0, UK1, 0",
                      "UK2, 0, 0, Ref, 0", "UK2, 0, 0, Ref, 0", "UK3, 0, 0, Ref, 0", "UK3, 0, 0, Ref, 0",
                      "UK2, 0, 0, UK1, 0", "UK2, 0, 0, UK1, 0", "UK3, 0, 0, UK1, 0", "UK3, 0, 0, UK1, 0")

    tree_mothers <- c("0, UK1, 0", "0, UK1, 0", "0, Victim, 0", "0, Victim, 0", "UK1, 0, 0", "Ref, 0, 0", "UK1, 0, 0", "Ref, 0, 0",
                      "UK2, UK2, 0, 0", "UK2, UK2, 0, 0", "UK2, UK2, 0, 0", "UK2, UK2, 0, 0",
                      "UK2, UK4, 0, 0, UK2, 0", "UK2, UK4, 0, 0, UK2, 0", "UK2, UK3, 0, 0, UK2, 0", "UK2, UK3, 0, 0, UK2, 0",
                      "UK2, UK4, 0, 0, UK2, 0", "UK2, UK4, 0, 0, UK2, 0", "UK2, UK3, 0, 0, UK2, 0", "UK2, UK3, 0, 0, UK2, 0",
                      "UK4, UK2, 0, 0, UK2, 0", "UK4, UK2, 0, 0, UK2, 0", "UK3, UK2, 0, 0, UK2, 0", "UK3, UK2, 0, 0, UK2, 0",
                      "UK4, UK2, 0, 0, UK2, 0", "UK4, UK2, 0, 0, UK2, 0", "UK3, UK2, 0, 0, UK2, 0", "UK3, UK2, 0, 0, UK2, 0",
                      "0, UK3, 0, UK1, 0", "0, UK3, 0, UK1, 0", "0, UK2, 0, UK1, 0", "0, UK2, 0, UK1, 0",
                      "0, UK3, 0, Victim, 0", "0, UK3, 0, Victim, 0", "0, UK2, 0, Victim, 0", "0, UK2, 0, Victim, 0",
                      "UK3, 0, 0, UK1, 0", "UK3, 0, 0, UK1, 0", "UK2, 0, 0, UK1, 0", "UK2, 0, 0, UK1, 0",
                      "UK3, 0, 0, Ref, 0", "UK3, 0, 0, Ref, 0", "UK2, 0, 0, Ref, 0", "UK2, 0, 0, Ref, 0")

    tree_founders <- c("Yes, No, Yes", "Yes, No, Yes", "Yes, No, Yes", "Yes, No, Yes", "No, Yes, Yes", "No, Yes, Yes", "No, Yes, Yes", "No, Yes, Yes",
                       "No, No, Yes, Yes", "No, No, Yes, Yes", "No, No, Yes, Yes", "No, No, Yes, Yes",
                       "No, No, Yes, Yes, No, Yes", "No, No, Yes, Yes, No, Yes", "No, No, Yes, Yes, No, Yes", "No, No, Yes, Yes, No, Yes",
                       "No, No, Yes, Yes, No, Yes", "No, No, Yes, Yes, No, Yes", "No, No, Yes, Yes, No, Yes", "No, No, Yes, Yes, No, Yes",
                       "No, No, Yes, Yes, No, Yes", "No, No, Yes, Yes, No, Yes", "No, No, Yes, Yes, No, Yes", "No, No, Yes, Yes, No, Yes",
                       "No, No, Yes, Yes, No, Yes", "No, No, Yes, Yes, No, Yes", "No, No, Yes, Yes, No, Yes", "No, No, Yes, Yes, No, Yes",
                       "Yes, No, Yes, No, Yes", "Yes, No, Yes, No, Yes", "Yes, No, Yes, No, Yes", "Yes, No, Yes, No, Yes",
                       "Yes, No, Yes, No, Yes", "Yes, No, Yes, No, Yes", "Yes, No, Yes, No, Yes", "Yes, No, Yes, No, Yes",
                       "No, Yes, Yes, No, Yes", "No, Yes, Yes, No, Yes", "No, Yes, Yes, No, Yes", "No, Yes, Yes, No, Yes",
                       "No, Yes, Yes, No, Yes", "No, Yes, Yes, No, Yes", "No, Yes, Yes, No, Yes", "No, Yes, Yes, No, Yes")

    dt_rel <- data.table(Relationship = name, Victim = victim, Reference = reference,
                         Pr_IBD2 = pibd2, Pr_IBD1 = pibd1, Pr_IBD0 = pibd0,
                         Paternal = paternal, Maternal = maternal,
                         Tree_persons = tree_persons, Tree_sexes = tree_sexes, Tree_fathers = tree_fathers, Tree_mothers = tree_mothers, Tree_founders = tree_founders)

    write.csv(dt_rel, paste0(path_pack, "/extdata/parameters/rel.csv"), row.names = FALSE)
  }

  return(dt_rel)
}

#' create_dt_myu
#'
#' @description The function to create the data.table for mutation rates
#' @param path_pack Package path
#' @param init Whether initial creation or not
create_dt_myu <- function(path_pack, init = TRUE){
  fn_par <- list.files(paste0(path_pack, "/extdata/parameters"))

  if(init && is.element("myu.csv", fn_par)){
    dt_myu <- fread(paste0(path_pack, "/extdata/parameters/myu.csv"))

    col_char <- c("Marker")
    options(warn = -1)
    dt_myu[, (col_char) := lapply(.SD, as.character), .SDcols = col_char]
    options(warn = 0)

    col_numeric <- c("Myu")
    options(warn = -1)
    dt_myu[, (col_numeric) := lapply(.SD, as.numeric), .SDcols = col_numeric]
    options(warn = 0)
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
create_dt_par_auto <- function(path_pack, init = TRUE){
  fn_par <- list.files(paste0(path_pack, "/extdata/parameters"))

  if(init && is.element("par_auto.csv", fn_par)){
    dt_par_auto <- fread(paste0(path_pack, "/extdata/parameters/par_auto.csv"))

    col_char <- c("Parameter")
    options(warn = -1)
    dt_par_auto[, (col_char) := lapply(.SD, as.character), .SDcols = col_char]
    options(warn = 0)

    col_numeric <- c("Value")
    options(warn = -1)
    dt_par_auto[, (col_numeric) := lapply(.SD, as.numeric), .SDcols = col_numeric]
    options(warn = 0)
  }else{
    dt_par_auto <- data.table(Parameter = c("maf"), Value = c(0.001))
    write.csv(dt_par_auto, paste0(path_pack, "/extdata/parameters/par_auto.csv"), row.names = FALSE)
  }

  return(dt_par_auto)
}
