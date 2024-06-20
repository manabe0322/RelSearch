#' create_dt_criteria
#'
#' @description The function to create the data.table for criteria
#' @param path_pack Package path
#' @param init Whether initial creation or not
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
                              Value = c(100, 2, 10000000, 2, 1, 0))
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

    col_char <- c("Relationship", "Paternal", "Maternal", "Tree_persons", "Tree_sexes", "Tree_fathers", "Tree_mothers", "Tree_founders")
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

    dt_rel <- data.table(Relationship = name,
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

    col_numeric <- c("Paternal_m2",	"Paternal_m1", "Paternal_0", "Paternal_p1", "Paternal_p2", "Maternal_m2", "Maternal_m1", "Maternal_0", "Maternal_p1", "Maternal_p2")
    options(warn = -1)
    dt_myu[, (col_numeric) := lapply(.SD, as.numeric), .SDcols = col_numeric]
    options(warn = 0)
  }else{
    dt_myu <- data.table(Marker = c("D3S1358",
                                    "vWA",
                                    "D16S539",
                                    "CSF1PO",
                                    "TPOX",
                                    "D8S1179",
                                    "D21S11",
                                    "D18S51",
                                    "D2S441",
                                    "D19S433",
                                    "TH01",
                                    "FGA",
                                    "D22S1045",
                                    "D5S818",
                                    "D13S317",
                                    "D7S820",
                                    "SE33",
                                    "D10S1248",
                                    "D1S1656",
                                    "D12S391",
                                    "D2S1338"),
                         Paternal_m2 = c(3.70E-05,
                                         0.000146413,
                                         8.41E-05,
                                         0,
                                         0,
                                         4.82E-05,
                                         3.70E-05,
                                         0.00014735,
                                         5.44E-05,
                                         0,
                                         0,
                                         0.000277755,
                                         5.44E-05,
                                         0,
                                         0,
                                         0,
                                         5.44E-05,
                                         5.44E-05,
                                         5.44E-05,
                                         5.44E-05,
                                         3.70E-05),
                         Paternal_m1 = c(0.001423565,
                                         0.002718279,
                                         0.00093427,
                                         0.002042983,
                                         0.000115566,
                                         0.001301255,
                                         0.000867972,
                                         0.000942863,
                                         0.001166981,
                                         0.000958424,
                                         4.48E-05,
                                         0.001497345,
                                         0.001166981,
                                         0.001420549,
                                         0.001369203,
                                         0.001302502,
                                         0.001166981,
                                         0.001166981,
                                         0.001166981,
                                         0.001166981,
                                         0.000867972),
                         Paternal_0 = c(0.997353441,
                                        0.994905627,
                                        0.997628583,
                                        0.996111085,
                                        0.999697045,
                                        0.997430135,
                                        0.998012667,
                                        0.997459246,
                                        0.997463648,
                                        0.998425005,
                                        0.999895495,
                                        0.995437569,
                                        0.997463648,
                                        0.9968405,
                                        0.997193828,
                                        0.997827141,
                                        0.997463648,
                                        0.997463648,
                                        0.997463648,
                                        0.997463648,
                                        0.998012667),
                         Paternal_p1 = c(0.001148329,
                                         0.002200527,
                                         0.001353042,
                                         0.001845931,
                                         0.000187388,
                                         0.001196466,
                                         0.001082339,
                                         0.001420767,
                                         0.001250749,
                                         0.000452336,
                                         5.97E-05,
                                         0.002707964,
                                         0.001250749,
                                         0.001198,
                                         0.001397392,
                                         0.000870357,
                                         0.001250749,
                                         0.001250749,
                                         0.001250749,
                                         0.001250749,
                                         0.001082339),
                         Paternal_p2 = c(3.77E-05,
                                         2.92E-05,
                                         0,
                                         0,
                                         0,
                                         2.39E-05,
                                         0,
                                         2.98E-05,
                                         6.42E-05,
                                         0.000164235,
                                         0,
                                         7.94E-05,
                                         6.42E-05,
                                         0.000540952,
                                         3.96E-05,
                                         0,
                                         6.42E-05,
                                         6.42E-05,
                                         6.42E-05,
                                         6.42E-05,
                                         0),
                         Maternal_m2 = c(0,
                                         0,
                                         0,
                                         0,
                                         0,
                                         0,
                                         3.60E-05,
                                         2.13E-05,
                                         1.49E-05,
                                         0,
                                         1.70E-05,
                                         0,
                                         1.49E-05,
                                         0,
                                         3.33E-05,
                                         0,
                                         1.49E-05,
                                         1.49E-05,
                                         1.49E-05,
                                         1.49E-05,
                                         3.60E-05),
                         Maternal_m1 = c(0.0001927,
                                         0.000125004,
                                         0.000484253,
                                         0.000212751,
                                         7.51E-05,
                                         0.000133755,
                                         0.000107823,
                                         0.000117496,
                                         0.000270098,
                                         0.000504888,
                                         5.46E-05,
                                         0.000348321,
                                         0.000270098,
                                         0.000258823,
                                         7.08E-05,
                                         6.22E-05,
                                         0.000270098,
                                         0.000270098,
                                         0.000270098,
                                         0.000270098,
                                         0.000107823),
                         Maternal_0 = c(0.99969733,
                                        0.999377764,
                                        0.999411835,
                                        0.99940775,
                                        0.999848864,
                                        0.999702176,
                                        0.999727236,
                                        0.999364128,
                                        0.999494265,
                                        0.99943539,
                                        0.999920126,
                                        0.999358247,
                                        0.999494265,
                                        0.999462401,
                                        0.999657082,
                                        0.999813227,
                                        0.999494265,
                                        0.999494265,
                                        0.999494265,
                                        0.999494265,
                                        0.999727236),
                         Maternal_p1 = c(7.56E-05,
                                         0.000444731,
                                         0.000103912,
                                         0.0003795,
                                         7.61E-05,
                                         0.000141848,
                                         0.000128943,
                                         0.000475601,
                                         0.000212047,
                                         5.97E-05,
                                         8.29E-06,
                                         0.000293432,
                                         0.000212047,
                                         0.000278776,
                                         0.000238821,
                                         0.000124612,
                                         0.000212047,
                                         0.000212047,
                                         0.000212047,
                                         0.000212047,
                                         0.000128943),
                         Maternal_p2 = c(3.44E-05,
                                         5.25E-05,
                                         0,
                                         0,
                                         0,
                                         2.22E-05,
                                         0,
                                         2.15E-05,
                                         8.71E-06,
                                         0,
                                         0,
                                         0,
                                         8.71E-06,
                                         0,
                                         0,
                                         0,
                                         8.71E-06,
                                         8.71E-06,
                                         8.71E-06,
                                         8.71E-06,
                                         0))
    write.csv(dt_myu, paste0(path_pack, "/extdata/parameters/myu.csv"), row.names = FALSE)
  }

  return(dt_myu)
}

#' create_dt_data_manage
#'
#' @description The function to create the data.table for setting of data management
#' @param path_pack Package path
#' @param init Whether initial creation or not
create_dt_data_manage <- function(path_pack, init = TRUE){
  fn_par <- list.files(paste0(path_pack, "/extdata/parameters"))

  if(init && is.element("data_manage.csv", fn_par)){
    dt_data_manage <- fread(paste0(path_pack, "/extdata/parameters/data_manage.csv"))

    col_char <- c("Parameter")
    options(warn = -1)
    dt_data_manage[, (col_char) := lapply(.SD, as.character), .SDcols = col_char]
    options(warn = 0)

    col_numeric <- c("Value")
    options(warn = -1)
    dt_data_manage[, (col_numeric) := lapply(.SD, as.numeric), .SDcols = col_numeric]
    options(warn = 0)
  }else{
    dt_data_manage <- data.table(Parameter = c("keep_min_lr", "max_data_displayed"),
                               Value = c(1, 100))
    write.csv(dt_data_manage, paste0(path_pack, "/extdata/parameters/data_manage.csv"), row.names = FALSE)
  }

  return(dt_data_manage)
}
