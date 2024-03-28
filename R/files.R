#' change_data_type_auto
#'
#' @description The function to change data type for the autosomal STR database
#' @param dt The data.table of the autosomal STR database
change_data_type_auto <- function(dt){
  col_init <- names(dt)

  col_tmp <- col_init
  pos_mk <- which(!is.element(col_init, c("SampleName", "Relationship")))
  col_tmp[pos_mk] <- paste0("Col", 1:length(pos_mk))
  names(dt) <- col_tmp

  col_numeric <- col_tmp[pos_mk]
  options(warn = -1)
  dt[, (col_numeric) := lapply(.SD, as.numeric), .SDcols = col_numeric]
  options(warn = 0)

  col_char <- col_tmp[which(is.element(col_init, c("SampleName", "Relationship")))]
  options(warn = -1)
  dt[, (col_char) := lapply(.SD, as.character), .SDcols = col_char]
  options(warn = 0)

  names(dt) <- col_init
  return(dt)
}

#' change_data_type_af
#'
#' @description The function to change data type for the allele frequency database
#' @param dt The data.table of the allele frequency database
change_data_type_af <- function(dt){
  col_numeric <- names(dt)
  options(warn = -1)
  dt[, (col_numeric) := lapply(.SD, as.numeric), .SDcols = col_numeric]
  options(warn = 0)

  return(dt)
}

#' change_data_type_y
#'
#' @description The function to change data type for the Y-STR database
#' @param dt The data.table of the Y-STR database
change_data_type_y <- function(dt){
  col_char <- names(dt)

  pos_not_mk <- which(is.element(col_char, c("SampleName", "Relationship")))
  pos_mk <- which(!is.element(col_char, c("SampleName", "Relationship")))
  dt_left <- dt[, pos_not_mk, with = FALSE]
  dt_right <- dt[, pos_mk, with = FALSE]
  dt_right <- mutate_all(dt_right, ~ifelse(.=="-", "",.))
  dt <- cbind(dt_left, dt_right)

  options(warn = -1)
  dt[, (col_char) := lapply(.SD, as.character), .SDcols = col_char]
  options(warn = 0)

  return(dt)
}

#' change_data_type_mt
#'
#' @description The function to change data type for the mtDNA database
#' @param dt The data.table of the mtDNA database
change_data_type_mt <- function(dt){
  col_char <- names(dt)

  pos_left <- which(is.element(col_char, c("SampleName", "Relationship")))
  pos_right <- which(!is.element(col_char, c("SampleName", "Relationship")))
  dt_left <- dt[, pos_left, with = FALSE]
  dt_right <- dt[, pos_right, with = FALSE]
  dt_right <- mutate_all(dt_right, ~ifelse(.=="-", "",.))
  dt <- cbind(dt_left, dt_right)

  options(warn = -1)
  dt[, (col_char) := lapply(.SD, as.character), .SDcols = col_char]
  options(warn = 0)

  return(dt)
}
