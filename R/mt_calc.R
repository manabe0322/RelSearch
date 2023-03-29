# Matching query and reference haplotypes of mtDNA (testthat)
match_mt <- function(query, ran_q, ref, ran_r){
  pos_mt_qr <- extract_pos_mt_qr(ran_q, ran_r)
  share_len <- length(pos_mt_qr)
  q_type <- strsplit(query, " ")[[1]]
  r_type <- strsplit(ref, " ")[[1]]
  if(share_len > 0){
    share_range <- make_share_range(pos_mt_qr)
    q_type <- q_type[is.element(round(parse_number(q_type), 0), pos_mt_qr)]
    r_type <- r_type[is.element(round(parse_number(r_type), 0), pos_mt_qr)]
    qr_type <- union(q_type, r_type)
    n_mm <- length(setdiff(qr_type, q_type)) + length(setdiff(qr_type, r_type))
  }else{
    n_mm <- ""
    share_range <- ""
  }
  return(c(n_mm, share_range, share_len))
}
