#include "header.h"

/*##############################################################
# The function to extract the integer positions from the range #
##############################################################*/

// [[Rcpp::export]]
std::vector<int> extract_pos_mt(std::string range){
  const char* del_1 = " ";
  const char* del_2 = "-";
  std::vector<std::string> sep_range = split(range, del_1);
  int len = sep_range.size();
  std::vector<int> pos_mt(0);

  for(int i = 0; i < len; ++i){
    std::vector<std::string> sep_range2 = split(sep_range[i], del_2);

    int len2 = sep_range2.size();
    std::vector<int> fromto(len2);
    for(int j = 0; j < len2; ++j){
      fromto[j] = str_to_int(sep_range2[j]);
    }

    std::vector<int> pos_mt_sub = tousa(fromto[0], fromto[len2 - 1], 1);
    pos_mt.insert(pos_mt.end(), pos_mt_sub.begin(), pos_mt_sub.end());
  }

  return(pos_mt);
}


/*###############################################################################
# The function to extract shared positions between the victim and the reference #
###############################################################################*/

// [[Rcpp::export]]
std::vector<int> extract_pos_mt_vr(std::string range_victim, std::string range_ref){
  std::vector<int> pos_mt_v = extract_pos_mt(range_victim);
  std::sort(pos_mt_v.begin(), pos_mt_v.end());

  std::vector<int> pos_mt_r = extract_pos_mt(range_ref);
  std::sort(pos_mt_r.begin(), pos_mt_r.end());

  std::vector<int> pos_mt_vr;
  std::set_intersection(pos_mt_v.begin(), pos_mt_v.end(), pos_mt_r.begin(), pos_mt_r.end(), std::inserter(pos_mt_vr, pos_mt_vr.end()));

  return(pos_mt_vr);
}


/*############################################################################
# The function to make shared range between victim range and reference range #
############################################################################*/

// [[Rcpp::export]]
std::string make_share_range(std::vector<int> pos_mt_vr){
  int len = pos_mt_vr.size();

  std::vector<int> pos_break;
  pos_break.push_back(-1);
  for(int i = 1; i < len; ++i){
    int sa = pos_mt_vr[i] - pos_mt_vr[i - 1];
    if(sa != 1){
      pos_break.push_back(i - 1);
    }
  }
  pos_break.push_back(len - 1);

  int n_seg = pos_break.size() - 1;

  std::string share_range = "";
  std::string hyphen = "-";
  std::string space = " ";

  for(int i = 0; i < n_seg; ++i){
    int pos_break_1 = pos_break[i] + 1;
    int pos_from_int = pos_mt_vr[pos_break_1];
    std::string pos_from_str = int_to_str(pos_from_int);

    int pos_break_2 = pos_break[i + 1];
    int pos_to_int = pos_mt_vr[pos_break_2];
    std::string pos_to_str = int_to_str(pos_to_int);

    if(pos_break_1 == pos_break_2){
      share_range += pos_from_str;
    }else{
      share_range += pos_from_str;
      share_range += hyphen;
      share_range += pos_to_str;
    }

    if(i < n_seg - 1){
      share_range += space;
    }
  }

  return(share_range);
}


/*#################################################
# The function to analyze mtDNA data of one pairs #
#################################################*/

// [[Rcpp::export]]
std::vector<std::string> match_mt(std::vector<std::string> prof_victim, std::string range_victim, std::vector<std::string> prof_ref, std::string range_ref){
  std::vector<int> pos_mt_vr = extract_pos_mt_vr(range_victim, range_ref);
  std::string n_mismatch_str;
  std::string share_range;
  int share_len = pos_mt_vr.size();

  if(share_len > 0){
    share_range = make_share_range(pos_mt_vr);

    /* Investigate the victim's type within the shared range */
    int n_type_v = prof_victim.size();
    std::vector<std::string> v_type_new;
    for(int i = 0; i < n_type_v; ++i){
      int pos_type_v = extract_integer(prof_victim[i]);
      if(std::count(pos_mt_vr.begin(), pos_mt_vr.end(), pos_type_v)){
        v_type_new.push_back(prof_victim[i]);
      }
    }

    /* Investigate the reference's type within the shared range */
    int n_type_r = prof_ref.size();
    std::vector<std::string> r_type_new;
    for(int i = 0; i < n_type_r; ++i){
      int pos_type_r = extract_integer(prof_ref[i]);
      if(std::count(pos_mt_vr.begin(), pos_mt_vr.end(), pos_type_r)){
        r_type_new.push_back(prof_ref[i]);
      }
    }

    /* Investigate the victim's specific type within the shared range */
    std::vector<std::string> only_v_type;
    std::set_difference(v_type_new.begin(), v_type_new.end(), r_type_new.begin(), r_type_new.end(), inserter(only_v_type, only_v_type.end()));

    /* Investigate the reference's specific type within the shared range */
    std::vector<std::string> only_r_type;
    std::set_difference(r_type_new.begin(), r_type_new.end(), v_type_new.begin(), v_type_new.end(), inserter(only_r_type, only_r_type.end()));

    /* Investigate the number of mismatches */
    int n_mismatch = only_v_type.size() + only_r_type.size();
    n_mismatch_str = int_to_str(n_mismatch);

  }else{
    n_mismatch_str = "";
    share_range = "";
  }

  std::string share_len_str = int_to_str(share_len);

  std::vector<std::string> ans(3);
  ans[0] = n_mismatch_str;
  ans[1] = share_range;
  ans[2] = share_len_str;
  return(ans);
}


/*#################################################
# The function to analyze mtDNA data of all pairs #
#################################################*/

// [[Rcpp::export]]
std::vector<std::vector<std::string>> match_mt_all(std::vector<std::vector<std::string>> hap_v_mt,
                                                   std::vector<std::vector<std::string>> hap_r_mt,
                                                   std::vector<std::string> range_v_mt,
                                                   std::vector<std::string> range_r_mt){
  /* Call the R function "message" */
  Function message("message");

  int n_v = hap_v_mt.size();
  int n_r = hap_r_mt.size();
  int n_vr = n_v * n_r;

  std::vector<std::vector<std::string>> result_mt(std::vector<std::vector<std::string>>(n_vr, std::vector<std::string>(3)));

  int counter_base = n_vr * 0.001;
  int counter = counter_base;

  for(int i = 0; i < n_r; ++i){
    std::vector<std::string> prof_ref = hap_r_mt.at(i);
    std::string range_ref = range_r_mt[i];

    for(int j = 0; j < n_v; ++j){
      std::vector<std::string> prof_victim = hap_v_mt.at(j);
      std::string range_victim = range_v_mt[j];

      std::vector<std::string> ans = match_mt(prof_victim, range_victim, prof_ref, range_ref);

      int pos = n_v * i + j;
      result_mt.at(pos) = ans;

      /* Display a message to the console to update the progress bar */
      if(pos >= counter){
        std::string txt_console = "mtDNA_Victim-Reference_ : ";
        txt_console += int_to_str(pos);
        message('\r', txt_console, _["appendLF"] = false);
        counter += counter_base;
      }
    }
  }

  return(result_mt);
}
