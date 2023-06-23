#include "header.h"

/*Extract positions from range (testthat)*/
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

/*Extract shared positions between the range for victim and the range for reference (testthat)*/
//' @export
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

/*Make shared range between query range and reference range (testthat)*/
//' @export
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

//' @export
// [[Rcpp::export]]
std::vector<std::string> match_mt(std::vector<std::string> profile_victim, std::string range_victim, std::vector<std::string> profile_ref, std::string range_ref){
  std::vector<int> pos_mt_vr = extract_pos_mt_vr(range_victim, range_ref);
  std::string n_mismatch_str;
  std::string share_range;
  int share_len = pos_mt_vr.size();
  if(share_len > 0){
    share_range = make_share_range(pos_mt_vr);

    int n_type_v = profile_victim.size();
    std::vector<std::string> v_type_new;
    int count_v = 0;
    for(int i = 0; i < n_type_v; ++i){
      int pos_type_v = extract_integer(profile_victim[i]);
      if(std::count(pos_mt_vr.begin(), pos_mt_vr.end(), pos_type_v)){
        v_type_new[count_v] = profile_victim[i];
        count_v += 1;
      }
    }

    int n_type_r = profile_ref.size();
    std::vector<std::string> r_type_new;
    int count_r = 0;
    for(int i = 0; i < n_type_r; ++i){
      int pos_type_r = extract_integer(profile_ref[i]);
      if(std::count(pos_mt_vr.begin(), pos_mt_vr.end(), pos_type_r)){
        r_type_new[count_r] = profile_ref[i];
        count_r += 1;
      }
    }

    std::vector<std::string> only_v_type;
    std::set_difference(v_type_new.begin(), v_type_new.end(), r_type_new.begin(), r_type_new.end(), inserter(only_v_type, only_v_type.end()));

    std::vector<std::string> only_r_type;
    std::set_difference(r_type_new.begin(), r_type_new.end(), v_type_new.begin(), v_type_new.end(), inserter(only_r_type, only_r_type.end()));

    int n_mismatch = only_v_type.size() + only_r_type.size();
    n_mismatch_str = int_to_str(n_mismatch);

  }else{
    n_mismatch_str = "";
    share_range = "";
  }
  std::string share_len_str = int_to_str(share_len);

  std::vector<std::string> ans;
  ans[0] = n_mismatch_str;
  ans[1] = share_range;
  ans[2] = share_len_str;
  return(ans);
}

//' @export
// [[Rcpp::export]]
std::vector<std::vector<std::string>> match_mt_all(std::vector<std::vector<std::string>> hap_v_mt,
                                                   std::vector<std::vector<std::string>> hap_r_mt,
                                                   std::vector<std::string> range_v_mt,
                                                   std::vector<std::string> range_r_mt){
  int n_v = hap_v_mt.size();
  int n_r = hap_r_mt.size();

  std::vector<std::vector<std::string>> results_mt;

  int count = 0;
  for(int i = 0; i < n_r; ++i){
    std::vector<std::string> profile_ref = hap_r_mt.at(i);
    std::string range_ref = range_r_mt[i];
    for(int j = 0; j < n_v; ++j){
      std::vector<std::string> profile_victim = hap_v_mt.at(j);
      std::string range_victim = range_v_mt[j];
      std::vector<std::string> tmp = match_mt(profile_victim, range_victim, profile_ref, range_ref);

      results_mt.at(count) = tmp;

      /*Update the number of counts for rows*/
      count += 1;
    }
  }
  return(results_mt);
}

