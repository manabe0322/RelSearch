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

/*Extract shared positions between query range and reference range (testthat)*/
//' @export
// [[Rcpp::export]]
std::vector<int> extract_pos_mt_qr(std::string ran_q, std::string ran_r){
  std::vector<int> pos_mt_q = extract_pos_mt(ran_q);
  std::sort(pos_mt_q.begin(), pos_mt_q.end());
  std::vector<int> pos_mt_r = extract_pos_mt(ran_r);
  std::sort(pos_mt_r.begin(), pos_mt_r.end());
  std::vector<int> pos_mt_qr;
  std::set_intersection(pos_mt_q.begin(), pos_mt_q.end(), pos_mt_r.begin(), pos_mt_r.end(), std::inserter(pos_mt_qr, pos_mt_qr.end()));
  return(pos_mt_qr);
}

/*Make shared range between query range and reference range (testthat)*/
//' @export
// [[Rcpp::export]]
std::string make_share_range(std::vector<int> pos_mt_qr){
  int len = pos_mt_qr.size();
  std::vector<int> pos_break;
  pos_break.push_back(-1);
  for(int i = 1; i < len; ++i){
    int sa = pos_mt_qr[i] - pos_mt_qr[i - 1];
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
    int pos_from_int = pos_mt_qr[pos_break_1];
    std::string pos_from_str = int_to_str(pos_from_int);
    int pos_break_2 = pos_break[i + 1];
    int pos_to_int = pos_mt_qr[pos_break_2];
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
