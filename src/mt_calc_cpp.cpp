#include "header.h"

// [[Rcpp::export]]
IntegerVector extPosMt(std::string range){
  const char* del_1 = " ";
  const char* del_2 = "-";
  std::vector<std::string> sepRange = split(range, del_1);
  int len = sepRange.size();
  IntegerVector posMt(0);
  for(int i = 0; i < len; ++i){
    std::vector<std::string> sepRange2 = split(sepRange[i], del_2);
    int len2 = sepRange2.size();
    IntegerVector fromto(len2);
    for(int j = 0; j < len2; ++j){
      fromto[j] = str_to_int(sepRange2[j]);
    }
    IntegerVector posMtSub = tousa(fromto[0], fromto[len2 - 1], 1);
    posMt.insert(posMt.end(), posMtSub.begin(), posMtSub.end());
  }
  sort(posMt.begin(), posMt.end());
  posMt.erase(unique(posMt.begin(), posMt.end()), posMt.end());
  return(posMt);
}
