#include "header.h"

// [[Rcpp::export]]
std::vector<int> extPosMt(std::string range){
  const char* del_1 = " ";
  const char* del_2 = "-";
  std::vector<std::string> sepRange = split(range, del_1);
  int len = sepRange.size();
  std::vector<int> posMt(0);
  for(int i = 0; i < len; ++i){
    std::vector<std::string> sepRange2 = split(sepRange[i], del_2);
    int len2 = sepRange2.size();
    std::vector<int> fromto(len2);
    for(int j = 0; j < len2; ++j){
      fromto[j] = str_to_int(sepRange2[j]);
    }
    std::vector<int> posMtSub = tousa(fromto[0], fromto[len2 - 1], 1);
    posMt.insert(posMt.end(), posMtSub.begin(), posMtSub.end());
/*    if(len2 == 1){
      posMt.insert(posMt.end(), fromto.begin(), fromto.end());
    }else if(len2 > 1){
      std::vector<int> posMtSub = tousa(fromto[0], fromto[len2 - 1], 1);
      posMt.insert(posMt.end(), posMtSub.begin(), posMtSub.end());
    }*/
  }
  std::sort(posMt.begin(), posMt.end());
  posMt.erase(std::unique(posMt.begin(), posMt.end()), posMt.end());
  return(posMt);
}

// [[Rcpp::export]]
std::vector<int> testPosMt(std::vector<int> fromto){
  std::vector<int> posMt(0);
  int len2 = fromto.size();
  std::vector<int> posMtSub = tousa(fromto[0], fromto[len2 - 1], 1);
  posMt.insert(posMt.end(), posMtSub.begin(), posMtSub.end());
  return(posMt);
}
