#include "header.h"

/*Extract positions from range (testthat)*/
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
  }
  std::sort(posMt.begin(), posMt.end());
  posMt.erase(std::unique(posMt.begin(), posMt.end()), posMt.end());
  return(posMt);
}

/*Extract shared positions between query range and reference range (testthat)*/
// [[Rcpp::export]]
std::vector<int> extPosMtQR(std::string qRan, std::string rRan){
  std::vector<int> posMtQ = extPosMt(qRan);
  std::vector<int> posMtR = extPosMt(rRan);
  std::vector<int> posMtQR;
  std::set_intersection(posMtQ.begin(), posMtQ.end(), posMtR.begin(), posMtR.end(), std::inserter(posMtQR, posMtQR.end()));
  return(posMtQR);
}
