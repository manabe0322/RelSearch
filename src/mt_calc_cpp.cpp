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
  return(posMt);
}

/*Extract positions from range (testthat)*/
// [[Rcpp::export]]
std::vector<int> testPosMt(std::string range){
  const char* del_1 = " ";
  const char* del_2 = "-";
  std::vector<std::string> sepRange = split(range, del_1);
  int len = sepRange.size();
  std::vector<int> posMt(0);
  std::vector<std::string> sepRange2 = split(sepRange[0], del_2);
  int len2 = sepRange2.size();
  std::vector<int> fromto(len2);
  for(int j = 0; j < len2; ++j){
    fromto[j] = str_to_int(sepRange2[j]);
  }
  return(fromto);
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

/*Make shared range between query range and reference range (testthat)*/
/*// [[Rcpp::export]]
std::string makeShareRange(std::vector<int> posMtQR){
  int len = posMtQR.size();
  std::vector<int> posBreak;
  posBreak.push_back(0);
  for(int i = 2; i < len; ++i){
    int sa = posMtQR[i] - posMtQR[i - 1];
    if(sa != 1){
      posBreak.push_back(i - 1);
    }
  }
  posBreak.push_back(len - 1);
  int nSeg = posBreak.size() - 1;
  std::vector<std::string> seg;
  for(int i = 0; i < nSeg; ++i){

  }

}*/
