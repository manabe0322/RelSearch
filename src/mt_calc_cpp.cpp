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
//' @export
// [[Rcpp::export]]
std::vector<int> extPosMtQR(std::string qRan, std::string rRan){
  std::vector<int> posMtQ = extPosMt(qRan);
  std::vector<int> posMtR = extPosMt(rRan);
  std::vector<int> posMtQR;
  std::set_intersection(posMtQ.begin(), posMtQ.end(), posMtR.begin(), posMtR.end(), std::inserter(posMtQR, posMtQR.end()));
  return(posMtQR);
}

/*Make shared range between query range and reference range (testthat)*/
//' @export
// [[Rcpp::export]]
std::string makeShareRange(std::vector<int> posMtQR){
  int len = posMtQR.size();
  std::vector<int> posBreak;
  posBreak.push_back(-1);
  for(int i = 1; i < len; ++i){
    int sa = posMtQR[i] - posMtQR[i - 1];
    if(sa != 1){
      posBreak.push_back(i - 1);
    }
  }
  posBreak.push_back(len - 1);
  int nSeg = posBreak.size() - 1;
  std::string shareRange = "";
  std::string hyphen = "-";
  std::string space = " ";
  for(int i = 0; i < nSeg; ++i){
    int posBreak_1 = posBreak[i] + 1;
    int posFromInt = posMtQR[posBreak_1];
    std::string posFrom = int_to_str(posFromInt);
    int posBreak_2 = posBreak[i + 1];
    int posToInt = posMtQR[posBreak_2];
    std::string posTo = int_to_str(posToInt);
    if(posBreak_1 == posBreak_2){
      shareRange += posFrom;
    }else{
      shareRange += posFrom;
      shareRange += hyphen;
      shareRange += posTo;
    }
    if(i < nSeg - 1){
      shareRange += space;
    }
  }
  return(shareRange);
}

/*Matching query and reference haplotypes of mtDNA (testthat)*/
/*//' @export
// [[Rcpp::export]]
std::vector<std::string> matchMt(std::string qHap, std::string qRan, std::string rHap, std::string rRan){
  std::vector<int> posMtQR = extPosMtQR(qRan, rRan);
  int lenShare_int = posMtQR.size();
  const char* del_1 = " ";
  std::vector<std::string> qHapSep = split(qHap, del_1);
  std::vector<std::string> rHapSep = split(rHap, del_1);
  if(lenShare_int > 0){
    Function f1("parse_number");
    std::string shareRange = makeShareRange(posMtQR);

    std::vector<double> qHap_1 = f1(qHap);
    std::vector<double> qHap_2 = round(qHap_1, 0);
    int len_q = qHap_2.length();
    std::vector<std::string> qHap_adopt;
    for(int i = 0; i < len_q; ++i){
      int posQ = searchPos_int(posMtQR, qHap_2[i]);
      if(posQ < lenShare_int){
        qHap_adopt.push_back(qHap[i]);
      }
    }
    CharacterVector qHap_rcpp = wrap(qHap_adopt);

    NumericVector rHap_1 = f1(rHap);
    IntegerVector rHap_2 = round(rHap_1, 0);
    int len_r = rHap_2.length();
    std::vector<std::string> rHap_adopt;
    for(int i = 0; i < len_r; ++i){
      int posR = searchPos_int(posMtQR, rHap_2[i]);
      if(posR < lenShare_int){
        rHap_adopt.push_back(rHap[i]);
      }
    }
    CharacterVector rHap_rcpp = wrap(rHap_adopt);

    CharacterVector qrHap_rcpp = union(qHap_rcpp, rHap_rcpp);
    CharacterVector notQHap = setdiff(qrHap_rcpp, qHap_rcpp);
    CharacterVector notRHap = setdiff(qrHap_rcpp, rHap_rcpp);
    int lenNotQ = notQHap.length();
    int lenNotR = notRHap.length();
    int nMis_int = lenNotQ + lenNotR;
    std::string nMis = int_to_str(nMis_int);
    std::string lenShare = int_to_str(lenShare_int);
    std::vector<std::string> result;
    result.push_back(nMis);
    result.push_back(shareRange);
    result.push_back(lenShare);
    return(result);
  }else{
    std::string nMis = "";
    std::string shareRange = "";
    std::string lenShare = int_to_str(lenShare_int);
    result.push_back(nMis);
    result.push_back(shareRange);
    result.push_back(lenShare);
  }
}*/
