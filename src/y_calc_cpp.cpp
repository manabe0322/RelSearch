#include <Rcpp.h>
#include <iostream>
#include <string>
#include <vector>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
std::vector<std::string> split(std::string str, const char* del){
  int first = 0;
  int last = str.find_first_of(del);
  std::vector<std::string> result;

  while(first < str.size()){
    std::string subStr(str, first, last - first);
    result.push_back(subStr);
    first = last + 1;
    last = str.find_first_of(del, first);
    if(last == string::npos){
      last = str.size();
    }
  }

  return(result);
}

// [[Rcpp::export]]
NumericVector obtainAl(std::string hap){
  hap.erase(remove(hap.begin(), hap.end(), ' '), hap.end());
  if(hap == ""){
    NumericVector al(0);
    return(al);
  }else{
    if(hap.find(",") != string::npos){
      const char* del = ",";
      std::vector<std::string> alPre = split(hap, del);
      int len = alPre.size();
      NumericVector al(len);
      for(int i = 0; i < len; ++i){
        std::string alOne = alPre[i];
        al[i] = stod(alOne);
      }
      return(al);
    }else{
      NumericVector al(1);
      al[0] = stod(hap);
      return(al);
    }
  }
}

//' @export
// [[Rcpp::export]]
IntegerMatrix matchY2(CharacterVector qHap, CharacterVector rHap) {
  int nL = qHap.length();
  IntegerMatrix judgeMat(3, nL + 1);
  int sumL = 0;
  std::vector<string> qHap_cpp = as<std::vector<string>>(qHap);
  std::vector<string> rHap_cpp = as<std::vector<string>>(rHap);
  for(int i = 0; i < nL; ++i){
    std::string qH = qHap_cpp[i];
    NumericVector qAl = obtainAl(qH);
    std::string rH = rHap_cpp[i];
    NumericVector rAl = obtainAl(rH);
    bool matchQR = setequal(qAl, rAl);
    /*mismatch or not*/
    if(matchQR == false){
      judgeMat(0, i) = 1;
      sumL = sumL + 1;
      /*ignore or not*/
      if(qAl.length() == 0){
        judgeMat(1, i) = 1;
      }else{

      }
    }
  }
  judgeMat(0, nL) = sumL;
  return(judgeMat);
}

