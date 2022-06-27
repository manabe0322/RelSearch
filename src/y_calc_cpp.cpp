#include <Rcpp.h>
#include <iostream>
#include <string>
#include <vector>
#include <math.h>
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

// [[Rcpp::export]]
int calcMuStep(NumericVector qAl, NumericVector rAl){
  int nqAl = qAl.length();
  int nrAl = rAl.length();
  IntegerVector diff(nqAl * nrAl, 99);
  int pos = 0;
  for(int i = 0; i < nqAl; ++i){
    double q1 = qAl[i];
    double remQ = fmod(q1, 1);
    if(remQ == 0){
      int q2 = (int)q1;
      for(int j = 0; i < nrAl; ++j){
        double r1 = rAl[j];
        double remR = fmod(r1, 1);
        if(remR == 0){
          int r2 = (int)r1;
          if(q2 > r2){
            diff[pos] = q2 - r2;
          }else if(q2 < r2){
            diff[pos] = r2 - q2;
          }
        }
        pos = pos + 1;
      }
    }else{
      pos = pos + nrAl;
    }
  }
  int muStep = min(diff);
  return(muStep);
}

//' @export
// [[Rcpp::export]]
IntegerMatrix matchY(CharacterVector qHap, CharacterVector rHap){
  int nL = qHap.length();
  IntegerMatrix judgeMat(3, nL + 1);
  int sumL0 = 0;
  int sumL1 = 0;
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
      sumL0 = sumL0 + 1;
      /*ignore or not*/
      if(qAl.length() == 0){
        judgeMat(1, i) = 1;
        sumL1 = sumL1 + 1;
      }else{
        judgeMat(2, i) = calcMuStep(qAl, rAl);
      }
    }
  }
  judgeMat(0, nL) = sumL0;
  judgeMat(1, nL) = sumL1;
  return(judgeMat);
}

