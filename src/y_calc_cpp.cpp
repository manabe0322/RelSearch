#include "header.h"

/*Obtain alleles from a haplotype*/
// [[Rcpp::export]]
NumericVector obtainAl(std::string hap){
  hap.erase(std::remove(hap.begin(), hap.end(), ' '), hap.end());
  if(hap == ""){
    NumericVector al(0);
    return(al);
  }else{
    if(hap.find(",") != std::string::npos){
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
bool is_integer(double x){
  return(std::floor(x) == x);
}

/*Calculate mutational step between query alleles and reference alleles (testthat)*/
// [[Rcpp::export]]
int calcMuStep(NumericVector qAl, NumericVector rAl){
  int muStep = 0;
  if(setequal(qAl, rAl) == false){
    int nqAl = qAl.length();
    int nrAl = rAl.length();
    IntegerVector diff(nqAl * nrAl, 99);
    int pos = 0;
    for(int i = 0; i < nqAl; ++i){
      double q1 = qAl[i];
      for(int j = 0; j < nrAl; ++j){
        double r1 = rAl[j];
        double d = 99;
        if(q1 > r1){
          d = q1 - r1;
        }else if(q1 < r1){
          d = r1 - q1;
        }
        if(is_integer(d)){
          diff[pos] = (int)d;
        }
        pos = pos + 1;
      }
    }
    muStep = min(diff);
  }
  return(muStep);
}

/*Matching query and reference Y haplotypes (testthat)*/
//' @export
// [[Rcpp::export]]
IntegerMatrix matchY(CharacterVector qHap, CharacterVector rHap){
  int nL = qHap.length();
  IntegerMatrix judgeMat(3, nL + 1);
  int sumL0 = 0;
  int sumL1 = 0;
  std::vector<std::string> qHap_cpp = as<std::vector<std::string>>(qHap);
  std::vector<std::string> rHap_cpp = as<std::vector<std::string>>(rHap);
  for(int i = 0; i < nL; ++i){
    std::string qH = qHap_cpp[i];
    NumericVector qAl = obtainAl(qH);
    std::string rH = rHap_cpp[i];
    NumericVector rAl = obtainAl(rH);
    bool matchQR = setequal(qAl, rAl);
    /*mismatch or not*/
    if(matchQR == false){
      /*ignore*/
      NumericVector qrAl = union_(qAl, rAl);
      bool judgeIgnore = setequal(qrAl, rAl);
      if(judgeIgnore){
        judgeMat(1, i) = 1;
        sumL1 = sumL1 + 1;
      /*not ignore*/
      }else{
        judgeMat(0, i) = 1;
        sumL0 = sumL0 + 1;
        judgeMat(2, i) = calcMuStep(qAl, rAl);
      }
    }
  }
  judgeMat(0, nL) = sumL0;
  judgeMat(1, nL) = sumL1;
  judgeMat(2, nL) = max(judgeMat(2, _));
  return(judgeMat);
}

