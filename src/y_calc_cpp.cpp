#include "header.h"

/*Obtain alleles from a haplotype*/
// [[Rcpp::export]]
std::vector<double> obtainAl(std::string hap){
  hap.erase(std::remove(hap.begin(), hap.end(), ' '), hap.end());
  if(hap == ""){
    std::vector<double> al(0);
    return(al);
  }else{
    if(hap.find(",") != std::string::npos){
      const char* del = ",";
      std::vector<std::string> alPre = split(hap, del);
      int len = alPre.size();
      std::vector<double> al(len);
      for(int i = 0; i < len; ++i){
        std::string alOne = alPre[i];
        al[i] = std::stod(alOne);
      }
      std::sort(al.begin(), al.end());
      return(al);
    }else{
      std::vector<double> al(1);
      al[0] = std::stod(hap);
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
int calcMuStep(std::vector<double> qAl, std::vector<double> rAl){
  int muStep = 0;

  int qAlSize = qAl.size();
  int rAlSize = rAl.size();
  bool matchQR = true;
  if(qAlSize == rAlSize){
    for(int i = 0; i < qAlSize; ++i){
      bool equalAl = qAl[i] == rAl[i];
      if(!equalAl){
        matchQR = false;
        break;
      }
    }
  }else{
    matchQR = false;
  }

  if(matchQR == false){
    int nqAl = qAl.size();
    int nrAl = rAl.size();
    std::vector<int> diff(nqAl * nrAl, 99);
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
    muStep = *min_element(diff.begin(), diff.end());
  }
  return(muStep);
}

/*Matching query and reference Y haplotypes (testthat)*/
//' @export
// [[Rcpp::export]]
std::vector<std::vector<int>> matchY(std::vector<std::string> qHap, std::vector<std::string> rHap){
  int nL = qHap.size();
  std::vector<std::vector<int>> judgeMat(3, std::vector<int>(nL + 1));
  int sumL0 = 0;
  int sumL1 = 0;
  int maxMuStep = 0;
  for(int i = 0; i < nL; ++i){
    std::string qH = qHap[i];
    std::vector<double> qAl = obtainAl(qH);
    std::string rH = rHap[i];
    std::vector<double> rAl = obtainAl(rH);

    int qAlSize = qAl.size();
    int rAlSize = rAl.size();
    bool matchQR = true;
    if(qAlSize == rAlSize){
      for(int i = 0; i < qAlSize; ++i){
        bool equalAl = qAl[i] == rAl[i];
        if(!equalAl){
          matchQR = false;
          break;
        }
      }
    }else{
      matchQR = false;
    }

    /*mismatch or not*/
    if(matchQR == false){
      /*ignore*/
      std::vector<double> qrAl;
      std::set_union(qAl.begin(), qAl.end(), rAl.begin(), rAl.end(), inserter(qrAl, qrAl.end()));

      std::vector<double> onlyRefAl;
      std::set_difference(rAl.begin(), rAl.end(), qrAl.begin(), qrAl.end(), inserter(onlyRefAl, onlyRefAl.end()));

      if(onlyRefAl.size() == 0){
        judgeMat.at(1).at(i) = 1;
        sumL1 = sumL1 + 1;
      /*not ignore*/
      }else{
        judgeMat.at(0).at(i) = 1;
        sumL0 = sumL0 + 1;
        int muStep = calcMuStep(qAl, rAl);
        judgeMat.at(2).at(i) = muStep;
        if(maxMuStep < muStep){
          maxMuStep = muStep;
        }
      }
    }
  }
  judgeMat.at(0).at(nL) = sumL0;
  judgeMat.at(1).at(nL) = sumL1;
  judgeMat.at(2).at(nL) = maxMuStep;
  return(judgeMat);
}

