#include <Rcpp.h>
#include <string>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
IntegerMatrix matchY2(CharacterVector qHap, CharacterVector rHap) {
  int nL = qHap.length();
  IntegerMatrix judgeMat(3, nL + 1);
  for(int i = 0; i < nL; ++i){
    string qAl = qHap[i];
    string rAl = rHap[i];
    bool matchQR = qAl == rAl;
    if(matchQR == true){
      judgeMat(0, i) = 1;
    }
  }
  return(judgeMat);
}
