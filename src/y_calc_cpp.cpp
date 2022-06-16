#include <Rcpp.h>
using namespace Rcpp;

//' @export
// [[Rcpp::export]]
IntegerMatrix matchY2(CharacterVector qHap, CharacterVector rHap) {
  int nL = qHap.length();
  IntegerMatrix judgeMat(3, nL + 1);
  int sumL = 0;
  for(int i = 0; i < nL; ++i){
    String qAl(qHap[i]);
    String rAl(rHap[i]);
    bool matchQR = qAl == rAl;
    if(matchQR == false){
      judgeMat(0, i) = 1;
      sumL = sumL + 1;
    }
  }
  judgeMat(0, nL) = sumL;
  return(judgeMat);
}
