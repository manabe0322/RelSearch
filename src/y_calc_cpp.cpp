#include <Rcpp.h>
using namespace Rcpp;

//' @export
// [[Rcpp::export]]
IntegerMatrix matchY2(CharacterVector qHap, CharacterVector rHap) {
  int nL = qHap.length();
  IntegerMatrix judgeMat(3, nL + 1);
  for(int i = 0; i < nL; ++i){
    String qAl(qHap[i]);
    String rAl(rHap[i]);
    bool matchQR = qAl == rAl;
    if(matchQR == true){
      judgeMat(0, i) = 1;
    }
  }
  return(judgeMat);
}
