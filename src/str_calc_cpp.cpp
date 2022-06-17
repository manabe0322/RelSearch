#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

//' @export
// [[Rcpp::export]]
NumericVector kinLike2(NumericVector qgt, NumericVector rgt, NumericVector af, NumericVector afAl, NumericVector probIBD,
                       bool consMu, double myu, double ape) {
  NumericVector likeH12(2);

  sort(qgt.begin(), qgt.end());
  qgt.erase(unique(qgt.begin(), qgt.end()), qgt.end());
  sort(rgt.begin(), rgt.end());
  rgt.erase(unique(rgt.begin(), rgt.end()), rgt.end());

  double k2 = probIBD[0];
  double k1 = probIBD[1];
  double k0 = probIBD[2];

  if(qgt.length() == 1){
    auto posQ = find(afAl.begin(), afAl.end(), qgt[0]);
    double a = af[posQ];
    double b = af[posQ];
  }else{
    auto posQ1 = find(afAl.begin(), afAl.end(), qgt[0]);
    double a = af[posQ1];
    auto posQ2 = find(afAl.begin(), afAl.end(), qgt[1]);
    double b = af[posQ2];
  }
  if(rgt.length() == 1){
    auto posR = find(afAl.begin(), afAl.end(), rgt[0]);
    double c = af[posR];
    double d = af[posR];
  }else{
    auto posR1 = find(afAl.begin(), afAl.end(), rgt[0]);
    double c = af[posR1];
    auto posR2 = find(afAl.begin(), afAl.end(), rgt[1]);
    double d = af[posR2];
  }

  NumericVector uniQRgt = union_(qgt, rgt);
  bool existQ1 = find(rgt.begin(), rgt.end(), qgt[0]) != rgt.end();
  if(qgt.length() == 2){
    bool existQ2 = find(rgt.begin(), rgt.end(), qgt[1]) != rgt.end();
  }else{
    bool existQ2 = find(rgt.begin(), rgt.end(), qgt[0]) != rgt.end();
  }

  if(!existQ1 && !existQ2){
    if(consMu){
      likeH12[0] = myu;
      likeH12[1] = ape;
    }else{
      if(qgt.length() == 1){
        if(rgt.length() == 1){
          likeH12[0] = a * a * c * c * k0;
          likeH12[1] = a * a * c * c;
        }else{
          likeH12[0] = a * a * 2 * c * d * k0;
          likeH12[1] = a * a * 2 * c * d;
        }
      }else{
        if(rgt.length() == 1){
          likeH12[0] = 2 * a * b * c * c * k0;
          likeH12[1] = 2 * a * b * c * c;
        }else{
          likeH12[0] = 2 * a * b * 2 * c * d * k0;
          likeH12[1] = 2 * a * b * 2 * c * d;
        }
      }
    }
  }else if(setequal(qgt, rgt)){
    if(qgt.length() == 1){
      likeH12[0] = c * c * (k2 + 2 * a * k1 + a * a * k0);
      likeH12[1] = c * c * a * a;
    }else{
      likeH12[0] = 2 * c * d * (k2 + a * k1 + b * k1 + 2 * a * b * k0);
      likeH12[1] = 2 * c * d * 2 * a * b;
    }
  }else if(uniQRgt.length() == 3){
    if(existQ1){
      likeH12[0] = 2 * c * d * (b * k1 + 2 * a * b * k0);
      likeH12[1] = 2 * c * d * 2 * a * b;
    }else{
      likeH12[0] = 2 * c * d * (a * k1 + 2 * a * b * k0);
      likeH12[1] = 2 * c * d * 2 * a * b;
    }
  }else{
    if(qgt.length() == 1){
      likeH12[0] = 2 * c * d * (a * k1 + a * a * k0);
      likeH12[1] = 2 * c * d * a * a;
    }else{
      likeH12[0] = c * c * (2 * a * k1 + 2 * a * b * k0);
      likeH12[1] = c * c * 2 * a * b;
    }
  }
  return(likeH12);
}
