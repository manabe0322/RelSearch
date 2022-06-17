#include <Rcpp.h>
#include <stdio.h>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
int searchPos(NumericVector vec, double target){
  int len = vec.length();
  int pos = len;
  for(int i = 0; i < len; ++i){
    if(vec[i] == target){
      pos = i;
      break;
    }
  }
  return(pos);
}

//' @export
// [[Rcpp::export]]
NumericVector kinLike2(NumericVector qgt, NumericVector rgt, NumericVector af, NumericVector afAl, NumericVector probIBD,
                       bool consMu, double myu, double ape){
  NumericVector likeH12(2);

  sort(qgt.begin(), qgt.end());
  qgt.erase(unique(qgt.begin(), qgt.end()), qgt.end());
  sort(rgt.begin(), rgt.end());
  rgt.erase(unique(rgt.begin(), rgt.end()), rgt.end());

  double k2 = probIBD[0];
  double k1 = probIBD[1] / 2;
  double k0 = probIBD[2];

  double a, b, c, d;
  if(qgt.length() == 1){
    int posQ = searchPos(afAl, qgt[0]);
    a = af[posQ];
    b = af[posQ];
  }else{
    int posQ1 = searchPos(afAl, qgt[0]);
    a = af[posQ1];
    int posQ2 = searchPos(afAl, qgt[1]);
    b = af[posQ2];
  }
  if(rgt.length() == 1){
    int posR = searchPos(afAl, rgt[0]);
    c = af[posR];
    d = af[posR];
  }else{
    int posR1 = searchPos(afAl, rgt[0]);
    c = af[posR1];
    int posR2 = searchPos(afAl, rgt[1]);
    d = af[posR2];
  }

  NumericVector uniQRgt = union_(qgt, rgt);
  bool existQ1 = searchPos(rgt, qgt[0]) != rgt.length();
  bool existQ2;
  if(qgt.length() == 2){
    existQ2 = searchPos(rgt, qgt[1]) != rgt.length();
  }else{
    existQ2 = searchPos(rgt, qgt[0]) != rgt.length();
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
