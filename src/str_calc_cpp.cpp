#include <Rcpp.h>
#include <stdio.h>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
IntegerVector tousa(int start, int end, int interval){
  int len = (end - start) / interval + 1;
  IntegerVector vec(len);
  for(int i = 0; i < len; ++i){
    vec[i] = start + i * interval;
  }
  return(vec);
}

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

// [[Rcpp::export]]
NumericVector kinLike(NumericVector qgt, NumericVector rgt, NumericVector af, NumericVector afAl, NumericVector probIBD,
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
      if(existQ1){
        likeH12[0] = c * c * (2 * b * k1 + 2 * a * b * k0);
        likeH12[1] = c * c * 2 * a * b;
      }else{
        likeH12[0] = c * c * (2 * a * k1 + 2 * a * b * k0);
        likeH12[1] = c * c * 2 * a * b;
      }
    }
  }
  return(likeH12);
}

// [[Rcpp::export]]
NumericMatrix makeDummyAf(NumericMatrix dummyGt, NumericVector af, NumericVector afAl){
  int nInitRow = dummyGt.nrow();
  NumericVector afAl_dummy(2 * nInitRow);
  for(int i = 0; i < nInitRow; ++i){
    NumericVector dummyGtOne = dummyGt(i, _);
    afAl_dummy[2 * i] = dummyGtOne[0];
    afAl_dummy[2 * i + 1] = dummyGtOne[1];
  }
  sort(afAl_dummy.begin(), afAl_dummy.end());
  afAl_dummy.erase(unique(afAl_dummy.begin(), afAl_dummy.end()), afAl_dummy.end());

  int len = afAl_dummy.length() - 1;
  IntegerVector posAl_1(len);
  for(int i = 0; i < len; ++i){
    posAl_1[i] = searchPos(afAl, afAl_dummy[i]);
  }
  IntegerVector posAl_all = tousa(0, afAl.length() - 1, 1);
  IntegerVector posAl_2 = setdiff(posAl_all, posAl_1);

  NumericMatrix dummyData(2, len + 1);
  for(int i = 0; i < len; ++i){
    int pos1 = posAl_1[i];
    dummyData(0, i) = af[pos1];
  }
  NumericVector freqQ = af[posAl_2];
  dummyData(0, len) = sum(freqQ);
  dummyData(1, _) = afAl_dummy;

  return(dummyData);
}

// [[Rcpp::export]]
NumericMatrix makeDummyGt(NumericVector qgt, NumericVector rgt){
  sort(rgt.begin(), rgt.end());
  rgt.erase(unique(rgt.begin(), rgt.end()), rgt.end());

  bool existQ = searchPos(rgt, qgt[0]) != rgt.length();

  if(rgt.length() == 1){
    if(existQ){
      NumericMatrix dummyGt(1, 2);
      dummyGt(0, 0) = qgt[0];
      dummyGt(0, 1) = 99;
      return(dummyGt);
    }else{
      NumericMatrix dummyGt(2, 2);
      dummyGt(0, 0) = qgt[0];
      dummyGt(0, 1) = rgt[0];
      dummyGt(1, 0) = qgt[0];
      dummyGt(1, 1) = 99;
      return(dummyGt);
    }
  }else{
    if(existQ){
      NumericMatrix dummyGt(2, 2);
      dummyGt(0, 0) = rgt[0];
      dummyGt(0, 1) = rgt[1];
      dummyGt(1, 0) = qgt[0];
      dummyGt(1, 1) = 99;
      return(dummyGt);
    }else{
      NumericMatrix dummyGt(3, 2);
      dummyGt(0, 0) = qgt[0];
      dummyGt(0, 1) = rgt[0];
      dummyGt(1, 0) = qgt[0];
      dummyGt(1, 1) = rgt[1];
      dummyGt(2, 0) = qgt[0];
      dummyGt(2, 1) = 99;
      return(dummyGt);
    }
  }
}

// [[Rcpp::export]]
NumericVector kinLikeDrop(NumericVector qgt, NumericVector rgt, NumericVector af, NumericVector afAl, NumericVector probIBD,
                          bool consMu, double myu, double ape, double pd){
  /*homozygote (no drop-out)*/
  NumericVector like1 = kinLike(qgt, rgt, af, afAl, probIBD, consMu, myu, ape);
  cout << like1[0] << endl;
  cout << like1[1] << endl;

  /*heterozygote (drop-out)*/
  NumericMatrix dummyGt = makeDummyGt(qgt, rgt);
  NumericMatrix dummyData = makeDummyAf(dummyGt, af, afAl);
  NumericVector af_dummy = dummyData(0, _);
  NumericVector afAl_dummy = dummyData(1, _);

  NumericVector like2(2);
  for(int i = 0; i < dummyGt.nrow(); ++i){
    NumericVector likeH12 = kinLike(dummyGt(i, _), rgt, af_dummy, afAl_dummy, probIBD, consMu, myu, ape);
    cout << likeH12[0] << endl;
    cout << likeH12[1] << endl;
    like2 = like2 + likeH12;
  }
  cout << like2[0] << endl;
  cout << like2[1] << endl;

  NumericVector likeDrop(2);
  likeDrop[0] = (1 - pd) * like1[0] + pd * like2[0];
  likeDrop[1] = (1 - pd) * like1[1] + pd * like2[1];
  return(likeDrop);
}

//' @export
// [[Rcpp::export]]
NumericMatrix calcKinLr(NumericVector query, NumericVector ref, List afList, List afAlList, NumericVector probIBD,
                        bool consMu, NumericVector myuAll, NumericVector apeAll, int dropMethStr, double pd){
  int nL = query.length() / 2;
  NumericMatrix ans(3, nL + 1);
  double cl_H1 = 1;
  double cl_H2 = 1;
  for(int i = 0; i < nL; ++i){
    IntegerVector rangeGt(2);
    rangeGt[0] = 2 * i;
    rangeGt[1] = 2 * i + 1;
    NumericVector qgtPre = query[rangeGt];
    NumericVector qgt = na_omit(qgtPre);
    NumericVector rgtPre = ref[rangeGt];
    NumericVector rgt = na_omit(rgtPre);
    NumericVector af = afList[i];
    NumericVector afAl = afAlList[i];

    double myu = myuAll[i];
    double ape = apeAll[i];

    /*locus drop-out or no information*/
    if(qgt.length() == 0 || rgt.length() == 0){
      ans(0, i) = 1;
      ans(1, i) = 1;
      /*considering drop-out*/
    }else if(dropMethStr != 0){
      NumericVector qgtUni = qgt;
      qgtUni.erase(unique(qgtUni.begin(), qgtUni.end()), qgtUni.end());
      /*qgt : heterozygote*/
      if(qgtUni.length() == 2){
        NumericVector likeH12 = kinLike(qgt, rgt, af, afAl, probIBD, consMu, myu, ape);
        ans(0, i) = likeH12[0];
        ans(1, i) = likeH12[1];
        cl_H1 = cl_H1 * likeH12[0];
        cl_H2 = cl_H2 * likeH12[1];
        /*qgt : homozygote or heterozygote*/
      }else{
        if(dropMethStr == 1){
          /*qgt : heterozygote*/
          if(qgt.length() == 2){
            NumericVector likeH12 = kinLike(qgt, rgt, af, afAl, probIBD, consMu, myu, ape);
            ans(0, i) = likeH12[0];
            ans(1, i) = likeH12[1];
            cl_H1 = cl_H1 * likeH12[0];
            cl_H2 = cl_H2 * likeH12[1];
            /*considering drop-out*/
          }else{
            myu = 0;
            ape = 0;
            NumericVector likeH12 = kinLikeDrop(qgt, rgt, af, afAl, probIBD, consMu, myu, ape, pd);
            ans(0, i) = likeH12[0];
            ans(1, i) = likeH12[1];
            cl_H1 = cl_H1 * likeH12[0];
            cl_H2 = cl_H2 * likeH12[1];
          }
          /*considering drop-out*/
        }else if(dropMethStr == 2){
          myu = 0;
          ape = 0;
          NumericVector likeH12 = kinLikeDrop(qgt, rgt, af, afAl, probIBD, consMu, myu, ape, pd);
          ans(0, i) = likeH12[0];
          ans(1, i) = likeH12[1];
          cl_H1 = cl_H1 * likeH12[0];
          cl_H2 = cl_H2 * likeH12[1];
        }
      }
      /*not considering drop-out*/
    }else{
      NumericVector likeH12 = kinLike(qgt, rgt, af, afAl, probIBD, consMu, myu, ape);
      ans(0, i) = likeH12[0];
      ans(1, i) = likeH12[1];
      cl_H1 = cl_H1 * likeH12[0];
      cl_H2 = cl_H2 * likeH12[1];
    }
  }

  ans(0, nL) = cl_H1;
  ans(1, nL) = cl_H2;
  ans(2, _) = ans(0, _) / ans(1, _);
  return(ans);
}

