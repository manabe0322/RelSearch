#include "header.h"

/*General calculation of likelihoods for pairwise kinship analysis (testthat)*/
// [[Rcpp::export]]
std::vector<double> kinLike(std::vector<double> qgt, std::vector<double> rgt, std::vector<double> af, std::vector<double> afAl,
                            std::vector<double> probIBD, bool consMu, double myu, double ape){
  std::vector<double> likeH12(2);

  std::sort(qgt.begin(), qgt.end());
  qgt.erase(std::unique(qgt.begin(), qgt.end()), qgt.end());
  std::sort(rgt.begin(), rgt.end());
  rgt.erase(std::unique(rgt.begin(), rgt.end()), rgt.end());

  double k2 = probIBD[0];
  double k1 = probIBD[1] / 2;
  double k0 = probIBD[2];

  int qgtSize = qgt.size();
  int rgtSize = rgt.size();

  double a, b, c, d;
  if(qgtSize == 1){
    int posQ = searchPos_double(afAl, qgt[0]);
    a = af[posQ];
    b = af[posQ];
  }else{
    int posQ1 = searchPos_double(afAl, qgt[0]);
    a = af[posQ1];
    int posQ2 = searchPos_double(afAl, qgt[1]);
    b = af[posQ2];
  }
  if(rgtSize == 1){
    int posR = searchPos_double(afAl, rgt[0]);
    c = af[posR];
    d = af[posR];
  }else{
    int posR1 = searchPos_double(afAl, rgt[0]);
    c = af[posR1];
    int posR2 = searchPos_double(afAl, rgt[1]);
    d = af[posR2];
  }

  std::vector<double> uniQRgt;
  std::set_union(qgt.begin(), qgt.end(), rgt.begin(), rgt.end(), inserter(uniQRgt, uniQRgt.end()));
  bool existQ1 = searchPos_double(rgt, qgt[0]) != rgtSize;
  bool existQ2;
  if(qgtSize == 2){
    existQ2 = searchPos_double(rgt, qgt[1]) != rgtSize;
  }else{
    existQ2 = searchPos_double(rgt, qgt[0]) != rgtSize;
  }
  bool equalQR = true;
  if(qgtSize == rgtSize){
    for(int i = 0; i < qgtSize; ++i){
      bool equalAl = qgt[i] == rgt[i];
      if(!equalAl){
        equalQR = false;
        break;
      }
    }
  }else{
    equalQR = false;
  }
/*  bool equalQR = std::equal(qgt.begin(), qgt.end(), rgt.begin(), rgt.end());*/

  if(!existQ1 && !existQ2){
    if(consMu){
      likeH12[0] = myu;
      likeH12[1] = ape;
    }else{
      if(qgtSize == 1){
        if(rgtSize == 1){
          likeH12[0] = a * a * c * c * k0;
          likeH12[1] = a * a * c * c;
        }else{
          likeH12[0] = a * a * 2 * c * d * k0;
          likeH12[1] = a * a * 2 * c * d;
        }
      }else{
        if(rgtSize == 1){
          likeH12[0] = 2 * a * b * c * c * k0;
          likeH12[1] = 2 * a * b * c * c;
        }else{
          likeH12[0] = 2 * a * b * 2 * c * d * k0;
          likeH12[1] = 2 * a * b * 2 * c * d;
        }
      }
    }
  }else if(equalQR){
    if(qgtSize == 1){
      likeH12[0] = c * c * (k2 + 2 * a * k1 + a * a * k0);
      likeH12[1] = c * c * a * a;
    }else{
      likeH12[0] = 2 * c * d * (k2 + a * k1 + b * k1 + 2 * a * b * k0);
      likeH12[1] = 2 * c * d * 2 * a * b;
    }
  }else if(uniQRgt.size() == 3){
    if(existQ1){
      likeH12[0] = 2 * c * d * (b * k1 + 2 * a * b * k0);
      likeH12[1] = 2 * c * d * 2 * a * b;
    }else{
      likeH12[0] = 2 * c * d * (a * k1 + 2 * a * b * k0);
      likeH12[1] = 2 * c * d * 2 * a * b;
    }
  }else{
    if(qgtSize == 1){
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


/*Make allele frequencies for dummy genotypes (testthat)*/
// [[Rcpp::export]]
std::vector<std::vector<double>> makeDummyAf(std::vector<std::vector<double>> dummyGt, std::vector<double> af, std::vector<double> afAl){
  int nInitRow = dummyGt.size();
  std::vector<double> afAl_dummy(2 * nInitRow);
  for(int i = 0; i < nInitRow; ++i){
    afAl_dummy[2 * i] = dummyGt.at(i).at(0);
    afAl_dummy[2 * i + 1] = dummyGt.at(i).at(1);
  }
  std::sort(afAl_dummy.begin(), afAl_dummy.end());
  afAl_dummy.erase(std::unique(afAl_dummy.begin(), afAl_dummy.end()), afAl_dummy.end());

  int len = afAl_dummy.size() - 1;
  std::vector<int> posAl_1(len);
  for(int i = 0; i < len; ++i){
    posAl_1[i] = searchPos_double(afAl, afAl_dummy[i]);
  }
  std::vector<int> posAl_all = tousa(0, afAl.size() - 1, 1);
  std::vector<int> posAl_2;
  std::set_difference(posAl_all.begin(), posAl_all.end(), posAl_1.begin(), posAl_1.end(), inserter(posAl_2, posAl_2.end()));

  std::vector<std::vector<double>> dummyData(2, std::vector<double>(len + 1));
  for(int i = 0; i < len; ++i){
    int pos1 = posAl_1[i];
    dummyData.at(0).at(i) = af[pos1];
  }

  int len2 = posAl_2.size();
  std::vector<double> freqQ(len2);
  for(int i = 0; i < len2; ++i){
    int pos2 = posAl_2[i];
    freqQ[i] = af[pos2];
  }
  dummyData.at(0).at(len) = std::accumulate(freqQ.begin(), freqQ.end(), 0.0);
  dummyData.at(1) = afAl_dummy;

  return(dummyData);
}

/*Determine dummy genotypes considering a drop-out allele (testthat)*/
// [[Rcpp::export]]
std::vector<std::vector<double>> makeDummyGt(std::vector<double> qgt, std::vector<double> rgt){
  std::sort(rgt.begin(), rgt.end());
  rgt.erase(std::unique(rgt.begin(), rgt.end()), rgt.end());
  int rgtSize = rgt.size();

  bool existQ = searchPos_double(rgt, qgt[0]) != rgtSize;

  if(rgtSize == 1){
    if(existQ){
      std::vector<std::vector<double>> dummyGt(1, std::vector<double>(2));
      dummyGt.at(0).at(0) = qgt[0];
      dummyGt.at(0).at(1) = 99;
      return(dummyGt);
    }else{
      std::vector<std::vector<double>> dummyGt(2, std::vector<double>(2));
      dummyGt.at(0).at(0) = qgt[0];
      dummyGt.at(0).at(1) = rgt[0];
      dummyGt.at(1).at(0) = qgt[0];
      dummyGt.at(1).at(1) = 99;
      return(dummyGt);
    }
  }else{
    if(existQ){
      std::vector<std::vector<double>> dummyGt(2, std::vector<double>(2));
      dummyGt.at(0).at(0) = rgt[0];
      dummyGt.at(0).at(1) = rgt[1];
      dummyGt.at(1).at(0) = qgt[0];
      dummyGt.at(1).at(1) = 99;
      return(dummyGt);
    }else{
      std::vector<std::vector<double>> dummyGt(3, std::vector<double>(2));
      dummyGt.at(0).at(0) = qgt[0];
      dummyGt.at(0).at(1) = rgt[0];
      dummyGt.at(1).at(0) = qgt[0];
      dummyGt.at(1).at(1) = rgt[1];
      dummyGt.at(2).at(0) = qgt[0];
      dummyGt.at(2).at(1) = 99;
      return(dummyGt);
    }
  }
}

/*Calculation of likelihoods for pairwise kinship analysis considering drop-out (testthat)*/
// [[Rcpp::export]]
std::vector<double> kinLikeDrop(std::vector<double> qgt, std::vector<double> rgt, std::vector<double> af, std::vector<double> afAl,
                                std::vector<double> probIBD, bool consMu, double myu, double ape, double pd){
  /*homozygote (no drop-out)*/
  std::vector<double> like_ho = kinLike(qgt, rgt, af, afAl, probIBD, consMu, myu, ape);
  double h1_ho = like_ho[0];
  double h2_ho = like_ho[1];

  /*heterozygote (drop-out)*/
  std::vector<std::vector<double>> dummyGt = makeDummyGt(qgt, rgt);
  std::vector<std::vector<double>> dummyData = makeDummyAf(dummyGt, af, afAl);
  std::vector<double> af_dummy = dummyData.at(0);
  std::vector<double> afAl_dummy = dummyData.at(1);

  double h1_he = 0;
  double h2_he = 0;
  int dummyGtSize = dummyGt.size();
  for(int i = 0; i < dummyGtSize; ++i){
    std::vector<double> like_he = kinLike(dummyGt.at(i), rgt, af_dummy, afAl_dummy, probIBD, consMu, myu, ape);
    h1_he = h1_he + like_he[0];
    h2_he = h2_he + like_he[1];
  }

  std::vector<double> likeDrop(2);
  likeDrop[0] = (1 - pd) * h1_ho + pd * h1_he;
  likeDrop[1] = (1 - pd) * h2_ho + pd * h2_he;
  return(likeDrop);
}

/*Calculation of likelihood ratio for kinship analysis*/
//' @export
// [[Rcpp::export]]
std::vector<std::vector<double>> calcKinLr(std::vector<double> query, std::vector<double> ref,
                                           std::vector<std::vector<double>> afList, std::vector<std::vector<double>> afAlList,
                                           std::vector<double> probIBD, bool consMu, std::vector<double> myus, std::vector<double> apes,
                                           int dropMethStr, double pd){
  int nL = query.size() / 2;
  std::vector<std::vector<double>> ans(3, std::vector<double>(nL + 1));
  double cl_H1 = 1;
  double cl_H2 = 1;
  for(int i = 0; i < nL; ++i){
    std::vector<double> qgt(2);
    qgt[0] = query[2 * i];
    qgt[1] = query[2 * i + 1];
    auto qgtEnd = std::remove(qgt.begin(), qgt.end(), -99);
    qgt.erase(qgtEnd, qgt.cend());
    std::vector<double> rgt(2);
    rgt[0] = ref[2 * i];
    rgt[1] = ref[2 * i + 1];
    auto rgtEnd = std::remove(rgt.begin(), rgt.end(), -99);
    qgt.erase(rgtEnd, rgt.cend());
    std::vector<double> af = afList[i];
    std::vector<double> afAl = afAlList[i];

    double myu = myus[i];
    double ape = apes[i];

    /*locus drop-out or no information*/
    if(qgt.size() == 0 || rgt.size() == 0){
      ans.at(0).at(i) = 0.2;
      ans.at(1).at(i) = 0.4;
      ans.at(2).at(i) = 0.6;
      /*considering drop-out*/
    }else if(dropMethStr != 0){
      std::vector<double> qgtUni = qgt;
      qgtUni.erase(std::unique(qgtUni.begin(), qgtUni.end()), qgtUni.end());
      /*qgt : heterozygote*/
      if(qgtUni.size() == 2){
        std::vector<double> likeH12 = kinLike(qgt, rgt, af, afAl, probIBD, consMu, myu, ape);
        ans.at(0).at(i) = likeH12[0];
        ans.at(1).at(i) = likeH12[1];
        ans.at(2).at(i) = likeH12[0] / likeH12[1];
        cl_H1 = cl_H1 * likeH12[0];
        cl_H2 = cl_H2 * likeH12[1];
        /*qgt : homozygote or heterozygote*/
      }else{
        if(dropMethStr == 1){
          /*qgt : heterozygote*/
          if(qgt.size() == 2){
            std::vector<double> likeH12 = kinLike(qgt, rgt, af, afAl, probIBD, consMu, myu, ape);
            ans.at(0).at(i) = likeH12[0];
            ans.at(1).at(i) = likeH12[1];
            ans.at(2).at(i) = likeH12[0] / likeH12[1];
            cl_H1 = cl_H1 * likeH12[0];
            cl_H2 = cl_H2 * likeH12[1];
            /*considering drop-out*/
          }else{
            std::vector<double> likeH12 = kinLikeDrop(qgt, rgt, af, afAl, probIBD, false, myu, ape, pd);
            ans.at(0).at(i) = likeH12[0];
            ans.at(1).at(i) = likeH12[1];
            ans.at(2).at(i) = likeH12[0] / likeH12[1];
            cl_H1 = cl_H1 * likeH12[0];
            cl_H2 = cl_H2 * likeH12[1];
          }
          /*considering drop-out*/
        }else if(dropMethStr == 2){
          std::vector<double> likeH12 = kinLikeDrop(qgt, rgt, af, afAl, probIBD, false, myu, ape, pd);
          ans.at(0).at(i) = likeH12[0];
          ans.at(1).at(i) = likeH12[1];
          ans.at(2).at(i) = likeH12[0] / likeH12[1];
          cl_H1 = cl_H1 * likeH12[0];
          cl_H2 = cl_H2 * likeH12[1];
        }
      }
      /*not considering drop-out*/
    }else{
      std::vector<double> likeH12 = kinLike(qgt, rgt, af, afAl, probIBD, consMu, myu, ape);
      ans.at(0).at(i) = likeH12[0];
      ans.at(1).at(i) = likeH12[1];
      ans.at(2).at(i) = likeH12[0] / likeH12[1];
      cl_H1 = cl_H1 * likeH12[0];
      cl_H2 = cl_H2 * likeH12[1];
    }
  }

  ans.at(0).at(nL) = cl_H1;
  ans.at(1).at(nL) = cl_H2;
  ans.at(2).at(nL) = cl_H1 / cl_H2;
  return(ans);
}

