#include "header.h"

/*General calculation of likelihoods for pairwise kinship analysis (testthat)*/
// [[Rcpp::export]]
std::vector<double> calc_kin_like(std::vector<double> vgt, std::vector<double> rgt, std::vector<double> af, std::vector<double> af_al,
                                  std::vector<double> pibd, bool cons_mu, double myu, double ape){
  std::vector<double> like_h12(2);

  std::sort(vgt.begin(), vgt.end());
  vgt.erase(std::unique(vgt.begin(), vgt.end()), vgt.end());
  std::sort(rgt.begin(), rgt.end());
  rgt.erase(std::unique(rgt.begin(), rgt.end()), rgt.end());

  double k2 = pibd[0];
  double k1 = pibd[1] / 2;
  double k0 = pibd[2];

  int size_vgt = vgt.size();
  int size_rgt = rgt.size();

  double a, b, c, d;
  if(size_vgt == 1){
    int pos_q = search_pos_double(af_al, vgt[0]);
    a = af[pos_q];
    b = af[pos_q];
  }else{
    int pos_q1 = search_pos_double(af_al, vgt[0]);
    a = af[pos_q1];
    int pos_q2 = search_pos_double(af_al, vgt[1]);
    b = af[pos_q2];
  }
  if(size_rgt == 1){
    int pos_r = search_pos_double(af_al, rgt[0]);
    c = af[pos_r];
    d = af[pos_r];
  }else{
    int pos_r1 = search_pos_double(af_al, rgt[0]);
    c = af[pos_r1];
    int pos_r2 = search_pos_double(af_al, rgt[1]);
    d = af[pos_r2];
  }

  std::vector<double> uni_qrgt;
  std::set_union(vgt.begin(), vgt.end(), rgt.begin(), rgt.end(), inserter(uni_qrgt, uni_qrgt.end()));
  bool presence_q1 = search_pos_double(rgt, vgt[0]) != size_rgt;
  bool presence_q2;
  if(size_vgt == 2){
    presence_q2 = search_pos_double(rgt, vgt[1]) != size_rgt;
  }else{
    presence_q2 = search_pos_double(rgt, vgt[0]) != size_rgt;
  }
  bool equal_qr = true;
  if(size_vgt == size_rgt){
    for(int i = 0; i < size_vgt; ++i){
      bool equal_al = vgt[i] == rgt[i];
      if(!equal_al){
        equal_qr = false;
        break;
      }
    }
  }else{
    equal_qr = false;
  }

  if(!presence_q1 && !presence_q2){
    if(cons_mu){
      like_h12[0] = myu;
      like_h12[1] = ape;
    }else{
      if(size_vgt == 1){
        if(size_rgt == 1){
          like_h12[0] = a * a * c * c * k0;
          like_h12[1] = a * a * c * c;
        }else{
          like_h12[0] = a * a * 2 * c * d * k0;
          like_h12[1] = a * a * 2 * c * d;
        }
      }else{
        if(size_rgt == 1){
          like_h12[0] = 2 * a * b * c * c * k0;
          like_h12[1] = 2 * a * b * c * c;
        }else{
          like_h12[0] = 2 * a * b * 2 * c * d * k0;
          like_h12[1] = 2 * a * b * 2 * c * d;
        }
      }
    }
  }else if(equal_qr){
    if(size_vgt == 1){
      like_h12[0] = c * c * (k2 + 2 * a * k1 + a * a * k0);
      like_h12[1] = c * c * a * a;
    }else{
      like_h12[0] = 2 * c * d * (k2 + a * k1 + b * k1 + 2 * a * b * k0);
      like_h12[1] = 2 * c * d * 2 * a * b;
    }
  }else if(uni_qrgt.size() == 3){
    if(presence_q1){
      like_h12[0] = 2 * c * d * (b * k1 + 2 * a * b * k0);
      like_h12[1] = 2 * c * d * 2 * a * b;
    }else{
      like_h12[0] = 2 * c * d * (a * k1 + 2 * a * b * k0);
      like_h12[1] = 2 * c * d * 2 * a * b;
    }
  }else{
    if(size_vgt == 1){
      like_h12[0] = 2 * c * d * (a * k1 + a * a * k0);
      like_h12[1] = 2 * c * d * a * a;
    }else{
      if(presence_q1){
        like_h12[0] = c * c * (2 * b * k1 + 2 * a * b * k0);
        like_h12[1] = c * c * 2 * a * b;
      }else{
        like_h12[0] = c * c * (2 * a * k1 + 2 * a * b * k0);
        like_h12[1] = c * c * 2 * a * b;
      }
    }
  }
  return(like_h12);
}

/*Make allele frequencies for dummy genotypes (testthat)*/
// [[Rcpp::export]]
std::vector<std::vector<double>> make_dummy_af(std::vector<std::vector<double>> dummy_gt, std::vector<double> af, std::vector<double> af_al){
  int n_init_row = dummy_gt.size();
  std::vector<double> af_al_dummy(2 * n_init_row);
  for(int i = 0; i < n_init_row; ++i){
    af_al_dummy[2 * i] = dummy_gt.at(i).at(0);
    af_al_dummy[2 * i + 1] = dummy_gt.at(i).at(1);
  }
  std::sort(af_al_dummy.begin(), af_al_dummy.end());
  af_al_dummy.erase(std::unique(af_al_dummy.begin(), af_al_dummy.end()), af_al_dummy.end());

  int len = af_al_dummy.size() - 1;
  std::vector<int> pos_al_1(len);
  for(int i = 0; i < len; ++i){
    pos_al_1[i] = search_pos_double(af_al, af_al_dummy[i]);
  }
  std::vector<int> pos_al_all = tousa(0, af_al.size() - 1, 1);
  std::vector<int> pos_al_2;
  std::set_difference(pos_al_all.begin(), pos_al_all.end(), pos_al_1.begin(), pos_al_1.end(), inserter(pos_al_2, pos_al_2.end()));

  std::vector<std::vector<double>> dummy_data(2, std::vector<double>(len + 1));
  for(int i = 0; i < len; ++i){
    int pos1 = pos_al_1[i];
    dummy_data.at(0).at(i) = af[pos1];
  }

  int len2 = pos_al_2.size();
  std::vector<double> freq_qal(len2);
  for(int i = 0; i < len2; ++i){
    int pos2 = pos_al_2[i];
    freq_qal[i] = af[pos2];
  }
  dummy_data.at(0).at(len) = std::accumulate(freq_qal.begin(), freq_qal.end(), 0.0);
  dummy_data.at(1) = af_al_dummy;

  return(dummy_data);
}

/*Determine dummy genotypes considering a drop-out allele (testthat)*/
// [[Rcpp::export]]
std::vector<std::vector<double>> make_dummy_gt(std::vector<double> target_gt, std::vector<double> uniq_vr_al){

  /* The size of target_gt */
  int size_target_gt = target_gt.size();

  /* No drop-out */
  if(size_target_gt == 2){

    /* Define a vector for dummy genotypes */
    std::vector<std::vector<double>> dummy_gt(1, std::vector<double>(2));

    /* Assign alleles */
    dummy_gt.at(0).at(0) = target_gt[0];
    dummy_gt.at(0).at(1) = target_gt[1];

    /* Return */
    return(dummy_gt);

  /* Possible drop-out */
  }else{

    /* The size of uniq_vr_al */
    int size_uniq_vr_al = uniq_vr_al.size();

    /* Define a vector for dummy genotypes */
    std::vector<std::vector<double>> dummy_gt(size_uniq_vr_al + 1, std::vector<double>(2));

    /* Repetitive execution for other alleles */
    for(int i = 0; i < size_uniq_vr_al; ++i){

      /* Define a vector for a dummy genotype */
      std::vector<double> dummy_gt_one(2);

      /* Assign alleles */
      dummy_gt_one[0] = target_gt[0];
      dummy_gt_one[1] = uniq_vr_al[i];

      /* Sort the dummy genotype */
      std::sort(dummy_gt_one.begin(), dummy_gt_one.end());

      /* Assign the dummy genotype */
      dummy_gt.at(i) = dummy_gt_one;
    }

    /* Assign alleles (allele 99) */
    dummy_gt.at(size_uniq_vr_al).at(0) = target_gt[0];
    dummy_gt.at(size_uniq_vr_al).at(1) = 99;

    /* Return */
    return(dummy_gt);
  }
}

/*Calculation of likelihoods for pairwise kinship analysis considering drop-out (testthat)*/
// [[Rcpp::export]]
std::vector<double> calc_kin_like_drop(std::vector<double> vgt, std::vector<double> rgt, std::vector<double> af, std::vector<double> af_al,
                                       std::vector<double> pibd, bool cons_mu, double myu, double ape, double pd){
  /*homozygote (no drop-out)*/
  std::vector<double> like_ho = calc_kin_like(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape);
  double h1_ho = like_ho[0];
  double h2_ho = like_ho[1];

  /*heterozygote (drop-out)*/
  std::vector<std::vector<double>> dummy_gt = make_dummy_gt(vgt, rgt);
  std::vector<std::vector<double>> dummy_data = make_dummy_af(dummy_gt, af, af_al);
  std::vector<double> af_dummy = dummy_data.at(0);
  std::vector<double> af_al_dummy = dummy_data.at(1);

  double h1_he = 0;
  double h2_he = 0;
  int size_dummy_gt = dummy_gt.size();
  for(int i = 0; i < size_dummy_gt; ++i){
    std::vector<double> like_he = calc_kin_like(dummy_gt.at(i), rgt, af_dummy, af_al_dummy, pibd, cons_mu, myu, ape);
    h1_he = h1_he + like_he[0];
    h2_he = h2_he + like_he[1];
  }

  std::vector<double> like_drop(2);
  like_drop[0] = (1 - pd) * h1_ho + pd * h1_he;
  like_drop[1] = (1 - pd) * h2_ho + pd * h2_he;
  return(like_drop);
}


/*##############################################
# The function to calculate the LR of one pair #
##############################################*/

// [[Rcpp::export]]
std::vector<std::vector<double>> calc_kin_lr(std::vector<double> prof_victim, std::vector<double> prof_ref,
                                             std::vector<std::vector<double>> af_list, std::vector<std::vector<double>> af_al_list,
                                             std::vector<double> pibd, bool cons_mu, std::vector<double> myus, std::vector<double> apes,
                                             int meth_d, double pd){
  /* The number of loci */
  int n_l = prof_victim.size() / 2;

  /* Define an array for saving the likelihood and the LR */
  std::vector<std::vector<double>> ans(3, std::vector<double>(n_l + 1));

  /* Define the initial cumulative likelihood of H1 and H2 */
  double cl_h1 = 1;
  double cl_h2 = 1;

  /* Repetitive execution for loci */
  for(int i = 0; i < n_l; ++i){

    /* Extract the victim's genotype in one locus */
    std::vector<double> vgt(2);
    vgt[0] = prof_victim[2 * i];
    vgt[1] = prof_victim[2 * i + 1];

    /* Remove '-99' (the sign of drop-out) from the victim's genotype */
    auto vgt_end = std::remove(vgt.begin(), vgt.end(), -99);
    vgt.erase(vgt_end, vgt.cend());

    /* Extract the reference's genotype in one locus */
    std::vector<double> rgt(2);
    rgt[0] = prof_ref[2 * i];
    rgt[1] = prof_ref[2 * i + 1];

    /* Remove '-99' (the sign of drop-out) from the reference's genotype */
    auto rgt_end = std::remove(rgt.begin(), rgt.end(), -99);
    vgt.erase(rgt_end, rgt.cend());

    /* Extract the allele frequencies in one locus */
    std::vector<double> af = af_list[i];
    std::vector<double> af_al = af_al_list[i];

    /* Extract the mutation rate in one locus */
    double myu = myus[i];

    /* Extract the average probability of exclusion in one locus */
    double ape = apes[i];

    /* Locus drop-out or no information */
    if(vgt.size() == 0 || rgt.size() == 0){
      ans.at(0).at(i) = 1;
      ans.at(1).at(i) = 1;
      ans.at(2).at(i) = 1;

    /* No undetected allele */
    }else if(vgt.size() == 2 && rgt.size() == 2){

      /* Calculate the likelihood in one locus */
      std::vector<double> like_h12 = calc_kin_like(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape);

      /* Assign the likelihood and the LR */
      ans.at(0).at(i) = like_h12[0];
      ans.at(1).at(i) = like_h12[1];
      ans.at(2).at(i) = like_h12[0] / like_h12[1];

      /* Update the cumulative likelihood of H1 and H2 */
      cl_h1 = cl_h1 * like_h12[0];
      cl_h2 = cl_h2 * like_h12[1];

    /* Not considering drop-out */
    }else if(meth_d == 0){

      /* Calculate the likelihood in one locus */
      std::vector<double> like_h12 = calc_kin_like(vgt, rgt, af, af_al, pibd, cons_mu, myu, ape);

      /* Assign the likelihood and the LR */
      ans.at(0).at(i) = like_h12[0];
      ans.at(1).at(i) = like_h12[1];
      ans.at(2).at(i) = like_h12[0] / like_h12[1];

      /* Update the cumulative likelihood of H1 and H2 */
      cl_h1 = cl_h1 * like_h12[0];
      cl_h2 = cl_h2 * like_h12[1];

    /* Considering drop-out */
    }else{

      /* Calculate the likelihood in one locus considering drop-out*/
      std::vector<double> like_h12 = calc_kin_like_drop(vgt, rgt, af, af_al, pibd, false, myu, ape, pd);

      /* Assign the likelihood and the LR */
      ans.at(0).at(i) = like_h12[0];
      ans.at(1).at(i) = like_h12[1];
      ans.at(2).at(i) = like_h12[0] / like_h12[1];

      /* Update the cumulative likelihood of H1 and H2 */
      cl_h1 = cl_h1 * like_h12[0];
      cl_h2 = cl_h2 * like_h12[1];
    }
  }

  ans.at(0).at(n_l) = cl_h1;
  ans.at(1).at(n_l) = cl_h2;
  ans.at(2).at(n_l) = cl_h1 / cl_h2;
  return(ans);
}


/*###############################################
# The function to calculate the LR of all pairs #
###############################################*/

// [[Rcpp::depends(RcppProgress)]]
// [[Rcpp::export]]
std::vector<std::vector<std::vector<double>>> calc_kin_lr_all(std::vector<std::vector<double>> gt_v_auto,
                                                              std::vector<std::vector<double>> gt_r_auto,
                                                              std::vector<std::string> assumed_rel_all,
                                                              std::vector<std::vector<double>> af_list,
                                                              std::vector<std::vector<double>> af_al_list,
                                                              std::vector<std::string> names_rel,
                                                              std::vector<std::string> degrees_rel,
                                                              std::vector<std::vector<double>> pibds_rel,
                                                              std::vector<double> myus,
                                                              std::vector<double> apes,
                                                              int meth_d,
                                                              double pd){
  /* The number of victims */
  int n_v = gt_v_auto.size();

  /* The number of references */
  int n_r = gt_r_auto.size();

  /* The number of loci */
  int n_l = gt_r_auto.at(0).size();

  /* The number of pairs */
  int n_vr = n_v * n_r;

  /* Set progress bar */
  Progress p(n_vr, true);

  /* Define an array for saving likelihoods and LRs */
  std::vector<std::vector<std::vector<double>>> result_auto(n_vr, std::vector<std::vector<double>>(3, std::vector<double>(n_l + 1)));

  /* Repetitive execution for references */
  for(int i = 0; i < n_r; ++i){

    /* Extract the assumed relationship of one reference */
    std::string assumed_rel = assumed_rel_all[i];

    /* Extract the profile of one reference */
    std::vector<double> prof_ref = gt_r_auto.at(i);

    /* Extract the IBD probabilities */
    std::vector<double> pibd;
    int pos_assumed_rel = search_pos_string(names_rel, assumed_rel);
    pibd = pibds_rel.at(pos_assumed_rel);

    /* Set the consideration of mutations */
    bool cons_mu;
    if(degrees_rel[pos_assumed_rel] == "1st_pc"){
      cons_mu = true;
    }else{
      cons_mu = false;
    }

    /* Repetitive execution for victims */
    for(int j = 0; j < n_v; ++j){

      /* Update progress bar */
      p.increment();

      /* Extract the profile of one victim */
      std::vector<double> prof_victim = gt_v_auto.at(j);

      /* Calculate the LR for the selected victim and reference */
      std::vector<std::vector<double>> ans = calc_kin_lr(prof_victim, prof_ref, af_list, af_al_list, pibd, cons_mu, myus, apes, meth_d, pd);

      /* Define the index for assigning the likelihood and the LR */
      int pos = n_v * i + j;

      /* Assign the likelihood and the LR */
      result_auto.at(pos).at(0) = ans.at(0);
      result_auto.at(pos).at(1) = ans.at(1);
      result_auto.at(pos).at(2) = ans.at(2);
    }
  }

  /* Return */
  return(result_auto);
}
