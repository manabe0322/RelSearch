#include "header.h"

/*#####################################################################
# The function to unite the victim alleles with the reference alleles #
#####################################################################*/

// [[Rcpp::export]]
std::vector<double> union_vr_al(std::vector<double> vgt, std::vector<double> rgt){
  std::vector<double> uniq_vr_al;
  std::set_union(vgt.begin(), vgt.end(), rgt.begin(), rgt.end(), inserter(uniq_vr_al, uniq_vr_al.end()));
  std::sort(uniq_vr_al.begin(), uniq_vr_al.end());
  uniq_vr_al.erase(std::unique(uniq_vr_al.begin(), uniq_vr_al.end()), uniq_vr_al.end());
  return(uniq_vr_al);
}


/*#####################################################################
# The function to calculate likelihoods for pairwise kinship analysis #
#####################################################################*/

// [[Rcpp::export]]
std::vector<double> calc_kin_like(std::vector<double> vgt, std::vector<double> rgt, std::vector<double> af, std::vector<double> af_al,
                                  std::vector<double> pibd, double myu, bool cons_mu, bool par_vic){
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
    int pos_v = search_pos_double(af_al, vgt[0]);
    a = af[pos_v];
    b = af[pos_v];
  }else{
    int pos_v1 = search_pos_double(af_al, vgt[0]);
    a = af[pos_v1];
    int pos_v2 = search_pos_double(af_al, vgt[1]);
    b = af[pos_v2];
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

  std::vector<double> uniq_vr_al = union_vr_al(vgt, rgt);
  bool presence_v1 = search_pos_double(rgt, vgt[0]) != size_rgt;
  bool presence_v2;
  if(size_vgt == 2){
    presence_v2 = search_pos_double(rgt, vgt[1]) != size_rgt;
  }else{
    presence_v2 = search_pos_double(rgt, vgt[0]) != size_rgt;
  }
  bool equal_vr = true;
  if(size_vgt == size_rgt){
    for(int i = 0; i < size_vgt; ++i){
      bool equal_al = vgt[i] == rgt[i];
      if(!equal_al){
        equal_vr = false;
        break;
      }
    }
  }else{
    equal_vr = false;
  }

  if(!presence_v1 && !presence_v2){
    if(cons_mu){
      if(size_vgt == 1){
        if(size_rgt == 1){
          if(par_vic){
            like_h12[0] = a * a * myu * c;
            like_h12[1] = a * a * c * c;
          }else{
            like_h12[0] = c * c * myu * a;
            like_h12[1] = c * c * a * a;
          }
        }else{
          if(par_vic){
            like_h12[0] = a * a * myu * (c + d);
            like_h12[1] =  a * a * 2 * c * d;
          }else{
            like_h12[0] = 2 * c * d * myu * a;
            like_h12[1] = 2 * c * d * a * a;
          }
        }
      }else{
        if(size_rgt == 1){
          if(par_vic){
            like_h12[0] = 2 * a * b * myu * c;
            like_h12[1] = 2 * a * b * c * c;
          }else{
            like_h12[0] = c * c * myu * (a + b);
            like_h12[1] = c * c * 2 * a * b;
          }
        }else{
          if(par_vic){
            like_h12[0] = 2 * a * b * myu * (c + d);
            like_h12[1] = 2 * a * b * 2 * c * d;
          }else{
            like_h12[0] = 2 * c * d * myu * (a + b);
            like_h12[1] = 2 * c * d * 2 * a * b;
          }
        }
      }
    }else{
      if(size_vgt == 1){
        if(size_rgt == 1){
          like_h12[0] = c * c * k0 * a * a;
          like_h12[1] = c * c * a * a;
        }else{
          like_h12[0] = 2 * c * d * k0 * a * a;
          like_h12[1] = 2 * c * d * a * a;
        }
      }else{
        if(size_rgt == 1){
          like_h12[0] = c * c * k0 * 2 * a * b;
          like_h12[1] = c * c * 2 * a * b;
        }else{
          like_h12[0] = 2 * c * d * k0 * 2 * a * b;
          like_h12[1] = 2 * c * d * 2 * a * b;
        }
      }
    }
  }else if(equal_vr){
    if(size_vgt == 1){
      like_h12[0] = c * c * (k2 + 2 * a * k1 + a * a * k0);
      like_h12[1] = c * c * a * a;
    }else{
      like_h12[0] = 2 * c * d * (k2 + a * k1 + b * k1 + 2 * a * b * k0);
      like_h12[1] = 2 * c * d * 2 * a * b;
    }
  }else if(uniq_vr_al.size() == 3){
    if(presence_v1){
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
      if(presence_v1){
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


/*#############################################################
# The function to make allele frequencies for dummy genotypes #
#############################################################*/

// [[Rcpp::export]]
std::vector<std::vector<double>> make_dummy_af(std::vector<double> uniq_vr_al, std::vector<double> af, std::vector<double> af_al){
  int len = uniq_vr_al.size();

  /* Indices of observed alleles */
  std::vector<int> pos_al_obs(len);
  for(int i = 0; i < len; ++i){
    pos_al_obs[i] = search_pos_double(af_al, uniq_vr_al[i]);
  }

  /* Indices of unobserved alleles */
  std::vector<int> pos_al_all = tousa(0, af_al.size() - 1, 1);
  std::vector<int> pos_al_unobs;
  pos_al_unobs = setdiff_int(pos_al_all, pos_al_obs);

  /* Assign observed allele frequencies */
  std::vector<std::vector<double>> dummy_af_data(2, std::vector<double>(len + 1));
  for(int i = 0; i < len; ++i){
    int pos1 = pos_al_obs[i];
    dummy_af_data.at(0).at(i) = af[pos1];
    dummy_af_data.at(1).at(i) = uniq_vr_al[i];
  }

  /* Assign unobserved allele frequencies */
  int len2 = pos_al_unobs.size();
  std::vector<double> freq_qal(len2);
  for(int i = 0; i < len2; ++i){
    int pos2 = pos_al_unobs[i];
    freq_qal[i] = af[pos2];
  }
  dummy_af_data.at(0).at(len) = std::accumulate(freq_qal.begin(), freq_qal.end(), 0.0);
  dummy_af_data.at(1).at(len) = 99;

  return(dummy_af_data);
}


/*###################################################################
# The function to make dummy genotypes considering drop-out alleles #
###################################################################*/

// [[Rcpp::export]]
std::vector<std::vector<double>> make_dummy_gt(std::vector<double> target_gt, std::vector<double> uniq_vr_al){
  int size_target_gt = target_gt.size();

  /* No drop-out */
  if(size_target_gt == 2){
    std::vector<std::vector<double>> dummy_gt(1, std::vector<double>(2));
    dummy_gt.at(0).at(0) = target_gt[0];
    dummy_gt.at(0).at(1) = target_gt[1];

    return(dummy_gt);

  /* Possible drop-out */
  }else{
    int size_uniq_vr_al = uniq_vr_al.size();
    std::vector<std::vector<double>> dummy_gt(size_uniq_vr_al + 1, std::vector<double>(2));

    for(int i = 0; i < size_uniq_vr_al; ++i){
      std::vector<double> dummy_gt_one(2);
      dummy_gt_one[0] = target_gt[0];
      dummy_gt_one[1] = uniq_vr_al[i];

      std::sort(dummy_gt_one.begin(), dummy_gt_one.end());
      dummy_gt.at(i) = dummy_gt_one;
    }

    dummy_gt.at(size_uniq_vr_al).at(0) = target_gt[0];
    dummy_gt.at(size_uniq_vr_al).at(1) = 99;

    return(dummy_gt);
  }
}


/*##########################################################################################
# The function to calculate likelihoods for pairwise kinship analysis considering drop-out #
##########################################################################################*/

// [[Rcpp::export]]
std::vector<double> calc_kin_like_drop(std::vector<double> vgt, std::vector<double> rgt, std::vector<double> af, std::vector<double> af_al,
                                       std::vector<double> pibd, double myu, bool cons_mu, bool par_vic){
  double l_h1 = 0;
  double l_h2 = 0;

  std::vector<double> uniq_vr_al = union_vr_al(vgt, rgt);

  std::vector<std::vector<double>> dummy_vgt = make_dummy_gt(vgt, uniq_vr_al);
  std::vector<std::vector<double>> dummy_rgt = make_dummy_gt(rgt, uniq_vr_al);

  std::vector<std::vector<double>> dummy_af_data = make_dummy_af(uniq_vr_al, af, af_al);
  std::vector<double> af_dummy = dummy_af_data.at(0);
  std::vector<double> af_al_dummy = dummy_af_data.at(1);

  int size_dummy_vgt = dummy_vgt.size();
  int size_dummy_rgt = dummy_rgt.size();

  for(int i = 0; i < size_dummy_rgt; ++i){
    std::vector<double> drgt1 = dummy_rgt.at(i);

    for(int j = 0; j < size_dummy_vgt; ++j){
      std::vector<double> dvgt1 = dummy_vgt.at(j);

      std::vector<double> l_h12_one = calc_kin_like(dvgt1, drgt1, af_dummy, af_al_dummy, pibd, myu, cons_mu, par_vic);
      l_h1 += l_h12_one[0];
      l_h2 += l_h12_one[1];
    }
  }

  std::vector<double> like_h12(2);
  like_h12[0] = l_h1;
  like_h12[1] = l_h2;
  return(like_h12);
}


/*##############################################
# The function to calculate the LR of one pair #
##############################################*/

// [[Rcpp::export]]
std::vector<std::vector<double>> calc_kin_lr(std::vector<double> prof_victim, std::vector<double> prof_ref,
                                             std::vector<std::vector<double>> af_list, std::vector<std::vector<double>> af_al_list,
                                             std::vector<double> pibd, std::vector<double> myus, bool cons_mu, bool par_vic){
  int n_l = prof_victim.size() / 2;
  std::vector<std::vector<double>> ans(3, std::vector<double>(n_l + 1));
  double cl_h1 = 1;
  double cl_h2 = 1;

  for(int i = 0; i < n_l; ++i){
    std::vector<double> vgt(2);
    vgt[0] = prof_victim[2 * i];
    vgt[1] = prof_victim[2 * i + 1];
    auto vgt_end = std::remove(vgt.begin(), vgt.end(), -99);
    vgt.erase(vgt_end, vgt.cend());

    std::vector<double> rgt(2);
    rgt[0] = prof_ref[2 * i];
    rgt[1] = prof_ref[2 * i + 1];
    auto rgt_end = std::remove(rgt.begin(), rgt.end(), -99);
    vgt.erase(rgt_end, rgt.cend());

    std::vector<double> af = af_list[i];
    std::vector<double> af_al = af_al_list[i];

    double myu = myus[i];

    /* Locus drop-out or no information */
    if(vgt.size() == 0 || rgt.size() == 0){
      ans.at(0).at(i) = 1;
      ans.at(1).at(i) = 1;
      ans.at(2).at(i) = 1;

    /* Calculate likelihoods considering drop-out */
    }else{
      std::vector<double> like_h12 = calc_kin_like_drop(vgt, rgt, af, af_al, pibd, myu, cons_mu, par_vic);

      ans.at(0).at(i) = like_h12[0];
      ans.at(1).at(i) = like_h12[1];
      ans.at(2).at(i) = like_h12[0] / like_h12[1];

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

// [[Rcpp::export]]
std::vector<std::vector<std::vector<double>>> calc_kin_lr_all(std::vector<std::vector<double>> gt_v_auto,
                                                              std::vector<std::vector<double>> gt_r_auto,
                                                              std::vector<std::string> assumed_rel_all,
                                                              std::vector<std::vector<double>> af_list,
                                                              std::vector<std::vector<double>> af_al_list,
                                                              std::vector<std::string> names_rel,
                                                              std::vector<std::vector<double>> pibds_rel,
                                                              std::vector<double> myus,
                                                              std::vector<bool> cons_mutations,
                                                              std::vector<bool> parent_victim
                                                              ){
  /* Call the R function "message" */
  Function message("message");

  int n_v = gt_v_auto.size();
  int n_r = gt_r_auto.size();
  int n_l = gt_r_auto.at(0).size();
  int n_vr = n_v * n_r;

  std::vector<std::vector<std::vector<double>>> result_auto(n_vr, std::vector<std::vector<double>>(3, std::vector<double>(n_l + 1)));

  int counter_base = n_vr * 0.001;
  int counter = counter_base;

  for(int i = 0; i < n_r; ++i){
    std::string assumed_rel = assumed_rel_all[i];
    std::vector<double> prof_ref = gt_r_auto.at(i);

    std::vector<double> pibd;
    int pos_assumed_rel = search_pos_string(names_rel, assumed_rel);
    pibd = pibds_rel.at(pos_assumed_rel);

    bool cons_mu = cons_mutations[pos_assumed_rel];
    bool par_vic = parent_victim[pos_assumed_rel];

    for(int j = 0; j < n_v; ++j){
      std::vector<double> prof_victim = gt_v_auto.at(j);

      std::vector<std::vector<double>> ans = calc_kin_lr(prof_victim, prof_ref, af_list, af_al_list, pibd, myus, cons_mu, par_vic);

      int pos = n_v * i + j;

      result_auto.at(pos).at(0) = ans.at(0);
      result_auto.at(pos).at(1) = ans.at(1);
      result_auto.at(pos).at(2) = ans.at(2);

      /* Display a message to the console to update the progress bar */
      if(pos >= counter){
        std::string txt_console = "STR_Victim-Reference_ : ";
        txt_console += int_to_str(pos);
        message('\r', txt_console, _["appendLF"] = false);
        counter += counter_base;
      }
    }
  }

  return(result_auto);
}
