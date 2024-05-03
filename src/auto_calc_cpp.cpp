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


/*#################################################################################
# The function to extract allele probabilities for victim and reference genotypes #
#################################################################################*/

// [[Rcpp::export]]
std::vector<double> extract_al_prob(std::vector<double> gt1,
                                    std::vector<double> gt2,
                                    std::vector<double> af_dummy,
                                    std::vector<double> af_al_dummy){
  std::vector<double> allele_prob(4);
  for(int i = 0; i < 2; ++i){
    int pos_af = search_pos_double(af_al_dummy, gt1[i]);
    allele_prob[i] = af_dummy[pos_af];
  }
  for(int i = 0; i < 2; ++i){
    int pos_af = search_pos_double(af_al_dummy, gt2[i]);
    allele_prob[i + 2] = af_dummy[pos_af];
  }
  return(allele_prob);
}


/*##################################################
# The function to calculate a genotype probability #
##################################################*/

// [[Rcpp::export]]
double calc_gt_prob(std::vector<double> gt,
                    double al_prob_1,
                    double al_prob_2){
  double gt_prob = al_prob_1 * al_prob_2;
  if(gt[0] != gt[1]){
    gt_prob *= 2;
  }
  return(gt_prob);
}


/*########################################################
# The function to calculate likelihoods for parent-child #
########################################################*/

// [[Rcpp::export]]
std::vector<double> calc_kin_like_pc(std::vector<double> pgt,
                                     std::vector<double> cgt,
                                     std::vector<double> al_prob,
                                     std::vector<double> myu_per_inheritance){
  std::vector<double> like_h12(2);

  double a, b, c, d;
  a = al_prob[0];
  b = al_prob[1];
  c = al_prob[2];
  d = al_prob[3];

  double myu1, myu2, myu3, myu4;
  myu1 = myu_per_inheritance[0];
  myu2 = myu_per_inheritance[1];
  myu3 = myu_per_inheritance[2];
  myu4 = myu_per_inheritance[3];

  double pgt_prob = calc_gt_prob(pgt, a, b);
  double cgt_prob = calc_gt_prob(cgt, c, d);

  if(cgt[0] == cgt[1]){ /* homozygote of cgt: myu1 = myu2, myu3 = myu4, c = d */
    like_h12[0] = 0.5 * pgt_prob * (myu1 * c + myu3 * c);
  }else{
    like_h12[0] = 0.5 * pgt_prob * (myu1 * d + myu2 * c + myu3 * d + myu4 * c);
  }
  like_h12[1] = pgt_prob * cgt_prob;

  return(like_h12);
}


/*#####################################################################
# The function to calculate likelihoods for pairwise kinship analysis #
#####################################################################*/

// [[Rcpp::export]]
std::vector<double> calc_kin_like(std::vector<double> vgt,
                                  std::vector<double> rgt,
                                  std::vector<double> al_prob,
                                  std::vector<double> pibd){
  std::vector<double> like_h12(2);

  double a, b, c, d;
  a = al_prob[0];
  b = al_prob[1];
  c = al_prob[2];
  d = al_prob[3];

  double vgt_prob = calc_gt_prob(vgt, a, b);
  double rgt_prob = calc_gt_prob(rgt, c, d);

  double k2 = pibd[0];
  double k1 = pibd[1] / 2;
  double k0 = pibd[2];

  std::vector<double> v_al = vgt;
  std::sort(v_al.begin(), v_al.end());
  v_al.erase(std::unique(v_al.begin(), v_al.end()), v_al.end());

  std::vector<double> r_al = rgt;
  std::sort(r_al.begin(), r_al.end());
  r_al.erase(std::unique(r_al.begin(), r_al.end()), r_al.end());

  int size_v_al = v_al.size();
  int size_r_al = r_al.size();

  std::vector<double> uniq_vr_al = union_vr_al(v_al, r_al);
  bool presence_r1 = search_pos_double(v_al, r_al[0]) != size_v_al;
  bool presence_r2;
  if(size_r_al == 2){
    presence_r2 = search_pos_double(v_al, r_al[1]) != size_v_al;
  }else{
    presence_r2 = search_pos_double(v_al, r_al[0]) != size_v_al;
  }
  bool equal_vr = true;
  if(size_v_al == size_r_al){
    for(int i = 0; i < size_v_al; ++i){
      bool equal_al = v_al[i] == r_al[i];
      if(!equal_al){
        equal_vr = false;
        break;
      }
    }
  }else{
    equal_vr = false;
  }

  like_h12[1] = vgt_prob * rgt_prob;

  if(!presence_r1 && !presence_r2){
    like_h12[0] = vgt_prob * rgt_prob * k0;
  }else if(equal_vr){
    like_h12[0] = vgt_prob * (k2 + c * k1 + d * k1 + rgt_prob * k0);
  }else if(uniq_vr_al.size() == 3){
    if(presence_r1){
      like_h12[0] = vgt_prob * (d * k1 + rgt_prob * k0);
    }else{
      like_h12[0] = vgt_prob * (c * k1 + rgt_prob * k0);
    }
  }else{
    if(size_r_al == 1){
      like_h12[0] = vgt_prob * (c * k1 + rgt_prob * k0);
    }else{
      if(presence_r1){
        like_h12[0] = vgt_prob * (2 * d * k1 + rgt_prob * k0);
      }else{
        like_h12[0] = vgt_prob * (2 * c * k1 + rgt_prob * k0);
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
std::vector<std::vector<double>> make_dummy_gt(std::vector<double> target_al, std::vector<double> uniq_vr_al){
  int size_target_al = target_al.size();

  /* No drop-out */
  if(size_target_al == 2){
    std::vector<std::vector<double>> dummy_gt(1, std::vector<double>(2));
    dummy_gt.at(0).at(0) = target_al[0];
    dummy_gt.at(0).at(1) = target_al[1];

    return(dummy_gt);

  /* Possible drop-out */
  }else{
    int size_uniq_vr_al = uniq_vr_al.size();
    std::vector<std::vector<double>> dummy_gt(size_uniq_vr_al + 1, std::vector<double>(2));

    for(int i = 0; i < size_uniq_vr_al; ++i){
      std::vector<double> dummy_gt_one(2);
      dummy_gt_one[0] = target_al[0];
      dummy_gt_one[1] = uniq_vr_al[i];

      std::sort(dummy_gt_one.begin(), dummy_gt_one.end());
      dummy_gt.at(i) = dummy_gt_one;
    }

    dummy_gt.at(size_uniq_vr_al).at(0) = target_al[0];
    dummy_gt.at(size_uniq_vr_al).at(1) = 99;

    return(dummy_gt);
  }
}


/*############################################################
# The function to set mutation rates per inheritance pattern #
############################################################*/

// [[Rcpp::export]]
std::vector<double> set_myu_per_inheritance(std::vector<double> pgt,
                                            std::vector<double> cgt,
                                            double myu_pat_m2,
                                            double myu_pat_m1,
                                            double myu_pat_0,
                                            double myu_pat_p1,
                                            double myu_pat_p2,
                                            double myu_mat_m2,
                                            double myu_mat_m1,
                                            double myu_mat_0,
                                            double myu_mat_p1,
                                            double myu_mat_p2,
                                            bool bool_parent_male){
  double myu_m2;
  double myu_m1;
  double myu_0;
  double myu_p1;
  double myu_p2;
  if(bool_parent_male){
    myu_m2 = myu_pat_m2;
    myu_m1 = myu_pat_m1;
    myu_0 = myu_pat_0;
    myu_p1 = myu_pat_p1;
    myu_p2 = myu_pat_p2;
  }else{
    myu_m2 = myu_mat_m2;
    myu_m1 = myu_mat_m1;
    myu_0 = myu_mat_0;
    myu_p1 = myu_mat_p1;
    myu_p2 = myu_mat_p2;
  }

  std::vector<double> myu_per_inheritance(4);

  for(int i = 0; i < 2; ++i){
    double pal = pgt[i];

    for(int j = 0; j < 2; ++j){
      double cal = cgt[j];

      double d = cal - pal;
      if(is_integer(d)){
        int step = (int)d;
        if(step == -2){
          myu_per_inheritance[2 * i + j] = myu_m2;
        }else if(step == -1){
          myu_per_inheritance[2 * i + j] = myu_m1;
        }else if(step == 0){
          myu_per_inheritance[2 * i + j] = myu_0;
        }else if(step == 1){
          myu_per_inheritance[2 * i + j] = myu_p1;
        }else if(step == 2){
          myu_per_inheritance[2 * i + j] = myu_p2;
        }else{
          myu_per_inheritance[2 * i + j] = 0;
        }
      }else{
        myu_per_inheritance[2 * i + j] = 0;
      }
    }
  }

  return(myu_per_inheritance);
}


/*##########################################################################################
# The function to calculate likelihoods for pairwise kinship analysis considering drop-out #
##########################################################################################*/

// [[Rcpp::export]]
std::vector<double> calc_kin_like_drop(std::vector<double> v_al,
                                       std::vector<double> r_al,
                                       std::vector<double> af,
                                       std::vector<double> af_al,
                                       std::vector<double> pibd,
                                       double myu_pat_m2,
                                       double myu_pat_m1,
                                       double myu_pat_0,
                                       double myu_pat_p1,
                                       double myu_pat_p2,
                                       double myu_mat_m2,
                                       double myu_mat_m1,
                                       double myu_mat_0,
                                       double myu_mat_p1,
                                       double myu_mat_p2,
                                       bool bool_pc,
                                       bool bool_parent_victim,
                                       bool bool_parent_male){
  double l_h1 = 0;
  double l_h2 = 0;

  std::vector<double> uniq_vr_al = union_vr_al(v_al, r_al);

  std::vector<std::vector<double>> dummy_vgt = make_dummy_gt(v_al, uniq_vr_al);
  std::vector<std::vector<double>> dummy_rgt = make_dummy_gt(r_al, uniq_vr_al);

  std::vector<std::vector<double>> dummy_af_data = make_dummy_af(uniq_vr_al, af, af_al);
  std::vector<double> af_dummy = dummy_af_data.at(0);
  std::vector<double> af_al_dummy = dummy_af_data.at(1);

  int size_dummy_vgt = dummy_vgt.size();
  int size_dummy_rgt = dummy_rgt.size();

  if(bool_pc){
    for(int i = 0; i < size_dummy_rgt; ++i){
      std::vector<double> drgt1 = dummy_rgt.at(i);

      for(int j = 0; j < size_dummy_vgt; ++j){
        std::vector<double> dvgt1 = dummy_vgt.at(j);

        std::vector<double> pgt(2);
        std::vector<double> cgt(2);
        if(bool_parent_victim){
          pgt = dvgt1;
          cgt = drgt1;
        }else{
          pgt = drgt1;
          cgt = dvgt1;
        }

        std::vector<double> myu_per_inheritance = set_myu_per_inheritance(pgt, cgt,
                                                                          myu_pat_m2, myu_pat_m1, myu_pat_0, myu_pat_p1, myu_pat_p2,
                                                                          myu_mat_m2, myu_mat_m1, myu_mat_0, myu_mat_p1, myu_mat_p2,
                                                                          bool_parent_male);

        std::vector<double> al_prob = extract_al_prob(pgt, cgt, af_dummy, af_al_dummy);

        std::vector<double> l_h12_one = calc_kin_like_pc(pgt, cgt, al_prob, myu_per_inheritance);
        l_h1 += l_h12_one[0];
        l_h2 += l_h12_one[1];
      }
    }
  }else{
    for(int i = 0; i < size_dummy_rgt; ++i){
      std::vector<double> drgt1 = dummy_rgt.at(i);

      for(int j = 0; j < size_dummy_vgt; ++j){
        std::vector<double> dvgt1 = dummy_vgt.at(j);

        std::vector<double> al_prob = extract_al_prob(dvgt1, drgt1, af_dummy, af_al_dummy);

        std::vector<double> l_h12_one = calc_kin_like(dvgt1, drgt1, al_prob, pibd);
        l_h1 += l_h12_one[0];
        l_h2 += l_h12_one[1];
      }
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
std::vector<std::vector<double>> calc_kin_lr(std::vector<double> prof_victim,
                                             std::vector<double> prof_ref,
                                             std::vector<std::vector<double>> af_list,
                                             std::vector<std::vector<double>> af_al_list,
                                             std::vector<double> pibd,
                                             std::vector<double> myus_paternal_m2,
                                             std::vector<double> myus_paternal_m1,
                                             std::vector<double> myus_paternal_0,
                                             std::vector<double> myus_paternal_p1,
                                             std::vector<double> myus_paternal_p2,
                                             std::vector<double> myus_maternal_m2,
                                             std::vector<double> myus_maternal_m1,
                                             std::vector<double> myus_maternal_0,
                                             std::vector<double> myus_maternal_p1,
                                             std::vector<double> myus_maternal_p2,
                                             bool bool_pc,
                                             bool bool_parent_victim,
                                             bool bool_parent_male
                                             ){
  int n_l = prof_victim.size() / 2;
  std::vector<std::vector<double>> ans(3, std::vector<double>(n_l + 1));
  double cl_h1 = 1;
  double cl_h2 = 1;

  for(int i = 0; i < n_l; ++i){
    std::vector<double> v_al(2);
    v_al[0] = prof_victim[2 * i];
    v_al[1] = prof_victim[2 * i + 1];
    auto v_al_end = std::remove(v_al.begin(), v_al.end(), -99);
    v_al.erase(v_al_end, v_al.cend());

    std::vector<double> r_al(2);
    r_al[0] = prof_ref[2 * i];
    r_al[1] = prof_ref[2 * i + 1];
    auto r_al_end = std::remove(r_al.begin(), r_al.end(), -99);
    r_al.erase(r_al_end, r_al.cend());

    std::vector<double> af = af_list[i];
    std::vector<double> af_al = af_al_list[i];

    double myu_pat_m2 = myus_paternal_m2[i];
    double myu_pat_m1 = myus_paternal_m1[i];
    double myu_pat_0 = myus_paternal_0[i];
    double myu_pat_p1 = myus_paternal_p1[i];
    double myu_pat_p2 = myus_paternal_p2[i];
    double myu_mat_m2 = myus_maternal_m2[i];
    double myu_mat_m1 = myus_maternal_m1[i];
    double myu_mat_0 = myus_maternal_0[i];
    double myu_mat_p1 = myus_maternal_p1[i];
    double myu_mat_p2 = myus_maternal_p2[i];

    /* Locus drop-out or no information */
    if(v_al.size() == 0 || r_al.size() == 0){
      ans.at(0).at(i) = 1;
      ans.at(1).at(i) = 1;
      ans.at(2).at(i) = 1;

    /* Calculate likelihoods considering drop-out */
    }else{
      std::vector<double> like_h12 = calc_kin_like_drop(v_al, r_al, af, af_al, pibd,
                                                        myu_pat_m2, myu_pat_m1, myu_pat_0, myu_pat_p1, myu_pat_p2,
                                                        myu_mat_m2, myu_mat_m1, myu_mat_0, myu_mat_p1, myu_mat_p2,
                                                        bool_pc, bool_parent_victim, bool_parent_male);

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
                                                              std::vector<double> myus_paternal_m2,
                                                              std::vector<double> myus_paternal_m1,
                                                              std::vector<double> myus_paternal_0,
                                                              std::vector<double> myus_paternal_p1,
                                                              std::vector<double> myus_paternal_p2,
                                                              std::vector<double> myus_maternal_m2,
                                                              std::vector<double> myus_maternal_m1,
                                                              std::vector<double> myus_maternal_0,
                                                              std::vector<double> myus_maternal_p1,
                                                              std::vector<double> myus_maternal_p2,
                                                              std::vector<bool> bool_pc_all,
                                                              std::vector<bool> bool_parent_victim_all,
                                                              std::vector<bool> bool_parent_male_all
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

    bool bool_pc = bool_pc_all[pos_assumed_rel];
    bool bool_parent_victim = bool_parent_victim_all[pos_assumed_rel];
    bool bool_parent_male = bool_parent_male_all[pos_assumed_rel];

    for(int j = 0; j < n_v; ++j){
      std::vector<double> prof_victim = gt_v_auto.at(j);

      std::vector<std::vector<double>> ans = calc_kin_lr(prof_victim, prof_ref, af_list, af_al_list, pibd,
                                                         myus_paternal_m2, myus_paternal_m1, myus_paternal_0, myus_paternal_p1, myus_paternal_p2,
                                                         myus_maternal_m2, myus_maternal_m1, myus_maternal_0, myus_maternal_p1, myus_maternal_p2,
                                                         bool_pc, bool_parent_victim, bool_parent_male);

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
