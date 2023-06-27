#include "header.h"

/*Obtain alleles from a haplotype*/
// [[Rcpp::export]]
std::vector<double> obtain_al(std::string hap){
  hap.erase(std::remove(hap.begin(), hap.end(), ' '), hap.end());
  if(hap == ""){
    std::vector<double> al(0);
    return(al);
  }else{
    if(hap.find(",") != std::string::npos){
      const char* del = ",";
      std::vector<std::string> al_pre = split(hap, del);
      int len = al_pre.size();
      std::vector<double> al(len);
      for(int i = 0; i < len; ++i){
        std::string al_one = al_pre[i];
        al[i] = std::stod(al_one);
      }
      std::sort(al.begin(), al.end());
      return(al);
    }else{
      std::vector<double> al(1);
      al[0] = std::stod(hap);
      return(al);
    }
  }
}

/*Calculate mutational step between victim alleles and reference alleles (testthat)*/
// [[Rcpp::export]]
int calc_mu_step(std::vector<double> v_al, std::vector<double> r_al){
  int mu_step = 0;

  int v_al_size = v_al.size();
  int r_al_size = r_al.size();
  bool same_vr = true;
  if(v_al_size == r_al_size){
    for(int i = 0; i < v_al_size; ++i){
      bool same_al = v_al[i] == r_al[i];
      if(!same_al){
        same_vr = false;
        break;
      }
    }
  }else{
    same_vr = false;
  }

  if(same_vr == false){
    std::vector<int> diff(v_al_size * r_al_size, 99);
    int pos = 0;
    for(int i = 0; i < v_al_size; ++i){
      double v1 = v_al[i];
      for(int j = 0; j < r_al_size; ++j){
        double r1 = r_al[j];
        double d = 99;
        if(v1 > r1){
          d = v1 - r1;
        }else if(v1 < r1){
          d = r1 - v1;
        }
        if(is_integer(d)){
          diff[pos] = (int)d;
        }
        pos = pos + 1;
      }
    }
    mu_step = *min_element(diff.begin(), diff.end());
  }
  return(mu_step);
}

/*Matching victim and reference Y haplotypes (testthat)*/
// [[Rcpp::export]]
std::vector<std::vector<int>> match_y(std::vector<std::string> prof_victim, std::vector<std::string> prof_ref){
  int n_l = prof_victim.size();
  std::vector<std::vector<int>> ans(3, std::vector<int>(n_l + 1));
  int sum_l_0 = 0;
  int sum_l_1 = 0;
  int max_mu_step = 0;
  for(int i = 0; i < n_l; ++i){
    std::string v_al_pre = prof_victim[i];
    std::vector<double> v_al = obtain_al(v_al_pre);
    std::string r_al_pre = prof_ref[i];
    std::vector<double> r_al = obtain_al(r_al_pre);

    int v_al_size = v_al.size();
    int r_al_size = r_al.size();

    /*ignore*/
    if(v_al_size == 0 || r_al_size == 0){
      ans.at(1).at(i) = 1;
      sum_l_1 += 1;
    }else{
      bool same_vr = true;
      if(v_al_size == r_al_size){
        for(int j = 0; j < v_al_size; ++j){
          bool same_al = v_al[j] == r_al[j];
          if(!same_al){
            same_vr = false;
            break;
          }
        }
      }else{
        same_vr = false;
      }

      if(same_vr == false){
        std::vector<double> only_v_al;
        std::set_difference(v_al.begin(), v_al.end(), r_al.begin(), r_al.end(), inserter(only_v_al, only_v_al.end()));

        /*ignore*/
        if(only_v_al.size() == 0){
          ans.at(1).at(i) = 1;
          sum_l_1 += 1;

          /*not ignore*/
        }else{
          ans.at(0).at(i) = 1;
          sum_l_0 += 1;
          int mu_step = calc_mu_step(v_al, r_al);
          ans.at(2).at(i) = mu_step;
          if(max_mu_step < mu_step){
            max_mu_step = mu_step;
          }
        }
      }
    }
  }
  ans.at(0).at(n_l) = sum_l_0;
  ans.at(1).at(n_l) = sum_l_1;
  ans.at(2).at(n_l) = max_mu_step;
  return(ans);
}

// [[Rcpp::depends(RcppProgress)]]
// [[Rcpp::export]]
std::vector<std::vector<std::vector<int>>> match_y_all(std::vector<std::vector<std::string>> hap_v_y,
                                                       std::vector<std::vector<std::string>> hap_r_y){
  int n_v = hap_v_y.size();
  int n_r = hap_r_y.size();
  int n_l = hap_r_y.at(0).size();

  int n_vr = n_v * n_r;
  Progress p(n_vr, true);

  std::vector<std::vector<std::vector<int>>> result_y(n_vr, std::vector<std::vector<int>>(3, std::vector<int>(n_l + 1)));

  for(int i = 0; i < n_r; ++i){
    std::vector<std::string> prof_ref = hap_r_y.at(i);
    for(int j = 0; j < n_v; ++j){
      p.increment();

      std::vector<std::string> prof_victim = hap_v_y.at(j);
      std::vector<std::vector<int>> ans = match_y(prof_victim, prof_ref);

      int pos = n_v * i + j;
      result_y.at(pos).at(0) = ans.at(0);
      result_y.at(pos).at(1) = ans.at(1);
      result_y.at(pos).at(2) = ans.at(2);
    }
  }
  return(result_y);
}

