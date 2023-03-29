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

// [[Rcpp::export]]
bool is_integer(double x){
  return(std::floor(x) == x);
}

/*Calculate mutational step between query alleles and reference alleles (testthat)*/
// [[Rcpp::export]]
int calc_mu_step(std::vector<double> q_al, std::vector<double> r_al){
  int mu_step = 0;

  int q_al_size = q_al.size();
  int r_al_size = r_al.size();
  bool same_qr = true;
  if(q_al_size == r_al_size){
    for(int i = 0; i < q_al_size; ++i){
      bool same_al = q_al[i] == r_al[i];
      if(!same_al){
        same_qr = false;
        break;
      }
    }
  }else{
    same_qr = false;
  }

  if(same_qr == false){
    std::vector<int> diff(q_al_size * r_al_size, 99);
    int pos = 0;
    for(int i = 0; i < q_al_size; ++i){
      double q1 = q_al[i];
      for(int j = 0; j < r_al_size; ++j){
        double r1 = r_al[j];
        double d = 99;
        if(q1 > r1){
          d = q1 - r1;
        }else if(q1 < r1){
          d = r1 - q1;
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

/*Matching query and reference Y haplotypes (testthat)*/
//' @export
// [[Rcpp::export]]
std::vector<std::vector<int>> match_y(std::vector<std::string> query, std::vector<std::string> ref){
  int n_l = query.size();
  std::vector<std::vector<int>> ans(3, std::vector<int>(n_l + 1));
  int sum_l_0 = 0;
  int sum_l_1 = 0;
  int max_mu_step = 0;
  for(int i = 0; i < n_l; ++i){
    std::string q_al_pre = query[i];
    std::vector<double> q_al = obtain_al(q_al_pre);
    std::string r_al_pre = ref[i];
    std::vector<double> r_al = obtain_al(r_al_pre);

    int q_al_size = q_al.size();
    int r_al_size = r_al.size();
    bool same_qr = true;
    if(q_al_size == r_al_size){
      for(int i = 0; i < q_al_size; ++i){
        bool same_al = q_al[i] == r_al[i];
        if(!same_al){
          same_qr = false;
          break;
        }
      }
    }else{
      same_qr = false;
    }

    /*mismatch or not*/
    if(same_qr == false){
      /*ignore*/
      std::vector<double> qr_al;
      std::set_union(q_al.begin(), q_al.end(), r_al.begin(), r_al.end(), inserter(qr_al, qr_al.end()));

      std::vector<double> only_r_al;
      std::set_difference(r_al.begin(), r_al.end(), qr_al.begin(), qr_al.end(), inserter(only_r_al, only_r_al.end()));

      if(only_r_al.size() == 0){
        ans.at(1).at(i) = 1;
        sum_l_1 = sum_l_1 + 1;
      /*not ignore*/
      }else{
        ans.at(0).at(i) = 1;
        sum_l_0 = sum_l_0 + 1;
        int mu_step = calc_mu_step(q_al, r_al);
        ans.at(2).at(i) = mu_step;
        if(max_mu_step < mu_step){
          max_mu_step = mu_step;
        }
      }
    }
  }
  ans.at(0).at(n_l) = sum_l_0;
  ans.at(1).at(n_l) = sum_l_1;
  ans.at(2).at(n_l) = max_mu_step;
  return(ans);
}

