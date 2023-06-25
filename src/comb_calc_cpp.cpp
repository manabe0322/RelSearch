#include "header.h"

// [[Rcpp::export]]
std::vector<int> search_pos_sn_comb(std::vector<std::string> sn_v_all,
                                    std::vector<std::string> sn_r_all,
                                    std::vector<std::string> sn_v_target,
                                    std::vector<std::string> sn_r_target){
  int n_r_target = sn_r_target.size();
  int n_v_target = sn_v_target.size();
  std::vector<int> pos_all(n_r_target * n_v_target);
  for(int i = 0; i < n_r_target; ++i){
    std::vector<int> pos_r = search_pos_string_all(sn_r_all, sn_r_target[i]);
    for(int j = 0; j < n_v_target; ++j){
      std::vector<int> pos_v = search_pos_string_all(sn_v_all, sn_v_target[j]);

      std::vector<int> pos_vr;
      std::set_intersection(pos_r.begin(), pos_r.end(), pos_v.begin(), pos_v.end(), std::inserter(pos_vr, pos_vr.end()));
      pos_all[n_v_target * i + j] = pos_vr[0];
    }
  }
  return(pos_all);
}
