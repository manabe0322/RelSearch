#include "header.h"

/*###############################################################################
# The function to obtain the numeric alleles from a character object (testthat) #
###############################################################################*/

// [[Rcpp::export]]
std::vector<double> obtain_al(std::string hap){

  /* Remove spaces */
  hap.erase(std::remove(hap.begin(), hap.end(), ' '), hap.end());

  /* No detected allele */
  if(hap == ""){

    /* Define an empty object */
    std::vector<double> al(0);

    /* Return */
    return(al);

  /* At least one allele is detected */
  }else{

    /* Two or more alleles are detected */
    if(hap.find(",") != std::string::npos){

      /* Separate alleles based on the comma */
      const char* del = ",";
      std::vector<std::string> al_pre = split(hap, del);

      /* String to double */
      int len = al_pre.size();
      std::vector<double> al(len);
      for(int i = 0; i < len; ++i){
        std::string al_one = al_pre[i];
        al[i] = std::stod(al_one);
      }

      /* Sort */
      std::sort(al.begin(), al.end());

      /* Return */
      return(al);

    /* One allele is detected */
    }else{
      std::vector<double> al(1);

      /* String to double */
      al[0] = std::stod(hap);

      /* Return */
      return(al);
    }
  }
}


/*###################################################################################################
# The function to calculate mutational step between victim alleles and reference alleles (testthat) #
###################################################################################################*/

// [[Rcpp::export]]
int calc_mu_step(std::vector<double> v_al, std::vector<double> r_al){

  /* Define the initial mutational step */
  int mu_step = 0;

  /* The number of victim's alleles */
  int v_al_size = v_al.size();

  /* The number of reference's alleles */
  int r_al_size = r_al.size();

  /* Check whether the victim's alleles are the same as the reference's alleles or not */
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

  /* If the victim's alleles are not the same as the reference's alleles */
  if(same_vr == false){

    /* Define an object to record each mutational step */
    std::vector<int> diff(v_al_size * r_al_size, 99);

    /* Define the initial index for the object "diff" */
    int pos = 0;

    /* Repetitive execution for the victim's alleles */
    for(int i = 0; i < v_al_size; ++i){

      /* Extract a victim's allele */
      double v1 = v_al[i];

      /* Repetitive execution for the reference's alleles */
      for(int j = 0; j < r_al_size; ++j){

        /* Extract a reference's allele */
        double r1 = r_al[j];

        /* Calculate the mutational step */
        double d = 99;
        if(v1 > r1){
          d = v1 - r1;
        }else if(v1 < r1){
          d = r1 - v1;
        }

        /* Record the step if it is the integer */
        if(is_integer(d)){
          diff[pos] = (int)d;
        }

        /* Update the index for the object "diff" */
        pos = pos + 1;
      }
    }

    /* Investigate the minimum value of mutational steps */
    mu_step = *min_element(diff.begin(), diff.end());
  }

  /* Return */
  return(mu_step);
}


/*###########################################################
# The function to analyze Y-STR data of one pair (testthat) #
###########################################################*/

// [[Rcpp::export]]
std::vector<std::vector<int>> match_y(std::vector<std::string> prof_victim, std::vector<std::string> prof_ref){

  /* The number of loci */
  int n_l = prof_victim.size();

  /* Define an object for saving all results */
  std::vector<std::vector<int>> ans(3, std::vector<int>(n_l + 1));

  /* Define an object for saving the total number of mismatched loci */
  int sum_l_0 = 0;

  /* Define an object for saving the total number of ignoreed loci */
  int sum_l_1 = 0;

  /* Define an object for saving the sum of the mutational steps */
  int sum_mu_step = 0;

  /* Repetitive execution for loci */
  for(int i = 0; i < n_l; ++i){

    /* Extract the victim's alleles in one locus */
    std::string v_al_pre = prof_victim[i];
    std::vector<double> v_al = obtain_al(v_al_pre);

    /* Extract the reference's alleles in one locus */
    std::string r_al_pre = prof_ref[i];
    std::vector<double> r_al = obtain_al(r_al_pre);

    /* The number of victim's alleles */
    int v_al_size = v_al.size();

    /* The number of reference's alleles */
    int r_al_size = r_al.size();

    /* Ignored locus */
    if(v_al_size == 0 || r_al_size == 0){
      ans.at(1).at(i) = 1;
      sum_l_1 += 1;

    /* Not ignored locus */
    }else{

      /* Check whether the victim's alleles are the same as the reference's alleles or not */
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

      /* If the victim's alleles are not the same as the reference's alleles */
      if(same_vr == false){

        /* Unique victim's alleles */
        std::vector<double> only_v_al;
        std::set_difference(v_al.begin(), v_al.end(), r_al.begin(), r_al.end(), inserter(only_v_al, only_v_al.end()));

        /* Ignored locus (explained by allelic drop-out) */
        if(only_v_al.size() == 0){
          ans.at(1).at(i) = 1;
          sum_l_1 += 1;

        /* Mismatched locus */
        }else{
          ans.at(0).at(i) = 1;
          sum_l_0 += 1;

          /* Calculate the mutational step */
          int mu_step = calc_mu_step(v_al, r_al);
          ans.at(2).at(i) = mu_step;
          sum_mu_step += mu_step;
        }
      }
    }
  }

  /* Assign results */
  ans.at(0).at(n_l) = sum_l_0;
  ans.at(1).at(n_l) = sum_l_1;
  ans.at(2).at(n_l) = sum_mu_step;

  /* Return */
  return(ans);
}


/*#################################################
# The function to analyze Y-STR data of all pairs #
#################################################*/

// [[Rcpp::export]]
std::vector<std::vector<std::vector<int>>> match_y_all(std::vector<std::vector<std::string>> hap_v_y,
                                                       std::vector<std::vector<std::string>> hap_r_y){
  /* Call the R function "message" */
  Function message("message");

  /* The number of victims */
  int n_v = hap_v_y.size();

  /* The number of references */
  int n_r = hap_r_y.size();

  /* The number of loci */
  int n_l = hap_r_y.at(0).size();

  /* The number of pairs */
  int n_vr = n_v * n_r;

  /* Define an object for saving results */
  std::vector<std::vector<std::vector<int>>> result_y(n_vr, std::vector<std::vector<int>>(3, std::vector<int>(n_l + 1)));

  /* Define the counter for updating the progress bar */
  int counter_base = n_vr * 0.001;
  int counter = counter_base;

  /* Repetitive execution for references */
  for(int i = 0; i < n_r; ++i){

    /* Extract the profile of one reference */
    std::vector<std::string> prof_ref = hap_r_y.at(i);

    /* Repetitive execution for victims */
    for(int j = 0; j < n_v; ++j){

      /* Extract the profile of one victim */
      std::vector<std::string> prof_victim = hap_v_y.at(j);

      /* Analyze Y-STR data of one pair */
      std::vector<std::vector<int>> ans = match_y(prof_victim, prof_ref);

      /* Define the index for assigning the result */
      int pos = n_v * i + j;

      /* Assign the result */
      result_y.at(pos).at(0) = ans.at(0);
      result_y.at(pos).at(1) = ans.at(1);
      result_y.at(pos).at(2) = ans.at(2);

      /* Display a message to the console to update the progress bar */
      if(pos >= counter){
        std::string txt_console = "Y-STR_Victim-Reference_ : ";
        txt_console += int_to_str(pos);
        message('\r', txt_console, _["appendLF"] = false);

        /* Update the counter */
        counter += counter_base;
      }
    }
  }

  /* Return */
  return(result_y);
}

