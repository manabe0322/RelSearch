#include "header.h"

/*#########################################################################
# The function to extract the integer positions from the range (testthat) #
#########################################################################*/

// [[Rcpp::export]]
std::vector<int> extract_pos_mt(std::string range){

  /* Define the sign to separate the range */
  const char* del_1 = " ";
  const char* del_2 = "-";

  /* Separate the range at the space */
  std::vector<std::string> sep_range = split(range, del_1);

  /* The number of segments */
  int len = sep_range.size();

  /* Define the initial positions of all segments */
  std::vector<int> pos_mt(0);

  /* Repetitive execution for segments */
  for(int i = 0; i < len; ++i){

    /* Separate the range at the hyphen */
    std::vector<std::string> sep_range2 = split(sep_range[i], del_2);

    /* Start and end positions of one segment */
    int len2 = sep_range2.size();
    std::vector<int> fromto(len2);
    for(int j = 0; j < len2; ++j){
      fromto[j] = str_to_int(sep_range2[j]);
    }

    /* All integer positions of one segment */
    std::vector<int> pos_mt_sub = tousa(fromto[0], fromto[len2 - 1], 1);

    /* Update positions of all segments */
    pos_mt.insert(pos_mt.end(), pos_mt_sub.begin(), pos_mt_sub.end());
  }

  /* Return */
  return(pos_mt);
}


/*##########################################################################################
# The function to extract shared positions between the victim and the reference (testthat) #
##########################################################################################*/

// [[Rcpp::export]]
std::vector<int> extract_pos_mt_vr(std::string range_victim, std::string range_ref){

  /* Extract the integer positions for the victim */
  std::vector<int> pos_mt_v = extract_pos_mt(range_victim);
  std::sort(pos_mt_v.begin(), pos_mt_v.end());

  /* Extract the integer positions for the reference */
  std::vector<int> pos_mt_r = extract_pos_mt(range_ref);
  std::sort(pos_mt_r.begin(), pos_mt_r.end());

  /* Investigate the shared positions between the victim and the reference */
  std::vector<int> pos_mt_vr;
  std::set_intersection(pos_mt_v.begin(), pos_mt_v.end(), pos_mt_r.begin(), pos_mt_r.end(), std::inserter(pos_mt_vr, pos_mt_vr.end()));

  /* Return */
  return(pos_mt_vr);
}


/*#######################################################################################
# The function to make shared range between victim range and reference range (testthat) #
#######################################################################################*/

// [[Rcpp::export]]
std::string make_share_range(std::vector<int> pos_mt_vr){

  /* The number of shared positions */
  int len = pos_mt_vr.size();

  /* Record the end positions of each segment */
  std::vector<int> pos_break;
  pos_break.push_back(-1);
  for(int i = 1; i < len; ++i){
    int sa = pos_mt_vr[i] - pos_mt_vr[i - 1];
    if(sa != 1){
      pos_break.push_back(i - 1);
    }
  }
  pos_break.push_back(len - 1);

  /* The number of segments */
  int n_seg = pos_break.size() - 1;

  /* Define some character objects */
  std::string share_range = "";
  std::string hyphen = "-";
  std::string space = " ";

  /* Repetitive execution for segments */
  for(int i = 0; i < n_seg; ++i){

    /* Investigate the start position of one segment */
    int pos_break_1 = pos_break[i] + 1;
    int pos_from_int = pos_mt_vr[pos_break_1];
    std::string pos_from_str = int_to_str(pos_from_int);

    /* Investigate the end position of one segment */
    int pos_break_2 = pos_break[i + 1];
    int pos_to_int = pos_mt_vr[pos_break_2];
    std::string pos_to_str = int_to_str(pos_to_int);

    /* If the start position is the same as the end position */
    if(pos_break_1 == pos_break_2){
      share_range += pos_from_str;

    /* If the start position is not the same as the end position */
    }else{
      share_range += pos_from_str;
      share_range += hyphen;
      share_range += pos_to_str;
    }

    /* Add a space between segments */
    if(i < n_seg - 1){
      share_range += space;
    }
  }

  /* Return */
  return(share_range);
}


/*############################################################
# The function to analyze mtDNA data of one pairs (testthat) #
############################################################*/

// [[Rcpp::export]]
std::vector<std::string> match_mt(std::vector<std::string> prof_victim, std::string range_victim, std::vector<std::string> prof_ref, std::string range_ref){

  /* Extract shared positions between the victim and the reference */
  std::vector<int> pos_mt_vr = extract_pos_mt_vr(range_victim, range_ref);

  /* Define an object for saving the number of mismatches */
  std::string n_mismatch_str;

  /* Define an object for saving the shared range */
  std::string share_range;

  /* Shared length */
  int share_len = pos_mt_vr.size();

  /* If the shared length is greater than zero */
  if(share_len > 0){

    /* Make shared range */
    share_range = make_share_range(pos_mt_vr);

    /* Investigate the victim's type within the shared range */
    int n_type_v = prof_victim.size();
    std::vector<std::string> v_type_new;
    for(int i = 0; i < n_type_v; ++i){
      int pos_type_v = extract_integer(prof_victim[i]);
      if(std::count(pos_mt_vr.begin(), pos_mt_vr.end(), pos_type_v)){
        v_type_new.push_back(prof_victim[i]);
      }
    }

    /* Investigate the reference's type within the shared range */
    int n_type_r = prof_ref.size();
    std::vector<std::string> r_type_new;
    for(int i = 0; i < n_type_r; ++i){
      int pos_type_r = extract_integer(prof_ref[i]);
      if(std::count(pos_mt_vr.begin(), pos_mt_vr.end(), pos_type_r)){
        r_type_new.push_back(prof_ref[i]);
      }
    }

    /* Investigate the victim's specific type within the shared range */
    std::vector<std::string> only_v_type;
    std::set_difference(v_type_new.begin(), v_type_new.end(), r_type_new.begin(), r_type_new.end(), inserter(only_v_type, only_v_type.end()));

    /* Investigate the reference's specific type within the shared range */
    std::vector<std::string> only_r_type;
    std::set_difference(r_type_new.begin(), r_type_new.end(), v_type_new.begin(), v_type_new.end(), inserter(only_r_type, only_r_type.end()));

    /* Investigate the number of mismatches */
    int n_mismatch = only_v_type.size() + only_r_type.size();
    n_mismatch_str = int_to_str(n_mismatch);

  /* If the shared length is zero */
  }else{
    n_mismatch_str = "";
    share_range = "";
  }

  /* Change integer to string for the shared length */
  std::string share_len_str = int_to_str(share_len);

  /* Define an object for saving all results */
  std::vector<std::string> ans(3);

  /* Assign results */
  ans[0] = n_mismatch_str;
  ans[1] = share_range;
  ans[2] = share_len_str;

  /* Return */
  return(ans);
}


/*#################################################
# The function to analyze mtDNA data of all pairs #
#################################################*/

// [[Rcpp::export]]
std::vector<std::vector<std::string>> match_mt_all(std::vector<std::vector<std::string>> hap_v_mt,
                                                   std::vector<std::vector<std::string>> hap_r_mt,
                                                   std::vector<std::string> range_v_mt,
                                                   std::vector<std::string> range_r_mt){
  /* Call the R function "message" */
  Function message("message");

  /* The number of victims */
  int n_v = hap_v_mt.size();

  /* The number of references */
  int n_r = hap_r_mt.size();

  /* The number of pairs */
  int n_vr = n_v * n_r;

  /* Define an object for saving results */
  std::vector<std::vector<std::string>> result_mt(std::vector<std::vector<std::string>>(n_vr, std::vector<std::string>(3)));

  /* Define the counter for updating the progress bar */
  int counter_base = n_vr * 0.001;
  int counter = counter_base;

  /* Repetitive execution for references */
  for(int i = 0; i < n_r; ++i){

    /* Extract the profile of one reference */
    std::vector<std::string> prof_ref = hap_r_mt.at(i);

    /* Extract the range of one reference */
    std::string range_ref = range_r_mt[i];

    /* Repetitive execution for victims */
    for(int j = 0; j < n_v; ++j){

      /* Extract the profile of one victim */
      std::vector<std::string> prof_victim = hap_v_mt.at(j);

      /* Extract the range of one victim */
      std::string range_victim = range_v_mt[j];

      /* Analyze mtDNA data of one pair */
      std::vector<std::string> ans = match_mt(prof_victim, range_victim, prof_ref, range_ref);

      /* Define the index for assigning the result */
      int pos = n_v * i + j;

      /* Assign the result */
      result_mt.at(pos) = ans;

      /* Display a message to the console to update the progress bar */
      if(pos >= counter){
        std::string txt_console = "mtDNA_Victim-Reference_ : ";
        txt_console += int_to_str(pos);
        message('\r', txt_console, _["appendLF"] = false);

        /* Update the counter */
        counter += counter_base;
      }
    }
  }

  /* Return */
  return(result_mt);
}

