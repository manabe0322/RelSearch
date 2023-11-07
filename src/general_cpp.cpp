#include "header.h"

/*#########################################################
# The function to split a string object using a character #
#########################################################*/

// [[Rcpp::export]]
std::vector<std::string> split(std::string str, const char* del){
  auto first = 0;
  auto last = str.find_first_of(del);
  int lenStr = str.size();
  std::vector<std::string> result;
  if(last == std::string::npos){
    result.push_back(str);
  }else{
    while(int(first) < lenStr){
      std::string subStr(str, first, last - first);
      result.push_back(subStr);
      first = last + 1;
      last = str.find_first_of(del, first);
      if(last == std::string::npos){
        last = lenStr;
      }
    }
  }
  return(result);
}


/*##############################################################
# The function to change a string object to the integer object #
##############################################################*/

// [[Rcpp::export]]
int str_to_int(std::string str){
  int num = atoi(str.c_str());
  return(num);
}


/*###############################################################
# The function to change an integer object to the string object #
###############################################################*/

// [[Rcpp::export]]
std::string int_to_str(int num){
  std::string str = std::to_string(num);
  return(str);
}


/*##########################################
# The function to make arithmetic sequence #
##########################################*/

// [[Rcpp::export]]
std::vector<int> tousa(int start, int end, int interval){
  int len = (end - start) / interval + 1;
  std::vector<int> vec(len);
  for(int i = 0; i < len; ++i){
    vec[i] = start + i * interval;
  }
  return(vec);
}


/*#######################################################
# The function to search an index for the double object #
#######################################################*/

// [[Rcpp::export]]
int search_pos_double(std::vector<double> vec, double target){
  int len = vec.size();
  int pos = len;
  for(int i = 0; i < len; ++i){
    if(vec[i] == target){
      pos = i;
      break;
    }
  }
  return(pos);
}


/*########################################################
# The function to search an index for the integer object #
########################################################*/

// [[Rcpp::export]]
int search_pos_int(std::vector<int> vec, int target){
  int len = vec.size();
  int pos = len;
  for(int i = 0; i < len; ++i){
    if(vec[i] == target){
      pos = i;
      break;
    }
  }
  return(pos);
}


/*#######################################################
# The function to search an index for the string object #
#######################################################*/

// [[Rcpp::export]]
int search_pos_string(std::vector<std::string> vec, std::string target){
  int len = vec.size();
  int pos = len;
  for(int i = 0; i < len; ++i){
    if(vec[i] == target){
      pos = i;
      break;
    }
  }
  return(pos);
}


/*###########################################################
# The function to extract an integer from the string object #
###########################################################*/

// [[Rcpp::export]]
int extract_integer(std::string target){
  const std::regex re("^0123456789.");
  std::string target_new = std::regex_replace(target, re, "");
  int integer = str_to_int(target_new);
  return(integer);
}


/*######################################################################
# The function to judge whether an object is the integer object or not #
######################################################################*/

// [[Rcpp::export]]
bool is_integer(double x){
  return(std::floor(x) == x);
}


/*#################################
# The setdiff function for string #
#################################*/

// [[Rcpp::export]]
std::vector<std::string> setdiff_string(std::vector<std::string> vec_str1, std::vector<std::string> vec_str2){
  std::vector<std::string> only_vec_str1;
  int n_str1 = vec_str1.size();
  int n_str2 = vec_str2.size();
  for(int i = 0; i < n_str1; ++i){
    std::string str1_1 = vec_str1[i];
    bool only = true;
    for(int j = 0; j < n_str2; ++j){
      std::string str2_1 = vec_str2[j];
      if(str1_1 == str2_1){
        only = false;
        break;
      }
    }
    if(only){
      only_vec_str1.push_back(str1_1);
    }
  }
  return(only_vec_str1);
}


/*##############################
# The setdiff function for int #
################################*/

// [[Rcpp::export]]
std::vector<int> setdiff_int(std::vector<int> vec_int1, std::vector<int> vec_int2){
  std::vector<int> only_vec_int1;
  int n_str1 = vec_int1.size();
  int n_str2 = vec_int2.size();
  for(int i = 0; i < n_str1; ++i){
    int str1_1 = vec_int1[i];
    bool only = true;
    for(int j = 0; j < n_str2; ++j){
      int str2_1 = vec_int2[j];
      if(str1_1 == str2_1){
        only = false;
        break;
      }
    }
    if(only){
      only_vec_int1.push_back(str1_1);
    }
  }
  return(only_vec_int1);
}


/*#################################
# The setdiff function for double #
#################################*/

// [[Rcpp::export]]
std::vector<double> setdiff_double(std::vector<double> vec_num1, std::vector<double> vec_num2){
  std::vector<double> only_vec_num1;
  int n_str1 = vec_num1.size();
  int n_str2 = vec_num2.size();
  for(int i = 0; i < n_str1; ++i){
    double str1_1 = vec_num1[i];
    bool only = true;
    for(int j = 0; j < n_str2; ++j){
      double str2_1 = vec_num2[j];
      if(str1_1 == str2_1){
        only = false;
        break;
      }
    }
    if(only){
      only_vec_num1.push_back(str1_1);
    }
  }
  return(only_vec_num1);
}
