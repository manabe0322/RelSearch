#include "header.h"

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

// [[Rcpp::export]]
int str_to_int(std::string str){
  int num = atoi(str.c_str());
  return(num);
}

// [[Rcpp::export]]
std::string int_to_str(int num){
  std::string str = std::to_string(num);
  return(str);
}

// [[Rcpp::export]]
std::vector<int> tousa(int start, int end, int interval){
  int len = (end - start) / interval + 1;
  std::vector<int> vec(len);
  for(int i = 0; i < len; ++i){
    vec[i] = start + i * interval;
  }
  return(vec);
}

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

// [[Rcpp::export]]
int extract_integer(std::string target){
  const std::regex re("^0123456789.");
  std::string target_new = std::regex_replace(target, re, "");
  int integer = str_to_int(target_new);
  return(integer);
}
