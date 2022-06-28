#include "header.h"

// [[Rcpp::export]]
std::vector<std::string> split(std::string str, const char* del){
  int first = 0;
  int last = str.find_first_of(del);
  std::vector<std::string> result;
  while(first < str.size()){
    std::string subStr(str, first, last - first);
    result.push_back(subStr);
    first = last + 1;
    last = str.find_first_of(del, first);
    if(last == string::npos){
      last = str.size();
    }
  }
  std::sort(result.begin(), result.end());
  result.erase(std::unique(result.begin(), result.end()), result.end());
  return(result);
}

// [[Rcpp::export]]
int str_to_int(std::string str){
  int num = atoi(str.c_str());
  return(num);
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
int searchPos(NumericVector vec, double target){
  int len = vec.length();
  int pos = len;
  for(int i = 0; i < len; ++i){
    if(vec[i] == target){
      pos = i;
      break;
    }
  }
  return(pos);
}

