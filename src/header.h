#include <Rcpp.h>
#include <stdio.h>
#include <set>
#include <iostream>
#include <string>
#include <vector>
#include <numeric>
#include <algorithm>
#include <regex>
using namespace Rcpp;

std::vector<std::string> split(std::string str, const char* del);
int str_to_int(std::string str);
std::string int_to_str(int num);
std::vector<int> tousa(int start, int end, int interval);
int search_pos_double(std::vector<double> vec, double target);
int search_pos_int(std::vector<int> vec, int target);
int search_pos_string(std::vector<std::string> vec, std::string target);
int extract_integer(std::string target);
bool is_integer(double x);
std::vector<std::string> setdiff_string(std::vector<std::string> vec_str1, std::vector<std::string> vec_str2);
std::vector<int> setdiff_int(std::vector<int> vec_int1, std::vector<int> vec_int2);
std::vector<double> setdiff_double(std::vector<double> vec_num1, std::vector<double> vec_num2);
