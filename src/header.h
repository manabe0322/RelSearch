#include <Rcpp.h>
#include <stdio.h>
#include <iostream>
#include <string>
#include <vector>
using namespace Rcpp;

std::vector<std::string> split(std::string str, const char* del);
int str_to_int(std::string str);
std::string int_to_str(int num);
std::vector<int> tousa(int start, int end, int interval);
int searchPos(NumericVector vec, double target);
