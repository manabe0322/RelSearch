/*Matching query and reference haplotypes of mtDNA (testthat)*/
/*//' @export
 // [[Rcpp::export]]
 std::vector<std::string> matchMt(std::string qHap, std::string qRan, std::string rHap, std::string rRan){
 std::vector<int> posMtQR = extPosMtQR(qRan, rRan);
 int lenShare_int = posMtQR.size();
 const char* del_1 = " ";
 std::vector<std::string> qHapSep = split(qHap, del_1);
 std::vector<std::string> rHapSep = split(rHap, del_1);
 if(lenShare_int > 0){
 Function f1("parse_number");
 std::string shareRange = makeShareRange(posMtQR);

 std::vector<double> qHap_1 = f1(qHap);
 std::vector<double> qHap_2 = round(qHap_1, 0);
 std::vector<int> qHap_3;
 qHap_3 = static_cast<std::vector<double>>(qHap_2);
 int len_q = qHap_3.length();
 std::vector<std::string> qHap_adopt;
 for(int i = 0; i < len_q; ++i){
 int posQ = searchPos_int(posMtQR, qHap_3[i]);
 if(posQ < lenShare_int){
 qHap_adopt.push_back(qHap[i]);
 }
 }
 CharacterVector qHap_rcpp = wrap(qHap_adopt);

 std::vector<double> rHap_1 = f1(rHap);
 std::vector<double> rHap_2 = round(rHap_1, 0);
 std::vector<int> rHap_3;
 rHap_3 = static_cast<std::vector<double>>(rHap_2);
 int len_r = rHap_3.length();
 std::vector<std::string> rHap_adopt;
 for(int i = 0; i < len_r; ++i){
 int posR = searchPos_int(posMtQR, rHap_3[i]);
 if(posR < lenShare_int){
 rHap_adopt.push_back(rHap[i]);
 }
 }
 CharacterVector rHap_rcpp = wrap(rHap_adopt);

 CharacterVector qrHap_rcpp = union(qHap_rcpp, rHap_rcpp);
 CharacterVector notQHap = setdiff(qrHap_rcpp, qHap_rcpp);
 CharacterVector notRHap = setdiff(qrHap_rcpp, rHap_rcpp);
 int lenNotQ = notQHap.length();
 int lenNotR = notRHap.length();
 int nMis_int = lenNotQ + lenNotR;
 std::string nMis = int_to_str(nMis_int);
 std::string lenShare = int_to_str(lenShare_int);
 std::vector<std::string> result;
 result.push_back(nMis);
 result.push_back(shareRange);
 result.push_back(lenShare);
 return(result);
 }else{
 std::string nMis = "";
 std::string shareRange = "";
 std::string lenShare = int_to_str(lenShare_int);
 result.push_back(nMis);
 result.push_back(shareRange);
 result.push_back(lenShare);
 }
 }*/
