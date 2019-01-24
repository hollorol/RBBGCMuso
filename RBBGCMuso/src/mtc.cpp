#include <Rcpp.h>
#include <numeric>
#include <iostream>
#include <algorithm>
#include <numeric>
#include <ctime>


using namespace Rcpp;
using namespace std;
extern "C" {    
        void mtc(char*);
    };

//' mtclim
//' 
//' This is the core mtclim function 
//' @importFrom Rcpp evalCpp
//' @useDynLib RBBGCMuso
//' @param iniFile is the name of the inifile
//' @keywords internal
//' @export 
// [[Rcpp::export]]
void mtclim(std::string iniFile){
  char *y = new char[iniFile.length() + 1]; // Allocate memory for char array input
  std::strcpy(y, iniFile.c_str()); // Copy c++ string to that input. 
  mtc(y);
}
