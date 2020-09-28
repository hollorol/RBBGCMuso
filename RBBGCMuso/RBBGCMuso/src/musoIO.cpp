#include <Rcpp.h>
#include <numeric>
#include <iostream>
#include <fstream>
#include <string>
#include <new>
#include <algorithm>
#include <numeric>
#include <ctime>
#include <math.h>


using namespace Rcpp;
using namespace std;
// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
IntegerVector getWritePositions(double a){
  //getWritePositions returns abstract rownumbers to rownumbers and other indexek
  // getWritePositions(173.62) = c(173,7,3) // it supports up to 10 subvalues
  IntegerVector outVec(3); // outVec is vector of rowIndex, colNumber, choosen Index
  a = a * 100;
  a = round(a); //without this line 155.92 ~= 155.9199999999, (int) 155.9199999999*100 = 15591
  outVec[0] = (int)a / 100;
  outVec[1] = ((int)a /10) % 10 + 1;
  outVec[2] = (int)a % 10;
  
  
  return outVec;
}

IntegerMatrix getPositions(NumericVector v){

  int numVari = v.size();
  IntegerMatrix indexek(numVari,3);
  IntegerVector positions(3);
  for(int i = 0; i < numVari; ++i){
    positions = getWritePositions(v[i]);
    indexek(i,_) = positions;
  }
  return indexek;
}


void goNextLine(std::ifstream& fin){
  char c='a';
  while((c!='\n')&&(fin.get(c))){}
}

#define NEXT goNextLine(fin)
int fileChanger(std::string inFile, std::string outFile, IntegerVector linum, NumericVector num, IntegerVector colnum, IntegerVector colindex){
  std::ifstream fin(inFile);
  if (!fin.is_open()) {
    stop("Cannot open " + inFile + " for read");
  }
  std::ofstream fot(outFile);

  if (!fot.is_open()) {
    stop("Cannot open " + outFile + " for read");    
  }
  string tempString;

  int counter = 1;
  int counterV = 0;
  while (!fin.eof()) {


    if(counter == linum[counterV]){
      if(colnum[counterV]==1){
	fot << num[counterV] << "\n";
	NEXT;
      } else {
	double * elements;
	elements = new double [colnum[counterV]];
	if(linum[counterV]!=linum[counterV+1]){
	  for(int i=0;i<colnum[counterV];++i){

	    fin >> elements[i];
	    
	    if(i==colindex[counterV]){
	      elements[i]=num[counterV];
	    }
	    // std::cout << colnum[counterV] << " " << colindex[counterV] << " " << elements[i] << " "<< i <<"\n";
	    // std::cout << colindex[counterV] << getWritePositions(155.92) <<"\n";
	    // std::cout << "======================== \n";
	    fot << elements[i] << '\t';
	  }
	} else {
	  int k=0;
	  for(int i=0;i<colnum[counterV];++i){

	    fin >> elements[i];
	    if(i==colindex[counterV + k]){
	      elements[i]=num[counterV + k];
	      if(linum[counterV +k]==linum[counterV + k + 1]){
		++k; 
	      }
	    }
	    fot << elements[i] << '\t';

	  }
	  counterV = counterV + k;
	}

	fot << "\n";
	delete [] elements;
	NEXT;
      }
      ++counterV;
    } else {
      getline(fin,tempString);
      fot << tempString << "\n";
    }
    ++counter;
    if(counter > 1000){
      stop("You modified a line which has not as many columns as you specified.");
    }
  }
  fin.close();
  fot.close();
  return 0;
}

//' changeMusoC
//' 
//' This function is fastly randomize values based on min and max values
//' @importFrom Rcpp evalCpp
//' @useDynLib RBBGCMuso
//' @param inFile is the big matrix
//' @param outFile is the small matrix
//' @export
// [[Rcpp::export]]
void changeMusoC(std::string inFile, std::string outFile, NumericMatrix inMat){
  int  numChanges = inMat.nrow();
  IntegerMatrix indexes(numChanges,3);
  indexes = getPositions(inMat(_,0));
  fileChanger(inFile,outFile,indexes(_,0),inMat(_,1),indexes(_,1),indexes(_,2));
}
