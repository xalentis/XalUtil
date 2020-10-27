#include <Rcpp.h>
using namespace Rcpp;



//' Compresses a one-hot vector
//'
//' @param A vector of one-hot values, maximum size 8 elements
//' @return A compressed integer
// [[Rcpp::export]]
int compress(NumericVector onehot) {
  int result = 0;
  int elements = onehot.size();
  if (elements > 8)
  {
    throw(Rcpp::exception("Elements exceed 8 bits","XalUtil.cpp",4));
    return 0;
  }
  for (int i=0; i<elements;i++)
  {
    if (onehot[i] == 1) ((result) |= (1ULL<<(elements-1-i)));
  }

  return result;
}

//' Decompresses an integer to one-hot vector
//'
//' @param An integer (<8 bits wide) to decompress
//' @return A one-hot vector
// [[Rcpp::export]]
NumericVector decompress(int compressed, int size) {
  NumericVector result (size);

  for (int i=0; i<=size;i++)
  {
    result[size-i-1]=(!!((compressed) & (1ULL<<(i))));
  }

  return result;
}
