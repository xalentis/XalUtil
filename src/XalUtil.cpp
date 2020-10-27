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

//' Compresses an array to one-hot vector
//'
//' @param Input array
//' @param Array row count
//' @param Array column count
//' @return A compressed vector
// [[Rcpp::export]]
NumericVector compress_block(NumericVector onehot, int rows, int cols) {
  NumericVector result (rows);

  int k=0;

  for( int r=0; r < rows; r++ ) {
    NumericVector temp(cols);

    int j=0;
    for (int c=0; c<cols; c++)
    {
      temp[j] = onehot[k];
      j++;
      k++;
    }
    int compressed = compress(temp);
    result[r] = compressed;
  }

  return result;
}

//' Decompresses an array to one-hot vector
//'
//' @param Input array
//' @param Array row count
//' @param Array column count
//' @return A decompressed array
// [[Rcpp::export]]
NumericVector decompress_block(NumericVector onehot, int rows, int cols) {
  NumericVector result (rows*cols);

  int k=0;

  for( int r=0; r < onehot.size(); r++ ) {
    NumericVector decompressed = decompress(onehot[r], cols);
    for (int c=0; c<cols;c++)
    {
      result[k] = decompressed[c];
      k++;
    }
  }

  result.attr("dim") = Dimension(rows, cols);
  return result;
}
