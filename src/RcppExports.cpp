// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// compress
int compress(NumericVector onehot);
RcppExport SEXP _XalUtil_compress(SEXP onehotSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type onehot(onehotSEXP);
    rcpp_result_gen = Rcpp::wrap(compress(onehot));
    return rcpp_result_gen;
END_RCPP
}
// decompress
NumericVector decompress(int compressed, int size);
RcppExport SEXP _XalUtil_decompress(SEXP compressedSEXP, SEXP sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type compressed(compressedSEXP);
    Rcpp::traits::input_parameter< int >::type size(sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(decompress(compressed, size));
    return rcpp_result_gen;
END_RCPP
}
// compress_block
NumericVector compress_block(NumericVector onehot, int rows, int cols);
RcppExport SEXP _XalUtil_compress_block(SEXP onehotSEXP, SEXP rowsSEXP, SEXP colsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type onehot(onehotSEXP);
    Rcpp::traits::input_parameter< int >::type rows(rowsSEXP);
    Rcpp::traits::input_parameter< int >::type cols(colsSEXP);
    rcpp_result_gen = Rcpp::wrap(compress_block(onehot, rows, cols));
    return rcpp_result_gen;
END_RCPP
}
// decompress_block
NumericVector decompress_block(NumericVector onehot, int rows, int cols);
RcppExport SEXP _XalUtil_decompress_block(SEXP onehotSEXP, SEXP rowsSEXP, SEXP colsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type onehot(onehotSEXP);
    Rcpp::traits::input_parameter< int >::type rows(rowsSEXP);
    Rcpp::traits::input_parameter< int >::type cols(colsSEXP);
    rcpp_result_gen = Rcpp::wrap(decompress_block(onehot, rows, cols));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_XalUtil_compress", (DL_FUNC) &_XalUtil_compress, 1},
    {"_XalUtil_decompress", (DL_FUNC) &_XalUtil_decompress, 2},
    {"_XalUtil_compress_block", (DL_FUNC) &_XalUtil_compress_block, 3},
    {"_XalUtil_decompress_block", (DL_FUNC) &_XalUtil_decompress_block, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_XalUtil(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
