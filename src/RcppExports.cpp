// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// is_constant_character
LogicalVector is_constant_character(CharacterVector x, bool na_rm);
RcppExport SEXP emil_is_constant_character(SEXP xSEXP, SEXP na_rmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    rcpp_result_gen = Rcpp::wrap(is_constant_character(x, na_rm));
    return rcpp_result_gen;
END_RCPP
}
// is_constant_complex
LogicalVector is_constant_complex(ComplexVector x, bool na_rm);
RcppExport SEXP emil_is_constant_complex(SEXP xSEXP, SEXP na_rmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< ComplexVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    rcpp_result_gen = Rcpp::wrap(is_constant_complex(x, na_rm));
    return rcpp_result_gen;
END_RCPP
}
// is_constant_numeric
LogicalVector is_constant_numeric(NumericVector x, bool na_rm);
RcppExport SEXP emil_is_constant_numeric(SEXP xSEXP, SEXP na_rmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< bool >::type na_rm(na_rmSEXP);
    rcpp_result_gen = Rcpp::wrap(is_constant_numeric(x, na_rm));
    return rcpp_result_gen;
END_RCPP
}
