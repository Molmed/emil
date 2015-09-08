#include <Rcpp.h>
using namespace Rcpp;

// Check if a vector or matrix contains missing values
//
// @param x A vector, matrix, or array of the appropriate type.
// @return A logical scalar.
// @author Christofer Backlin

// [[Rcpp::export]]
LogicalVector has_na_character(CharacterVector x) {
    int i;
    int n = x.size();
    for(i = 0; i < n && !CharacterVector::is_na(x[i]); i++);
    return i < n;
}

// [[Rcpp::export]]
LogicalVector has_na_complex(ComplexVector x) {
    int i;
    int n = x.size();
    for(i = 0; i < n && !ComplexVector::is_na(x[i]); i++);
    return i < n;
}

// [[Rcpp::export]]
LogicalVector has_na_expression(ExpressionVector x) {
    int i;
    int n = x.size();
    for(i = 0; i < n && !ExpressionVector::is_na(x[i]); i++);
    return i < n;
}

// [[Rcpp::export]]
LogicalVector has_na_numeric(NumericVector x) {
    int i;
    int n = x.size();
    for(i = 0; i < n && !NumericVector::is_na(x[i]); i++);
    return i < n;
}

