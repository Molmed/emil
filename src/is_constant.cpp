#include <Rcpp.h>
using namespace Rcpp;

// Check if a vector contains more than one unique value
//
// @param x A vector, matrix, or array of the appropriate type.
// @param na_rm Whether to ignore missing values.
// @return See the R-wrapper in ../R/RcppWrappers.r

// [[Rcpp::export]]
LogicalVector is_constant_character(CharacterVector x, bool na_rm){
    int n = x.size();

    int i = 0;
    while(i < n && CharacterVector::is_na(x[i])) i++; // Find first non-missing value
    if(i == n) return LogicalVector::create(NA_LOGICAL); // x has no non-missing values

    int j = i + 1;
    while(j < n && x[j] == x[i]) j++; // Find next value different from x[i]
    if(j == n){ // None was found
        if(i == 0 || na_rm) return true;
        else return LogicalVector::create(NA_LOGICAL);
    }
    if(!CharacterVector::is_na(x[j])) return false; // x contains at least two different values

    // Find next value that is not missing nor equal to x[i]
    while(j < n && (CharacterVector::is_na(x[j]) || x[j] == x[i])) j++;
    if(j == n){ // None was found
        if(na_rm) return true;
        else return LogicalVector::create(NA_LOGICAL);
    }
    return false;
}

// [[Rcpp::export]]
LogicalVector is_constant_complex(ComplexVector x, bool na_rm){
    int n = x.size();

    int i = 0;
    while(i < n && ComplexVector::is_na(x[i])) i++; // Find first non-missing value
    if(i == n) return LogicalVector::create(NA_LOGICAL); // x has no non-missing values

    int j = i + 1;
    while(j < n && x[j] == x[i]) j++; // Find next value different from x[i]
    if(j == n){ // None was found
        if(i == 0 || na_rm) return true;
        else return LogicalVector::create(NA_LOGICAL);
    }
    if(!ComplexVector::is_na(x[j])) return false; // x contains at least two different values

    // Find next value that is not missing nor equal to x[i]
    while(j < n && (ComplexVector::is_na(x[j]) || x[j] == x[i])) j++;
    if(j == n){ // None was found
        if(na_rm) return true;
        else return LogicalVector::create(NA_LOGICAL);
    }
    return false;
}

// [[Rcpp::export]]
LogicalVector is_constant_numeric(NumericVector x, bool na_rm){
    int n = x.size();

    int i = 0;
    while(i < n && NumericVector::is_na(x[i])) i++; // Find first non-missing value
    if(i == n) return LogicalVector::create(NA_LOGICAL); // x has no non-missing values

    int j = i + 1;
    while(j < n && x[j] == x[i]) j++; // Find next value different from x[i]
    if(j == n){ // None was found
        if(i == 0 || na_rm) return true;
        else return LogicalVector::create(NA_LOGICAL);
    }
    if(!NumericVector::is_na(x[j])) return false; // x contains at least two different values

    // Find next value that is not missing nor equal to x[i]
    while(j < n && (NumericVector::is_na(x[j]) || x[j] == x[i])) j++;
    if(j == n){ // None was found
        if(na_rm) return true;
        else return LogicalVector::create(NA_LOGICAL);
    }
    return false;
}

