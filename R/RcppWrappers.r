#' @useDynLib emil
#' @importFrom Rcpp evalCpp
NULL

#' Check if an object contains missing values
#' 
#' @param x Vector or matrix.
#' @return Logical scalar that is \code{TRUE} if \code{x} contains one or more
#'   missing values and \code{FALSE} if \code{x} contains no missing values.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
has_na <- function(x){
    UseMethod("has_na")
}
#' @export
has_na.data.frame <- function(x){
    Reduce(`any`, vapply(x, has_na, logical(1)))
}
#' @export
has_na.default <- function(x){
    switch(base::mode(x),
        character = has_na_character(x),
        complex = has_na_complex(x),
        expression = has_na_expression(x),
        logical = has_na_numeric(x),
        numeric = has_na_numeric(x), # Also catches integer and factor
        raw = stop("`has_na` does not support raw objects."),
        any(is.na(x)))
    # According to a test with microbenchmark there is no need to implement
    # separete methods for integer, logical, or factor ()
}

#' Check if an object contains more than one unique value
#' 
#' @param x Vector or matrix.
#' @param na.rm Whether to ignore missing values.
#' @return Logical scalar that is \code{TRUE} if \code{x} contains more than one
#'   unique value and \code{FALSE} if not.
#' @return A logical scalar that is \code{TRUE} if \code{x} contains more than
#'   one unique value and \code{FALSE} otherwise. In case \code{x} contains
#'   missing values \code{NA} is returned if \code{na_rm = FALSE}. If there are
#'   no non-missing values \code{NA} is always returned.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
is_constant <- function(x, na.rm=FALSE){
    switch(base::mode(x),
        character = is_constant_character(x, na.rm),
        complex = is_constant_complex(x, na.rm),
        logical = is_constant_numeric(x, na.rm),
        numeric = is_constant_numeric(x, na.rm),
        stop(sprintf("Objects of mode `%s` are not supported.", base::mode(x))))
}

