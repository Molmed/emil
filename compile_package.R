#!/usr/bin/R

requireNamespace("Rcpp")
Rcpp::compileAttributes(pkgdir = ".", verbose = getOption("verbose"))

requireNamespace("roxygen2")
roxygen2::roxygenize()

