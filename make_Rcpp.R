#!/usr/bin/R

requireNamespace("Rcpp")
Rcpp::compileAttributes(pkgdir = ".", verbose = getOption("verbose"))

