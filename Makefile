version := $(shell grep "^Version: " < DESCRIPTION | cut -c 10-)
pkg := emil_$(version).tar.gz

.PHONY: NAMESPACE

clean:
	rm -r emil.Rcheck
	rm *.Rout

build: $(pkg)

$(pkg): src/RcppExports.cpp NAMESPACE
	R CMD build .

src/RcppExports.cpp: make_Rcpp.R src/init.c src/is_constant.cpp
	Rscript $<

NAMESPACE: make_NAMESPACE.R
	Rscript $<

test: $(pkg)
	R CMD check $<

cran-test: $(pkg)
	R CMD check $< --as-cran
	grep "The Date field is over a month old." < emil.Rcheck/00check.log

install: test
	R CMD install $(pkg)
