# Developed on macOS High Sierra
source := $(shell git ls-files .)
version := $(shell grep -r "Version" DESCRIPTION | cut -c 10-)
pkg := emil_$(version).tar.gz

build: $(pkg)
	@echo "Built $<"

$(pkg): $(source) compile_package.R
	R -f compile_package.R
	R CMD build .

check: $(pkg)
	R CMD check $<

cran-check: $(pkg)
	R CMD check $< --as-cran

install: $(pkg)
	R CMD INSTALL $<

clean:
	rm -rf emil.Rcheck
	rm *.tar.gz
