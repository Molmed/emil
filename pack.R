source("update_description.R")
cd("~/Documents/R/egna paket/classify")
library("roxygen2")
#options(useFancyQuotes = FALSE)

roxygen.update.description()
roxygenize("classify", "classify.roxygen", unlink.target = TRUE)
system("rm -rf classify.roxygen/inst")
system("R CMD check classify.roxygen")

system("R CMD INSTALL classify.roxygen")
system("R CMD build classify.roxygen") # Build package
system(sprintf("scp %s backch@mumble:projects/R_packages",
               rev(dir(, "classify_.*\\.tar\\.gz"))[1]))

#system("R CMD check classify.roxygen --use-gct") # Check package with GC-torture
#system("R CMD INSTALL --build --clean classify.roxygen") # Build binary

