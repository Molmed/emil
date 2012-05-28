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
system(sprintf("scp %s chrib@kalkyl.uppmax.uu.se:R_packages",
               rev(dir(, "classify_.*\\.tar\\.gz"))[1]))

# Big change? Remember to put it in the git repo!
system("git status")
system("git commit -a")
system("git push")


#system("R CMD check classify.roxygen --use-gct") # Check package with GC-torture
#system("R CMD INSTALL --build --clean classify.roxygen") # Build binary

