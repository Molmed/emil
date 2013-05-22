cd("~/Documents/R/egna paket/predict")
source("update_description.R")
library("roxygen2")
#options(useFancyQuotes = FALSE)

roxygen.update.description()
roxygenize("predict", "predict.roxygen", unlink.target = TRUE)
system("rm -rf predict.roxygen/inst")
system("R CMD check predict.roxygen")

system("R CMD INSTALL predict.roxygen")
system("R CMD build predict.roxygen") # Build package
system(sprintf("scp %s backch@mumble:~/R_packages/src/contrib/",
               rev(dir(, "predict_.*\\.tar\\.gz"))[1]))
system(sprintf("scp %s chrib@kalkyl.uppmax.uu.se:~/R_packages/src/contrib/",
               rev(dir(, "predict_.*\\.tar\\.gz"))[1]))

# Big change? Remember to put it in the git repo!
system("git status")
system("git commit -a")
system("git push")


#system("R CMD check predict.roxygen --use-gct") # Check package with GC-torture
#system("R CMD INSTALL --build --clean predict.roxygen") # Build binary

