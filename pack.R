#cd("/home/christofer/Documents/R/egna paket/emil")
#if(!exists("roxygen.update.description")) source("update_description.R")
library("roxygen2")

roxygen.update.description()
file.remove(c(dir("emil/man", full.names=TRUE), "emil/man",
              "emil/NAMESPACE", "emil/emil-Ex.R"))
roxygenize("emil")
system("R CMD check emil")

# To just check examples
system("R CMD check emil --no-clean --no-codoc --no-install --no-manual --no-vignettes")

system("R CMD INSTALL emil")
system("R CMD build emil") # Build package
system(sprintf("scp %s chrba104@tank:~/R_packages/src/contrib",
               rev(dir(, "emil_.*\\.tar\\.gz"))[1]))
system(sprintf("cp %s ~/R_packages/src/contrib",
               rev(dir(, "emil_.*\\.tar\\.gz"))[1]))
system("mv emil_*.tar.gz builds")


#---------------------------------------------------------------------[ Commit ]

# Big change? Remember to put it in the git repo!
system("git status")
system("git commit -a")
system("git push")


#----------------------------------------------------[ When submitting to CRAN ]

system("R CMD check emilBase.roxygen --use-gct") # Check package with GC-torture
system("R CMD INSTALL --build --clean emilBase.roxygen") # Build binary


X <- matrix(rnorm(80*4), 80)
y <- gl(2, 40)
cv <- resample.crossval(y, 5, 5)
pred <- batch.emil(X, y, "nsc", test.subset=cv)
subtree(pred$cv, T, 1, "error")

