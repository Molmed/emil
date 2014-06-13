#cd("/home/christofer/Documents/R/egna paket/emil")
#if(!exists("roxygen.update.description")) source("update_description.R")
library("roxygen2")


#-----------------------------------------------------------------------[ emil ]

roxygen.update.description("emil")
doc.files <- setdiff(c(dir("emil/man", full.names=TRUE), "emil/NAMESPACE",
                       "emil/emil-Ex.R"),
                     sprintf("emil/man/%s.Rd", c("emil", "emil.extensions")))
file.remove(doc.files)
roxygenize("emil")
system("R CMD check emil")

# To just check examples
system("R CMD check emil --no-clean --no-codoc --no-install --no-manual --no-vignettes")

system("R CMD INSTALL emil")
system("R CMD build emil")


#------------------------------------------------------------------[ emilPlots ]

library("roxygen2")
roxygen.update.description("emilPlots")
file.remove(dir("emilPlots/man", full.names=TRUE))
roxygenize("emilPlots")
system("R CMD check emilPlots")

system("R CMD INSTALL emilPlots")
system("R CMD build emilPlots")


#-----------------------------------------------------------------[ Distribute ]

new.build <- tail(dir(, "emil.*\\.tar\\.gz"), 1)
system("~/bin/tank0.sh")
system(paste("scp", new.build, "tank:~/R_packages/src/contrib"))
tools::write_PACKAGES("~/R_packages/src/contrib") # On tank
system(paste("cp", new.build, "~/R-repos/b2010028/src/contrib"))
tools::write_PACKAGES("~/R-repos/b2010028/src/contrib")
system(paste("cp", new.build, "~/R-repos/p2010042/src/contrib"))
tools::write_PACKAGES("~/R-repos/p2010042/src/contrib")
system("mv emil*.tar.gz builds")


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

