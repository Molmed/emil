#cd("/home/christofer/Documents/R/egna paket/emil")
#if(!exists("roxygen.update.description")) source("update_description.R")
library("roxygen2")


#-----------------------------------------------------------------------[ emil ]

bump.version("emil")
roxygenize("emil")
system("R CMD build emil")
f <- file.info(dir(, "^emil_*\\.tar\\.gz"))
new.build <- rownames(f)[which.max(f$mtime)]
system(sprintf("R CMD check %s --as-cran", new.build))
system(sprintf("R CMD INSTALL %s", new.build))

# To just check examples
system(sprintf("R CMD check %s --no-clean --no-codoc --no-install --no-manual --no-vignettes", new.build))


#------------------------------------------------------------------[ emilPlots ]

library("roxygen2")
roxygen.update.description("emilPlots")
file.remove(dir("emilPlots/man", full.names=TRUE))
roxygenize("emilPlots")
system("R CMD check emilPlots")

system("R CMD INSTALL emilPlots")
system("R CMD build emilPlots")


#-----------------------------------------------------------------[ Distribute ]

install_github("backlin/emil@develop")

f <- file.info(dir(, "emil.*\\.tar\\.gz"))
new.build <- rownames(f)[which.max(f$mtime)]

# On laptop
uppmax.repo()
file.copy(new.build, "~/mnt/uppmax-repo/src/contrib") # The p2010042 repo
tools::write_PACKAGES("~/mnt/uppmax-repo/src/contrib")
system(paste("scp", new.build, "uppmax:~/R-repos/b2010028/src/contrib"))
tools::write_PACKAGES("~/R-repos/b2010028/src/contrib") # On uppmax
system(paste("scp", new.build, "tank:~/R_packages/src/contrib"))
tools::write_PACKAGES("~/R_packages/src/contrib") # On tank
system("mv emil*.tar.gz builds")

system("~/bin/tank0.sh")

system(paste("scp", new.build, "tank:~/R_packages/src/contrib"))
tools::write_PACKAGES("~/R_packages/src/contrib") # On tank

# With mounted uppmax repo
system(paste("cp", new.build, "~/R-repos/b2010028/src/contrib"))
tools::write_PACKAGES("~/R-repos/b2010028/src/contrib") # On uppmax

# Locally on uppmax
system(paste("cp", new.build, "~/R-repos/b2010028/src/contrib"))
tools::write_PACKAGES("~/R-repos/b2010028/src/contrib")
system(paste("cp", new.build, "~/R-repos/p2010042/src/contrib"))
tools::write_PACKAGES("~/R-repos/p2010042/src/contrib")


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

