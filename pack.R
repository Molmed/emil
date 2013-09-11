cd("/home/christofer/Documents/R/egna paket/predict")
if(!exists("roxygen.update.description")) source("update_description.R")
library("roxygen2")

#----------------------------------------------------------------[ predictBase ]

cd("predictBase")
roxygen.update.description()
roxygenize("predictBase", "predictBase.roxygen", unlink.target = TRUE)
system("rm -rf predictBase.roxygen/inst")
system("R CMD check predictBase.roxygen")

system("R CMD INSTALL predictBase.roxygen")
system("R CMD build predictBase.roxygen") # Build package
system(sprintf("scp %s backch@tank:~/R_packages/src/contrib",
               rev(dir(, "predictBase_.*\\.tar\\.gz"))[1]))
system(sprintf("scp %s chrib@kalkyl.uppmax.uu.se:R_packages/src/contrib",
               rev(dir(, "predictBase_.*\\.tar\\.gz"))[1]))
system("mv predictBase_*.tar.gz ../builds")
cd("..")


#--------------------------------------------------------------------[ predict ]

cd("predict")
roxygen.update.description()
roxygenize("predict", "predict.roxygen", unlink.target = TRUE)
system("rm -rf predict.roxygen/inst")
system("R CMD check predict.roxygen")

system("R CMD INSTALL predict.roxygen")
system("R CMD build predict.roxygen") # Build package
system(sprintf("scp %s backch@tank:~/R_packages/src/contrib",
               rev(dir(, "predict_.*\\.tar\\.gz"))[1]))
system(sprintf("scp %s chrib@kalkyl.uppmax.uu.se:R_packages/src/contrib",
               rev(dir(, "predict_.*\\.tar\\.gz"))[1]))
system("mv predict_*.tar.gz ../builds")
cd("..")


#---------------------------------------------------------------------[ Commit ]

# Big change? Remember to put it in the git repo!
system("git status")
system("git commit -a")
system("git push")


#----------------------------------------------------[ When submitting to CRAN ]

system("R CMD check predictBase.roxygen --use-gct") # Check package with GC-torture
system("R CMD INSTALL --build --clean predictBase.roxygen") # Build binary


X <- matrix(rnorm(80*4), 80)
y <- gl(2, 40)
cv <- resample.crossval(y, 5, 5)
pred <- batch.predict(X, y, "nsc", test.subset=cv)
ssubtree(pred$cv, T, 1, "error")

