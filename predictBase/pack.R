cd("/home/christofer/Documents/R/egna paket/predictBase/predictBase")
source("../update_description.R")
library("roxygen2")
#options(useFancyQuotes = FALSE)

roxygen.update.description()
roxygenize("predictBase", "predictBase.roxygen", unlink.target = TRUE)
system("rm -rf predictBase.roxygen/inst")
clc()
system("R CMD check predictBase.roxygen")

system("R CMD INSTALL predictBase.roxygen")
system("R CMD build predictBase.roxygen") # Build package
system(sprintf("scp %s backch@mumble:~/R_packages/src/contrib",
               rev(dir(, "predictBase_.*\\.tar\\.gz"))[1]))
system(sprintf("scp %s chrib@kalkyl.uppmax.uu.se:R_packages/src/contrib",
               rev(dir(, "predictBase_.*\\.tar\\.gz"))[1]))
system("mv predictBase_*.tar.gz ../builds")

# Big change? Remember to put it in the git repo!
system("git status")
system("git commit -a")
system("git push")


#system("R CMD check predictBase.roxygen --use-gct") # Check package with GC-torture
#system("R CMD INSTALL --build --clean predictBase.roxygen") # Build binary

