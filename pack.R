source("update_description.R")
library("roxygen2")
#options(useFancyQuotes = FALSE)

roxygen.update.description()
roxygenize("classifyBase", "classifyBase.roxygen", unlink.target = TRUE)
system("rm -rf classifyBase.roxygen/inst")
clc()
system("R CMD check classifyBase.roxygen")

system("R CMD INSTALL classifyBase.roxygen")
system("R CMD build classifyBase.roxygen") # Build package
system(sprintf("scp %s backch@mumble:R_packages/src/contrib",
               rev(dir(, "classifyBase_.*\\.tar\\.gz"))[1]))
system(sprintf("scp %s chrib@kalkyl.uppmax.uu.se:R_packages/src/contrib",
               rev(dir(, "classifyBase_.*\\.tar\\.gz"))[1]))

# Big change? Remember to put it in the git repo!
system("git status")
system("git commit -a")
system("git push")


#system("R CMD check classifyBase.roxygen --use-gct") # Check package with GC-torture
#system("R CMD INSTALL --build --clean classifyBase.roxygen") # Build binary

