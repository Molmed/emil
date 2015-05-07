
library(devtools)
bump.version()
check(".", cran=TRUE)

build()
library(testthat)
testenv <- new.env(parent=parent.env(global.env()))

install()

check(".", args="--no-examples")

#---------------------------------------------------------------------[ Commit ]

# Big change? Remember to put it in the git repo!
system("git status")
system("git commit -a")
system("git pull origin develop")
system("git push origin develop")
system("git push public develop")

install_github("molmed/emil/emil@develop")

