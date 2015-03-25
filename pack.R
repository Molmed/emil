
library(devtools)
bump.version()
check(".", cran=TRUE)

install()

check(".", args="--no-examples")

#-----------------------------------------------------------------[ Distribute ]

f <- file.info(dir(, "emil.*\\.tar\\.gz"))
new.build <- rownames(f)[which.max(f$mtime)]

# Laptop --> Uppmax
uppmax.repo()
file.copy(new.build, "~/mnt/uppmax-repo/src/contrib") # The p2010042 repo
tools::write_PACKAGES("~/mnt/uppmax-repo/src/contrib")
system(paste("scp", new.build, "uppmax:~/R-repos/b2010028/src/contrib"))
tools::write_PACKAGES("~/R-repos/b2010028/src/contrib") # On Uppmax

# Laptop --> Tank
system(paste("scp", new.build, "tank:~/R_packages/src/contrib"))
tools::write_PACKAGES("~/R_packages/src/contrib") # On Tank
install.packages("emil")

# Uppmax --> Tank
system("~/bin/tank0.sh")
system(paste("scp", new.build, "tank:~/R_packages/src/contrib"))
tools::write_PACKAGES("~/R_packages/src/contrib") # On tank

# On Uppmax or Tank
install.packages("emil")

#---------------------------------------------------------------------[ Commit ]

# Big change? Remember to put it in the git repo!
system("git status")
system("git commit -a")
system("git pull origin develop")
system("git push origin develop")
system("git push public develop")

install_github("molmed/emil/emil@develop")

