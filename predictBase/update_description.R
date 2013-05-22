# Set the working directory to the root of a package and call this function to
# increases the version number in the files
#
#   <name>/DESCRIPTION
#   <name>/R/<name>-package.R
#
# where <name> is the name of the package ( = name of the working directory).

roxygen.update.description <- function(){
    pkgName <- gsub(".*/", "", getwd())
    if(!pkgName %in% dir()) stop("Cannot update version number, are you in the right folder?")

    setwd(pkgName)
    system("cp DESCRIPTION DESCRIPTION.bak")
    tab <- read.table("DESCRIPTION", sep="\n",
                      quote="~", comment.char="~",
                      stringsAsFactors=FALSE, blank.lines.skip=FALSE)[[1]]
    i <- grep("^Version:", tab)
    build.no <- as.integer(sub(".*-", "", tab[i])) + 1
    tab[i] <- sub("\\d+$", build.no, tab[i])
    tab[i+1] <- paste("Date", format(Sys.time(), "%Y-%m-%d"), sep=": ")
    write(tab, file="DESCRIPTION")

    setwd("R")
    f <- sprintf("%s-package.R", pkgName)
    system(sprintf("cp %s %s.bak", f, f))
    tab <- read.table(f, sep="\n", quote="~", comment.char="~",
                      stringsAsFactors=FALSE, blank.lines.skip=FALSE)[[1]]
    i <- grep("Version:", tab)
    tab[i] <- sub("-\\d+", sprintf("-%i", build.no), tab[i])
    tab[i+1] <- sub("\\d{4}-\\d{2}-\\d{2}", format(Sys.time(), "%Y-%m-%d"), tab[i+1])
    write(tab, f)

    setwd("../..")
}

