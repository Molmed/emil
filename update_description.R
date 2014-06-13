# Set the working directory to the root of a package and call this function to
# increases the version number in the files
#
#   <name>/DESCRIPTION
#   <name>/R/<name>-package.R
#
# where <name> is the name of the package ( = name of the working directory).

roxygen.update.description <- function(pkgName){
    if(missing(pkgName)) pkgName <- gsub(".*/", "", getwd())
    if(!pkgName %in% dir()) stop("Cannot update version number, are you in the right folder?")

    setwd(pkgName)
    tab <- read.table("DESCRIPTION", sep="\n",
                      quote="~", comment.char="~",
                      stringsAsFactors=FALSE, blank.lines.skip=FALSE)[[1]]
    i <- grep("^Version:", tab)
    build.no <- as.integer(sub(".*-", "", tab[i])) + 1
    tab[i] <- sub("\\d+$", build.no, tab[i])
    tab[i+1] <- paste("Date", format(Sys.time(), "%Y-%m-%d"), sep=": ")
    write(tab, file="DESCRIPTION")

    setwd("..")
}

