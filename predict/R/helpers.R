##' Wrapper for several methods to test if a variable is empty
##'
##' This is mainly an internal function but as other dependent packages also
##' use it sometimes and it generally is quite handy to have it is exported for
##' public use.
##' 
##' @param x A variable.
##' @param false.triggers Whether \code{FALSE} should be considered as empty.
##' @return Logical telling if variable is blank.
##' @examples
##' is.blank(NULL)
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
is.blank <- function(x, false.triggers=FALSE){
    if(is.function(x)) return(FALSE) # Some of the tests below trigger warnings when used on functions
    is.null(x) ||
    length(x) == 0 ||
    all(is.na(x)) ||
    all(x=="") ||
    (false.triggers && all(!x))
}


##' Replace values with something else
##'
##' @param x Variable containg NAs.
##' @param pattern The values in \code{x} to be replaced. Can also be a
##'   function.
##' @param replacement The value which is to replace the values matching
##'   \code{pattern}.
##' @param invert Whether to fill all values except the ones matching
##'   \code{pattern}.
##' @return An imputed version of \code{x}.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @examples
##' fill(1:10, function(x) x %% 2 == 1, 0)
##' na.fill(c(1,2,NA,4,5), 3)
##' @export
fill <- function(x, pattern, replacement, invert=FALSE){
    if(is.function(pattern)){
        x[xor(invert, pattern(x))] <- replacement
    } else {
        x[xor(invert, x %in% pattern)] <- replacement
    }
    x
}
##' @rdname fill
##' @export
na.fill <- function(x, replacement){
    fill(x, is.na, replacement)
}


##' Test if a save prefix is valid
##' 
##' @param save.prefix String including path to directory and file prefix.
##' @param create.dir Create directory if needed.
##' @return Nothing, throws an error if unsuccessful.
##' @examples
##' \dontrun{save.test("myFolder")}
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @noRd
save.test <- function(save.prefix=NULL, create.dir){
    if(!is.blank(save.prefix)){
        save.path <- sub("/[^/]*$", "", sprintf("./%s", save.prefix))
        if(!file.exists(save.path)){
            if(missing(create.dir)){
                cat("Directory does not exist. Do you want to create it? [Y/n]\n")
                create.dir <- tolower(readline()) %in% c("", "y", "yes")
            }
            if(create.dir){
                dir.create(save.path, recursive=TRUE)
            } else {
                stop("Directory does not exist.")
            }
        }
        tryCatch({
            about <- "Classify package just tested if a path for saving results was valid. It did. Please delete this file."
            save(about, file=sprintf("%s/.savetest.Rdata", save.path))
            unlink(sprintf("%s/.savetest", save.path))
            rm(about)
        }, error=function(...){
            stop(sprintf("Could not save to prefix \"%s\"", save.path))
        })
    }
    invisible()
}


##' Recursive unlisting of nested lists
##' 
##' @param x Nested lists.
##' @param n Number of levels to unlist.
##' @examples
##' ll <- list(list(list(list(list("hello!")))))
##' rec.unlist(ll, 3)
##' @seealso unlist, subtree
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
rec.unlist <- function(x, n){
    if(is.list(x) && n > 0){
        return(unlist(rec.unlist(x, n-1), recursive=FALSE, use.names=FALSE))
    } else {
        return(x)
    }
}


##' Extract a subset of a tree of nested lists
##' 
##' This function can only be used to extract data, not to assign.
##' 
##' @param x List of lists.
##' @param i Indices to extract on the first level of the tree.
##' @param ... Indices to extract on subsequent levels.
##' @param fun \code{\link{lapply}} or \code{\link{sapply}} which either
##'   preserves the structure of the tree or simplifies it.
##' @param flatten How many levels from the root to flatten.
##' @return A subset of the list tree.
##' @examples
##' l <- list(a=1:3, b=4, c=5)
##' ll <- list(l, l, l, l)
##' lll <- list(cat=ll, mouse=ll, escalator=ll)
##' subtree(lll, 1:2, TRUE, "b")
##' @seealso ssubtree, rec.flat
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
subtree <- function(x, i, ..., fun=lapply, flatten=0){
    if(missing(i)) return(rec.unlist(x, flatten))
    rec.unlist(fun(x[i], function(xx) subtree(xx, ..., fun=fun)), flatten)
}
##' Shortcut for sapply subtree
##'
##' Same as \code{\link{subtree}} but with the argument \code{fun=sapply}.
##' 
##' @param ... Sent to \code{\link{subtree}}.
##' @seealso subtree
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
ssubtree <- function(...) subtree(..., fun=sapply)


##' Trapezoid rule numerical integration
##' 
##' @param x Integrand.
##' @param y Function values.
##' @return Area under function.
##' @examples
##' x <- seq(0, pi, length.out=100)
##' trapz(x, sin(x))
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @noRd
trapz <- function(x,y){
    idx <- order(x)
    x <- x[idx]
    y <- y[idx]
    idx <- !is.na(x) & !is.na(y)
    x <- x[idx]
    y <- y[idx]

    n <- length(x)
    if(n != length(y)) stop("x and y must have same length")
    sum((y[-1]+y[-n])/2 * (x[-1] - x[-n]))
}


##' Print a timestamped and indented log message
##' 
##' @param level Indentation level.
##' @param ... Sent to \code{\link{sprintf}}.
##' @param time Whether or not to print timestamp.
##' @param linebreak Whether to finish the message with a linebreak or not.
##' @param file Sent to \code{\link{cat}}.
##' @examples
##' 
##' equipment <- c("flashlight", "snacks", "pick")
##' {
##'     trace.msg(1, "Begin descent")
##'     trace.msg(2, "Oh no, forgot the %s!", sample(equipment, 1))
##'     trace.msg(2, "Hello? Can you throw it down to me?", time=FALSE)
##'     trace.msg(1, "Aw shucks, I'm coming back up.")
##' }
##' 
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
trace.msg <- function(level=1, ..., time=TRUE, linebreak=TRUE, file="")
    cat(sep="", file=file,
        # timestamp
        if(time) format(Sys.time(), "%d %b %H:%M") else
                 paste(rep(" ", 12), collapse=""),
        # indent
        rep("  ", level),
        # message
        sprintf(...),
        # linebreak
        if(linebreak) "\n" else "")
        
