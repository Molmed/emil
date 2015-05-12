#' Wrapper for several methods to test if a variable is empty
#'
#' This is mainly an internal function but as other dependent packages also
#' use it sometimes and it generally is quite handy to have it is exported for
#' public use.
#' 
#' @param x A variable.
#' @param false_triggers Whether \code{FALSE} should be considered as empty.
#' @return Logical telling if variable is blank.
#' @examples
#' is_blank(NULL)
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
is_blank <- function(x, false_triggers=FALSE){
    if(is.function(x)) return(FALSE) # Some of the tests below trigger warnings when used on functions
    is.null(x) ||
    length(x) == 0 ||
    all(is.na(x)) ||
    all(x=="") ||
    (false_triggers && all(!x))
}


#' Detect if modeling results contains multiple procedures
#' 
#' @param x Modeling results, as returned by \code{\link{evaluate}}.
#' @return Logical scalar.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
is_multi_procedure <- function(result){
    if(inherits(result, "modeling_result")){
        if(all(sapply(result, inherits, "model"))){
            return(FALSE)
        } else if(all(sapply(result, sapply, inherits, "model")) &&
                  all(sapply(result, length) == length(result[[1]]))){
            return(TRUE)
        }
    }
    stop("Invalid modeling result.")
}


#' Replace values with something else
#'
#' @param x Variable containing NAs.
#' @param pattern The values in \code{x} to be replaced. Can also be a
#'   function.
#' @param replacement The value which is to replace the values matching
#'   \code{pattern}.
#' @param invert Whether to fill all values except the ones matching
#'   \code{pattern}.
#' @return An imputed version of \code{x}.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @examples
#' fill(1:10, function(x) x %% 2 == 1, 0)
#' na_fill(c(1,2,NA,4,5), 3)
#' @export
fill <- function(x, pattern, replacement, invert=FALSE){
    if(is.function(pattern)){
        x[xor(invert, pattern(x))] <- replacement
    } else {
        x[xor(invert, x %in% pattern)] <- replacement
    }
    x
}
#' @rdname fill
#' @export
na_fill <- function(x, replacement){
    fill(x, is.na, replacement)
}


#' Load a package and offer to install if missing
#' 
#' If running R in interactive mode, the user is prompted for installing
#' missing packages. If running in batch mode an error is thrown.
#'
#' @param pkg Package name.
#' @param reason A status message that informs the user why the package is
#'   needed.
#' @return Nothing
#' @examples
#' nice_require("base", "is required to do anything at all")
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
nice_require <- function(pkg, reason){
    pkg.loaded <- sapply(pkg, requireNamespace)
    if(!all(pkg.loaded) && interactive()){
        pkg <- pkg[!pkg.loaded]
        if(missing(reason))
            reason <- paste(if(length(pkg) > 1) "are" else "is", "required but not installed")
        cat(sprintf("Package%s %s %s. Install now? (Y/n) ",
                    if(length(pkg) > 1) "s" else "",
                    paste("`", pkg, "`", sep="", collapse=", "),
                    reason))
        if(grepl("^(y(es)?)?$", tolower(readline()))){
            install.packages(pkg)
            pkg.loaded <- sapply(pkg, requireNamespace)
            if(!all(pkg.loaded))
                stop("Cannot load package `%s`.", pkg[!pkg.loaded])
        } else {
            stop(sprintf("%s was not installed.",
                         paste("`", pkg, "`", sep="", collapse=", ")))
        }
    }
}


#' Trapezoid rule numerical integration
#' 
#' Only intended for internal use.
#'
#' @param x Integrand.
#' @param y Function values.
#' @return Area under function.
#' @examples
#' x <- seq(0, pi, length.out=100)
#' trapz(x, sin(x))
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @noRd
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


