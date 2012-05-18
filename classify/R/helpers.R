##' @import utils
{}

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
    return(
        is.null(x) ||
        length(x) == 0 ||
        all(is.na(x)) ||
        all(x=="") ||
        (false.triggers && all(!x))
    )
}


##' Replace NA with other value
##' 
##' @param x Vector, matrix or data frame.
##' @param fill The value to replace \code{NA} with.
##' @return \code{x} but with all \code{NA} replaced with \code{fill}.
##' @examples
##' a <- matrix(0, 3, 3)
##' a[2] <- NA
##' na.fill(a, 1)
##' is.blank(NULL)
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
na.fill <- function(x, fill){
    x[is.na(x)] <- fill
    return(x)
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


##' Stack a tree of list into a table
##' 
##' Practical for usage together with \code{\link{lattice}}.
##'
##' @method stack list
##' @param x Tree of lists.
##' @param names Column names of the data frame.
##' @param ... Ignored, only for S3 consistency.
##' @return The contents of \code{x} in the form of a stacked \code{data.frame}.
##' @examples
##' l <- list(a=1:3, b=4, c=5)
##' ll <- list(l, l, l, l)
##' lll <- list(cat=ll, mouse=ll, escalator=ll)
##' stack(ll)
##' my.stack <- stack(subtree(lll, 1:2, , "b"), names=c("Animal", "Index", "Type", "no.Legs"))
##' xyplot(Index ~ no.Legs | Animal, my.stack)
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso subtree
##' @export
stack.list <- function(x, names=NULL, ...){
    if(is.list(x)){
        nam <- if(is.null(names(x))) 1:length(x) else names(x)
        dat <- do.call(rbind, lapply(nam, function(nm) data.frame(ind=nm, v=stack.list(x[[nm]]))))
        if(!is.null(names)) names(dat) <- names
        return(dat)
    } else {
        return(x)
    }
}


##' Extract objects matching a set of criteria
##'
##' Can also be called for multiple groups. 
##'
##' @param x Data frame with sample descriptors.
##' @param strata A list containing either a number of criteria in the form of
##'   named elements, or lists containing criteria for multiple groups. The
##'   named element \code{a=c("b", "c")} will match all objects with attribute
##'   \code{a="b"} or \code{a="c"}.
##'
##'   The depth of \code{strata} is arbitrary but on no levels can lists and
##'   vectors be mixed.
##' @return A logical vector specifying which samples match the criteria, or a
##'   list of logical vectors for multiple groups.
##' @example examples/stratify.R
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
stratify <- function(x, strata){
#     # This was an idea about nifty recursive stratification.
#     # It appears unintuitive to the user though, but I don't
#     # want to throw it away just yet.
#     strat.func <- function(strata, idx){
#         for(field in names(strata)[!sapply(strata, is.list)])
#             idx[!x[[field]] %in% strata[[field]]] <- FALSE
#         return(c(list(idx), lapply(strata[sapply(strata, is.list)], function(s) strat.func(s, idx))))
#     }
#     return(strat.func(strata, rep(TRUE, nrow(x))))

    if(!is.blank(strata) && all(sapply(strata, is.list))){
        return(lapply(strata, function(s) stratify(x, s)))
    } else if(all(sapply(strata, is.vector))){
        idx <- rep(TRUE, nrow(x))
        for(field in names(strata))
            idx[!x[[field]] %in% strata[[field]]] <- FALSE
        return(idx)
    } else {
        stop("Invalid stratification criteria.")
    }
}


##' Trapezoid rule numerical integration
##' 
##' @param x Integrand.
##' @param y Function values.
##' @return Area under function.
##' @examples
##' x <- seq(0, pi, length=100)
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


##' Generate all combinations of variables
##' 
##' @param vars Names of the varibales to 
##' @param multi How many variables to choose e.g. \code{2} meaning all pairs of
##'   variables, \code{1:3} means all combinations of 1, 2, or 3 variables.
##' @return A list of variable combinations.
##' @examples
##' data(USArrests)
##' y <- factor(with(USArrests, UrbanPop > median(UrbanPop)), labels=c("Low", "High"))
##' test.set <- holdout.groups(y, 1/3)
##' var.sets <- variable.subsets(c("Murder", "Assault", "Rape"), 1:3)
##' n.error <- sapply(var.sets, function(vs){
##'     fit <- design("qda", USArrests[vs], y, subset=!test.set)
##'     sum(y[test.set] != predict(fit, USArrests[test.set, vs, drop=FALSE])$pred)
##' })
##' cat(sprintf("Best predictor(s): %s", paste(sapply(var.sets[n.error == min(n.error)], function(vs){
##'     paste(vs, collapse=" & ")
##' }), collapse=" or ")))
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
variable.subsets <- function(vars, multi){
    combs <- do.call(c, lapply(multi, function(m){
        as.list(as.data.frame(combn(vars, m), stringsAsFactors=FALSE))
    }))
    names(combs) <- NULL
    return(combs)
}

