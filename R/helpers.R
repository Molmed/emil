#' Wrapper for several methods to test if a variable is empty
#'
#' This is mainly an internal function but as other dependent packages also
#' use it sometimes and it generally is quite handy to have it is exported for
#' public use.
#' 
#' @param x A variable.
#' @param false.triggers Whether \code{FALSE} should be considered as empty.
#' @return Logical telling if variable is blank.
#' @examples
#' is.blank(NULL)
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
is.blank <- function(x, false.triggers=FALSE){
    if(is.function(x)) return(FALSE) # Some of the tests below trigger warnings when used on functions
    is.null(x) ||
    length(x) == 0 ||
    all(is.na(x)) ||
    all(x=="") ||
    (false.triggers && all(!x))
}


#' Detect if modeling results contains multiple procedures
#' 
#' @param x Modeling results, as returned by \code{\link{evaluate.modeling}}.
#' @return Logical scalar.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
is.multi.proc <- function(x){
    if(inherits(x, "modeling.result")){
        if(all(sapply(x, inherits, "model"))){
            return(FALSE)
        } else if(all(sapply(x, sapply, inherits, "model")) &&
                  all(sapply(x, length) == length(x[[1]]))){
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
#' na.fill(c(1,2,NA,4,5), 3)
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
na.fill <- function(x, replacement){
    fill(x, is.na, replacement)
}


#' Load a package and offer to install if missing
#' 
#' If running R in interactive mode, the user is prompted.
#'
#' @param pkg Package name.
#' @param reason A status message that informs the user why the package is
#'   needed.
#' @return Nothing
#' @examples
#' nice.require("base", "is required to do anything at all")
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
nice.require <- function(pkg, reason){
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


#' Compare true response to resampled predictions
#' 
#' This function can be used to compare a true response vector to predictions
#' returned from \code{\link{evaluate.modeling}}. For each fold, the correct
#' subset of the true response vector is extracted and fed to a given function 
#' together with the matching predictions.
#' 
#' @param fun Function to run.
#' @param resample Resampling scheme (see \code{\link{resample}}).
#' @param true True response vector.
#' @param pred Predictions, as returned from \code{\link{evaluate.modeling}}.
#' @param ... Sent to \code{\link{mapply}}.
#' @examples
#' proc <- modeling.procedure("lda")
#' ho <- resample("holdout", iris$Species, frac=1/3, nfold=4)
#' perf <- evaluate.modeling(proc, iris[-5], iris$Species, resample=ho)
#' confusion.tables <- resample.mapply(
#'     function(truth, prediction) table(truth, prediction$pred$pred),
#'     ho, iris$Species, pred=perf, SIMPLIFY=FALSE)
#' Reduce("+", confusion.tables)
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
resample.mapply <- function(fun, resample, true, pred, ...){
    mapply(fun,
           lapply(resample, function(fold) true[index.test(fold)]),
           pred, ...)
}


#' Extract a subset of a tree of nested lists
#' 
#' Modeling results produced by \code{\link{evaluate.modeling}} comes in the
#' form of nested lists. This function can be used to subset or rearrange parts
#' of the results into vectors, matrices or data frames.
#' Also note the \code{\link[emil]{select}} function that provides an extension
#' to the \code{dplyr} package for data manipulation.
#' 
#' This function can only be used to extract data, not to assign.
#' 
#' @param x List of lists.
#' @param i Indexes to extract on the first level of the tree. Can also be a
#'   function that will be applied to the downstream result of the function.
#' @param ... Indexes to extract on subsequent levels.
#' @param error.value A template for the return value in case it is missing or
#'   invalid. Note that \code{NA} is a \code{\link{logical}} by default,
#'   causing \code{subtree} to also convert existing results to logicals.
#'   To get around this, please specify it as \code{as.numeric(NA)},
#'   \code{as.character(NA)}, or similar (see the example below).
#' @param warn Specifies whether warnings should be displayed (\code{0}),
#'   ignored (\code{-1}), or break execution (\code{1}). Works like the
#'   \code{\link{options}} parameter \code{warn}.
#' @param simplify Whether to collapse results into vectors or matrices when
#'   possible (\code{TRUE}) or to preserve the original tree structure as a
#'   list (\code{FALSE}).
#' @return A subset of the list tree.
#' @example examples/subtree.R
#' @seealso \code{\link{select}}
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
subtree <- function(x, i, ..., error.value, warn, simplify=TRUE){
    if(missing(error.value)) error.value <- NULL
    if(missing(warn)) warn <- is.null(error.value)
    if(is.null(error.value) && warn < 1){
        warning("With no `error.value` `warn` is ignored and all errors break the execution")
        warn <- 1
    }
    ret <- if(is.function(i)){
        if(missing(...)){
            i(x)
        } else {
            i(subtree(x, ..., error.value=error.value, warn=warn, simplify=simplify))
        }
    } else if(missing(...)){
        if(is.null(error.value)){
            x[i]
        } else {
            coerce.class <- function(x){
                x <- as(x, class(error.value))
                if(length(x) != length(error.value))
                    stop(sprintf("values must be length %i, but result is length %i",
                                 length(error.value), length(x)))
                x
            }
            lapply(x[i], function(xi){
                if(warn < 1) tryCatch({
                    coerce.class(xi)
                }, error = function(err){
                    if(warn == 0)
                        warning(err$message)
                    error.value
                }) else {
                    coerce.class(xi)
                }
            })
        }
    } else {
        lapply(x[i], subtree, ..., error.value=error.value, warn=warn, simplify=simplify)
    }
    if(simplify){
        if(length(ret) == 1){
            ret <- ret[[1]]
        } else if(all(sapply(ret, length) == 1)){
            ret <- unlist(ret, recursive=FALSE)
        } else if(all(sapply(lapply(ret, dim), is.null)) &&
                  all(sapply(ret, length) == length(ret[[1]]))){
            ret.class <- sapply(ret, class)
            ret.na <- sapply(ret, function(x) all(is.na(x)))
            i <- head(which(!ret.na), 1)
            if((is.numeric(ret[[i]]) || is.character(ret[[i]]) || is.logical(ret[[i]])) &&
                    length(unique(ret.class[!ret.na])) == 1){
                ret <- do.call(cbind, ret)
            }
        }
        if(is.null(dim(ret)) && !is.null(dim(x)) && length(ret) == length(x)){
            ret <- array(ret, dim=dim(x), dimnames=dimnames(x))
        }
    }
    ret
}

#' emil and dplyr integration
#' 
#' Modeling results can be converted to tabular format and manipulated using
#' dplyr and other Hadleyverse packages. This is accomplished by a class
#' specific \code{\link[dplyr]{select_}} function that differs somewhat in syntax
#' from the default \code{\link[dplyr]{select_}}.
#'
#' @param .data Modeling results, as returned by \code{\link{evaluate.modeling}}.
#' @param ... Not used, kept for consistency with \code{dplyr}.
#' @param .dots Indices to select on each level of \code{.data}, i.e.
#'   the first index specifies which top level elements of \code{.data} to
#'   select, the second specifies second-level-elements etc.
#'   The last index must select elements that can be converted to a data frame.
#'   In case the desired bottom-level element is related to the observations of
#'   a modeling task, e.g. the predctions of a test set, you must supply the
#'   resampling scheme used to produce \code{.data} at the appropriate level
#'   (see the examples).
#'
#'   The names of the \code{...} arguments specifies the names of the resulting
#'   data frame. Non-named arguments will be used to traverse the data but not
#'   returned.
#'
#'   In summary the \code{...} indices can be on the following forms:
#'   \describe{
#'     \item{Simple indices}{Anything that can be used to subset objects,
#'       e.g. integers, logicals, or characters.}
#'     \item{Functions}{A function that produces a data frame, vector or
#'       factor.}
#'     \item{Resampling schemes}{The same resampling scheme that was used to
#'       produce the modeling results.}
#'   }
#' @return A \code{\link{data.frame}} in long format.
#' @examples
#' # Produce some results
#' x <- iris[-5]
#' y <- iris$Species
#' names(y) <- sprintf("orchid%03i", seq_along(y))
#' cv <- resample("crossval", y, nfold=3, nrep=2)
#' procedures <- list(nsc = modeling.procedure("pamr"),
#'                    rf = modeling.procedure("randomForest"))
#' result <- evaluate.modeling(procedures, x, y, resample=cv)
#' 
#' # Get the foldwise error for the NSC method
#' result %>% select(Fold = TRUE, "nsc", Error = "error")
#'
#' # Compare both methods 
#' result %>%
#'     select(Fold = TRUE, Method = TRUE, Error = "error") %>%
#'     spread(Method, Error)
#' result %>%
#'     select(Fold = TRUE, Method = TRUE, Error = "error") %>%
#'     group_by(Method) %>% summarize(MeanError = mean(Error))
#'
#' # Investigate the variability in estimated class 2 probability across folds
#' result %>%
#'     select(Fold = cv, "nsc", "pred", Probability = function(x) x$prob[,2]) %>%
#'     spread(Fold, Probability)
#' @seealso subtree
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @rdname select
#' @import dplyr
#' @import lazyeval
#' @import tidyr
#' @export
select_.modeling.result <- function(.data, ..., .dots){
    requireNamespace("data.table")
    select_modeling_result(.data, lazyeval::lazy_eval(.dots))
}
#' @noRd
select_modeling_result <- function(.data, .dots, id=NULL){
    named <- !is.null(names(.dots)) && names(.dots)[1] != ""
    subsetting <- inherits(.dots[[1]], c("numeric", "integer", "logical", "character"))

    if(length(.dots) == 1){
        if(subsetting){
            d <- data.frame(Value = .data[[.dots[[1]]]])
            if(named && length(d) > 0)
                names(d) <- names(.dots)
        } else if(is.function(.dots[[1]])){
            d <- .dots[[1]](.data)
            if(!inherits(d, "data.frame")){
                if(named){
                    d <- data.frame(d)
                    names(d) <- names(.dots)
                } else {
                    stop("Functions must either be named or return a data frame.")
                }
            }
        } else {
            stop("Invalid argument.")
        }
        if(!is.null(id)){
            if(nrow(d) != length(id)) stop("Fold did not match data.")
            d$id <- id
        }
    } else {
        if(subsetting){
            d <- lapply(.data[.dots[[1]]], select_modeling_result, .dots[-1], id=id)
            if(named && length(d) > 0){
                d <- data.frame(..tmp = factor(rep(seq_along(d), sapply(d, nrow)),
                                          seq_along(d), names(d)),
                           data.table::rbindlist(d))
                names(d)[1] <- names(.dots)[1]
            } else {
                d <- data.table::rbindlist(d)
            }
        } else if(is.function(.dots[[1]]) && length(.dots) > 1){
            stop("Not implemented.")
        } else if(inherits(.dots[[1]], "resample")){
            if(!identical(names(.data), names(.dots[[1]])))
                .data <- .data[names(.dots[[1]])]
            r <- if(identical(rownames(.dots[[1]]), as.character(1:nrow(.dots[[1]])))){
                seq_len(nrow(.dots[[1]]))
            } else {
                rownames(.dots[[1]])
            }
            d <- Map(function(x, i) select_modeling_result(x, .dots[-1], id=r[index.test(i)]),
                     .data, .dots[[1]])
            d <- data.frame(Fold = factor(rep(names(.dots[[1]]), sapply(d, nrow)),
                                          names(.dots[[1]])),
                            rbindlist(d))
        } else {
            stop("Invalid argument.")
        }
    }
    d
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


#' Print a timestamped and indented log message
#'
#' To suppress messages below a given indentation level set the global
#' \code{\link{option}} setting \code{emil.max.indent}, as in the example below.
#'
#' @param level Indentation level.
#' @param ... Sent to \code{\link{sprintf}}.
#' @param time Whether or not to print timestamp.
#' @param linebreak Whether to finish the message with a linebreak or not.
#' @param file Sent to \code{\link{cat}}.
#' @examples
#' equipment <- c("flashlight", "snacks", "pick")
#' {
#'     trace.msg(1, "Begin descent")
#'     trace.msg(2, "Oh no, forgot the %s!", sample(equipment, 1))
#'     trace.msg(2, "Hello? Can you throw it down to me?", time=FALSE)
#'     trace.msg(1, "Aw shucks, I'm coming back up.")
#' }
#'
#' for(verbose in c(TRUE, FALSE)){
#'     cat("It's", verbose, "\n")
#'     for(i in 0:3)
#'         trace.msg(indent(verbose, i), "Down")
#' }
#'
#' options(emil.max.indent = 2)
#' for(i in 1:3)
#'     trace.msg(i, "Down")
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
trace.msg <- function(level=1, ..., time=TRUE, linebreak=TRUE, file=""){
    if(level > 0){
        for(msg in sprintf(...)){
        cat(sep="", file=file,
            # timestamp
            if(time) format(Sys.time(), "%d %b %H:%M") else
                     paste(rep(" ", 12), collapse=""),
            # indent
            rep("  ", level),
            # message
            msg,
            # linebreak
            if(linebreak) "\n" else "")
        }
    }
}
#' @param base Base indentation level of the function printing the message.
#' @param indent Extra indentation of this message.
#' @rdname trace.msg
#' @export
indent <- function(base, indent){
    if(base > 0 && base + indent <= getOption("emil.max.indent", Inf)){
        base + indent
    } else {
        0
    }
}


#' Print a warning message if not printed earlier
#' 
#' To avoid flooding the user with identical warning messages, this function
#' keeps track of which have already been shown.
#' 
#' @param id Warning message id. This is used internally to refer to the
#'   message.
#' @param ... Sent to \code{\link{warning}}.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
warn.once <- function(id, ...){
    if(!id %in% getOption("emil.warnings")){
        warning(...)
        options(emil.warnings = c(getOption("predict.warnings"), id))
    }
}
#' @rdname warn.once
#' @export
reset.warn.once <- function(){
    options(emil.warnings = NULL)
}

