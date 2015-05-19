#' Resampling schemes
#' 
#' Performance evaluation and parameter tuning use resampling methods to
#' estimate the performance of models. These are defined by resampling
#' schemes, which are data frames where each column corresponds to a
#' division of the data set into mutually exclusive training and test sets.
#' Repeated hold out and cross-validation are two methods to create such
#' schemes.
#'
#' Note that when setting up analyzes, the user should not call
#' \code{resample_holdout} or \code{resample_crossvalidation} directly, as
#' \code{resample} performs additional necessary processing of the scheme.
#'
#' Resampling scheme can be visualized in a human digestible form with the
#' \code{\link[=image.resample]{image}} function.
#' 
#' Functions for generating custom resampling schemes should be implemented as
#' follows and then called by \code{resample("myMethod", ...)}:
#'
#' \code{resample_myMethod <- function(y, ..., subset)}
#' \describe{
#'     \item{\code{y}}{Response vector.}
#'     \item{\code{...}}{Method specific attributes.}
#'     \item{\code{subset}}{Indexes of observations to be excluded for the
#'         resampling.}
#' }
#' The function should return a list of the following elements:
#' \describe{
#'     \item{\code{folds}}{A data frame with the folds of the scheme that
#'         conforms to the description in the 'Value' section below.}
#'     \item{\code{parameter}}{A list with the parameters necessary to generate
#'         such a resampling scheme. These are needed when creating subschemes
#'         needed for parameter tuning, see \code{\link{subresample}}.}
#' }
#' 
#' @param method The resampling method to use, e.g. \code{"holdout"} or
#'   \code{"crossvalidation"}.
#' @param y Observations to be divided. Can either be supplied as the response
#'   of the observations themselves, or as a scalar which is interpreted as the
#'   number of objects.
#' @param ... Sent to the method specific function, e.g.
#'   \code{"resample_holdout"}.
#' @param nfold Number of folds.
#' @param balanced Whether the sets should be balanced or not, i.e. if the
#'   class ratio over the sets should be kept constant (as far as possible).
#' @param subset Which objects in \code{y} that are to be divided and which
#'   that are not to be part of neither set.
#'   If \code{subset} is a resampling scheme, a list of inner
#'   cross-validation schemes will be returned.
#' @return A data frame defining a resampling scheme. \code{TRUE} or a positive integer
#'   codes for training set and \code{FALSE} or \code{0} codes for test set.
#'   Positive integers > 1 code for multiple copies of an observation in the
#'   training set. \code{NA} codes for neither training nor test set and is
#'   used to exclude observations from the analysis altogether.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @seealso \code{\link{emil}}, \code{\link{subresample}},
#'   \code{\link{image.resample}}, \code{\link{index_fit}}
#' @export
resample <- function(method, y, ..., subset=TRUE){
    x <- get(sprintf("resample_%s", method))(y, ..., subset=subset)
    class(x$folds) <- c(method, "resample", "data.frame")
    if(all(grepl("^V\\prediction+$", names(x$folds))))
        names(x$folds) <- sprintf("fold%i", seq_along(x$folds))
    x$folds[T] <- Map(function(f, n) structure(f, class=c(method, "fold", class(f)),
                      parameter=x$parameter, fold.name=n), x$folds, names(x$folds))
    if(!is.null(names(y))) rownames(x$folds) <- names(y)
    x$folds
}


#' Generate resampling subschemes
#'
#' A subscheme is a resampling scheme that only includes observations in the
#' training set of an original scheme. This function
#' automatically fetches the type and parameters of the prototype and use them
#' to generate the subscheme.
#'
#' @param fold A resampling scheme or fold to use to define the sub scheme(s).
#' @param y The observations used to create the resampling scheme. See
#'   \code{\link{resample}} for details.
#' @return A resampling scheme.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @examples
#' cv <- resample("holdout", y=12, fraction=1/4, nfold=3)
#' inner.cv <- subresample(cv, y=12)
#' @seealso \code{\link{emil}}, \code{\link{resample}}
#' @export
subresample <- function(fold, y=length(fold)){
    if(is.data.frame(fold)){
        # The user inputted a full scheme
        lapply(fold, subresample, y)
    } else {
        do.call(resample,
            c(list(method = class(fold)[1], y = y),
              attr(fold, "parameter"),
              list(subset = as.logical(fold))))
    }
}


#' Convert a fold to row indexes of fittdng or test set
#'
#' @param fold A fold of a resampling scheme.
#' @param allow_oversample Whether or not to allow individual observation to
#'   exist in multiple copies in the training set. This is typically not the
#'   case, but can be used when a class is underrepresented in the data set.
#' @return An integer vector of row indexes.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @seealso \code{\link{emil}}, \code{\link{resample}}
#' @export
index_fit <- function(fold, allow_oversample=TRUE){
    if(allow_oversample){
        rep(seq_along(fold), na_fill(fold, 0))
    } else {
        which(!fold %in% c(0, NA))
    }
}
#' @rdname index_fit
#' @aliases index_test
#' @export
index_test <- function(fold){
    which(fold %in% 0)
}


#' @param fraction Fraction of objects to hold out (0 < fraction < 1).
#' @examples
#' resample("holdout", 50, fraction=1/3)
#' resample("holdout", factor(runif(60) >= .5))
#' @rdname resample
#' @export
resample_holdout <- function(y=NULL, fraction=.5, nfold=5, balanced=is.factor(y), subset){
    # Convert subset to integer vector
    n <- if(length(y) == 1 && is.numeric(y)) y else length(y)
    subset <- if(is.logical(subset)){
        (1:n) %in% (1:n)[subset & !is.na(y)]
    } else {
        subset[!subset %in% which(is.na(y))]
    }

    class.subset <- if(balanced) split(which(subset), y[subset]) else which(subset)
    class.ho <- round(fraction * sapply(class.subset, length))

    ho <- as.data.frame(replicate(nfold, {
        idx <- rep(NA, n)
        idx[subset] <- TRUE
        idx[unlist(Map(sample, class.subset, class.ho))] <- FALSE
        idx
    }))
    names(ho) <- sprintf("fold%i", seq_along(ho))
    list(folds=ho, parameter=list(fraction=fraction, nfold=nfold, balanced=balanced))
}

#' @param nreplicate Number of fold sets to generate.
#' @examples
#' y <- factor(runif(60) >= .5)
#' cv <- resample("crossvalidation", y)
#' image(cv, main="Cross-validation scheme")
#' @rdname resample
#' @export
resample_crossvalidation <- function(y, nfold=5, nreplicate=5, balanced=is.factor(y), subset){
    n <- if(length(y) == 1) y else length(y)
    if(inherits(y, "Surv"))
        y <- as.factor(y[,"status"])
    if(n < nfold) stop("Number of objects cannot be smaller than number of groups")
    if(inherits(y, "Surv")) y <- dichotomize(y, to_factor=TRUE)

    # Convert subset to logical vector
    subset <- (1:n) %in% (1:n)[subset]
    subset[is.na(y)] <- FALSE
    
    folds <- as.data.frame(replicate(nreplicate, {
        idx <- if(!balanced){
            sample(which(subset))
        } else {
            levs <- if(is.factor(y)) levels(y) else unique(y)
            unlist(lapply(levs[order(table(y[subset]))], function(lev){
                w <- which(y == lev)
                w <- w[w %in% (1:n)[subset]]
                if(length(w) < 2) w else sample(w)
            }))
        }
        idx <- matrix(c(idx, rep(NA, ceiling(length(idx)/nfold)*nfold-length(idx))),
                      ncol=nfold, byrow=TRUE)
        apply(idx, 2, function(i) !1:n %in% i)
    }))
    folds[!subset,] <- NA
    names(folds) <- sprintf("fold%i.%i", rep(1:nreplicate, each=nfold), rep(1:nfold, nreplicate))
    list(folds=folds, parameter=list(nfold=nfold, nreplicate=nreplicate, balanced=balanced))
}


#' Visualize resampling scheme
#'
#' Class specific extension to \code{\link{image}}.
#' 
#' @method image resample
#' @param x Resampling scheme, as returned by \code{\link{resample}}.
#' @param col Color palette matching the values of \code{x}.
#'   Can also be the response vector used to create the
#'   scheme for automatic coloring.
#' @param ... Sent to \code{\link{plot}}.
#' @return Nothing, produces a plot.
#' @examples
#' image(resample("holdout", 60, fraction=1/3, nfold=20))
#'
#' y <- gl(2, 30)
#' image(resample("crossvalidation", y, nfold=3, nreplicate=8), col=y)
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @seealso \code{\link{emil}}, \code{\link{resample}}
#' @export
image.resample <- function(x, col, ...){
    x <- as.matrix(x)
    if(missing(col)) col <- gl(1, nrow(x))
    if(inherits(col, "Surv")) col <- dichotomize(col, to_factor=TRUE)
    if(is.factor(col)){
        y <- col
        if(length(y) != nrow(x))
            stop("Color vector does not match resampling scheme.")
        nice_require("RColorBrewer")
        if(length(levels(y)) > 12)
            warning("Too few colors to assign unique ones to each class.")
        col <- rep(RColorBrewer::brewer.pal(12, "Set3"),
                   ceiling(length(levels(y))/12))[seq_along(levels(y))]
        col <- apply(col2rgb(col), 2, function(cl){
              apply(cl %o% seq(.7, 1, length.out=max(x, na.rm=TRUE)+1), 2,
                    function(cc) do.call(rgb, as.list(cc/255)))
        })
        mat <- matrix(col[cbind(as.vector(x)+1, as.integer(y))], nrow(x), ncol(x))
    } else {
        mat <- 1 + x
        mat <- matrix(col[mat], nrow(mat))
    }
    mat[is.na(x)] <- "transparent"
    par(xaxs="i", yaxs="i")
    plot(c(.5, ncol(x)+.5), c(.5, nrow(x)+.5), type="n",
         axes=FALSE, xlab="Folds", ylab="Observations", ...)
    rasterImage(mat, .5, .5, ncol(x)+.5, nrow(x)+.5, interpolate=FALSE)
    nice_axis(1)
    ticks <- pretty(pretty(par("usr")[3:4]))
    nice_axis(2, at=nrow(x)-ticks+1, labels=ticks, las=1)
    nice_box()
}


#' @method image crossvalidation
#' @rdname image.resample
#' @export
image.crossvalidation <- function(x, col, ...){
    image.resample(x, col, ...)
    parameter <- attr(x[[1]], "parameter")
    if(parameter$nreplicate > 1){
        l <- 1:(parameter$nreplicate-1)*parameter$nfold + .5
        segments(l, par("usr")[3], l, par("usr")[4])
    }
}

