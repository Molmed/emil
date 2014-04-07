##' Resampling schemes
##' 
##' Performance evaluation and variable tuning use resampling methods to
##' estimate the performance of models. These are defined by resampling
##' schemes, which are data frames where each column corresponds to a
##' division of the data set into mutually exclusive training and test sets.
##' Cross validation and repeated hold out are two methods to create such
##' schemes.
##'
##' Resampling scheme can be visualized in a human digestable form with the
##' \code{\link{image}} function.
##' 
##' @param y Observations to be divided. Can either be supplied as the response
##'   of the observations themselves, or as a scalar which is interpreted as the
##'   number of objects.
##' @param nfold Number of folds.
##' @param balanced Whether the sets should be balanced or not, i.e. if the
##'   class ratio over the sets should be kept constant (as far as possible).
##' @param subset Which objects in \code{y} that are to be divided and which
##'   that are not to be part of neither set.
##'   If \code{subset} is a resampling scheme, a list of inner
##'   cross validation schemes will be returned.
##' @return A data frame defining a resampling scheme, or a list of data frames
##'   defining multiple resampling schemes. \code{FALSE} codes for training set
##'   and \code{TRUE} codes for test set. \code{NA} codes for neither and is
##'   used to exclude observations from the analysis altogether.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @name resample
{}

##' Generate a cross validation scheme
##'
##' @param nrep Number of fold sets to generate.
##' @examples
##' y <- factor(runif(60) >= .5)
##' cv <- resample.crossval(y)
##' image(cv, main="Cross validation scheme")
##' @rdname resample
##' @export
resample.crossval <- function(y, nfold=5, nrep=5, balanced=is.factor(y), subset=TRUE){
    n <- if(length(y) == 1) y else length(y)
    if(inherits(y, "Surv"))
        y <- as.factor(y[,"status"])
    if(n < nfold) stop("Number of objects cannot be smaller than number of groups")
    if(is.outcome(y)) y <- factor.events(y)

    # Convert subset to logical vector
    subset <- (1:n) %in% (1:n)[subset]
    subset[is.na(y)] <- FALSE
    
    folds <- as.data.frame(replicate(nrep, {
        idx <- if(!balanced){
            sample((1:n)[subset])
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
        apply(idx, 2, function(i) 1:n %in% i)
    }))
    folds[!subset,] <- NA

    # Put information of the whole scheme into every fold,
    # for `resample.subset` to work.
    folds[T] <- lapply(folds, structure, class=c("crossval", "fold"),
        nfold = nfold, nrep = nrep, balanced = balanced)
    structure(folds,
        class = c("crossval", "data.frame"),
        names = sprintf("fold%i:%i", rep(1:nrep, each=nfold), rep(1:nfold, nrep))
    )
}


##' Generate a repeated holdout scheme
##'
##' @param frac Fraction of objects to hold out (0 < frac < 1).
##' @examples
##' resample.holdout(50, 1/3)
##' resample.holdout(factor(runif(60) >= .5))
##' @rdname resample
##' @export
resample.holdout <- function(y=NULL, frac=.5, nfold=5, balanced=is.factor(y), subset=TRUE){
    n <- if(length(y) == 1) y else length(y)
    class.idx <- if(balanced){
        lapply(levels(y), function(lev) intersect(which(y == lev), (1:n)[subset]))
    } else {
        list((1:n)[subset])
    }
    class.ho <- round(frac*sapply(class.idx, length))
    ho <- as.data.frame(replicate(nfold, {
        idx <- rep(NA, n)
        idx[subset] <- FALSE
        for(i in seq_along(class.idx))
            idx[sample(class.idx[[i]], class.ho[[i]])] <- TRUE
        idx
    }))
    ho[T] <- lapply(ho, structure, class=c("holdout", "logical"),
        frac = frac, nfold=nfold, balanced = balanced)
    structure(ho, class = c("holdout", "data.frame"),
        names = sprintf("fold%i", 1:nfold)
    )
}


##' Generate a resampling subscheme
##'
##' A subscheme is a resampling scheme that only includes observations in the
##' training set of an original scheme, called prototype. This function
##' automatically fetches the type and parameters of the prototype and use them
##' to generate the subscheme.
##'
##' @param fold A resampling fold to use to define the sub scheme. If missing,
##'   the subschemes of all folds of the prototype are returned.
##' @return A resampling scheme.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @examples
##' cv <- resample.crossval(100)
##' inner.cv <- resample.subset(100, cv)
##' @rdname resample
##' @export resample.subset
resample.subset <- function(y, fold){
    if(is.list(fold)){
        # The user inputted a full scheme
        lapply(fold, function(f) resample.subset(y, f))
    } else if(inherits(fold, "crossval")){
        resample.crossval(y, nfold=attr(fold, "nfold"),
            nrep=attr(fold, "nrep"), balanced=attr(fold, "balanced"),
            subset=na.fill(!fold, FALSE))
    } else if(inherits(fold, "holdout")){
        resample.holdout(y, frac=attr(fold, "frac"), nfold=attr(fold, "nfold"),
            balanced=attr(fold, "balanced"), subset=na.fill(!fold, FALSE))
    } else {
        stop("Unknown type of resampling scheme.")
    }
}


##' Visualize cross validation scheme
##' 
##' @param x Cross validation scheme, as returned by \code{\link{resample.crossval}}.
##' @param ... Ignored, kept for S3 consistency.
##' @return Nothing, produces a plot.
##' @examples
##' image(resample.crossval(60, nfold=3, nrep=8))
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
image.crossval <- function(x, ...){
    col.matrix <- matrix(c("#0091cd", "#54cdff", "#e0a70e", "#fff000")
                         [1 + as.matrix(x) + 2*(as.integer(gl(2, nrow(x)*attr(x[[1]], "nfold"), prod(dim(x))))-1)],
                         nrow(x), ncol(x))
    col.matrix[is.na(x)] <- "transparent"
    plot(c(.5, ncol(x)+.5), c(.5, nrow(x)+.5), type="n", las=1,
         xlab=sprintf("Folds (%i sets of %i folds)", attr(x[[1]], "nrep"), attr(x[[1]], "nfold")),
         ylab="Observations")
    rasterImage(col.matrix, .5, .5, ncol(x)+.5, nrow(x)+.5, interpolate=FALSE)
}


##' Visualize repeated holdout scheme
##' 
##' @param x Repeated holdout scheme, as returned by \code{\link{resample.holdout}}.
##' @param ... Ignored, kept for S3 consistency.
##' @return Nothing, produces a plot.
##' @examples
##' image(resample.holdout(60, frac=1/3, nfold=20))
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
image.holdout <- function(x, ...){
    col.matrix <- matrix(c("#0091cd", "#54cdff", "#e0a70e", "#fff000")
                         [1 + as.matrix(x) + 2*(as.integer(gl(2, nrow(x), prod(dim(x))))-1)],
                         nrow(x), ncol(x))
    col.matrix[is.na(x)] <- "transparent"
    plot(c(.5, ncol(x)+.5), c(.5, nrow(x)+.5), type="n", las=1,
         xlab="Folds", ylab="Observations")
    rasterImage(col.matrix, .5, .5, ncol(x)+.5, nrow(x)+.5, interpolate=FALSE)
}


##' Assemble a list of values calculated with a resampling scheme
##' 
##' @param x List of values.
##' @param test.subset Resampling scheme, as returned by
##'   \code{\link{resample.crossval}} or \code{\link{resample.holdout}}.
##' @return If \code{x} contains vectors, a data frame where rows correspond
##'   observations and columns correspond resampling replicates. If \code{x}
##'   contains matrices or data frames, a list of matrices or data frames whose
##'   rows correspond to observations, and elements of the list correspond to
##'   resampling replicates.
##' @examples
##' \dontrun{
##' cv <- ...
##' pred <- evaluate.modeling(..., test.subset=cv)
##' assemble(subtree(pred, T, T, "pred", "prob"), cv)
##' }
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
assemble <- function(x, test.subset){
    if(is.matrix(x[[1]]) || is.data.frame(x[[1]])){
        # Matrix assembly
        m <- matrix(NA, nrow(test.subset), ncol(x[[1]]))
        if(is.data.frame(x[[1]])){
            m <- as.data.frame(m)
            names(m) <- names(x[[1]])
        }
        values <- rep(list(m), attr(test.subset, "nrep"))
        if(inherits(test.subset, "crossval")){
            for(i in 1:attr(test.subset, "nrep")){
                for(j in 1:attr(test.subset, "nfold")){
                    k <- j+(i-1)*attr(test.subset, "nfold")
                    values[[i]][test.subset[, k], ] <- x[[k]]
                }
            }
        } else {
            for(i in 1:attr(test.subset, "nrep")){
                values[[i]][test.subset[,i], ] <- x[[i]]
            }
        }
        if(attr(test.subset, "nrep") == 1) values <- values[[1]]
    } else {
        # Vector assembly
        if(inherits(test.subset, "crossval")){
            values <- matrix(NA, nrow(test.subset), attr(test.subset, "nrep"))
            for(i in 1:attr(test.subset, "nrep")){
                for(j in 1:attr(test.subset, "nfold")){
                    k <- j+(i-1)*attr(test.subset, "nfold")
                    values[test.subset[, k], i] <- x[[k]]
                }
            }
        } else if(inherits(test.subset, "holdout")) {
            values <- mapply(function(y, i){
                z <- rep(NA, nrow(test.subset))
                z[i] <- y
                return(z)
            }, x, as.data.frame(test.subset))
        }
        if(is.factor(x[[1]])){
            values <- as.data.frame(lapply(as.data.frame(values), function(v){
                return(factor(v, levels=1:length(levels(x[[1]])), labels=levels(x[[1]])))
            }))
        }
    }
    return(values)
}

