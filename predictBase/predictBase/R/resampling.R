##' @import foreach
{}

##' Generate groups for crossvalidation
##'
##' @param y Objects to be divided into groups. Can either be supplied as the
##'   data itself or as a scalar which is interpreted as number of objects.
##' @param nfold Number of folds.
##' @param nrep Number of fold sets to generate.
##' @param balanced Whether the groups should be balanced or not, i.e. if the
##'   class ratio over groups should be kept constant (as far as possible).
##' @param subset Which objects in \code{y} that are to be selected from (and
##'   which that are to be held out).
##'   Observations not in \code{subset} are coded as \code{NA} in the returned
##'   matrix. If \code{subset} is a resampling scheme, a list of inner
##'   cross validation schemes will be returned.
##' @return A list of numeric vectors containing the indices of the objects in
##'   each fold.
##' @examples
##' y <- factor(runif(60) >= .5)
##' cv <- resample.crossval(y)
##' inner.cv <- resample.crossval(y, subset=cv)
##' layout(t(1:2)); image(cv, main="Outer CV"); image(inner.cv[[5]], main="Inner CV, fold #5")
##' @seealso crossval, expand.smaller.class
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
resample.crossval <- function(y, nfold=5, nrep=5, balanced=is.factor(y), subset=TRUE){
    if(inherits(subset, c("crossval", "holdout"))){
        return(lapply(subset, function(s) resample.crossval(y, nfold, nrep, balanced, subset=!s)))
    }

    n <- if(length(y) == 1) y else length(y)
    if(n < nfold) stop("Number of objects cannot be smaller than number of groups")
    if(is.outcome(y)) y <- factor.events(y)

    # Convert subset to logical vector
    subset <- (1:n) %in% (1:n)[subset]
    subset[is.na(y)] <- FALSE
    
    folds <- foreach(r = 1:nrep, .combine=data.frame) %do% {
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
    }
    if(nrep == 1) folds <- data.frame(folds)
    folds[!subset,] <- NA

    structure(folds,
        class = c("crossval", "data.frame"),
        names = sprintf("fold%i:%i", rep(1:nrep, each=nfold), rep(1:nfold, nrep)),
        nfold = nfold,
        nrep = nrep,
        balanced = balanced)
}


##' Generate groups for repeated hold out
##'
##' @param y Objects to be divided into groups. Can either be supplied as the
##'   data itself or as a scalar which is interpreted as number of objects.
##' @param frac Fraction of objects to hold out (0 < frac < 1).
##' @param nrep Number of replicates.
##' @param balanced Whether the groups should be balanced or not, i.e. if the
##'   class ratio over groups should be kept constant.
##' @param subset Which objects in \code{y} that are to be selected from (and
##'   which that are to be held out).
##'   Observations not in \code{subset} are coded as \code{NA} in the returned
##'   matrix. If \code{subset} is a resampling scheme, a list of inner
##'   cross validation schemes will be returned.
##' @return A list of two vectors or data.frames, depending on \code{ngroup},
##'   containing indices of objects that go into each group.
##' @examples
##' resample.holdout(50, 1/3)
##' resample.holdout(factor(runif(60) >= .5))
##' @seealso resample.crossval
##' @author Christofer Backlin
##' @export
resample.holdout <- function(y=NULL, frac=.5, nrep=5, balanced=is.factor(y), subset=TRUE){
    if(inherits(subset, c("crossval", "holdout"))){
        return(lapply(subset, function(s) resample.holdout(y, frac, nrep, balanced, subset=!s)))
    }

    n <- if(length(y) == 1) y else length(y)
    class.idx <- if(balanced){
        lapply(levels(y), function(lev) intersect(which(y == lev), (1:n)[subset]))
    } else {
        list((1:n)[subset])
    }
    class.ho <- round(frac*sapply(class.idx, length))
    ho <- foreach(r = 1:nrep, .combine=data.frame) %do% {
        idx <- rep(NA, n)
        idx[subset] <- FALSE
        foreach(ci = class.idx, c.ho = class.ho) %do% {
            idx[sample(ci, c.ho)] <- TRUE
        }
        return(idx)
    }
    structure(ho, class = c("holdout", "data.frame"),
        names = sprintf("fold%i", 1:nrep),
        frac = frac,
        balanced = balanced)
}


##' Extract or replace parts of a crossval matrix
##' 
##' This class methods is designed to maintain the class and attributes
##' of crossval matrices, something which naturally the ordinary
##' \code{\link{[}} is not.
##' 
##' @method [ crossval
##' @param x Object to be subsetted.
##' @param ... Sent to \code{\link{[}}.
##' @return See \code{\link{[}}.
##' @examples
##' cv <- resample.crossval(20, nfold=5, nrep=10)
##' cv2 <- cv[, 11:20]
##' class(cv2)
##' attr(cv2, "nrep")
##' cv[, 1:10] <- cv2
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @rdname extract.crossval
##' @export
`[.crossval` <- function(x, ...){
    nfold <- attr(x, "nfold")
    nrep <- attr(x, "nrep")
    balanced <- attr(x, "balanced")
    old.class <- setdiff(class(x), "crossval")
    x <- NextMethod()
    if(is.null(dim(x))) return(x)

    if(ncol(x) %% nfold != 0){
        valid <- FALSE                                                  # The number of columns is not a multiple of the number of folds
    } else {
        ok <- sapply(1:(ncol(x)/nfold), function(k) {     # Go through all sets of folds
            apply(as.matrix(x)[, 1:nfold + (k-1)*nfold,drop=FALSE], 1, function(r){           # and make sure every row is either
                if(all(is.na(r))) 
                   NA                                                   # all NA
                else
                   !any(is.na(r)) && sum(r) == 1                        # or has no NA and exactly one element == TRUE
            })
        })
        ok <- apply(as.matrix(ok), 1, function(r){                      # Make sure all folds
            all(is.na(r)) || (!any(is.na(r)) && all(r))                 # have the same NA rows or are all valid
        })
        valid <- all(ok)
    }
    if(valid){
        structure(x, class=c("crossval", old.class), nfold = nfold,
            nrep = ncol(x)/nfold, balanced = balanced)
    } else {
        frac <- apply(x, 2, mean, na.rm=TRUE)
        structure(x, class=c("holdout", old.class),
            nfold = NULL, nrep = NULL,
            frac=if(length(unique(frac)) == 1) frac[1] else NA,
            balanced=balanced)
    }
}


##' Change class of resampling object
##' 
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @name resample.conversion
{}

##' @method as.data.frame crossval
##' @param x crossval object
##' @param ... Sent to \code{\link{as.data.frame}} or \code{\link{as.matrix}}.
##' @param save.class Logical, if true class and attributes will be preserved.
##' @rdname resample.conversion
##' @export
as.data.frame.crossval <- function(x, ..., save.class=FALSE){
    if(save.class){
        structure(NextMethod("as.data.frame", x, ...), class=c("crossval", "data.frame"),
            nfold=attr(x, "nfold"), nrep=attr(x, "nrep"), balanced=attr(x, "balanced"))
    } else {
        NextMethod("as.data.frame", x)
    }
}
##' @method as.matrix crossval
##' @rdname resample.conversion
##' @export
as.matrix.crossval <- function(x, ..., save.class=FALSE){
    if(save.class){
        structure(NextMethod("as.matrix", x, ...), class=c("crossval", "matrix"),
            nfold=attr(x, "nfold"), nrep=attr(x, "nrep"), balanced=attr(x, "balanced"))
    } else {
        NextMethod("as.matrix", x)
    }
}
##' @method as.data.frame holdout
##' @rdname resample.conversion
##' @export
as.data.frame.holdout <- function(x, ..., save.class=FALSE){
    if(save.class){
        structure(NextMethod("as.data.frame", x, ...), class=c("holdout", "data.frame"),
            frac=attr(x, "frac"), balanced=attr(x, "balanced"))
    } else {
        NextMethod("as.data.frame", x)
    }
}
##' @method as.matrix holdout
##' @rdname resample.conversion
##' @export
as.matrix.holdout <- function(x, ..., save.class=FALSE){
    if(save.class){
        structure(NextMethod("as.matrix", x, ...), class=c("holdout", "matrix"),
            frac=attr(x, "frac"), balanced=attr(x, "balanced"))
    } else {
        NextMethod("as.matrix", x)
    }
}


##' Plot crossval replicates as a heatmap
##' 
##' @method image crossval
##' @param x Crossval matrix as returned by \code{\link{resample.crossval}}.
##' @param ... Ignored, kept for S3 consistency.
##' @return Nothing
##' @examples
##' image(resample.crossval(60, nfold=3, nrep=8))
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
image.crossval <- function(x, ...){
    if(!is.matrix(x)) x <- as.matrix(x, save.class=TRUE)
    col.matrix <- matrix(c("red", "yellow", "blue", "green")
                         [1 + x + 2*(as.integer(gl(2, nrow(x)*attr(x, "nfold"), length(x)))-1)],
                         nrow(x), ncol(x))
    col.matrix[is.na(x)] <- "transparent"
    plot(c(.5, ncol(x)+.5), c(.5, nrow(x)+.5), type="n", xlab="Folds & Replicates", ylab="Observations/Objects")
    rasterImage(col.matrix, .5, .5, ncol(x)+.5, nrow(x)+.5, interpolate=FALSE)
}


##' Plot holdout replicates as a heatmap
##' 
##' @method image holdout
##' @param x Holdout matrix as returned by \code{\link{resample.holdout}}.
##' @param ... Ignored, kept for S3 consistency.
##' @return Nothing
##' @examples
##' image(resample.holdout(60, frac=1/3, nrep=20))
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
image.holdout <- function(x, ...){
    col.matrix <- matrix(c("red", "yellow", "blue", "green")
                         [1 + as.matrix(x) + 2*(as.integer(gl(2, nrow(x), nrow(x)*ncol(x)))-1)],
                         nrow(x), ncol(x))
    col.matrix[is.na(x)] <- "transparent"
    plot(c(.5, ncol(x)+.5), c(.5, nrow(x)+.5), type="n", xlab="Replicates", ylab="Observations/Objects")
    rasterImage(col.matrix, .5, .5, ncol(x)+.5, nrow(x)+.5, interpolate=FALSE)
}


##' Assemble cross validation folds of batch predictions into complete replicates
##'
##' All fields present in any replicate will be passed on to the assembly.
##' Fields that match \code{test.subset} in length (e.g. \code{pred}) or
##' number of rows will be assebled into combined vectors, matrices or data frames.
##' Fields that do not match \code{test.subset} (e.g. \code{fit} or
##' \code{error}) will be assembled into lists.
##'
##' @param batch List of predictions, as returned by \code{\link{batch.predict}}
##'   or \code{\link{predict}}.
##' @param y True class labels. If supplied ROC curve will be calculated (only
##'   for binary problems).  
##' @param test.subset CV folds, as returned by \code{\link{resample.crossval}}.
##' @return A combined prediction object on the same form as returned by
##'   \code{\link{batch.predict}}.
##' @examples
##' # TODO
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso assemble, subtree
##' @export
assemble.cv <- function(batch, y=NULL, test.subset){
    if(missing(test.subset)) stop("test.subset missing.")

    # Find out what fields there are and which are to be assembled
    types <- names(batch[[1]])
    names(types) <- types   # To preserve names of lapply and mapply
    fields <- lapply(types, function(type){
        my.fields <- unique(unlist(lapply(subtree(batch, T, type, flatten=1), names)))
        sapply(my.fields, function(field){
            f <- subtree(batch, T, type, field, flatten=2)
            if(all(sapply(f, is.vector)) || all(sapply(f, is.factor))){
                return(all(sapply(f, length) == apply(test.subset, 2, sum)))
            } else if(all(sapply(f, is.matrix)) || all(sapply(f, is.data.frame))){
                return(all(sapply(f, nrow) == apply(test.subset, 2, sum)))
            } else {
                return(FALSE)
            }
        })
    })

    # Assemble
    nrep <- attr(test.subset, "nrep")
    nfold <- attr(test.subset, "nfold")
    res <- lapply(1:nrep, function(r){
        rr <- 1:nfold + (r-1)*nfold
        lapply(types, function(type){
            mapply(function(field, do.assemble){
                f <- subtree(batch, rr, type, field, flatten=2)
                if(do.assemble){
                    val <- if(is.vector(f[[1]])){
                        rep(NA, nrow(test.subset))
                    } else if(is.factor(f[[1]])){
                        factor(rep(NA, nrow(test.subset)), levels=levels(f[[1]]))
                    } else if(is.matrix(f[[1]])){
                        matrix(NA, nrow(test.subset), ncol(f[[1]]))
                    } else {
                        stop("Batch contains predictions of a type not known how to assemble.")
                    }
                    for(i in 1:nfold) val[test.subset[,rr[i]]] <- f[[i]]
                    val
                } else {
                    f
                }
            }, names(fields[[type]]), fields[[type]], SIMPLIFY=FALSE)
        })
    })
    return(res)
}


##' Assemble a list of values calculated with a CV or repeated holdout scheme
##' 
##' This function serves as a generalization of \code{\link{assemble.cv}} which
##' only accepts prediction objects of the type returned by the functions of
##' this package.
##' 
##' @param x List of values.
##' @param test.subset Matrix of test subsets as returned by
##'   \code{\link{resample.crossval}} or \code{\link{resample.holdout}}.
##' @return A matrix with rows corresponding to observations and values
##'   assembled across CV folds/HO replicates.
##' @examples
##' # TODO
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
assemble <- function(x, test.subset){
    if(is.matrix(x[[1]])){
        # Matrix assembly
        if(!inherits(test.subset, "crossval"))
            stop("Matrix assembly is only implemented for cross validation runs.")
        values <- vector("list", attr(test.subset, "nrep"))
        for(i in 1:attr(test.subset, "nrep")){
            values[[i]] <- matrix(NA, nrow(test.subset), ncol(x[[1]]))
            for(j in 1:attr(test.subset, "nfold")){
                k <- j+(i-1)*attr(test.subset, "nfold")
                values[[i]][test.subset[, k], ] <- x[[k]]
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

