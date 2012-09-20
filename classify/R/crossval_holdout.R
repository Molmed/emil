##' @import foreach
{}


##' Expand classes to same size
##'
##' Add duplicates of observations in smaller classes to make all class sizes
##' equal.
##'
##' @param y Class membership vector.
##' @param balanced Logical, whether to make sure all objects within a class occur
##'   as closely as possible to an equal number of counts.
##' @return A vector of indices of \code{y} which gives equal class sizes.
##' @examples
##' y <- runif(20) < .7
##' table(y)
##' table(y[expand.classes(y)])
##' @author Christofer Backlin
##' @export
expand.classes <- function(y, balanced=TRUE){
    if(!is.factor(y)) y <- factor(y)
    n <- max(table(y))
    sort(foreach(idx=split(1:length(y), y), .combine=c) %do% {
        if(balanced) {
            c(idx, rep(sample(idx), length.out=n-length(idx)))
        } else {
            c(idx, sample(idx, n-length(idx), replace=TRUE))
        }
    })
}


##' Generate groups for crossvalidation
##'
##' This function is correct and working perfectly but should be tidied up
##' internally when I get the time.
##'
##' @param y Objects to be divided into groups. Can either be supplied as the
##'   data itself or as a scalar which is interpreted as number of objects.
##' @param nfold Number of folds.
##' @param nrep Number of fold sets to generate.
##' @param balanced Whether the groups should be balanced or not, i.e. if the
##'   class ratio over groups should be kept constant (as far as possible).
##' @param subset Which objects in \code{y} that are to be selected from (and
##'   which that are to be held out). Observations no in \code{subset} are coded
##'   as \code{NA} in the returned matrix.
##' @param include.na Whether to let the \code{NA} values of \code{y} be part of
##'   the crossvalidatiton set. Otherwise they are set to \code{NA} in all
##'   folds.
##' @return A list of numeric vectors containing the indices of the objects in
##'   each fold.
##' @examples
##' y <- factor(runif(60) >= .5)
##' cv <- crossval.groups(y)
##' inner.cv <- lapply(as.data.frame(cv), function(x) crossval.groups(y, subset=!x))
##' layout(t(1:2)); image(cv, main="Outer CV"); image(inner.cv[[5]], main="Inner CV, fold #5")
##' @seealso crossval, expand.smaller.class
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
crossval.groups <- function(y, nfold=5, nrep=1, balanced=is.factor(y), subset=TRUE, include.na=FALSE){
    n <- if(length(y) == 1) y else length(y)
    if(n < nfold) stop("Number of objects cannot be smaller than number of groups")
    if(is.outcome(y)) y <- factor.events(y)

    # Convert subset to logical vector
    subset <- (1:n) %in% (1:n)[subset]
    if(!include.na){
        subset[is.na(y)] <- FALSE
    } else if(is.factor(y)){
        y <- factor(y, levels=c(levels(y), "NA-proxy-level"))
        y[is.na(y)] <- "NA-proxy-level"
    }
    
    folds <- foreach(r = 1:nrep, .combine=cbind) %do% {
        idx <- if(!balanced){
            sample((1:n)[subset])
        } else {
            levs <- if(is.factor(y)) levels(y) else unique(y)
            foreach(lev=levs[order(table(y[subset]))], .combine=c) %do% {
                w <- which(y == lev)
                w <- w[w %in% (1:n)[subset]]
                if(length(w) < 2) w else sample(w)
            }
        }
        idx <- matrix(c(idx, rep(NA, ceiling(length(idx)/nfold)*nfold-length(idx))), ncol=nfold, byrow=TRUE)
        apply(idx, 2, function(i) 1:n %in% i)
    }
    if(!include.na && !is.factor(y)) folds[is.na(y),] <- NA
    # Make observations not in the subset equal to NA
    folds[!subset,] <- NA

    class(folds) <- c("crossval", "matrix")
    dimnames(folds) <- list(NULL, `rep:fold`=paste(rep(1:nrep, each=nfold), rep(1:nfold, nrep), sep=":"))
    attr(folds, "nfold") <- nfold
    attr(folds, "nrep") <- nrep
    return(folds)
}


##' Generate groups for repeated hold out
##'
##' @param y Objects to be divided into groups. Can either be supplied as the
##'   data itself or as a scalar which is interpreted as number of objects.
##' @param frac Fraction of objects to hold out (0 < frac < 1).
##' @param nrep Number of replicates.
##'   from design sets.
##' @param balanced Whether the groups should be balanced or not, i.e. if the
##'   class ratio over groups should be kept constant.
##' @param subset Which objects in \code{y} that are to be selected from (and
##'   which that are to be held out).
##' @return A list of two vectors or data.frames, depending on \code{ngroup},
##'   containing indices of objects that go into each group.
##' @examples
##' holdout.groups(50, 1/3)
##' holdout.groups(factor(runif(60) >= .5))
##' @seealso crossval.groups
##' @author Christofer Backlin
##' @export
holdout.groups <- function(y=NULL, frac=.5, nrep=1, balanced=is.factor(y), subset=TRUE){
    n <- if(length(y) == 1) y else length(y)
    class.idx <- if(balanced){
        lapply(levels(y), function(lev) intersect(which(y == lev), (1:n)[subset]))
    } else {
        list((1:n)[subset])
    }
    class.ho <- round(frac*sapply(class.idx, length))
    ho <- as.matrix(foreach(r = 1:nrep, .combine=cbind) %do% {
        idx <- rep(NA, n)
        idx[subset] <- FALSE
        foreach(ci = class.idx, c.ho = class.ho) %do% {
            idx[sample(ci, c.ho)] <- TRUE
        }
        return(idx)
    })
    class(ho) <- c("holdout", "matrix")
    dimnames(ho) <- list(NULL, replicate=NULL)
    return(ho)
}


##' Extract or replace parts of a crossval matrix
##' 
##' These class methods are designed to maintain the classes and attributes
##' of crossval matrices, something which naturally the ordinary
##' \code{\link{[}} is not.
##' 
##' @method [ crossval
##' @param x Object to be subsetted.
##' @param i Row indices.
##' @param j Col indices.
##' @param silent Wether to warn if the subset in itself is not a valid cross
##'   validation scheme.
##' @return See \code{\link{[}}.
##' @examples
##' cv <- crossval.groups(20, nfold=5, nrep=10)
##' cv2 <- cv[, 11:20]
##' class(cv2)
##' attr(cv2, "nrep")
##' cv[, 1:10] <- cv2
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @rdname extract.crossval
##' @export
`[.crossval` <- function(x, i=TRUE, j=TRUE, silent=TRUE){
    y <- unclass(x)[i, j, drop=FALSE]

    nfold <- attr(x, "nfold")
    nrep <- attr(x, "nrep")

    if(ncol(y) %% nfold != 0){
        valid <- FALSE                                                  # The number of columns is not a multiple of the number of folds
    } else {
        ok <- foreach(k = 1:(ncol(y)/nfold), .combine=cbind) %do% {     # Go through all sets of folds
            apply(y[, 1:nfold + (k-1)*nfold,drop=FALSE], 1, function(r){           # and make sure every row is either
                if(all(is.na(r))) 
                   NA                                                   # all NA
                else
                   !any(is.na(r)) && sum(r) == 1                        # or has no NA and exactly one element == TRUE
            })
        }
        ok <- apply(as.matrix(ok), 1, function(r){                      # Make sure all folds
            all(is.na(r)) || (!any(is.na(r)) && all(r))                 # have the same NA rows or are all valid
        })
        valid <- all(ok)
    }
    if(valid){
        class(y) <- class(x)
        attr(y, "nfold") <- nfold
        attr(y, "nrep") <- length((1:ncol(x))[j])/nfold
    } else {
        class(y) <- c("holdout", class(y))
        attr(y, "nrep") <- ncol(y)
        if(!silent) warning("Extracted subset is not a valid crossvalidation set. Converted to holdout set.")
    }
    return(y)
}


##' Extract or replace parts of a holdout matrix
##' 
##' @method [ holdout
##' @param x Object to be subsetted.
##' @param i Row indices.
##' @param j Col indices.
##' @return See \code{\link{[}}.
##' @examples
##' ho <- holdout.groups(20, frac=1/3, nrep=10)
##' ho2 <- ho[, 4:6]
##' class(ho2)
##' ho[, 1:3] <- ho2
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @rdname extract.holdout
##' @export
`[.holdout` <- function(x, i=TRUE, j=TRUE){
    y <- unclass(x)[i, j, drop=FALSE]
    col.sums <- apply(y, 2, sum)
    valid <- all(col.sums == col.sums[1])
    if(valid){
        class(y) <- class(x)
    }# else {
    #    warning("Extracted subset is not a valid holdout set. Class membership is discarded.")
    #}
    return(y)
}


##' Coerce crossval matrix to a Data Frame
##' 
##' @method as.data.frame crossval
##' @param x crossval object
##' @param ... Sent to \code{\link{as.data.frame}}
##' @param save.class Logical, if true class and attributes will be preserved.
##' @return crossval data.frame.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
as.data.frame.crossval <- function(x, ..., save.class=FALSE){
    y <- as.data.frame(unclass(x), ...)
    if(save.class){
        attr(y, "nfold") <- attr(x, "nfold")
        attr(y, "nrep") <- attr(x, "nrep")
        class(y) <- c("crossval", "data.frame")
    }
    return(y)
}


##' Coerce holdout matrix to a Data Frame
##' 
##' @method as.data.frame holdout
##' @param x holdout object
##' @param ... Sent to \code{\link{as.data.frame}}
##' @param save.class Logical, if true class will be preserved.
##' @return holdout data.frame.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
as.data.frame.holdout <- function(x, ..., save.class=FALSE){
    y <- as.data.frame(unclass(x), ...)
    if(save.class){
        class(y) <- c("holdout", "data.frame")
    }
    return(y)
}


##' Convert crossval, matrix or data frame to holdout set
##' 
##' @param x Object
##' @return holdout matrix or data frame.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
as.holdout <- function(x){
    if(inherits(x, "crossval") ||
            (is.matrix(x) && is.logical(x)) ||
            (is.data.frame(x) && all(sapply(x, is.logical)))){
        class(x) <- c("holdout", setdiff(class(x), "crossval"))
        attr(x, "nrep") <- ncol(x)
        attr(x, "nfold") <- NULL
        return(x)
    } else {
        stop("Unsupported data type.")
    }
}


##' Plot crossval replicates as a heatmap
##' 
##' @method image crossval
##' @param x Crossval matrix as returned by \code{\link{crossval.groups}}.
##' @param ... Ignored, kept for S3 consistency.
##' @return Nothing
##' @examples
##' image(crossval.groups(60, nfold=3, nrep=8))
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
image.crossval <- function(x, ...){
    col.matrix <- matrix(c("red", "yellow", "blue", "green")
                         [1 + unclass(x) + 2*(as.integer(gl(2, nrow(x)*attr(x, "nfold"), length(x)))-1)],
                         nrow(x), ncol(x))
    col.matrix[is.na(x)] <- "transparent"
    plot(c(.5, ncol(x)+.5), c(.5, nrow(x)+.5), type="n", xlab="Folds & Replicates", ylab="Observations/Objects")
    rasterImage(col.matrix, .5, .5, ncol(x)+.5, nrow(x)+.5, interpolate=FALSE)
}


##' Plot holdout replicates as a heatmap
##' 
##' @method image holdout
##' @param x Holdout matrix as returned by \code{\link{holdout.groups}}.
##' @param ... Ignored, kept for S3 consistency.
##' @return Nothing
##' @examples
##' image(holdout.groups(60, frac=1/3, nrep=20))
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
image.holdout <- function(x, ...){
    col.matrix <- matrix(c("red", "yellow", "blue", "green")
                         [1 + unclass(x) + 2*(as.integer(gl(2, nrow(x), length(x)))-1)],
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
##' @param batch List of predictions, as returned by \code{\link{batch.design}}
##'   or \code{\link{predict}}.
##' @param y True class labels. If supplied ROC curve will be calculated (only
##'   for binary problems).  
##' @param test.subset CV folds, as returned by \code{\link{crossval.groups}}.
##' @param subset Subset of data to design on.
##' @return A combined prediction object on the same form as returned by
##'   \code{\link{design}} and \code{\link{predict}}.
##' @examples
##' # TODO
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso assemble, subtree
##' @export
assemble.cv <- function(batch, y=NULL, subset=TRUE, test.subset){
    if(missing(test.subset)) stop("test.subset missing.")
    if(!identical(subset, TRUE)){
        y <- y[subset]
        test.subset <- test.subset[subset,]
    }

    # Find out what fields there are and which are to be assembled
    types <- names(batch[[1]])
    names(types) <- types   # To preserve names of lapply and mapply
    fields <- lapply(types, function(type){
        my.fields <- unique(unlist(lapply(subtree(batch, T, type, flatten=1), names)))
        sapply(my.fields, function(field){
            f <- subtree(batch, T, type, field, flatten=2)
            if(all(sapply(f, is.vector)) || all(sapply(f, is.factor))){
                return(all(sapply(f, length) == apply(test.subset, 2, sum, na.rm=TRUE)))
            } else if(all(sapply(f, is.matrix)) || all(sapply(f, is.data.frame))){
                return(all(sapply(f, nrow) == apply(test.subset, 2, sum, na.rm=TRUE)))
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
                    return(assemble(f, test.subset[,rr]))
                } else {
                    return(f)
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
##'   \code{\link{crossval.groups}} or \code{\link{holdout.groups}}.
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

