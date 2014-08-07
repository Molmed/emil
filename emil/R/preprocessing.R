##' Data preprocessing
##' 
##' These functions are run in \code{\link{batch.model}} just prior to model
##' fitting and serve two purposes. 1) They extract fitting and test sets from
##' the entire dataset and 2) they can at the same time apply a transformation
##' to pre-process the data for handling missing values, scaling, compression
##' etc.
##' They can also be used to modify the form of the data, if required by the
##' fitting function, e.g. \code{\link{pre.pamr}} that transposes the dataset
##' to make it compatible with the \code{pamr} classification method.
##' 
##' Note that all transformations are defined based on the fitting data only
##' and then applied to both fitting set and test set. It is important to not let
##' the test data in any way be part of the model fitting, including the
##' preprocessing, to not risk information leakage and biased results!
##'
##' The imputation functions can also be used outside of the resampling scheme,
##' see \code{\link{impute}}.
##' 
##' @return A list with the following components
##' \describe{
##'     \item{\code{fit}}{Fitting set.}
##'     \item{\code{test}}{Test set.}
##'     \item{\code{features}}{Logical vector indicating which features were kept
##'         (TRUE) and discarded (FALSE). This is only set in case of variable
##'         selection.}
##' }
##'
##' @examples
##' # A splitter that only keeps variables with a class-wise mean difference > `d`
##' my.split <- function(x, y, fold, d=2){
##'     fit.idx <- index.fit(fold)
##'     test.idx <- index.test(fold)
##'     class.means <- sapply(
##'         split(x[fit.idx,, drop=FALSE], y[fit.idx]),
##'         sapply, mean, na.rm=TRUE)
##'     diff.feats <- apply(class.means, 1, function(x) diff(range(x))) > d
##'     return(list(
##'         fit = list(x = x[fit.idx, diff.feats, drop=FALSE],
##'                    y = y[fit.idx]),
##'         test = list(x = x[test.idx, diff.feats, drop=FALSE],
##'                     y = y[test.idx]),
##'         features = diff.feats))
##' }
##' 
##' # Use it during modeling
##' proc <- modeling.procedure("lda")
##' perf <- evaluate.modeling(proc, x = iris[-5], y = iris$Species,
##'                           pre.process = my.split)
##' 
##' # Example of how the end user can change the `d` parameter,
##' # without redefining the function
##' perf <- evaluate.modeling(proc, x = iris[-5], y = iris$Species,
##'                           pre.process = function(...) my.split(..., d = 1.3))
##' @seealso \code{\link{emil}}, \code{\link{pre.impute.knn}}
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @name pre.process
{}


##' @param x Dataset.
##' @param y Response vector.
##' @param fold A logical vector with \code{FALSE} for fitting observations,
##'   \code{TRUE} for test observations and \code{NA} for observations not 
##'   to be included.
##' @rdname pre.process
##' @export
pre.split <- function(x, y, fold){
    list(fit = list(x = x[index.fit(fold),,drop=FALSE],
                  y = y[index.fit(fold)]),
         test = list(x = x[index.test(fold),,drop=FALSE],
                     y = y[index.test(fold)]))
}

##' @rdname pre.process
##' @export
pre.center <- function(x, y, fold){
    pre.scale(x, fold, scale=FALSE)
}

##' @param scale Whether to scale each feature to have standard deviation = 1.
##' @rdname pre.process
##' @export
pre.scale <- function(x, y, fold, scale=TRUE){
    m <- apply(x[index.fit(fold),,drop=FALSE], 2, mean, na.rm=TRUE)
    if(scale){
        s <- apply(x[index.fit(fold),,drop=FALSE], 2, sd, na.rm=TRUE)
        fun <- function(z) sweep(sweep(z, 2, m, "-"), 2, s, "/")
    } else {
        fun <- function(z) sweep(z, 2, m, "-")
    }
    list(fit = list(x = fun(x[index.fit(fold),,drop=FALSE]),
                    y = y[index.fit(fold)]),
         test = list(x = fun(x[index.test(fold),,drop=FALSE]),
                     y = y[index.test(fold)]))
}

##' @rdname pre.process
##' @export
pre.impute.median <- function(x, y, fold){
    na.ind <- which(is.na(unname(x)), arr.ind=TRUE)
        # Duplicate names may cause problems otherwise
    na.ind <- na.ind[!is.na(fold[na.ind[,1]]),,drop=FALSE]
    na.feats <- unique(na.ind[,"col"])
    fills <- apply(x[index.fit(fold), na.feats, drop=FALSE], 2, median, na.rm=TRUE)
    x[na.ind] <- fills[match(na.ind[,"col"], na.feats)]
    list(fit = list(x = x[index.fit(fold),,drop=FALSE],
                    y = y[index.fit(fold)]),
         test = list(x = x[index.test(fold),,drop=FALSE],
                     y = y[index.test(fold)]))
}

##' kNN imputation
##' 
##' Nearest neighbor methods needs to have a distance matrix of the dataset it works on.
##' When doing repeated model fittings on subsets of the entire dataset it is
##' unnecessary to recalculate it every time, therefore this function requires
##' the user to manually calculate it prior to resampling and supply it in a
##' wrapper function.
##' 
##' @param x Dataset.
##' @param y Response vector.
##' @param fold A logical vector with \code{FALSE} for fitting observations,
##'   \code{TRUE} for test observations and \code{NA} for observations not 
##'   to be included.
##' @param k Number of nearest neighbors to calculate mean from. Set to < 1 to
##'   specify a fraction.
##' @param distmat Distance matrix. A matrix, \code{\link{dist}} object or
##'   \code{"auto"}. Notice that \code{"auto"} will recalculate the distance
##'   matrix in each fold, which is only meaningful in case the features of
##'   \code{x} vary between folds. Otherwise you are just wasting time.
##'   
##' @examples
##' x <- iris[-5]
##' x[sample(nrow(x), 30), 3] <- NA
##' my.dist <- dist(x)
##' evaluate.modeling(modeling.procedure("lda"), x=x, y=iris$Species,
##'     pre.process=function(...) pre.impute.knn(..., k=4, my.dist))
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
pre.impute.knn <- function(x, y, fold, k=.05, distmat){
    na.ind <- which(is.na(unname(x)), arr.ind=TRUE)
        # Duplicate names may cause problems otherwise
    na.ind <- na.ind[!is.na(fold[na.ind[,1]]),,drop=FALSE]

    if(length(na.ind) == 0){
        return(pre.split(x, y, fold))
    }


    if(k < 1) k <- max(1, round(.05*length(index.fit(fold))))
    if(k > sum(fold > 0, na.rm=TRUE)) stop("k is larger than number of fitting observations.")

    # If a feature has fewer non-NAs than k exclude it
    non.na.count <- apply(!is.na(x[index.fit(fold, allow.oversample=FALSE),]), 2, sum)
    features <- non.na.count >= k
    na.ind <- na.ind[na.ind[,"col"] %in% which(features),]

    if(missing(distmat))
        stop("You must supply a distance matrix, see `?pre.impute.knn` for details.")
    if(is.character(distmat) && distmat == "auto"){
        idx <- !is.na(fold)
        d <- as.matrix(dist(x[idx,]))
        if(any(is.na(d)))
            stop("Could not calculate distance matrix, check data set for observations with all values missing.")
        distmat <- matrix(NA, nrow(x), nrow(x))
        distmat[idx, idx] <- d
    } else if(!is.matrix(distmat)){
        distmat <- as.matrix(distmat)
    }
    if(any(nrow(x) != dim(distmat)))
        stop("Distance matrix does not match dataset.")

    NN <- apply(distmat, 1, function(z)
        intersect(order(z), index.fit(fold)))
    fills <- apply(na.ind, 1, function(i){
        mean(setdiff(x[NN[,i[1]], i[2]], NA)[1:k])
    })
    if(any(is.na(fills)))
        stop("Could not impute all missing values, too few non-missing values for some features.")
    x[na.ind] <- fills
    c(
        list(fit = list(x = x[index.fit(fold), features, drop=FALSE],
                        y = y[index.fit(fold)]),
             test = list(x = x[index.test(fold), features, drop=FALSE],
                         y = y[index.test(fold)])),
        if(all(features)) NULL else list(features = features)
    )
}



##' Regular imputation
##'
##' If you want to impute, build model and predict you should use
##' \code{\link{pre.impute.median}} or \code{\link{pre.impute.knn}}.
##' This function imputes using all observations
##' without caring about cross-validation folds.
##'
##' For additional information on the parameters see \code{\link{pre.impute.knn}}
##' and \code{\link{pre.impute.median}}.
##' 
##' @param x Dataset.
##' @param k Number of nearest neighbors to use.
##' @param distmat Distance matrix.
##' @return An imputed matrix.
##' @examples
##' x <- matrix(rnorm(36), 6, 6)
##' x[sample(length(x), 5)] <- NA
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @name impute
##' @seealso \code{\link{emil}}, \code{\link{pre.process}},
##'   \code{\link{pre.impute.knn}}, \code{\link{pre.impute.median}}
{}
##' @examples
##' impute.knn(x)
##' @rdname impute
##' @export
impute.knn <- function(x, k=.05, distmat="auto"){
    pre.impute.knn(x, NULL, fold=rep(TRUE, nrow(x)), k=k, distmat=distmat)$fit$x
}
##' @examples
##' impute.median(x)
##' @rdname impute
##' @export
impute.median <- function(x){
    pre.impute.median(x, NULL, rep(TRUE, nrow(x)))$fit$x
}

