#TODO: Make sure all functions can handle both matrices and data.frames

##' Data preprocessing
##' 
##' These functions are run in \code{\link{batch.model}} just prior to model
##' fitting and serve two purposes. 1) They extract fitting and test sets from
##' the entire dataset and 2) they can at the same time apply a transformation
##' to pre-process the data for handling missing values, scaling, compression
##' etc.
##'
##' They can also be used to modify the form of the data, if required by the
##' fitting function, e.g. \code{\link{pre.pamr}} that transposes the dataset
##' to make it compatible with the \code{pamr} and \code{glmnet} processes.
##' 
##' The following functions are provided in this package:
##' \describe{
##'     \item{\code{\link{pre.split}}}{Only split, no transformation.}
##'     \item{\code{\link{pre.center}}}{Center data to have mean 0 of each feature.}
##'     \item{\code{\link{pre.scale}}}{Center and scale data to have mean 0 and standard deviation 1. }
##'     \item{\code{\link{pre.impute.median}}}{Impute missing values with feature medians.}
##'     \item{\code{\link{pre.impute.knn}}}{Impute missing values with k-NN, see
##'         \code{\link{pre.impute.knn}} for details on how to set parameters.}
##' }
##' 
##' Note that all transformations are defined based on the fitting data only
##' and then applied to both fitting set and test set. It is important to not let
##' the test data in any way be part of the model fitting, including the
##' preprocessing, to not risk information leakage and biased results!
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
##' # A splitter that only keeps variables with a classwise mean difference > `d`
##' \dontrun{
##' my.split <- function(x, y, fold, d=2){
##'     fit.idx <- na.fill(!fold, FALSE)
##'     test.idx <- na.fill(fold, FALSE)
##'     class.means <- sapply(
##'         split(as.data.frame(x[fit.idx,,drop=FALSE]), y[fit.idx]),
##'         sapply, mean, na.rm=TRUE)
##'     diff.feats <- apply(class.means, 1, range) > d
##'     return(list(
##'         fit = x[fit.idx, diff.feats, drop=FALSE],
##'         test = x[test.idx, diff.feats, drop=FALSE],
##'         features = diff.feats))
##' }
##'
##' # Use it during modeling
##' proc <- modeling.procedure("rf")
##' perf <- evaluate.modeling(proc, x, y, pre.process=my.split)
##'
##' # Example of how the end user can change the `d` parameter,
##' # without redefining the function
##' perf <- evaluate.modeling(proc, x, y,
##'                            pre.process=function(...) my.split(..., d=4))
##' }
##' @name pre.process
{}


##' @param x Dataset.
##' @param y Response vector.
##' @param fold A logical vector with \code{FALSE} for fitting observations,
##'   \code{TRUE} for test observations and \code{NA} for observations not 
##'   to be included.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @rdname pre.process
##' @export
pre.split <- function(x, y, fold){
    list(fit=x[na.fill(!fold, FALSE),,drop=FALSE],
         test=x[na.fill(fold, FALSE),,drop=FALSE])
}

##' @rdname pre.process
##' @export
pre.center <- function(x, y, fold){
    pre.scale(x, fold, scale=FALSE)
}

##' @param scale Wether to scale each feature to have standard deviation = 1.
##' @rdname pre.process
##' @export
pre.scale <- function(x, y, fold, scale=TRUE){
    m <- apply(x[na.fill(!fold, FALSE),,drop=FALSE], 2, mean, na.rm=TRUE)
    if(scale){
        s <- apply(x[na.fill(!fold, FALSE),,drop=FALSE], 2, sd, na.rm=TRUE)
        fun <- function(z) sweep(sweep(z, 2, m, "-"), 2, s, "/")
    } else {
        fun <- function(z) sweep(z, 2, m, "-")
    }
    list(fit=fun(x[na.fill(!fold, FALSE),,drop=FALSE]),
         test=fun(x[na.fill(fold, FALSE),,drop=FALSE]))
}

##' @author Christofer \enc{Bäcklin}{Backlin}
##' @rdname pre.process
##' @export
pre.impute.median <- function(x, y, fold){
    na.ind <- which(is.na(unname(x)), arr.ind=TRUE)
        # Duplicate names may cause problems otherwise
    na.ind <- na.ind[!is.na(fold[na.ind[,1]]),,drop=FALSE]
    na.feats <- unique(na.ind[,"col"])
    fills <- apply(x[na.fill(!fold, FALSE), na.feats, drop=FALSE], 2, median, na.rm=TRUE)
    x[na.ind] <- fills[match(na.ind[,"col"], na.feats)]
    list(fit=x[na.fill(!fold, FALSE),,drop=FALSE],
         test=x[na.fill(fold, FALSE),,drop=FALSE])
}
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @rdname impute
##' @export
impute.median <- function(x){
    pre.impute.median(x, , rep(FALSE, nrow(x)))$fit
}

##' Foldwise kNN imputation
##' 
##' Any k-NN method needs to have a distance matrix of the dataset it works on.
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
##' @param k Number of nearest neightbors to calculate mean from. Set to < 1 to
##'   specify a fraction.
##' @param distmat Distance matrix. A matrix, \code{\link{dist}} object or
##'   \code{"auto"}. Notice that \code{"auto"} will recalculate the distance
##'   matrix in each fold, which is only meaningful in case the features of
##'   \code{x} vary between folds. Otherwise you are just wasting time.
##'   
##' @examples
##' \dontrun{
##' x <- sweep(matrix(rnorm(60*10), 60), 1, rep(0:1/3, each=30))
##' x[sample(length(x), 10)] <- NA
##' y <- gl(2,30)
##' my.dist <- dist(x)
##' evaluate.modeling(modeling.procedure("lda"), x, y,
##'     pre.process=function(...) pre.impute.knn(..., k=4, my.dist))
##' }
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
pre.impute.knn <- function(x, y, fold, k=.05, distmat){
    if(!is.matrix(x))
        stop("kNN does not work on data with mixed featured types. Therefore as a precausion kNN imputation only accept data in matrix form.")

    if(k < 1) k <- max(1, round(.05*nrow(x)))
    if(k > sum(!fold, na.rm=TRUE)) stop("k is larger than number of fitting observations.")

    if(missing(distmat))
        stop("You must supply a diastance matrix, see `?pre.impute.knn` for details.")
    if(is.character(distmat) && distmat == "auto"){
        distmat <- matrix(NA, nrow(x), nrow(x))
        idx <- !is.na(fold)
        distmat[idx, idx] <- as.matrix(dist(x[idx,]))
    } else if(!is.matrix(distmat)){
        distmat <- as.matrix(distmat)
    }
    if(any(nrow(x) != dim(distmat)))
        stop("Distance matrix does not match dataset.")

    na.ind <- which(is.na(unname(x)), arr.ind=TRUE)
        # Duplicate names may cause problems otherwise
    na.ind <- na.ind[!is.na(fold[na.ind[,1]]),,drop=FALSE]

    NN <- apply(distmat, 1, function(z)
        setdiff(order(z), which(na.fill(fold, TRUE))))
    fills <- apply(na.ind, 1, function(i){
        mean(na.exclude(x[NN[-1, i[1]], i[2]])[1:k])
    })
    x[na.ind] <- fills
    list(fit=x[na.fill(!fold, FALSE),,drop=FALSE],
         test=x[na.fill(fold, FALSE),,drop=FALSE])
}

##' Regular imputation
##'
##' If you want to impute, build model and predict you should use
##' \code{\link{pre.impute.median}} or \code{\link{pre.impute.knn}}.
##' This function imputes using all observations
##' without caring about crossvalidation folds.
##'
##' For additional information on the parameters see \code{\link{pre.impute.knn}}.
##' 
##' @param x Dataset.
##' @param k Number of nearest neighbors to use.
##' @param distmat Distance matrix.
##' @return An imputed matrix.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @rdname impute
##' @export
impute.knn <- function(x, k=.05, distmat){
    pre.impute.knn(x, fold=rep(FALSE, nrow(x)), k=k, distmat=distmat)$fit
}

