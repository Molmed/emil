##' Data preprocessing
##' 
##' These functions are run in \code{\link{batch.predict}} just prior to model
##' fitting and serve two purposes. 1) They extract design and test sets from
##' the entire dataset and 2) they can at the same time apply a transformation
##' to preprocess the data for handling missing values, scaling, compression
##' etc.
##' 
##' The following functions are provided in this package:
##' \tabular{ll}{
##'     \code{\link{pre.split}} \tab Only split, no transformation.\cr
##'     \code{\link{pre.center}} \tab Center data to have mean 0 of each feature.\cr
##'     \code{\link{pre.scale}} \tab Center and scale data to have mean 0 and standard deviation 1. \cr
##'     \code{\link{pre.impute.median}} \tab Impute missing values with feature medians.\cr
##'     \code{\link{pre.impute.knn}} \tab Impute missing values with k-NN, see
##'         \code{\link{pre.impute.knn}} for details on how to set parameters.\cr
##' }
##' 
##' Note that all transformations are defined based on the design data only
##' and then applied to both design set and test set. It is important to not let
##' the test data in any way be part of the fitting of a model, including the
##' preprocessing, to not risk information leakage and biased results!
##' 
##' @return A list with the following components
##' \tabular{ll}{
##'     \code{design} \tab Design set.\cr
##'     \code{test} \tab Test set.\cr
##'     \code{features} \tab Logical vector indicating which features were kept
##'         (TRUE) and discarded (FALSE). This is only set in case of variable
##'         selection.\cr
##' }
##' @name pre.trans
{}


##' @param x Dataset.
##' @param y Response vector.
##' @param fold A logical vector with \code{FALSE} for design observations,
##'   \code{TRUE} for test observations and \code{NA} for observations not 
##'   to be included.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @rdname pre.trans
##' @export
pre.split <- function(x, y, fold){
    list(design=x[na.fill(!fold, FALSE),,drop=FALSE],
         test=x[na.fill(fold, FALSE),,drop=FALSE])
}

##' @rdname pre.trans
##' @export
pre.center <- function(x, y, fold){
    pre.scale(x, fold, scale=FALSE)
}

##' @param scale Wether to scale each feature to have standard deviation = 1.
##' @rdname pre.trans
##' @export
pre.scale <- function(x, y, fold, scale=TRUE){
    m <- apply(x[na.fill(!fold, FALSE),,drop=FALSE], 2, mean, na.rm=TRUE)
    if(scale){
        s <- apply(x[na.fill(!fold, FALSE),,drop=FALSE], 2, sd, na.rm=TRUE)
        fun <- function(z) sweep(sweep(z, 2, m, "-"), 2, s, "/")
    } else {
        fun <- function(z) sweep(z, 2, m, "-")
    }
    list(design=fun(x[na.fill(!fold, FALSE),,drop=FALSE]),
         test=fun(x[na.fill(fold, FALSE),,drop=FALSE]))
}

##' @author Christofer \enc{Bäcklin}{Backlin}
##' @rdname pre.trans
##' @export
pre.impute.median <- function(x, y, fold){
    na.ind <- which(is.na(unname(x)), arr.ind=TRUE)
        # Duplicate names may cause problems otherwise
    na.ind <- na.ind[!is.na(fold[na.ind[,1]]),]
    na.feats <- unique(na.ind[,"col"])
    fills <- apply(x[na.fill(!fold, FALSE), na.feats], 2, median, na.rm=TRUE)
    x[na.ind] <- fills[match(na.ind[,"col"], na.feats)]
    list(design=x[na.fill(!fold, FALSE),,drop=FALSE],
         test=x[na.fill(fold, FALSE),,drop=FALSE])
}
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @rdname impute
##' @export
impute.median <- function(x){
    pre.impute.median(x, , rep(FALSE, nrow(x)))$design
}

##' Foldwise kNN imputation
##' 
##' Any k-NN method needs to have a distance matrix of the dataset it works on.
##' When doing repeated model designs on subsets of the entire dataset it is
##' unnecessary to recalculate it every time, therefore this function requires
##' the user to manually calculate it prior to resampling and supply it in a
##' wrapper function.
##' 
##' @param x Dataset.
##' @param y Response vector.
##' @param fold A logical vector with \code{FALSE} for design observations,
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
##' batch.predict(x, y, "lda", pre.trans=function(...) pre.impute.knn(..., k=4, my.dist))
##' }
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
pre.impute.knn <- function(x, y, fold, k=.05, distmat){
    if(!is.matrix(x))
        stop("kNN does not work on data with mixed featured types. Therefore as a precausion kNN imputation only accept data in matrix form.")

    if(k < 1) k <- max(1, round(.05*nrow(x)))
    if(k > sum(!fold, na.rm=TRUE)) stop("k is larger than number of design observations.")

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
    list(design=x[na.fill(!fold, FALSE),,drop=FALSE],
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
    pre.impute.knn(x, fold=rep(TRUE, nrow(x)), k=k, distmat=distmat)$design
}

