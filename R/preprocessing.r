#' Data preprocessing
#' 
#' These functions are run in \code{\link{batch_model}} just prior to model
#' fitting and serve two purposes. 1) They extract fitting and test sets from
#' the entire dataset and 2) they can at the same time apply a transformation
#' to pre-process the data for handling missing values, scaling, compression
#' etc.
#' They can also be used to modify the form of the data, if required by the
#' fitting function, e.g. \code{\link{pre_pamr}} that transposes the dataset
#' to make it compatible with the \code{pamr} classification method.
#' 
#' Note that all transformations are defined based on the fitting data only
#' and then applied to both fitting set and test set. It is important to not let
#' the test data in any way be part of the model fitting, including the
#' preprocessing, to not risk information leakage and biased results!
#'
#' The imputation functions can also be used outside of the resampling scheme,
#' see \code{\link{impute}}.
#' 
#' @return A list with the following components
#' \describe{
#'     \item{\code{fit}}{Fitting set.}
#'     \item{\code{test}}{Test set.}
#'     \item{\code{features}}{Logical vector indicating which features were kept
#'         (TRUE) and discarded (FALSE). This is only set in case of variable
#'         selection.}
#' }
#'
#' @examples
#' # A splitter that only keeps variables with a class-wise mean difference > `prediction`
#' my_split <- function(x, y, fold, prediction=2){
#'     fit.idx <- index_fit(fold)
#'     test.idx <- index_test(fold)
#'     class.means <- sapply(
#'         split(x[fit.idx,, drop=FALSE], y[fit.idx]),
#'         sapply, mean, na.rm=TRUE)
#'     diff.feats <- apply(class.means, 1, function(x) diff(range(x))) > prediction
#'     return(list(
#'         fit = list(x = x[fit.idx, diff.feats, drop=FALSE],
#'                    y = y[fit.idx]),
#'         test = list(x = x[test.idx, diff.feats, drop=FALSE],
#'                     y = y[test.idx]),
#'         features = diff.feats))
#' }
#' 
#' # Use it during modeling
#' procedure <- modeling_procedure("lda")
#' perf <- evaluate(procedure, x = iris[-5], y = iris$Species,
#'                           pre_process = my_split)
#' 
#' # Example of how the end user can change the `prediction` parameter,
#' # without redefining the function
#' perf <- evaluate(procedure, x = iris[-5], y = iris$Species,
#'                           pre_process = function(...) my_split(..., prediction = 1.3))
#' @seealso \code{\link{emil}}, \code{\link{pre_impute_knn}}
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @name pre_process
{}


#' @param x Dataset.
#' @param y Response vector.
#' @param fold A logical or numeric vector with \code{TRUE} or positive numbers
#'   for fitting observations, \code{FALSE} or \code{0} for test
#'   observations, and \code{NA} for observations not to be included.
#' @rdname pre_process
#' @export
pre_split <- function(x, y, fold){
    if(missing(fold)){
        fold <- structure(rep(TRUE, nrow(x)),
                          class=c("fit_only", "fold", "logical"))
    }
    structure(
        list(fit = list(x = x[index_fit(fold),,drop=FALSE],
                        y = y[index_fit(fold)]),
             test = list(x = x[index_test(fold),,drop=FALSE],
                     y = y[index_test(fold)]),
             feature_selection = structure(rep(TRUE, ncol(x)),
                                           names = colnames(x)),
             fold = fold),
        class=c("preprocessed_data", "list"))
}

#' @export
pre_convert <- function(data, x_fun, y_fun){
    if(!missing(x_fun)){
        data$fit$x <- x_fun(data$fit$x)
        data$test$x <- x_fun(data$test$x)
    }
    if(!missing(y_fun)){
        data$fit$y <- y_fun(data$fit$y)
        data$test$y <- y_fun(data$test$y)
    }
    data
}

#' @export
pre_transpose <- function(data){
    pre_convert(data, x_fun=t)
}

#' @export
pre_remove <- function(data, feature){
    if(!any(feature)){
        return(data)
    }
    if(is.logical(feature)){
        feature <- which(feature)
    } else if(is.character(feature)){
        feature <- match(feature, colnames(data$fit$x))
    }
    data$fit$x <- data$fit$x[, -feature, drop=FALSE]
    data$test$x <- data$test$x[, -feature, drop=FALSE]
    data$feature_selection[feature] <- FALSE
    data
}

#' @rdname pre_process
#' @export
pre_center <- function(data, y=FALSE, na.rm=TRUE){
    m <- colMeans(data$fit$x, na.rm=na.rm)
    data$fit$x <- sweep(data$fit$x, 2, m, "-")
    data$test$x <- sweep(data$test$x, 2, m, "-")
    if(y){
        my <- mean(y, na.rm=na.rm)
        if(is.na(my)) stop("Could not calculate the mean of `y`.")
        data$fit$y <- data$fit$y - my
        data$test$y <- data$test$y - my
    }
    data
}

#' @param scale Whether to scale each feature to have standard deviation = 1.
#' @rdname pre_process
#' @export
pre_scale <- function(data, y=FALSE, na.rm=TRUE, center=TRUE){
    if(center) data <- pre_center(data, y=y)
    s <- apply(data$fit$x, 2, sd, na.rm=na.rm)
    data$fit$x <- sweep(data$fit$x, 2, s, "/")
    data$test$x <- sweep(data$test$x, 2, s, "/")
    if(y){
        sy <- sd(y)
        data$fit$y <- data$fit$y / sy
        data$test$y <- data$test$y / sy
    }
    data
}

#' @export
pre_remove_constant <- function(data){
    pre_remove(data, apply(data$fit$x, 2, sd) == 0)
}

#' @export
pre_remove_correlated <- function(data, cutoff){
    if(missing(cutoff)) stop("`pre_remove_correlated` requires a cutoff.")
    nice_require("caret")
    pre_remove(data, caret::findCorrelation(cor(data$fit$x), cutoff = .75))
}


#' @rdname pre_process
#' @export
pre_impute_median <- function(x, y, fold){
    na.ind <- which(is.na(unname(x)), arr.ind=TRUE)
        # Duplicate names may cause problems otherwise
    na.ind <- na.ind[!is.na(fold[na.ind[,1]]),,drop=FALSE]
    na.feats <- unique(na.ind[,"col"])
    fills <- apply(x[index_fit(fold), na.feats, drop=FALSE], 2, median, na.rm=TRUE)
    x[na.ind] <- fills[match(na.ind[,"col"], na.feats)]
    list(fit = list(x = x[index_fit(fold),,drop=FALSE],
                    y = y[index_fit(fold)]),
         test = list(x = x[index_test(fold),,drop=FALSE],
                     y = y[index_test(fold)]))
}

#' kNN imputation
#' 
#' Nearest neighbor methods needs to have a distance matrix of the dataset it works on.
#' When doing repeated model fittings on subsets of the entire dataset it is
#' unnecessary to recalculate it every time, therefore this function requires
#' the user to manually calculate it prior to resampling and supply it in a
#' wrapper function.
#'
#' Features with fewer than \code{k} non-missing values will be removed
#' automatically.
#' 
#' @param x Dataset.
#' @param y Response vector.
#' @param fold A logical vector with \code{FALSE} for fitting observations,
#'   \code{TRUE} for test observations and \code{NA} for observations not 
#'   to be included.
#' @param k Number of nearest neighbors to calculate mean from. Set to < 1 to
#'   specify a fraction.
#' @param distmat Distance matrix. A matrix, \code{\link{dist}} object or
#'   \code{"auto"}. Notice that \code{"auto"} will recalculate the distance
#'   matrix in each fold, which is only meaningful in case the features of
#'   \code{x} vary between folds. Otherwise you are just wasting time.
#'   
#' @examples
#' x <- iris[-5]
#' x[sample(nrow(x), 30), 3] <- NA
#' my.dist <- dist(x)
#' evaluate(modeling_procedure("lda"), x=x, y=iris$Species,
#'     pre_process=function(...) pre_impute_knn(..., k=4, my.dist))
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
pre_impute_knn <- function(x, y, fold, k=.05, distmat){
    na.ind <- which(is.na(unname(x)), arr.ind=TRUE)
        # Duplicate names may cause problems otherwise
    na.ind <- na.ind[!is.na(fold[na.ind[,1]]),,drop=FALSE]

    if(length(na.ind) == 0)
        return(pre_split(x, y, fold))

    if(k < 1) k <- max(1, round(.05*length(index_fit(fold))))
    if(k > sum(fold > 0, na.rm=TRUE)) stop("k is larger than number of fitting observations.")

    # If a feature has fewer non-NAs than k exclude it
    non.na.count <- apply(!is.na(x[index_fit(fold, allow_oversample=FALSE),]), 2, sum)
    features <- non.na.count >= k
    na.ind <- na.ind[na.ind[,"col"] %in% which(features),,drop=FALSE]

    if(missing(distmat))
        stop("You must supply a distance matrix, see `?pre_impute_knn` for details.")
    if(is.character(distmat) && distmat == "auto"){
        idx <- !is.na(fold)
        prediction <- as.matrix(dist(x[idx,,drop=FALSE]))
        if(any(is.na(prediction)))
            stop("Could not calculate distance matrix, check data set for observations with all values missing.")
        distmat <- matrix(NA, nrow(x), nrow(x))
        distmat[idx, idx] <- prediction
    } else if(!is.matrix(distmat)){
        distmat <- as.matrix(distmat)
    }
    if(any(nrow(x) != dim(distmat)))
        stop("Distance matrix does not match dataset.")

    diag(distmat) <- NA
    NN <- apply(distmat, 1, function(z)
        intersect(order(z), index_fit(fold)))
    fills <- apply(na.ind, 1, function(i){
        xf <- x[NN[,i[1]], i[2]]
        xf <- xf[!is.na(xf)]
        if(length(xf) < k) NA else mean(xf[1:k])
    })
    if(any(is.na(fills)))
        stop("Could not impute all missing values, too few non-missing values for some features.")
    x[na.ind] <- fills
    c(
        list(fit = list(x = x[index_fit(fold), features, drop=FALSE],
                        y = y[index_fit(fold)]),
             test = list(x = x[index_test(fold), features, drop=FALSE],
                         y = y[index_test(fold)])),
        if(all(features)) NULL else list(features = features)
    )
}



#' Regular imputation
#'
#' If you want to impute, build model and predict you should use
#' \code{\link{pre_impute_median}} or \code{\link{pre_impute_knn}}.
#' This function imputes using all observations
#' without caring about cross-validation folds.
#'
#' For additional information on the parameters see \code{\link{pre_impute_knn}}
#' and \code{\link{pre_impute_median}}.
#' 
#' @param x Dataset.
#' @param k Number of nearest neighbors to use.
#' @param distmat Distance matrix.
#' @return An imputed matrix.
#' @examples
#' x <- matrix(rnorm(36), 6, 6)
#' x[sample(length(x), 5)] <- NA
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @name impute
#' @seealso \code{\link{emil}}, \code{\link{pre_process}},
#'   \code{\link{pre_impute_knn}}, \code{\link{pre_impute_median}}
{}
#' @examples
#' impute_knn(x)
#' @rdname impute
#' @export
impute_knn <- function(x, k=.05, distmat="auto"){
    pre_impute_knn(x, NULL, fold=rep(TRUE, nrow(x)), k=k, distmat=distmat)$fit$x
}
#' @examples
#' impute_median(x)
#' @rdname impute
#' @export
impute_median <- function(x){
    pre_impute_median(x, NULL, rep(TRUE, nrow(x)))$fit$x
}

