#' Data preprocessing
#' 
#' These functions are run in \code{\link{evaluate}} just prior to model
#' fitting, to extract fitting and test sets from the entire dataset and apply
#' transformations to pre-process the data (for handling missing values,
#' scaling, compression etc.).
#' They can also be used to adapt the form of the data to a specific
#' fitting function, e.g. \code{\link{pre_pamr}} that transposes the dataset
#' to make it compatible with the \code{pamr} classification method.
#' 
#' When supplied to \code{\link{evaluate}}, pre-processing functions can be
#' chained (i.e. executed sequentially) after an initating call to
#' \code{\link{pre_split}}.
#' This can either be done using the \code{\link[=chain]{pipe operator}} defined
#' in the \pkg{magrittr} package or by putting all pre-processing functions in a
#' regular list (see the examples).
#' 
#' Note that all transformations are defined based on the fitting data only
#' and then applied to both fitting set and test set. It is important to not let
#' the test data in any way be part of the model fitting, including the
#' preprocessing, to not risk information leakage and biased results!
#'
#' The imputation functions can also be used outside of
#' \code{\link{evaluate}} by not supplying a fold to
#' \code{\link{pre_split}}.
#' See the code of \code{\link{impute_median}} for an example.
#' 
#' @return A list with the following components
#' \describe{
#'     \item{\code{fit}}{Fitting set.}
#'     \item{\code{test}}{Test set.}
#'     \item{\code{feature_selection}}{Logical vector indicating which features were kept
#'         (TRUE) and discarded (FALSE).}
#'     \item{\code{fold}}{The fold that was used to split the data.}
#' }
#'
#' @example examples/pre-process.r
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
    if(is.character(y) && length(y) == 1){
        y_col <- match(y, colnames(x))
        if(is.na(y_col))
            stop(paste("The is no column named", y, "in `x`."))
        y <- x[,y_col]
        x <- x[,-y_col]
    }
    structure(
        list(name = deparse(substitute(x)),
             fit = list(x = x[index_fit(fold),,drop=FALSE],
                        y = y[index_fit(fold)]),
             test = list(x = x[index_test(fold),,drop=FALSE],
                         y = y[index_test(fold)]),
             feature_selection = structure(rep(TRUE, ncol(x)),
                                           names = colnames(x)),
             fold = fold),
        class=c("preprocessed_data", "list"))
}

#' @param data Fitting and testing data sets, as returned by
#'   \code{\link{pre_split}}.
#' @param x_fun Function to apply to the descriptors of the datasets
#'   (e.g. \code{x}). This function will be applied independenly to the fitting
#'   and testing sets.
#' @param y_fun Function to be applied to the response of the training and test
#'   sets (independently).
#' @param ... Sent to internal methods, see the code of each function.
#' @rdname pre_process
#' @export
pre_convert <- function(data, x_fun, y_fun, ...){
    if(!missing(x_fun)){
        data$fit$x <- x_fun(data$fit$x, ...)
        data$test$x <- x_fun(data$test$x, ...)
    }
    if(!missing(y_fun)){
        data$fit$y <- y_fun(data$fit$y, ...)
        data$test$y <- y_fun(data$test$y, ...)
    }
    data
}

#' @rdname pre_process
#' @export
pre_transpose <- function(data){
    pre_convert(data, x_fun=t)
}

#' @param feature The features to be removed. Can be integer, logical or
#'   character.
#' @rdname pre_process
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

#' @param na.rm A logical value indicating whether \code{NA} values should be
#'   ignored.
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

#' @param center Whether to center the data before scaling.
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

#' @rdname pre_process
#' @export
pre_remove_constant <- function(data){
    pre_remove(data, apply(data$fit$x, 2, sd) == 0)
}

#' @param cutoff See \code{\link[caret]{findCorrelation}}.
#' @rdname pre_process
#' @export
pre_remove_correlated <- function(data, cutoff){
    if(missing(cutoff)) stop("`pre_remove_correlated` requires a cutoff.")
    nice_require("caret")
    pre_remove(data, caret::findCorrelation(cor(data$fit$x), cutoff = .75))
}

#' @param ncomponent Number of PCA components to use. Missing all components
#'   are used.
#' @rdname pre_process
#' @export
pre_pca <- function(data, ncomponent, ...){
    if(missing(ncomponent)){
        pca <- prcomp(data$fit$x, ..., retx = TRUE)
        data$fit$x <- pca$x
    } else {
        pca <- prcomp(data$fit$x, ..., retx = FALSE)
        pca$rotation <- pca$rotation[,1:ncomponent]
        data$fit$x <- predict(pca, data$fit$x)
    }
    data$test$x <- predict(pca, data$test$x)
    data
}

#' Support function for identifying missing values
#' 
#' @param data Fitting and testing data sets, as returned by
#'   \code{\link{pre_split}}.
#' @return Data frame containing row and column indices of missing values or
#'   \code{NULL} if the data doesn't contain any.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
na_index <- function(data){
    # `unname` is needed to avoid problems with duplicate names
    fit.na  <- which(is.na(unname(data$fit$x)), arr.ind=TRUE)
    test.na <- which(is.na(unname(data$test$x)), arr.ind=TRUE)
    rbind(
        if(any(fit.na)) data.frame(set = "fit", fit.na) else NULL,
        if(any(test.na)) data.frame(set = "test", test.na) else NULL
    )
}

#' Basic imputation
#'
#' This solution is optimized for the scenario that the dataset is very large
#' but only contains missing values in a small number of columns.
#'
#' @param data Fitting and test datasets, as returned by \code{\link{pre_split}}
#'   or any other standard pre-processing function.
#' @param fun Function for calculating imputation values. Should take a vector
#'  and return a scalar.
#' @param ... Sent to \code{fun}.
#' @return A pair of fitting and testing datasets.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
pre_impute <- function(data, fun, ...){
    na.ind <- na_index(data)
    if(is.null(na.ind)) return(data)

    na.feats <- unique(na.ind$col)
    m <- rep(NA, ncol(data$fit$x))
    m[na.feats] <- apply(data$fit$x[,na.feats,drop=FALSE], 2, fun, ...)

    na.ind %<>%
        split(na.ind$set) %>%
        lapply(function(x) as.matrix(x[-1]))
    if(!is.null(na.ind$fit))  data$fit$x[na.ind$fit]   <- m[na.ind$fit[,"col"]]
    if(!is.null(na.ind$test)) data$test$x[na.ind$test] <- m[na.ind$test[,"col"]]

    impute_failed <- intersect(which(is.na(m)), na.feats)
    if(any(impute_failed)){
        data <- pre_remove(data, impute_failed)
        warning(sprintf("Could not impute %i features.", length(impute_failed)))
    }
    data
}
#' @rdname pre_impute
#' @export
pre_impute_median <- function(data){
    pre_impute(data, fun=median, na.rm=TRUE)
}
#' @rdname pre_impute
#' @export
pre_impute_mean <- function(data){
    pre_impute(data, fun=mean, na.rm=TRUE)
}

#' Nearest neighbors imputation
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
#' @param data Fitting and testing data sets, as returned by
#'   \code{\link{pre_split}}.
#' @param k Number of nearest neighbors to calculate mean from. Set to < 1 to
#'   specify a fraction.
#' @param distance_matrix A matrix, \code{\link{dist}} object or
#'   \code{"auto"}. Notice that \code{"auto"} will recalculate the distance
#'   matrix in each fold, which is only meaningful in case the features of
#'   \code{x} vary between folds. Otherwise you are just wasting time.
#'   
#' @examples
#' x <- iris[-5]
#' x[sample(nrow(x), 30), 3] <- NA
#' my.dist <- dist(x)
#' evaluate(modeling_procedure("lda"), x = x, y = iris$Species,
#'     pre_process = function(...){
#'         pre_split(...) %>% pre_impute_knn(k = 4, distance_matrix = my.dist)
#'     }
#' )
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
pre_impute_knn <- function(data, k=.05, distance_matrix){
    na.ind <- na_index(data)
    if(is.null(na.ind)) return(data)

    if(k < 1) k <- max(1, round(.05*length(index_fit(data$fold))))
    if(k > sum(data$fold > 0, na.rm=TRUE))
        stop("k is larger than number of fitting observations.")

    # Exclude features with fewer non-NAs than k
    na.count <- na.ind %>%
        filter_("set == 'fit'") %>%
        group_by_("col") %>%
        summarize_(n = "length(row)")
    impute_failed <- nrow(data$fit$x) - na.count$n < k
 
    if(any(impute_failed)){
        data <- pre_remove(data, impute_failed)
        warning(sprintf("Could not knn-impute %i features.", 
                        length(impute_failed)))
        na.ind %<>% filter_(~!col %in% impute_failed)
        na.ind$col <- na.ind$col - findInterval(na.ind$col, which(impute_failed))
    }

    # Check that the distance matrix is in order
    if(missing(distance_matrix))
        stop("You must supply a distance matrix, see `?pre_impute_knn` for details.")
    if(is.character(distance_matrix) && distance_matrix == "auto"){
        distance_matrix <- matrix(nrow = length(data$fold), ncol = length(data$fold))
        ind <- c(index_fit(data$fold, allow_oversample=FALSE),
                 index_test(data$fold))
        distance_matrix[ind, ind] <- as.matrix(dist(rbind(data$fit$x, data$test$x)))
    } else if(!is.matrix(distance_matrix)){
        distance_matrix <- as.matrix(distance_matrix)
    }
    if(any(length(data$fold) != dim(distance_matrix)))
        stop("Distance matrix does not match dataset.")

    # Perform the imputation
    diag(distance_matrix) <- NA
    NN <- as.data.frame(apply(distance_matrix[na.ind$row, index_fit(data$fold)], 1, order))
    na.ind$fill <- mapply(function(i, col){
        x <- data$fit$x[i, col]
        mean(x[!is.na(x)][1:k])
    }, NN, na.ind$col)

    if(any(na.ind$set == "fit"))
        data$fit$x[as.matrix(na.ind[na.ind$set == "fit", c("row", "col")])] <-
            na.ind$fill[na.ind$set == "fit"]
    if(any(na.ind$set == "test"))
        data$test$x[as.matrix(na.ind[na.ind$set == "test", c("row", "col")])] <-
            na.ind$fill[na.ind$set == "test"]
    data
}

#' Regular imputation
#'
#' If you want to impute, build model and predict you should use
#' \code{\link{pre_impute_median}} or \code{\link{pre_impute_knn}}.
#' This function imputes using all observations
#' without caring about cross-validation folds.
#'
#' For additional information on the parameters see \code{\link{pre_impute_knn}}
#' and \code{\link{pre_impute}}.
#' 
#' @param x Dataset.
#' @param k Number of nearest neighbors to use.
#' @param distance_matrix Distance matrix.
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
impute_knn <- function(x, k=.05, distance_matrix="auto"){
    if(identical(distance_matrix, "auto"))
        distance_matrix <- dist(x)
    pre_split(x, y=NULL) %>%
        pre_impute_knn(k=k, distance_matrix=distance_matrix) %>%
        (function(data) data$fit$x)
}
#' @examples
#' impute_median(x)
#' @rdname impute
#' @export
impute_median <- function(x){
    pre_split(x, y=NULL) %>%
        pre_impute_median %>%
        (function(data) data$fit$x)
}

#' Print method for pre-processed data
#' 
#' @method print preprocessed_data
#' @param x Pre-processed data, as produced by \code{\link{pre_split}}.
#' @param ... Ignored, kept for S3 consistency.
#' @return Nothing
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
print.preprocessed_data <- function(x, ...){
    feature <- table(factor(x$feature_selection, c(FALSE, TRUE)))
    cat("Pre-processed data set `", x$name, "` of ",
        feature[2], " features",
        if(feature[1] > 0) sprintf("(%i removed).\n", feature[1]) else ".\n",
        nrow(x$fit$x), " observations for model fitting,\n",
        nrow(x$test$x), " observations for model evaluation.\n", sep="")
}
