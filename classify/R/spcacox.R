##' @import superpc
{}

##' Design a proportional hazards model on super-PCA-data
##'
##' Modelled after a paper by Bair and Tibshirani 2004. Reference coming.
##'
##' @param x Dataset, no missing values allowed.
##' @param y Survival vector of class \code{\link{outcome}} or \code{\link{Surv}}.
##' @param verbose Wether to print output 
##' @return A fitted classifier.
##' @author Eva Freyhult, Christofer \enc{Bäcklin}{Backlin}
##' @export
design.spcacox <- function(x, y, verbose=FALSE){
    if(is.outcome(y)) y <- as.Surv(y)
    if(any(is.na(y))){
        x <- t(x[!is.na(y),])
        y <- y[!is.na(y)]
    } else {
        x <- t(x)
    }
    if(any(is.na(x))) stop("Data contains missing values, `spcacox` could not be designed.")

    training.data <- list(x = x, y = y[,1], censoring.status = 1-y[,2])
    if(!verbose) sink("/dev/null")
    f1 <- superpc.train(training.data, type = "survival")
    f2 <- superpc.cv(f1, training.data, n.components=10)
    if(!verbose) sink()

    # Get the best threshold and number of principal components
    imax <- which(f2$scor == max(f2$scor, na.rm=TRUE), arr.ind = TRUE)
    n.components <- imax[1]
    threshold <- f2$threshold[imax[2]]

    # Project the data
    which.features <- (abs(f1$feature.scores) >= threshold)
    x <- x[which.features,]
    x.sml.svd <- getFromNamespace("mysvd", "superpc")(x, n.components = n.components)
    x <- t(scale(t(x), center=x.sml.svd$feature.means, scale=FALSE))
    scal <- apply(scale(abs(x.sml.svd$u), center=FALSE, scale=x.sml.svd$d), 2, sum)
    cur.v0 <- scale(t(x) %*% x.sml.svd$u, center=FALSE, scale=scal * x.sml.svd$d)

    # Fit Cox model
    coxfit <- coxph(y ~ ., as.data.frame(cur.v0))

    return(list(train=f1, cv=f2, n.components=n.components, threshold=threshold,
                which.features=which.features, svd=x.sml.svd, scal=scal, coxfit=coxfit))
}


##' Prediction using fitted supercox classifier
##' 
##' @method predict spcacox
##' @param object Fitted classifier.
##' @param x New data to make predictions on.
##' @param ... Ignored, kept fer S3 consistency.
##' @return A vector of risk scores for the observations in \code{test.subset}.
##' @author Eva Freyhult, Christofer \enc{Bäcklin}{Backlin}
##' @export
predict.spcacox <- function(object, x, ...){
    x <- t(scale(x[,object$which.features], center=object$svd$feature.means, scale=FALSE))
    cur.v0 <- scale(t(x) %*% object$svd$u, center=FALSE, scale=object$scal * object$svd$d)
    return(list(risk=predict(object$coxfit, as.data.frame(cur.v0))))
}


# ##' Cox regression on principal components
# ##'
# ##' @param time Time to event.
# ##' @param event Event type, level 1 for no event, 2 for event of interest, >2 for
# ##'   competing events.
# ##' @param x Covariates, observations as columns. Missing values are kNN-imputed.
# ##'   The deviation from the package standard of having observations as rows is
# ##'   due to that the function is typically used in molecular biology where it
# ##'   is customary to have observations (which are few) as columns and variables
# ##'   as rows (which are many).
# ##' @param test.subset Observations to predict on, but not to be used for design.
# ##' @param ncomp Number of components to keep. If \code{ncomp < 1} it is
# ##'   interpreted as the minimal number components required to model that
# ##'   fraction of the variance in the design set of \code{x}.
# ##' @return A vector of risk scores for the observations in \code{test.subset}.
# ##' @author Christofer \enc{Bäcklin}{Backlin}
# ##' @export
# pca.cox <- function(time, event, x, test.subset, ncomp=.5){
#     if(any(is.na(x))){
#         keep <- apply(x, 1, function(xx) mean(is.na(xx))) < .1
#         if(any(!keep)) x <- x[keep,]
#         # Impute designset using only the design set
#         x[,!test.subset] <- impute.knn(x[,!test.subset],
#                                        k=max(1, round(.05*sum(!test.subset))))[[1]]
#         # Impute testset with the entire dataset
#         x[,test.subset]  <- impute.knn(x, k=max(1, round(.05*ncol(x))))[[1]][,test.subset]
#     }
#     x.pca <- prcomp(t(x[,!test.subset]), retx=FALSE)
#     if(ncomp < 1){
#         ncomp <- which(cumsum(x.pca$sdev^2)/sum(x.pca$sdev^2) > ncomp)[1]
#     }
#     x <- t(sweep(x, 1, apply(x, 1, mean))) %*% x.pca$rotation[,1:ncomp]
# 
#     coxfit <- coxph(Surv(time, as.integer(event) == 2) ~ x, subset=!test.subset)
#     return(predict(coxfit, as.data.frame(x))[test.subset])
# }


