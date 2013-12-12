##' Design of nearest shrunken centroids.
##'
##' Wrapped version of the \code{pamr} package implementation. Note that
##' this function uses internal crossvalidation for determining the value
##' of the shrinkage threshold.
##'
##' @param x Dataset, numerical matrix with observations as rows.
##' @param y Class labels, factor.
##' @param error.fun Error function for tuning.
##' @param slim.fit Set to \code{TRUE} if you want to return the fitted
##'   classifier but discard pamr's \code{cv.objects}, which can be large.
##'   memory efficient. This means that the element \code{cv$cv.objects} 
##'   containing the cross validated fits will be dropped from the returned
##'   classifier.
##' @param cv Cross validation scheme for shrinkage tuning. It should
##'   be supplied on one of the following forms:
##'   \itemize{
##'     \item{Resampling scheme produced with \code{\link{resample.crossval}}
##'       or \code{\link{resample.holdout}}.}
##'     \item{List with elements named \code{nrep} and \code{nfold}}
##'     \item{\code{NA}, \code{NULL} or \code{FALSE} to suppress shrinkage tuning.}
##'   }
##' @param threshold Shrinkage thresholds to try (referred to as 'lambda' in the
##'   litterature). Chosen and tuned automatically by default, but must be given
##'   by the user if not tuned (see the \code{cv} argument) if you wish to use
##'   it with \code{\link{batch.predict}}.
##' @param ... Sent to \code{\link[pamr]{pamr.train}}.
##' @return Fitted LDA or QDA.
##' @examples
##' # TODO 
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso design
##' @export
design.nsc <- function(x, y, error.fun, cv, threshold=NULL, ..., slim.fit=FALSE){
    library(pamr)
    if(missing(error.fun)){
        error.fun.frame <- sapply(sys.frames(), function(env){
            # Dirty hack! TODO: Make nicer
            tryCatch({
                ef <- get("error.fun", envir=env)
                TRUE
            }, error=function(...) FALSE)
        })
        if(any(error.fun.frame)){
            error.fun <- get("error.fun", envir=sys.frames()[[max(which(error.fun.frame))]])
        } else {
            if(is.factor(y)){
                error.fun <- error.rate
            } else if(is.numeric(y)){
                error.fun <- rmse
            } else {
                stop("You must specify an error function!")
            }
        }
    }
    p.data <- list(x=t(x), y=y)
    rm(x, y)
    warn.status <- unlist(options("warn"))
    if(nrow(p.data$x) == 1){
        warning("pamr implementation of NSC is not designed to handle univariate data. Dirty hack used.")
        options(warn = -1)
        p.data$x <- rbind(p.data$x, dummy=0)
    }
    invisible(capture.output(
        tryCatch({
            fit <- pamr::pamr.train(p.data, ...)
            if(missing(cv)){
                fit.cv <- pamr::pamr.cv(fit, p.data)
            } else if(is.null(cv) || (length(cv) == 1 && (is.na(cv) || cv == FALSE))) {
                fit.cv <- NULL
            } else {
                if(!inherits(cv, c("crossval", "holdout")))
                    cv <- resample.crossval(p.data$y, nrep=cv$nrep, nfold=cv$nfold)
                if(nrow(cv) != length(p.data$y))
                    stop("Resampling set for shrinkage selection does not match dataset in size.")
                fit.cv <- pamr::pamr.cv(fit, p.data,
                    folds=lapply(as.data.frame(cv), which))
                options(warn = warn.status)
                if(slim.fit){
                    fit.cv$cv.objects <- NULL
                }
                fit.cv$error <- sapply(seq_along(fit.cv$thres), function(i)
                    error.fun(fit.cv$y, list(pred=fit.cv$yhat[[i]], prob=fit.cv$prob[,,i])))
            }
        }, error=function(...){
            options(warn = warn.status)
            stop(...)
        })
    ))
    return(list(fit=fit, cv=fit.cv))
}


##' Prediction using nearest shrunken centroids.
##'
##' In case multiple thresholds give the same error the largest one is chosen
##' (i.e. the one keeping the fewest features).
##'
##' @method predict nsc
##' @param object Fitted classifier.
##' @param x Dataset of observations to be classified.
##' @param threshold What threshold to use for classification. Can be supplied
##'   in the following ways:
##'   \describe{
##'     \item{Numeric scalar}{A predefined value. In this case you also want to
##'       run \code{\link{design.nsc}} with \code{cv=FALSE} to not do any
##'       unnecessary tuning.}
##'     \item{unset}{Uses the threshold that got the best tuning performance. In
##'       case of ties, the largest threshold is used, i.e. resulting in the
##'       smallest number of variables.}
##'     \item{function}{A function for resolving ties in the tuning. Set it to
##'       \code{\link{min}} to use the smallest threshold that got the best tuning
##'       performance for example.}
##'   }
##' @param ... Ignored
##' @return TODO
##' @examples
##' # TODO
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso predict
##' @export
predict.nsc <- function(object, x, threshold, ...){
    library(pamr)
    if(ncol(x) == 1){
        x <- rbind(t(x), dummy=0)
    } else {
        x <- t(x)
    }
    if(missing(threshold)){
        if(is.null(object$cv)){
            if(length(object$fit$threshold) == 1){
                thres <- object$fit$threshold
            } else {
                stop("The shrinkage parameter `threshold` was neither tuned during the design nor given explicitly.")
            }
        } else {
            thres <- max(object$fit$threshold[object$cv$error == min(object$cv$error)])
        }
    } else if(is.function(threshold)){
        thres <- threshold(object$fit$threshold[object$cv$error == min(object$cv$error)])
    }

    rev(object$fit$threshold)[which.min(rev(object$cv$error))]
    list(pred=pamr::pamr.predict(object$fit, x, type="class", threshold=thres, ...),
         prob=pamr::pamr.predict(object$fit, x, type="posterior", threshold=thres, ...))
}


##' Variable importance of nearest shrunken centroids.
##' 
##' Calculated as the absolute difference between the overall centroid and a
##' classwise shrunken centroid (which is the same for both classes except sign).
##'
##' In case multiple thresholds give the same error the largest one is chosen
##' (i.e. the one keeping the fewest features).
##' 
##' @method vimp nsc
##' @param object Fitted NSC classifier
##' @param threshold What threshold to use for classification.
##' @param ... Ignored.
##' @return An importance vector with elements corresponding to variables.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
vimp.nsc <- function(object, ..., threshold){
    require(pamr)
    if(missing(threshold)){
        if(is.null(object$cv)){
            if(length(object$fit$threshold) == 1){
                thres <- object$fit$threshold
            } else {
                stop("The shrinkage parameter `threshold` was neither tuned during the design nor given explicitly.")
            }
        } else {
            thres <- max(object$fit$threshold[object$cv$error == min(object$cv$error)])
        }
    }
    cen <- (pamr::pamr.predict(object$fit, , thres, type="centroid") -
            object$fit$centroid.overall) / object$fit$sd
    names(cen) <- object$descriptors
    return(cen)
}

