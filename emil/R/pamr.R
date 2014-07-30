##' PAMR adapted dataset pre-processing
##'
##' The predict framework is designed to work with dataset where rows correspond
##' to observations and columns to descriptors. PAMR wants it the other way, and
##' also to have the fitting set response vector supplied in a list with the
##' descriptors. This function applies a standard pre-processing function and
##' then reformats the result to satisfy PAMR.
##'
##' @param x Dataset.
##' @param y Response vector.
##' @param fold A logical vector with \code{FALSE} for fitting observations,
##'   \code{TRUE} for test observations and \code{NA} for observations not 
##'   to be included.
##' @param pre.process A pre-processing function to be wrapped.
##' @param ... Sent to \code{pre.process}.
##' @return A list with fitting and testing sets, formatted the way pamr wants
##'   them.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso \code{\link{emil}}, \code{\link{pre.process}}
##' @export
pre.pamr <- function(x, y, fold, pre.process=pre.split, ...){
    sets <- pre.process(x, y, fold, ...)
    if(ncol(x) == 1){
        warn.once("pamr_univariate", "PAMR not designed to handle univariate data. An all-zero dummy variable added as a workaround.")
    }
    sets$fit$x <- structure(class = "pamr.data", .Data = list(
        x = if(ncol(x) == 1) rbind(t(sets$fit$x), dummy=0) else t(sets$fit$x),
        y = sets$fit$y))
    sets$test$x <- if(ncol(x) == 1) rbind(t(sets$test$x), dummy=0) else t(sets$test$x)
    sets
}

##' Fit nearest shrunken centroids model.
##'
##' Wrapped version of the \code{pamr} package implementation. Note that
##' this function uses internal cross-validation for determining the value
##' of the shrinkage threshold.
##'
##' @param x Dataset, numerical matrix with observations as rows.
##' @param y Class labels, factor.
##' @param error.fun Error function for tuning.
##' @param slim.fit Set to \code{TRUE} if you want to return the fitted
##'   classifier but discard pamr's \code{cv.objects}, which can be large.
##'   memory efficient. This means that the element \code{cv$cv.objects} 
##'   containing the cross-validated fits will be dropped from the returned
##'   classifier.
##' @param cv Cross-validation scheme for shrinkage tuning. It should
##'   be supplied on one of the following forms:
##'   \itemize{
##'     \item{Resampling scheme produced with \code{\link{resample}}
##'       or \code{\link{resample.holdout}}.}
##'     \item{List with elements named \code{nrep} and \code{nfold}}
##'     \item{\code{NA}, \code{NULL} or \code{FALSE} to suppress shrinkage tuning.}
##'   }
##' @param threshold Shrinkage thresholds to try (referred to as 'lambda' in the
##'   literature). Chosen and tuned automatically by default, but must be given
##'   by the user if not tuned (see the \code{cv} argument) if you wish to use
##'   it with \code{\link{batch.model}}.
##' @param ... Sent to \code{\link[pamr]{pamr.train}}.
##' @param thres.fun Threshold selection function. Note that it is not uncommon
##'   that several thresholds will result in the same tuning error.
##' @return Fitted pamr classifier.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso \code{\link{emil}}, \code{\link{emil.predict.pamr}},
##'   \code{\link{emil.vimp.pamr}}, \code{\link{modeling.procedure}}
##' @export
emil.fit.pamr <- function(x, y, error.fun, cv, threshold=NULL, ...,
                          thres.fun = function(thr, err) median(thr[err == min(err)]),
                          slim.fit=FALSE){
    nice.require("pamr")
    if(!(is.list(x) && all(c("x", "y") %in% names(x)) && ncol(x$x) == length(x$y))){
        warn.once("pamr_preprocess", "Please use the `pre.pamr` function for pre-processing data for pamr.")
        x <- list(x=t(x), y=y)
    }
    rm(y)
    if(missing(error.fun)){
        error.fun.frame <- sapply(sys.frames(), function(env){
            tryCatch({
                ef <- get("error.fun", envir=env)
                TRUE
            }, error=function(...) FALSE)
        })
        if(any(error.fun.frame)){
            error.fun <- get("error.fun", envir=sys.frames()[[max(which(error.fun.frame))]])
        } else {
            if(is.factor(x$y)){
                error.fun <- error.rate
            } else if(is.numeric(x$y)){
                error.fun <- rmse
            } else {
                stop("You must specify an error function!")
            }
        }
    }
    invisible(capture.output(
        tryCatch({
            fit <- pamr::pamr.train(x, threshold=threshold, ...)
            if(missing(threshold) || length(threshold) > 1){
                if(missing(cv)){
                    fit.cv <- pamr::pamr.cv(fit, x)
                } else if(is.blank(cv)){
                    stop("You cannot skip cross-validation when multiple thresholds are given.")
                } else {
                    if(!inherits(cv, c("crossval", "holdout")))
                        cv <- resample("crossval", x$y, nrep=cv$nrep, nfold=cv$nfold)
                    if(nrow(cv) != length(x$y))
                        stop("Resampling set for shrinkage selection does not match dataset in size.")
                    fit.cv <- pamr::pamr.cv(fit, x,
                        folds=lapply(cv, function(x) which(x == 0)))
                    if(slim.fit){
                        fit.cv$cv.objects <- NULL
                    }
                    fit.cv$error <- sapply(seq_along(fit.cv$threshold), function(i)
                        error.fun(fit.cv$y, list(pred=fit.cv$yhat[[i]], prob=fit.cv$prob[,,i])))
                }
            } else {
                if(!missing(cv) && !is.blank(cv))
                    warn.once("pamr_ignoring_cv", "Ignoring threshold tuning since only one threshold value was given.")
                fit.cv <- NULL
            }
        }, error=function(...){
            stop(...)
        })
    ))
    return(list(fit=fit, cv=fit.cv, thres.fun=thres.fun))
}


##' Prediction using nearest shrunken centroids.
##'
##' In case multiple thresholds give the same error the largest one is chosen
##' (i.e. the one keeping the fewest features).
##'
##' @param object Fitted classifier.
##' @param x Dataset of observations to be classified.
##' @param threshold Threshold to use for classification. This argument is only
##'   needed if you want to override the value set during model fitting.
##' @param thres.fun Threshold selection function. Only needed if you want to
##'   override the function set during model fitting.
##' @param ... Sent to \code{\link[pamr]{pamr.predict}}.
##' @return A list with elements:
##' \itemize{
##'     \item{\code{pred}: Factor of predicted class memberships.}
##'     \item{\code{prob}: Data frame of predicted class probabilities.}
##' }
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso \code{\link{emil}}, \code{\link{emil.fit.pamr}},
##'   \code{\link{emil.vimp.pamr}}, \code{\link{modeling.procedure}}
##' @export
emil.predict.pamr <- function(object, x, threshold, thres.fun, ...){
    nice.require("pamr")
    if(nrow(x) != nrow(object$fit$centroids)){
        if(ncol(x) != nrow(object$fit$centroids))
            stop("PAMR takes datasets with observations as columns and descriptors as rows.")
        x <- t(x)
    }
    if(missing(threshold)){
        if(length(object$fit$threshold) == 1){
            threshold <- object$fit$threshold
        } else {
            if(missing(thres.fun)){
                thres.fun <- object$thres.fun
            }
            threshold <- thres.fun(object$cv$threshold, object$cv$error)
        }
    }
    list(pred=pamr::pamr.predict(object$fit, x, type="class", threshold=threshold, ...),
         prob=pamr::pamr.predict(object$fit, x, type="posterior", threshold=threshold, ...))
}


##' Variable importance of nearest shrunken centroids.
##' 
##' Calculated as the absolute difference between the overall centroid and a
##' class-wise shrunken centroid (which is the same for both classes except sign).
##'
##' In case multiple thresholds give the same error the largest one is chosen
##' (i.e. the one keeping the fewest features).
##' 
##' @param object Fitted pamr classifier
##' @param threshold Threshold to use for classification. This argument is only
##'   needed if you want to override the value set during model fitting.
##' @param thres.fun Threshold selection function. Only needed if you want to
##'   override the function set during model fitting.
##' @param ... Sent to \code{\link[pamr]{pamr.predict}}.
##' @return A matrix of variable importance scores where the rows represent
##'   variables and the columns represent classes.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso \code{\link{emil}}, \code{\link{emil.fit.pamr}},
##'   \code{\link{emil.predict.pamr}}, \code{\link{modeling.procedure}}
##' @export
emil.vimp.pamr <- function(object, threshold, thres.fun, ...){
    nice.require("pamr")
    if(missing(threshold)){
        if(length(object$fit$threshold) == 1){
            threshold <- object$fit$threshold
        } else {
            if(missing(thres.fun)){
                thres.fun <- object$thres.fun
            }
            threshold <- thres.fun(object$cv$threshold, object$cv$error)
        }
    }
    cen <- (pamr::pamr.predict(object$fit, , threshold, type="centroid", ...) -
            object$fit$centroid.overall) / object$fit$sd
    names(cen) <- object$descriptors
    return(cen)
}

