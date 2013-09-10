##' @import pamr
##' @import predictBase
{}


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
##' @param ... Sent to \code{\link{pamr.train}}.
##' @return Fitted LDA or QDA.
##' @examples
##' # TODO 
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso design
##' @export
design.nsc <- function(x, y, error.fun, slim.fit=FALSE, ...){
    p.data <- list(x=t(x), y=y)
    rm(x, y)
    warn.status <- unlist(options("warn"))
    if(nrow(p.data$x) == 1){
        warning("pamr implementation of NSC is not designed to handle univariate data. Dirty hack used.")
        options(warn = -1)
        p.data$x <- rbind(p.data$x, dummy=0)
    }
    if(missing(error.fun)){
        error.fun.frame <- sapply(sys.frames(), function(env) "error.fun" %in% ls(env))
        if(any(error.fun.frame)){
            error.fun <- get("error.fun", envir=sys.frames()[[max(which(error.fun.frame))]])
        } else {
            if(is.factor(y)){
                error.rate
            } else if(is.numeric(y)){
                rmse
            } else {
                stop("You must specify an error function!")
            }
        }
    }
    invisible(capture.output(
        tryCatch({
            fit <- pamr::pamr.train(p.data, ...)
            cv <- pamr::pamr.cv(fit, p.data)
            options(warn = warn.status)
            if(slim.fit){
                cv$cv.objects <- NULL
            }
            cv$error <- sapply(seq(cv$thres), function(i)
                error.fun(cv$y, list(pred=cv$yhat[[i]], prob=cv$prob[,,i])))
        }, error=function(...){
            options(warn = warn.status)
            stop(...)
        })
    ))
    return(list(fit=fit, cv=cv))
}


##' Prediction using nearest shrunken centroids.
##'
##' @method predict nsc
##' @param object Fitted classifier.
##' @param x Dataset of observations to be classified.
##' @param thres What threshold to use for classification.
##' @param ... Ignored
##' @return TODO
##' @examples
##' # TODO
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso predict
##' @export
predict.nsc <- function(object, x, thres, ...){
    if(ncol(x) == 1){
        x <- rbind(t(x), dummy=0)
    } else {
        x <- t(x)
    }
    if(missing(thres))
        thres <- rev(object$fit$threshold)[which.min(rev(object$cv$error))]
    list(pred=pamr::pamr.predict(object$fit, x, type="class", threshold=thres, ...),
         prob=pamr::pamr.predict(object$fit, x, type="posterior", threshold=thres, ...))
}


##' Variable importance of nearest shrunken centroids.
##' 
##' Calculated as the absolute difference between the overall centroid and a
##' classwise shrunken centroid (which is the same for both classes except sign).
##' 
##' @method vimp nsc
##' @param object Fitted NSC classifier
##' @param ... Ignored.
##' @return An importance vector with elements corresponding to variables.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
vimp.nsc <- function(object, ...){
    cen <- pamr::pamr.predict(object$fit, ,
            object$fit$threshold[which.min(object$cv$error)], type="centroid") -
        object$fit$centroid.overall / object$fit$sd
    names(cen) <- object$descriptors
    return(cen)
}

