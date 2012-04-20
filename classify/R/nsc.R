##' @import pamr
{}


##' Design of nearest shrunken centroids.
##'
##' Wrapped version of the \code{pamr} package implementation. Note that
##' this function uses internal crossvalidation for determining the value
##' of the shrinkage threshold.
##'
##' @param x Dataset, numerical matrix with observations as rows.
##' @param y Class labels, factor.
##' @param low.mem Set to \code{TRUE} if you want the classifier to be more
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
design.nsc <- function(x, y, low.mem=FALSE, ...){
    non.na <- complete.cases(x) & !is.na(y)
    p.data <- list(x=t(x[non.na,, drop=FALSE]), y=y[non.na])
    rm(x, y)
    warn.status <- unlist(options("warn"))
    if(nrow(p.data$x) == 1){
        warning("pamr implementation of NSC is not designed to handle univariate data. Dirty hack used.")
        options(warn = -1)
        p.data$x <- rbind(p.data$x, dummy=0)
    }
    tryCatch({
        sink("/dev/null")
        fit <- pamr.train(p.data, ...)
        cv <- pamr.cv(fit, p.data)
        options(warn = warn.status)
        sink()
        if(low.mem){
            cv$cv.objects <- NULL
        }
    }, error=function(...){
        options(warn = warn.status)
        sink()
        stop(...)
    })
    return(list(fit=fit, cv=cv))
}


##' Prediction using nearest shrunken centroids.
##'
##' @method predict nsc
##' @param object Fitted classifier.
##' @param x Dataset of observations to be classified.
##' @param ... Ignored
##' @return TODO
##' @examples
##' # TODO
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso predict
##' @export
predict.nsc <- function(object, x, ...){
    pred <- list(pred = factor(rep(NA, nrow(x)), levels=levels(object$fit$y)),
                 prob = matrix(NA, nrow(x), length(levels(object$fit$y))))
    non.na <- apply(x, 1, function(xx) !any(is.na(xx)))
    x <- t(object$pre.trans(x[non.na,, drop=FALSE]))
    # Choose the threshold with the lowest error and most shrinkage
    thres <- rev(object$fit$threshold)[which.min(rev(object$cv$error))]
    pred$pred[non.na]  <- pamr.predict(object$fit, x, type="class", threshold=thres, ...)
    pred$prob[non.na,] <- pamr.predict(object$fit, x, type="posterior", threshold=thres, ...)
    return(pred)
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
    cen <- abs(pamr.predict(object$fit, , object$fit$threshold[which.min(object$cv$error)],
                            type="centroid")[,1] - object$fit$centroid.overall)
    names(cen) <- object$descriptors
    return(cen)
}

