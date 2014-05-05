##' Variable importance of a fitted classifier.
##'
##' \code{vimp} is a generic function for extracting variable importance from
##' fitted prediction models. The function invokes particular _methods_ which depend
##' on the 'class' of the first argument. Note that different _method_ calculates
##' variable importance in different ways and that they are not directly comparable.
##'
##' @param object Fitted model.
##' @param ... Sent on to class method.
##' @return Variable importance vector.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
emil.vimp <- function(object, ...){
    # if(!any(sapply(sprintf("vimp.%s", class(object)), exists)))
    #     stop(sprintf("No variable importance measure is implemented for classifier type \"%s\".", class(object$fit)[1]))
    UseMethod("emil.vimp", object)
}


##' Tidy up variable importance estimates
##' 
##' Only intended for internal use.
##' 
##' @param x Variable importance estimates.
##' @param feat A logical vector indicating which features passed feature
##'   selection in the pre-processing.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @noRd
fixvimp <- function(x, feat){
    if(is.null(feat)){
        return(x)
    } else if(is.matrix(x)){
        y <- matrix(NA, length(feat), ncol(x))
        y[feat,] <- x
    } else if(is.vector(x)){
        y <- rep(NA, length(feat))
        y[feat] <- x
    } else {
        stop("Invalid variable importance format.")
    }
    y
}

