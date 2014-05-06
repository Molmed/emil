##' Variable importance of a fitted model
##'
##' Note that different methods calculates variable importance in different
##' ways and that they are not directly comparable.
##'
##' @param object Modeling procedure.
##' @param model Fitted model.
##' @param ... Sent on to the procedure's variable importance scoring function.
##' @return A vector of length p or an p-x-c matrix of variable importance
##'   scores where p is the number of descriptors and c is the number of classes.
##' @examples
##' proc <- modeling.procedure("randomForest")
##' mod <- fit(proc, x=iris[-5], y=iris$Species)
##' vimp(proc, mod)
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso \code{\link{emil}}
##' @export
vimp <- function(object, model, ...){
    object$vimp.fun(model, ...)
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

