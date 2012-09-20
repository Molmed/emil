##' @import glmnet
{}

##' Design GLM with LASSO or elastic net regularization.
##' 
##' Calling the \code{glmnet} package implementation.
##' 
##' @param x Dataset.
##' @param y Response vector. Can be of many different types for solving
##'   different problems, see \code{\link{glmnet}}.
##' @param family Determines the the type of problem to solve. Auto detected if
##'   \code{y} is numeric or survival.
##' @param ... Sent to \code{\link{cv.glmnet}}.
##' @return Fitted GLM.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso design
##' @export
design.gn <- function(x, y, family, ...){
    if(missing(family)){
        if(inherits(y, c("outcome", "Surv"))) family <- "cox" else
        if(is.numeric(y)) family <- "gaussian" else
        stop("Could not auto detect glmnet family, see `?design.glmnet`.")
    }
    if(inherits(y, "outcome")) y <- as.Surv(y)
    cv.glmnet(x, y, family=family, ...)
}


##' Predict using GLM with LASSO or elastic net regularization
##' 
##' @method predict gn
##' @param object Fitted model.
##' @param x New data to be predicted.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso predict
##' @export
predict.gn <- function(object, x, ...){
    list(pred = NextMethod(object, x, ...))
}

