##' @import caret
{}

##' Fit a model using the caret package
##' 
##' @param x Descriptors.
##' @param y Response.
##' @param ... Sent to \code{\link{train}}.
##' @author Christofer \enc{Bäcklin}{Backlin}
design.caret <- function(x, y, ...){
    train(x, y, ...)
}

##' Predict using a caret method
##' 
##' @param object Fitted caret model, as produced by \code{\link{design.caret}}
##'   or \code{\link{train}}.
##' @author Christofer \enc{Bäcklin}{Backlin}
predict.caret <- function(object, x, ...){
    list(pred = predict(object$finalModel, newdata=x, ...))
}

