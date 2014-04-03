##' Fit a model using the caret package
##' 
##' @param x Descriptors.
##' @param y Response.
##' @param ... Sent to \code{\link[caret]{train}}.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
fit.caret <- function(x, y, ...){
    nice.require("caret")
    train(x, y, ...)
}

##' Predict using a caret method
##' 
##' This is not guaranteed to work with all caret methods. If it doesn't work
##' for a particular method, the user will need to rewrite it.
##'
##' @param ... Sent to \code{\link{predict}} that forwards it to the
##'   appropriate predict function in the caret package.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
predict.caret <- function(...){
    nice.require("caret")
    tryCatch(
        list(pred = predict(...)),
        error = function(...){
            stop("When using the `caret` package to fit and tune models within the `predict` framework, you may need to supply your own prediction function.")
        }
    )
}

