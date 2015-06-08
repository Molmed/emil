#' Fit a model using the \pkg{caret} package
#' 
#' @param x Descriptors.
#' @param y Response.
#' @param ... Sent to \code{\link[caret]{train}}.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
fit_caret <- function(x, y, ...){
    nice_require("caret")
    caret::train(x, y, ...)
}

#' Predict using a \pkg{caret} method
#' 
#' This is not guaranteed to work with all \pkg{caret} methods. If it doesn't 
#' work for a particular method, the user will need to rewrite it.
#'
#' @param ... Sent to \code{\link{predict}} that forwards it to the
#'   appropriate predict function in the \pkg{caret} package.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
predict_caret <- function(...){
    nice_require("caret")
    tryCatch(
        list(prediction = predict(...)),
        error = function(...){
            stop("When using the `caret` package to fit and tune models within the `predict` framework, you may need to supply your own prediction function.")
        }
    )
}

