##' Design of linear model.
##'
##' This model is implemented for regression only.
##'
##' @param x Dataset, numeric matrix with observations as rows.
##' @param y Responses, numeric.
##' @param formula Formula.
##' @param ... Sent to \code{\link{lm}}.
##' @return Fitted model.
##' @examples
##' # TODO
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso design
##' @export
design.ols <- function(x, y, formula=y~., ...) {
    lm(formula, as.data.frame(x), ...)
}


##' Prediction using already trained classifier.
##'
##' @method predict ols
##' @param object Fitted classifier as produced by \code{\link{batch.predict}}.
##' @param x Dataset of observations to be classified.
##' @param ... Sent to \code{\link{predict.lm}}.
##' @return Predicted values.
##' @examples
##' # TODO
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso predict
##' @export
predict.ols <- function(object, x, ...){
    class(object) <- "lm"
    return(list(pred = predict.lm(object, as.data.frame(x), ...)))
}


