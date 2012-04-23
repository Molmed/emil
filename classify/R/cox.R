##' @import cmprsk
{}

##' Design of Cox proportional hazards model
##'
##' @param x Dataset, numeric matrix with observations as rows.
##' @param y Responses, numeric.
##' @param formula Formula.
##' @param ... Sent to \code{\link{coxph}}.
##' @return Fitted model.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso design, design.crr
##' @export
design.ph <- function(x, y, formula=y~., ...) {
    y <- as.Surv(y)
    return(coxph(formula, as.data.frame(x), ...))
}


##' Prediction using already trained classifier.
##'
##' @method predict ph
##' @param object Fitted classifier as produced by \code{\link{design}}.
##' @param x Dataset of observations to be classified.
##' @param ... Sent to \code{\link{predict.coxph}}.
##' @return Predicted values.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso predict
##' @export
predict.ph <- function(object, x, ...){
    class(object) <- "coxph"
    return(list(pred = predict(object, as.data.frame(x), ...)))
}


##' Design of competing risk regression model
##' 
##' @param x Dataset, numeric matrix with observations as rows.
##' @param y Responses, numeric.
##' @param ... Sent to \code{\link{crr}}.
##' @return Fitted model.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso design, design.ph
##' @export
design.crr <- function(x, y, ...) {
    return(crr(y$time, integer.events(y), x, ...))
}


##' Prediction using already trained classifier.
##'
##' @method predict crr
##' @param object Fitted classifier as produced by \code{\link{design}}.
##' @param x Dataset of observations to be classified.
##' @param ... Sent to \code{\link{predict.crr}}.
##' @return Predicted values.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso predict
##' @export
predict.crr <- function(object, x, ...){
    stop("predict.crr is not yet properly implemented.")
    return(list(pred = cmprsk::predict.crr(object, x, ...)))
}

