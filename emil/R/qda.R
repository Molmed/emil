##' Fit quadratic discriminant.
##'
##' Wrapper for the MASS package implementation.
##'
##' @param x Dataset, numerical matrix with observations as rows.
##' @param y Class labels, factor.
##' @param ... Sent to \code{\link{qda}}.
##' @return Fitted QDA.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso \code{\link{emil}}, \code{\link{emil.predict.qda}},
##'   \code{\link{modeling.procedure}}
##' @export
emil.fit.qda <- function(x, y, ...){
    nice.require("MASS")
    qda(x, y, ...)
}


##' Prediction using already trained classifier.
##'
##' Wrapper for the MASS package implementation.
##'
##' @param object Fitted classifier as produced by \code{\link{batch.model}}.
##' @param x Dataset of observations to be classified.
##' @param ... Sent to \code{\link{predict.qda}}.
##' @return A list with elements:
##' \itemize{
##'     \item{\code{pred}: Factor of predicted class memberships.}
##'     \item{\code{prob}: Data frame of predicted class probabilities.}
##' }
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso \code{\link{emil}}, \code{\link{emil.fit.qda}},
##'   \code{\link{modeling.procedure}}
##' @export
emil.predict.qda <- function(object, x, ...){
    nice.require("MASS")
    pred <- predict(object, newdata=x, ...)
    return(list(pred = pred$class,
                prob = as.data.frame(pred$posterior)))
}

