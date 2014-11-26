#' Fit linear discriminant
#'
#' Wrapper for the MASS package implementation.
#'
#' @param x Dataset, numerical matrix with observations as rows.
#' @param y Class labels, factor.
#' @param ... Sent to \code{\link{lda}}.
#' @return Fitted linear discriminant.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @seealso \code{\link{emil}}, \code{\link{emil.predict.lda}},
#'   \code{\link{modeling.procedure}}
#' @export
emil.fit.lda <- function(x, y, ...) {
    nice.require("MASS")
    lda(x, y, ...)
}

#' Prediction using already trained prediction model
#'
#' Wrapper for the MASS package implementation.
#'
#' @param object Fitted classifier as produced by \code{\link{batch.model}}.
#' @param x Dataset of observations to be classified.
#' @param ... Sent to \code{\link{predict.lda}}.
#' @return A list with elements:
#' \itemize{
#'     \item{\code{pred}: Factor of predicted class memberships.}
#'     \item{\code{prob}: Data frame of predicted class probabilities.}
#' }
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @seealso \code{\link{emil}}, \code{\link{emil.fit.lda}},
#'   \code{\link{modeling.procedure}}
#' @export
emil.predict.lda <- function(object, x, ...){
    nice.require("MASS")
    pred <- predict(object, newdata=x, ...)
    return(list(pred = pred$class,
                prob = as.data.frame(pred$posterior)))
}

