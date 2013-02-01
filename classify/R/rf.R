##' @import randomForest
{}


##' Design of random forest.
##'
##' Directly calling the \code{randomForest} package implementation. See
##' \code{\link{randomForest}} for parameter specification.
##'
##' @param x Dataset, numerical matrix with observations as rows.
##' @param y Class labels, factor.
##' @param importance Whether to calculate permuted OOB error as a variable
##'   importance measure, see \code{\link{importance.randomForest}}. Set to
##'   \code{FASLE} to speed up computation.
##' @param ... Sent to \code{\link{randomForest}}.
##' @return Fitted random forest.
##' @examples
##' # TODO
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso design
##' @export
design.rf <- function(x, y, importance=FALSE, ...){
    idx <- apply(x, 1, function(xx) !any(is.na(xx))) & !is.na(y)
    x <- x[idx,, drop=FALSE]
    y <- y[idx]
    randomForest::randomForest(x, y, ..., importance=importance)
}


##' Prediction using random forest.
##'
##' @method predict rf
##' @param object Fitted model.
##' @param x Dataset of observations to be classified.
##' @param ... Ignored
##' @return TODO
##' @examples
##' # TODO
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso predict
##' @export
predict.rf <- function(object, x, ...){
    # TODO: Fix missing values
    non.na <- apply(x, 1, function(xx) !any(is.na(xx)))
    x <- x[non.na,, drop=FALSE]
    if(is.factor(object$y)){
        pred <- list(pred = factor(rep(NA, nrow(x)), levels=levels(object$y)),
                     prob = matrix(NA, nrow(x), length(levels(object$y))))
        pred$pred[non.na]  <- NextMethod(object, newdata=x, type="response")
        pred$prob[non.na,] <- NextMethod(object, newdata=x, type="prob", subset=non.na)
    } else {
        pred <- list(pred = rep(NA, nrow(x)))
        pred$pred[non.na] <- NextMethod(object, newdata=x, type="response")
    }
    return(pred)
}


##' Variable importance of random forest.
##' 
##' @method vimp rf
##' @param object Fitted NSC classifier
##' @param type Importance can be assessed in two ways:
##'   \tabular{ll}{
##'     1.\tab Permuted out-of-bag prediction error (default). This can only be
##'            used if the classifier was designed with argument
##'            \code{importance=TRUE} which is default.\cr
##'     2.\tab Total decrease in node impurity.\cr
##'   }
##' @param ... Ignored.
##' @return An importance vector with elements corresponding to variables.
##' @seealso vimp
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
vimp.rf <- function(object, type=1, ...){
    object$importance
    #if(is.factor(object$y)){
    #    perm.oob <- "MeanDecreaseAccuracy" %in% colnames(object$importance)
    #    if(missing(type) && !perm.oob){
    #        type <- 2
    #        warning("Permutation test was not performed during classifier fitting. Using node impurity as importance measure. If this is really what you want you should specify it explicitly.")
    #    }
    #    imp <- object$importance[, switch(type, "MeanDecreaseAccuracy", "MeanDecreaseGini")]
    #} else {
    #    # TODO: Replace this dirty hack with something nicer
    #    imp <- object$importance[,2]
    #}
    #return(imp)
}

