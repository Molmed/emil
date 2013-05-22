##' @import randomForest
##' @import party
##' @import predictBase
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
    #idx <- apply(x, 1, function(xx) !any(is.na(xx))) & !is.na(y)
    #x <- x[idx,, drop=FALSE]
    #y <- y[idx]
    #randomForest::randomForest(x, y, ..., importance=importance)
    tryCatch(randomForest::randomForest(x, y, ..., importance=importance),
             error=function(e){
                 if(any(is.na(x))){
                     stop("Random forest does not accept any missing values.")
                 } else {
                     stop(e)
                 }
             })
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
    p <- getFromNamespace("predict.randomForest", "randomForest")
    if(is.factor(object$y)){
        list(pred = p(object, newdata=x, type="response"),
             prob = p(object, newdata=x, type="prob"))
    } else {
        list(pred = p(object, newdata=x, type="response"))
    }
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
    if(is.blank(object$importance))
        stop("To calculate variable importance of random forsests you must set the design parameter `importance=TRUE`, see `?design.rf`.")
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

