##' Fit random forest.
##'
##' Directly calling the \code{randomForest} package implementation. See
##' \code{\link[randomForest]{randomForest}} for parameter specification.
##'
##' @param x Dataset, numerical matrix with observations as rows.
##' @param y Class labels, factor.
##' @param importance Whether to calculate permuted OOB error as a variable
##'   importance measure, see \code{\link[randomForest]{importance.randomForest}}. Set to
##'   \code{FASLE} to speed up computation.
##' @param ... Sent to \code{\link[randomForest]{randomForest}}.
##' @return Fitted random forest.
##' @examples
##' # TODO
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso fit
##' @export
emil.fit.randomForest <- function(x, y, importance=FALSE, ...){
    nice.require("randomForest")
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
##' @param object Fitted model.
##' @param x Dataset of observations to be classified.
##' @param ... Ignored
##' @return TODO
##' @examples
##' # TODO
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso predict
##' @export
emil.predict.randomForest <- function(object, x, ...){
    nice.require("randomForest")
    if(is.factor(object$y)){
        list(pred = predict(object, newdata=x, type="response"),
             prob = predict(object, newdata=x, type="prob"))
    } else {
        list(pred = predict(object, newdata=x, type="response"))
    }
}


##' Variable importance of random forest.
##' 
##' @param object Fitted NSC classifier
##' @param type Importance can be assessed in two ways:
##'   \describe{
##'     \item{1.}{Permuted out-of-bag prediction error (default). This can only be
##'            used if the classifier was fitted with argument
##'            \code{importance=TRUE} which is default.}
##'     \item{2.}{Total decrease in node impurity.}
##'   }
##' @param ... Ignored.
##' @return An importance vector with elements corresponding to variables.
##' @seealso vimp
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
emil.vimp.randomForest <- function(object, type=1, ...){
    if(is.blank(object$importance))
        stop("To calculate variable importance of random forsests you must set the fitting parameter `importance=TRUE`, see `?emil.fit.randomForest`.")
    object$importance
}

