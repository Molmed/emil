##' Error functions
##' 
##' For classification problems:
##' \describe{
##'   \item{\code{error.rate}}{Fraction of predictions that were incorrect.}
##'   \item{\code{neg.auc}}{-AUC.}
##'   \item{\code{\link{neg.geo.mean}}}{Negative geometric mean. Good for
##'     problems with imbalanced class sizes.}
##' }
##' For regression problems:
##' \describe{
##'     \item{\code{mse}}{Mean square error.}
##'     \item{\code{rmse}}{Root mean square error.}
##' }
##' For survival analysis no error functions are implemented so far.
##'
##' @param true The true response values, be it class labels, numeric values or
##'   survival outcomes.
##' @param pred A prediction object.
##' @name error.fun
##' @author Christofer \enc{Bäcklin}{Backlin}
{}

##' @rdname error.fun
##' @export
error.rate <- function(true, pred){
    if(!is.factor(true) || !is.factor(pred$pred))
        stop("Incorrect class of input variables.")
    if(!identical(levels(true), levels(pred$pred)))
        stop("Levels of predicted labels do not match levels of the true labels.")
    mean(true != pred$pred)
}

##' Weighted error rate
##' 
##' If different types of errors are associated with different costs a weighted
##' error function might be more appropriate than the standard.
##' 
##' This function is not in itself an error function, but used to generate error
##' functions. Either supply a (manually) predefined cost matrix or a response
##' vector for a classification problem to define it automatically.
##' 
##' The automatically defined error function will return 0 if all predictions
##' are correct, 1 if all predictions are incorrect and 0.5 if all predictions
##' are the same (regardless of class, i.e. if one class is smaller it will be
##' given a higher missclassification cost).
##' 
##' @param x Cost matrix or factor response vector.
##' @return An error function.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
weighted.error.rate <- function(x){
    if(is.factor(x)){
        if(length(levels(x)) != 2)
            stop("In multi-class-problems you must manually supply a cost matrix.")
        x <- matrix(c(0, rev(length(x)/table(x)*.5), 0), 2)
    }
    function(true, pred){
        mean(rep(x, table(true, pred$pred)))
    }
}

##' @rdname error.fun
##' @export
neg.auc <- function(true, pred){
    if(!is.factor(true) || !is.numeric(pred$prob))
        stop("Incorrect class of input variables.")
    if(length(levels(true)) != 2)
        stop("AUC can only be calculated on binary classification problems.")
    if(any(table(true) == 0))
        stop("There needs to be at least one example of each class to calculate AUC.")
    true <- true == levels(true)[2]
    thres <- rev(c(-Inf, sort(unique(pred$prob[,2]))))
    conf <- sapply(thres, function(thr){
        thr.pred <- pred$prob[,2] > thr
        c(sum(!true & !thr.pred, na.rm=TRUE),
          sum( true & !thr.pred, na.rm=TRUE),
          sum(!true &  thr.pred, na.rm=TRUE),
          sum( true &  thr.pred, na.rm=TRUE))
    })
    -trapz(conf[3,]/(conf[1,]+conf[3,]), conf[4,]/(conf[2,]+conf[4,]))
}

##' @rdname error.fun
##' @export
rmse <- function(true, pred){
    sqrt(mean((true-pred$pred)^2))
}

##' @rdname error.fun
##' @export
mse <- function(true, pred){
    mean((true-pred$pred)^2)
}

##' Negative geometric mean of class specific predictive accuracy
##' 
##' When dealing with imbalanced classification problem, i.e. where the class
##' sizes are very different, small classes tend to be overlooked when tuning
##' parameters by optimizing error rate. Blagus and Lusa (2013) suggested to
##' remedy the problem by using this performance measure instead.
##' 
##' @param true See \code{\link{error.fun}}.
##' @param pred See \code{\link{error.fun}}.
##' @return A numeric scalar.
##' @seealso error.fun
##' @references
##' Blagus, R., & Lusa, L. (2013).
##' \emph{Improved shrunken centroid classifiers for high-dimensional class-imbalanced data.}
##' BMC bioinformatics, 14, 64.
##' doi:10.1186/1471-2105-14-64
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
neg.geo.mean <- function(true, pred){
    -exp(mean(log( tapply(pred$pred == true, true, mean) )))
}
