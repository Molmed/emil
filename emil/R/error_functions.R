##' Performance estimation functions
##' 
##' These functions determine the performance of fitted model based on its
##' predictions. They are used both for evaluating whole modeling procedures and
##' to tune model parameters, i.e. find the parameter values with the best
##' performance.
##' The parameter tuning routine is designed to minimize its error function (or
##' optimization criteria), which is why functions that are to be maximized must
##' have their sign changed, like \code{neg.auc}.
##'
##' Custom performance estimation functions should be implemented as follows:
##' 
##' \code{function(true, pred)}
##' \describe{
##'     \item{\code{true}}{A vector of true responses.}
##'     \item{\code{pred}}{Prediction returned from the prediction function.}
##' }
##' 
##' In most cases the true response and the predictions are of the same type,
##' e.g. true and fitted values in a regression or class labels in a
##' classification problem, but it is not a requirement. An example of different
##' types could be if the prediction function produce class probabilities for
##' all classes rather than one label, or the risks that the observations will
##' experience the event of interest, to be compared to the actual outcome that
##' it did occur or has not yet occurred at a specific time point.
##' See neg.harrell.C for an example of the latter.
##' 
##' @param true The true response values, be it class labels, numeric values or
##'   survival outcomes.
##' @param pred A prediction object.
##' @name error.fun
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso \code{\link{emil}}, \code{\link{neg.gmpa}},
##'   \code{\link{modeling.procedure}}, \code{\link{emil.extensions}}
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
##' functions. Either supply a predefined cost matrix or a response
##' vector for a classification problem to define it automatically.
##' 
##' The automatically generated cost matrix will generate an error of 0 if all
##' predictions are correct, 1 if all predictions are incorrect and 0.5 if all
##' predictions are the same (regardless of class, i.e. if one class is smaller
##' it will be given a higher misclassification cost).
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
##' @seealso \code{\link{error.fun}}
##' @references
##' Blagus, R., & Lusa, L. (2013).
##' \emph{Improved shrunken centroid classifiers for high-dimensional class-imbalanced data.}
##' BMC bioinformatics, 14, 64.
##' doi:10.1186/1471-2105-14-64
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
neg.gmpa <- function(true, pred){
    -exp(mean(log( tapply(pred$pred == true, true, mean) )))
}

##' @rdname error.fun
##' @export
neg.harrell.C <- function(true, pred){
    nice.require("Hmisc", "is required for calculating Harrell's C")
    -Hmisc::rcorr.cens(pred$risk, as.Surv(true))[1]
}



error.convergence <- function(x){
    if("error" %in% x[[1]]){
        # Single procedure
        err <- subtree(x, TRUE, "error")
    } else {
        err <- subtree(x, TRUE, TRUE, "error")
    }

}

