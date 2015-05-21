#' Performance estimation functions
#' 
#' These functions determine the performance of fitted model based on its
#' predictions. They are used both for evaluating whole modeling procedures and
#' to tune model parameters, i.e. find the parameter values with the best
#' performance.
#' The parameter tuning routine is designed to minimize its error function (or
#' optimization criteria), which is why functions that are to be maximized must
#' have their sign changed, like \code{neg_auc}.
#'
#' Custom performance estimation functions should be implemented as follows:
#' 
#' \code{function(truth, prediction)}
#' \describe{
#'     \item{\code{truth}}{A vector of true responses.}
#'     \item{\code{prediction}}{Prediction returned from the prediction function.}
#' }
#' 
#' In most cases the true response and the predictions are of the same type,
#' e.g. true and fitted values in a regression or class labels in a
#' classification problem, but it is not a requirement. An example of different
#' types could be if the prediction function produce class probabilities for
#' all classes rather than one label, or the risks that the observations will
#' experience the event of interest, to be compared to the actual outcome that
#' it did occur or has not yet occurred at a specific time point.
#' See \code{\link{neg_harrell_c}} for an example of the latter.
#' 
#' @param truth The true response values, be it class labels, numeric values or
#'   survival outcomes.
#' @param prediction A prediction object.
#' @name error_fun
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @seealso \code{\link{emil}}, \code{\link{neg_gmpa}},
#'   \code{\link{modeling_procedure}}, \code{\link{extension}}
{}

#' @rdname error_fun
#' @export
error_rate <- function(truth, prediction){
    if(!is.factor(truth) || !is.factor(prediction$prediction))
        stop("Incorrect class of input variables.")
    if(!identical(levels(truth), levels(prediction$prediction)))
        stop("Levels of predicted labels do not match levels of the truth labels.")
    mean(truth != prediction$prediction)
}

#' Weighted error rate
#' 
#' If different types of errors are associated with different costs a weighted
#' error function might be more appropriate than the standard.
#' 
#' This function is not in itself an error function, but used to generate error
#' functions. Either supply a predefined cost matrix or a response
#' vector for a classification problem to define it automatically.
#' 
#' The automatically generated cost matrix will generate an error of 0 if all
#' predictions are correct, 1 if all predictions are incorrect and 0.5 if all
#' predictions are the same (regardless of class, i.e. if one class is smaller
#' it will be given a higher misclassification cost).
#' 
#' @param x Cost matrix or factor response vector.
#' @return An error function.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
weighted_error_rate <- function(x){
    if(is.factor(x)){
        if(length(levels(x)) != 2)
            stop("In multi-class-problems you must manually supply a cost matrix.")
        x <- matrix(c(0, rev(length(x)/table(x)*.5), 0), 2)
    }
    function(truth, prediction){
        mean(rep(x, table(truth, prediction$prediction)))
    }
}

#' @rdname error_fun
#' @export
neg_auc <- function(truth, prediction){
    if(!is.factor(truth) || !is.numeric(prediction$probability))
        stop("Incorrect class of input variables.")
    if(length(levels(truth)) != 2)
        stop("AUC can only be calculated on binary classification problems.")
    if(any(table(truth) == 0))
        stop("There needs to be at least one example of each class to calculate AUC.")
    truth <- truth == levels(truth)[2]
    thres <- rev(c(-Inf, sort(unique(prediction$probability[,2]))))
    conf <- sapply(thres, function(thr){
        thr.prediction <- prediction$probability[,2] > thr
        c(sum(!truth & !thr.prediction, na.rm=TRUE),
          sum( truth & !thr.prediction, na.rm=TRUE),
          sum(!truth &  thr.prediction, na.rm=TRUE),
          sum( truth &  thr.prediction, na.rm=TRUE))
    })
    -trapz(conf[3,]/(conf[1,]+conf[3,]), conf[4,]/(conf[2,]+conf[4,]))
}

#' @rdname error_fun
#' @export
rmse <- function(truth, prediction){
    sqrt(mean((truth-prediction$prediction)^2))
}

#' @rdname error_fun
#' @export
mse <- function(truth, prediction){
    mean((truth-prediction$prediction)^2)
}

#' Negative geometric mean of class specific predictive accuracy
#' 
#' When dealing with imbalanced classification problem, i.e. where the class
#' sizes are very different, small classes tend to be overlooked when tuning
#' parameters by optimizing error rate. Blagus and Lusa (2013) suggested to
#' remedy the problem by using this performance measure instead.
#' 
#' @param truth See \code{\link{error_fun}}.
#' @param prediction See \code{\link{error_fun}}.
#' @return A numeric scalar.
#' @seealso \code{\link{error_fun}}
#' @references
#' Blagus, R., & Lusa, L. (2013).
#' \emph{Improved shrunken centroid classifiers for high-dimensional class-imbalanced data.}
#' BMC bioinformatics, 14, 64.
#' doi:10.1186/1471-2105-14-64
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
neg_gmpa <- function(truth, prediction){
    -exp(mean(log( tapply(prediction$prediction == truth, truth, mean) )))
}

#' @rdname error_fun
#' @export
neg_harrell_c <- function(truth, prediction){
    stopifnot(inherits(truth, "Surv"))
    nice_require("Hmisc", "is required for calculating Harrell's C")
    risk <- prediction$prediction
    risk <- ifelse(is.infinite(risk), 1.1*max(abs(risk[!is.infinite(risk)]))*sign(risk), risk)
    -Hmisc::rcorr.cens(risk, truth)[1]
}

#' @rdname error_fun
#' @export
brier_score <- function(truth, prediction){
    stop("Not yet implemented")  
}