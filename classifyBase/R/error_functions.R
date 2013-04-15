##' Error function overview
##' 
##' For classification problems:
##' \tabular{rl}{
##'     \code{error.rate} \tab Fraction of predictions that were incorrect.\cr
##'     \code{neg.auc} \tab -AUC.\cr
##' }
##' 
##' For regression problems:
##' \tabular{rl}{
##'     \code{mse} \tab Mean square error.\cr
##'     \code{rmse} \tab Root mean square error.\cr
##' }
##'
##' For survival analysis no error functions are implemented so far.
##' @name error.fun
{}

##' Error rate
##' 
##' @param true The true response values, be it class labels, numeric values or
##'   survival outcomes.
##' @param pred A prediction object.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
##' @rdname error.fun
error.rate <- function(true, pred){
    if(!is.factor(true) || !is.factor(pred$pred))
        stop("Incorrect class of input variables.")
    if(!identical(levels(true), levels(pred$pred)))
        stop("Levels of predicted labels do not match levels of the true labels.")
    mean(true != pred$pred)
}

##' neg.auc title
##' 
##' @param true The true response values, be it class labels, numeric values or
##'   survival outcomes.
##' @param pred A prediction object.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
neg.auc <- function(true, pred){
    if(!is.factor(true) || !is.numeric(pred$prob))
        stop("Incorrect class of input variables.")
    if(length(levels(true)) != 2)
        stop("AUC can only be calculated on binary classification problems.")
    if(any(table(true) == 0))
        stop("There needs to be at least one example of each class to calculate AUC.")
    true <- true == levels(true)[2]
    thres <- rev(c(-Inf, sort(unique(pred$prob))))
    conf <- sapply(thres, function(thr){
        thr.pred <- prob > thr
        c(sum(!true & !thr.pred, na.rm=TRUE),
          sum( true & !thr.pred, na.rm=TRUE),
          sum(!true &  thr.pred, na.rm=TRUE),
          sum( true &  thr.pred, na.rm=TRUE))
    })
    -trapz(conf[3,]/(conf[1,]+conf[3,]), conf[4,]/(conf[2,]+conf[4,]))
}

##' rmse title
##' 
##' @param true The true response values, be it class labels, numeric values or
##'   survival outcomes.
##' @param pred A prediction object.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
rmse <- function(true, pred){
    sqrt(mean((true-pred$pred)^2))
}

##' mse title
##' 
##' @param true The true response values, be it class labels, numeric values or
##'   survival outcomes.
##' @param pred A prediction object.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
mse <- function(true, pred){
    mean((true-pred$pred)^2)
}
