##' @import cmprsk
##' @import survival
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
    return(survival::coxph(formula, as.data.frame(x), ...))
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
    return(cmprsk::crr(y$time, integer.events(y), x, ...))
}


##' Prediction using already trained classifier.
##'
##' @method predict crr
##' @param object Fitted classifier as produced by \code{\link{design}}.
##' @param x Dataset of observations to be classified.
##' @param at Time point(s) to evaluate survival curves at. If omitted it is set
##'   to the last observed time point.
##' @param ... Sent to \code{\link{predict.crr}}.
##' @return Predicted values.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso predict
##' @export
predict.crr <- function(object, x, at, ...){
  if (missing(at))
    at = object$uftime[length(object$uftime)]
  
  ##As is done in predict.crr, compute cumulative incidence function
  if (length(object$coef) == length(as.matrix(x))) {
    L <- cumsum(exp(sum(x * object$coef)) * object$bfitj)
    CIF <- 1 - exp(-L)
    
    ##For the specified time points
    CIF = matrix(sapply(at, function(t) {ind = which(t >= object$uftime); if (length(ind)==0) {0} else {CIF[max(ind)]}}),nrow=1)
  }
  else {
    x <- as.matrix(x)
    if (ncol(x) > length(object$coef))
      x = x[,names(object$coef)]
    L <- matrix(0, nrow = length(object$uftime), ncol = nrow(x))
    for (j in 1:nrow(x))
      L[, j] <- cumsum(exp(sum(x[j,] * object$coef)) * object$bfitj)
    N = nrow(x)
    CIF <- 1-exp(-L)
    
    ##For the specified time points
    CIF = sapply(at, function(t) {ind = which(t >= object$uftime); if (length(ind)==0) {rep(0,N)} else {CIF[max(ind),]}})
  }
  
  return(list(risk = CIF))
}


