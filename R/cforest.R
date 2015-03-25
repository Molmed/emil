#' Fit conditional inference forest
#' 
#' A \code{\link[party]{cforest}} is a random forest based on conditional inference
#' trees, using the implementation in the \code{party} package.
#' These trees can be used for classification, regression or survival
#' analysis, but only the survival part has been properly tested so far.
#' 
#' The parameters to \code{\link[party]{cforest}} are set using a
#' \code{\link[party]{cforest_control}} object. You should read the documentation
#' as the default values are chosen for technical reasons, not predictive
#' performance!
#' Pay special attention to \code{mtry} which is set very low by default.
#'
#' @param x Dataset, observations as rows and descriptors as columns.
#' @param y Responses.
#' @param formula Formula linking response to descriptors.
#' @param ctrl.fun Which control function to use, see \code{\link[party]{cforest_control}}.
#' @param ... Sent to the function specified by \code{ctrl.fun}.
#' @return A fitted \code{\link[party]{cforest}} model.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @seealso \code{\link{emil}}, \code{\link{emil.predict.cforest}},
#'   \code{\link{modeling.procedure}}
#' @export
emil.fit.cforest <- function(x, y, formula=y~., ctrl.fun=cforest_unbiased, ...){
    nice.require("party")
    nice.require("survival")
    if(!inherits(y, "Surv")){
        warn.once("The `cforest` wrappers has only been properly tested for survival analysis problems so far, proceed with care.")
    } else if(attr(y, "type") %in% c("mright", "mcounting")){
        stop("cforest cannot handle competing events.")
    }
    if(any(is.na(y))) stop("`y` contains missing values")
    party::cforest(formula, data.frame(y=y, as.data.frame(x)), controls=ctrl.fun(...))
}


#' Predict with conditional inference forest
#' 
#' Prediction function for models fitted with \code{\link{emil.fit.cforest}}.
#' 
#' @param object Fitted \code{cforest} classifier, as returned by
#'   \code{\link{emil.fit.cforest}}.
#' @param x New data to be used for predictions.
#' @param at Time point to evaluate survival curves at. If omitted it is set
#'   to the last observed time point.
#' @param ... Sent to \code{\link[party]{treeresponse}}·
#' @return The predicted chance of survival.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @seealso \code{\link{emil}}, \code{\link{emil.fit.cforest}},
#'   \code{\link{modeling.procedure}}
#' @export
emil.predict.cforest <- function(object, x, at, ...){
    nice.require("party")
    list(pred = predict(object, OOB=FALSE, as.data.frame(x)))
}

