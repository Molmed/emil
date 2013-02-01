##' @import party
{}

##' Design cforest
##' 
##' A \code{\link{cforest}} is a random forest based on conditional inference
##' trees, using the implementation in the \code{party} package.
##' These trees can be used for classification, regression or survival
##' analysis, but only the survival part has been properly tested.
##' 
##' The parameters to \code{\link{cforest}} are set using a
##' \code{\link{cforest_control}} object. You should read the documentation
##' as the default values are choosen for technical reasons, not performance!
##' Pay special attention to \code{mtry} which is set very low by default.
##'
##' @param x Dataset, observations as rows and descriptors as columns.
##' @param y Responses.
##' @param formula Formula linking response to descriptors.
##' @param ctrl.fun Which control function to use, see \code{\link{cforest_control}}.
##' @param ... Sent to the function specified by \code{ctrl.fun}.
##' @return A fitted \code{\link{cforest}} model.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
design.cf <- function(x, y, formula=y~., ctrl.fun=party::cforest_unbiased, ...){
    if(is.outcome(y)){
        if(any(table(y$event)[-1] > 0))
            stop("cforest cannot handle competing events.")
        y <- as.Surv(y)
    }
    if(any(is.na(y))) stop("`y` contains missing values")
    list(fit = party::cforest(formula, data.frame(y=y, as.data.frame(x)), controls=ctrl.fun(...)))
}


##' Predict with cforest
##' 
##' Prediction function for models fitted with \code{\link{design.cf}}.
##' 
##' @method predict cf
##' @param object Fitted \code{cforest} classifier, as returned by \code{\link{design.cf}}.
##' @param x New data to be used for predictions.
##' @param at Time point to evaluate survival curves at. If omitted it is set
##'   to the last observed time point.
##' @param ... Sent to \code{\link{treeresponse}}·
##' @return The predicted chance of survival.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
predict.cf <- function(object, x, at, ...){
    preds <- party::treeresponse(object$fit, newdata=as.data.frame(x), ...)
    if (missing(at)) {
        risk <- sapply(preds, function(p) {
            1 - p$surv[length(p$surv)]
        })
    } else {
        risk <- sapply(preds, function(p) {
            1 - p$surv[which(p$time > at)[1] - 1]
        })
    }
    return(list(risk = risk))
}


# ##' Risk estimation with conditional inference trees [OBSOLETE]
# ##'
# ##' @param time Time to event.
# ##' @param event Event type, level 1 for no event, 2 for event of interest, >2 for
# ##'   competing events (these will however not be part of training).
# ##' @param x Covariates, observations as columns. Missing values are kNN-imputed.
# ##'   The deviation from the package standard of having observations as rows is
# ##'   due to that the function is typically used in molecular biology where it
# ##'   is customary to have observations (which are few) as columns and variables
# ##'   as rows (which are many).
# ##' @param test.subset Observations to predict on, but not to be used for design.
# ##' @param at Time point to evaluate predictions at.
# ##' @return A vector of estimated cumulative risks for the observations at
# ##'   \code{at} in \code{test.subset}.
# ##' @author Christofer \enc{Bäcklin}{Backlin}
# ##' @export
# cforest.pred <- function(time, event, x, test.subset, at){
#     if(any(is.na(x))){
#         x <- impute.knn.simple(x, subset=!test.subset)
#     }
#     event[!event %in% levels(event)[1:2]] <- NA
#     s <- Surv(time, as.integer(event)-1)
#     s.na <- is.na(s)
#     fit <- cforest(s ~ ., data.frame(s=s, t(x))[!s.na,], subset=!test.fold[!s.na])
#     preds <- treeresponse(fit, newdata=data.frame(t(x[,test.fold])))
#     return(sapply(preds, function(p){
#         1-p$surv[which(p$time > at)[1]-1]
#     }))
# }


