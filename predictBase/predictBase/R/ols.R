##' Design of a linear model fitted with ordinary least squares
##' 
##' Based on \code{\link{lm}}.
##' 
##' @param x Dataset.
##' @param y Response, numeric.
##' @param formula See \code{\link{lm}}.
##' @param ... Sent to \code{\link{lm}}.
##' @return Fitted linear model.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
design.ols <- function(x, y, formula=y~., ...){
    df <- data.frame(y, x)
    rm(y,x)
    vars.missing <- all.vars(formula)[!all.vars(formula) %in% names(df)]
    if(!is.blank(vars.missing)){
        omitted <- length(vars.missing) - 20
        vars.missing <- paste(sprintf("`%s`", head(vars.missing, 20)), collapse=", ")
        if(omitted > 0) vars.missing <- paste(vars.missing, "+", omitted, "more")

        omitted <- length(names(df)) - 20
        vars.present <- paste(sprintf("`%s`", head(names(df), 20)), collapse=", ")
        if(omitted > 0) vars.present <- paste(vars.present, "+", omitted, "more")

        stop(sprintf("Variables %s not found in data frame. Variables available are %s.",
            vars.missing, vars.present))
    }
    lm(formula, data.frame(y, x), ...)
}

##' Prediction using linear model
##' 
##' @method predict ols
##' @param object Fitted classifier produced by \code{\link{design.ols}}.
##' @param x Dataset to be predicted upon.
##' @param ... Sent to \code{\link{predict.lm}}
##' @return A prediction list.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso predict
##' @export
predict.ols <- function(object, x, ...){
    list(pred = predict.lm(object, data.frame(y=NA, x), ...))
}

