##' @import glmnet
{}

design.gl <- function(x, y, formula=y~., family, ...){
    if(missing(family)){
        if(is.integer(y) & all(y >= 0)) family <- "poisson" else
        if(is.numeric(y)) family <- "gaussian" else
        if(is.factor(y)) family <- "binomial" else
        stop("Could not auto detect glmnet family, see `?design.glmnet`.")
    }
    if(inherits(y, "outcome")) y <- as.Surv(y)
    glm(formula, family=family, as.data.frame(x), ...)
}

predict.gl <- function(object, x, type="response", ...){
    p <- NextMethod(object, as.data.frame(x), type=type, ...)
    if(object$family$family == "binomial"){
        lev <- levels(object$model[[as.character(as.list(fits[[1]]$formula)[[2]])]])
        list(pred = factor(p > .5, levels=c(FALSE, TRUE), labels = lev),
             prob = matrix(c(1-p, p), ncol=2, dimnames=list(NULL, lev)))
    } else {
        list(pred=p)
    }
}


##' Design GLM with LASSO or elastic net regularization.
##' 
##' Calling the \code{glmnet} package implementation.
##' 
##' The \code{alhpa} parameter of \code{\link{glmnet}} controls the type of
##' penalty. Use \code{0} (default) for lasso only, \code{1} for ridge only, or
##' an intermediate for a combination. This is typically the variable to tune
##' on. The shrinkage, controlled by the \code{lambda} parameter, can be left
##' unspecified for internal tuning (works the same way as
##' \code{\link{design.nsc}}).
##' 
##' @param x Dataset.
##' @param y Response vector. Can be of many different types for solving
##'   different problems, see \code{\link{glmnet}}.
##' @param family Determines the the type of problem to solve. Auto detected if
##'   \code{y} is numeric or survival. See \code{\link{family}} for details.
##' @param ... Sent to \code{\link{cv.glmnet}}.
##' @return Fitted GLM.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso design
##' @export
design.gln <- function(x, y, family, ...){
    if(missing(family)){
        if(inherits(y, c("outcome", "Surv"))) family <- "cox" else
        if(is.integer(y) & all(y >= 0)) family <- "poisson" else
        if(is.numeric(y)) family <- "gaussian" else
        if(is.factor(y)) family <- "binomial" else
        stop("Could not auto detect glmnet family, see `?design.glmnet`.")
    }
    if(inherits(y, "outcome")) y <- as.Surv(y)
    glmnet::cv.glmnet(x, y, family=family, ...)
}


##' Predict using GLM with LASSO or elastic net regularization
##' 
##' @method predict glm
##' @param object Fitted model.
##' @param x New data to be predicted.
##' @param ... Sent to \code{\link{predict.glmnet}}.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso predict
##' @export
predict.gln <- function(object, x, ...){
    list(pred = NextMethod(object, x, ...))
}

