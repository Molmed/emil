
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


