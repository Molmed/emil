##' Fit GLM with LASSO, Ridge or elastic net regularization.
##' 
##' Calling the \code{glmnet} package implementation.
##' 
##' The \code{alhpa} parameter of \code{\link[glmnet]{glmnet}} controls the type of
##' penalty. Use \code{0} (default) for lasso only, \code{1} for ridge only, or
##' an intermediate for a combination. This is typically the variable to tune
##' on. The shrinkage, controlled by the \code{lambda} parameter, can be left
##' unspecified for internal tuning (works the same way as
##' \code{\link{fit.glmnet}}).
##' 
##' @param x Dataset.
##' @param y Response vector. Can be of many different types for solving
##'   different problems, see \code{\link[glmnet]{glmnet}}.
##' @param family Determines the the type of problem to solve. Auto detected if
##'   \code{y} is numeric or survival. See \code{\link{family}} for details.
##' @param nfolds See \code{\link[glmnet]{cv.glmnet}}.
##' @param foldid See \code{\link[glmnet]{cv.glmnet}}.
##' @param alpha Regularization parameter, see \code{\link[glmnet]{glmnet}}.
##' @param lambda Regularization parameter, see \code{\link[glmnet]{glmnet}}.
##' @param ... Sent to \code{\link[glmnet]{cv.glmnet}}.
##' @return Fitted GLM.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso fit
##' @export
fit.glmnet <- function(x, y, family, nfolds, foldid, alpha=1, lambda=NULL, ...){
    if(!is.factor(y))
        stop("The glmnet wrapper is only implemented for classification (factor response) so far.")
    nice.require("glmnet", "is required to fit elastic net models")
    if(missing(family)){
        if(inherits(y, c("outcome", "Surv"))) family <- "cox" else
        if(is.factor(y))
            if(length(levels(y)) == 2) family <- "binomial" else
            family <- "multinomial" else
        if(is.integer(y) & all(y >= 0)) family <- "poisson" else
        if(is.numeric(y)) family <- "gaussian" else
        stop("Could not auto detect glmnet family, see `?fit.glmnet`.")
    }
    if(inherits(y, "outcome")) y <- as.Surv(y)

    if(length(lambda) > 1 || length(lambda) != 1){
        if(missing(nfolds)){
            nfolds <- if(missing(foldid)) 10 else max(foldid)
        }
        if(missing(foldid)){
            foldid <- apply(resample.crossval(y, nfold=nfolds, nrep=1), 1, which)
        }
    }

    if(length(alpha) > 1){
        # Tune alpha, and lambda if needed
        fits <- lapply(alpha, function(a){
            fit <- glmnet::cv.glmnet(x, y, family=family,
                nfolds=nfolds, foldid=foldid, alpha=a, lambda=lambda, ...)
            fit$glmnet.fit$beta <- NULL # This can be very large, and we don't need it
            fit
        })
        cvm <- subtree(fits, T, "cvm")
        alpha.i <- which.min(sapply(cvm, min))
        vars <- c("lambda", "cvm", "cvsd", "cvup", "cvlo", "nzero")
        names(vars) <- vars
        return(out <- c(
            list(family = family, alpha = alpha, alpha.min = alpha[alpha.i]),
            lapply(vars, function(x) subtree(fits, T, x)),
            list(name = fits[[1]]$name,
                 glmnet.fit = glmnet(x, y, family=family, alpha=alpha[alpha.i], lambda=lambda, ...)),
            fits[[alpha.i]][c("lambda.min", "lambda.1se")]))

    } else if(length(lambda) != 1){
        # Tune only lambda
        return(c(list(family = family, alpha = alpha),
            glmnet::cv.glmnet(x, y, family=family, nfolds=nfolds, foldid=foldid,
                alpha=alpha, lambda=lambda, ...)))

    } else {
        # Train single glmnet
        return(c(list(family = family, alpha = alpha, lambda = lambda, lambda.min = lambda),
            glmnet.fit = glmnet(x, y, family=family, alpha=alpha, lambda=lambda, ...)))
    }
}


##' Predict using GLM with LASSO or elastic net regularization
##'
##' Due to the way \code{\link[glmnet]{glmnet}} is implemented, the regularization alpha
##' can not be modified after the model is fitted.
##'
##' @param object Fitted model.
##' @param x New data to be predicted.
##' @param s Regularization parameter lambda.
##' @param ... Sent to \code{\link[glmnet]{predict.glmnet}}.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso predict
##' @export
predict.glmnet <- function(object, x, s, ...){
    # In case the glmnet package accidentally fetches this function when calling
    # `predict`, just pass it along to the correct method.
    if(inherits(object, "glmnet"))
        return(glmnet:::predict.glmnet(object, newx=x, s=s, ...))

    nice.require("glmnet", "is required to make precdictions with an elastic net model")
    if(missing(s)){
        if("lambda.min" %in% names(object)){
            s <- object$lambda.min
        } else {
            stop("The glmnet model has not been tuned.")
        }
    }
    p <- predict(object$glmnet.fit, x, s=s, type="response", ...)
    switch(object$family,
        binomial = { p <- matrix(c(1-p, p), ncol=2, dimnames=list(rownames(p), object$glmnet.fit$classnames)) },
        multinomial = { p <- p[T,T,1,drop=TRUE] }
    )
    list(pred = factor(predict(object$glmnet.fit, x, s=s, type="class", ...),
                       levels=object$glmnet.fit$classnames),
         prob = as.data.frame(p))
}

