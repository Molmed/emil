##' Design of quadratic discriminant.
##'
##' @param x Dataset, numerical matrix with observations as rows.
##' @param y Class labels, factor.
##' @param pi Class probabilities. Defaults to fraction of objects in each class.
##'   In case of heavily unbalanced classes this might not be desirable. 
##' @param use "complete.obs" or "everything" 
##' @return Fitted QDA.
##' @examples
##' # TODO
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso design
##' @export
design.qda <- function(x, y, pi=table(y)/sum(!is.na(y)), use="complete.obs"){
    na.method <- pmatch(use, c("complete.obs", "everything"))
    if(na.method == 1){
        idx <- apply(x, 1, function(xx) !any(is.na(xx))) & !is.na(y)
        x <- x[idx,, drop=FALSE]
        y <- y[idx]
    }

    # Check that all classes are represented
    y.table <- table(y)
    if(any(y.table < 2)){
        stop(sprintf("Too few observations in class %s",
                     paste("`", names(y.table)[y.table < 2], "`", sep="", collapse=", ")))
    }

    fit <- list(pi = pi,
                mu = matrix(sapply(levels(y), function(lev)
                                   apply(x[y==lev,, drop=FALSE], 2, mean)),
                            ncol(x), length(levels(y)),
                            dimnames=list(NULL, levels(y))),
                S = lapply(levels(y), function(lev) cov(x[y==lev,, drop=F])))
    names(fit$S) <- names(fit$pi)
    return(fit)
}


##' Prediction using already trained classifier.
##'
##' @method predict qda
##' @param object Fitted classifier.
##' @param x Dataset of observations to be classified.
##' @param ... Ignored
##' @return TODO
##' @examples
##' # TODO
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso predict
##' @export
predict.qda <- function(object, x, ...){
    log.disc.func <- sapply(object$responses, function(lev){
        -log( (2*pi)^(.5*length(object$S))*det(object$S[[lev]])^.5 ) +
        -.5*apply(sweep(as.matrix(x), 2, object$mu[,lev])^2 %*% solve(object$S[[lev]]), 1, sum)
    })
    return(list(pred = factor(object$responses[apply(log.disc.func, 1, function(x) c(which.max(x + log(object$pi)), NA)[1])],
                              levels=object$responses),
                prob = t(apply(log.disc.func, 1, function(x) exp(x)*object$pi/sum(exp(x)*object$pi)))))
}

