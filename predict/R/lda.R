##' Fit linear discriminant
##'
##' @param x Dataset, numerical matrix with observations as rows.
##' @param y Class labels, factor.
##' @param pi Class probabilities. Defaults to fraction of objects in each
##'   class. In case of heavily unbalanced classes this might not be
##'   desirable.
##' @param use Sent to \code{\link{cov}}.
##' @return Fitted linear discriminant.
##' @examples
##' # TODO
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso fit
##' @export
fit.lda <- function(x, y, pi=prop.table(table(y)), use="complete.obs") {
    s <- matrix(0, ncol(x), ncol(x))
    for(lev in levels(y))
        s <- s + cov(x[y==lev,, drop=F], use=use) * (sum(y==lev)-1)
    s <- s / sum(table(y)-1)
    fit <- list(responses=levels(y),
                pi = pi,
                mu = matrix(sapply(levels(y), function(lev)
                                   apply(x[y==lev,, drop=FALSE], 2, mean)),
                            ncol(x), length(levels(y)),
                            dimnames=list(NULL, levels(y))),
                S = s)
    fit
}

##' Prediction using already trained prediction model
##'
##' @param object Fitted classifier as produced by \code{\link{batch.model}}.
##' @param x Dataset of observations to be classified.
##' @param ... Ignored, kept for S3 consistency.
##' @return TODO
##' @examples
##' # TODO
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso predict
##' @export
predict.lda <- function(object, x, ...){
    log.disc.func <- sapply(object$responses, function(lev){
        -log( (2*pi)^(.5*nrow(object$S))*det(object$S)^.5 ) +
        -.5*apply(sweep(as.matrix(x), 2, object$mu[,lev])^2 %*% solve(object$S), 1, sum)
    })
    return(list(pred = factor(object$responses[apply(log.disc.func, 1, function(x) c(which.max(x + log(object$pi)),NA)[1])],
                              levels=object$responses),
                prob = t(apply(log.disc.func, 1, function(x) exp(x)*object$pi/sum(exp(x)*object$pi)))))
}

