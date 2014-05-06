##' Extraction of p-value from a statistical test
##'
##' These calculations are written in such a way that they avoid rounding off errors
##' that plague the \code{survival} and \code{cmprsk} packages. 
##'
##' @param x Test, i.e. a fitted object of a supported type.
##' @param log.p Whether to return the logarithm of the p-value.
##' @param ... Sent to class method.
##' @return p-value.
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @seealso \code{\link{p.value.crr}}, \code{\link{p.value.survdiff}},
##'   \code{\link{p.value.cuminc}}
##' @export
p.value <- function(x, log.p=FALSE, ...) UseMethod("p.value")


##' Extract p-value from a Cox proportional hazards model
##' 
##' Based on \code{\link{summary.coxph}}.
##'
##' @param x Fitted \code{\link[survival]{coxph}} model.
##' @param log.p Whether to return the logarithm of the p-value.
##' @param test What test to calculate. \code{"likelihood"} is short for means
##'   likelihood ratio test.
##' @param ... Ignored. Kept for S3 consistency.
##' @return p-value.
##' @seealso \code{\link{p.value}}
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
p.value.coxph <- function(x, log.p=FALSE, test=c("logrank", "wald", "likelihood"), ...){
    test <- match.arg(test)
    df <- sum(!is.na(x$coefficients))
    switch(test,
        logrank = pchisq(x$score, df, lower.tail=FALSE, log.p=log.p),
        wald = pchisq(x$score, df, lower.tail=FALSE, log.p=log.p),
        likelihood = pchisq(x$logtest, df, lower.tail=FALSE, log.p=log.p))
}


##' Extract p-value from a cumulative incidence estimation
##' 
##' This is also known as Gray's test.
##' 
##' @param x Fitted \code{\link[cmprsk]{cuminc}} estimate.
##' @param log.p Whether to return the logarithm of the p-value.
##' @param ... Ignored. Kept for S3 consistency.
##' @return p-value.
##' @seealso \code{\link{p.value}}
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
p.value.cuminc <- function(x, log.p=FALSE, ...){
    pchisq(x$Tests[,"stat"], x$Tests[,"df"], lower.tail=FALSE, log.p=log.p)
}


##' Extracts p-value from a competing risk model
##' 
##' @param x Fitted crr model, as returned by \code{\link[cmprsk]{crr}}.
##' @param log.p Whether to return the logarithm of the p-value.
##' @param ... Ignored. Kept for S3 consistency.
##' @return Two-sided p-value.
##' @examples
##' library(cmprsk)
##' time <- 1:20
##' event <- c(rep(0, 9), rep(2, 3), rep(1, 8))
##' data <- rep(0:1, each=10)
##' x <- crr(time, event, data)
##' 
##' # Compare p-values of implementations
##' print(x)
##' p.value(x)
##' @seealso \code{\link{p.value}}
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
p.value.crr <- function(x, log.p=FALSE, ...){
    pval <- pnorm(abs(x$coef)/sqrt(diag(x$var)), lower.tail=FALSE, log.p=log.p)
    if(log.p) log(2) + pval else 2 * pval
}

##' Extracts p-value from a logrank test
##' 
##' @param x Logrank test result, as returned by \code{\link[survival]{survdiff}}.
##' @param log.p Whether to return the logarithm of the p-value.
##' @param ... Ignored. Kept for S3 consistency.
##' @return p-value.
##' library(survival)
##' y <- Surv(time=1:100, event=rep(1:0, each=50))
##' groups <- rep(1:2, each=50)
##' x <- survdiff(y ~ groups)
##' 
##' # Compare p-values of implementations
##' print(x)
##' p.value(x)
##' @seealso \code{\link{p.value}}
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
p.value.survdiff <- function(x, log.p=FALSE, ...){
    pchisq(x$chisq, length(x$n) - 1, lower.tail=FALSE, log.p=log.p)
}

