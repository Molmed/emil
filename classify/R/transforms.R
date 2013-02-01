##' @import cmprsk
##' @import survival
{}

##' Dataset transforms and varible selection
##' 
##' This is a class of functions that based on a design dataset creates a
##' transform that can be applied to other datasets. Typically they are fed a
##' designset and produce a transform that is applied both to the designset
##' itself before classifier fitting and to a testset before prediction. The key
##' here is that the testset was never allowed to be part of the definition of
##' the transform. This can be automated in \code{\link{batch.design}} by
##' pluging in a transform function using the \code{pre.trans} argument.
##' 
##' This package contain the following transform functions, note that variable
##' selection methods are essentially a form of transform aswell.
##' 
##' \tabular{ll}{
##'   \code{\link{center.transform}} \tab Centers data.\cr
##'   \code{\link{standard.transform}} \tab Centers and scales data to have
##'     mean 0 and variance 1.\cr
##'   \code{\link{coxscore.selection}} \tab Ranks variables with Cox scoring
##'     and keeps the best ones.\cr
##' }
##' 
##' @name transforms
{}


##' Define function to center data
##' 
##' This transform centers the data to have mean 0.
##'
##' @param x Dataset
##' @param ... Ignored.
##' @return A transform function.
##' @seealso transforms
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
center.transform <- function(x, ...){
    xm <- apply(x, 2, mean, na.rm=TRUE)
    rm(x)
    function(x) sweep(x, 2, xm)
}


##' Define function to standardize data
##' 
##' This transform centers and scales the data to have mean 0 and variance 1.
##'
##' @param x Dataset
##' @param ... Ignored.
##' @return A transform function.
##' @seealso transforms
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
standard.transform <- function(x, ...){
  xm <- apply(x, 2, mean, na.rm=TRUE)
  xs <- apply(x, 2, sd, na.rm=TRUE)
  rm(x)
  function(x) sweep(sweep(x, 2, xm), 2, xs, "/")
}


##' Variable selection with Cox scoring
##' 
##' Call this function to create a transform that can be plugged into
##' \code{\link{batch.design}}, do not plug in \code{coxscore.selection} itself.
##' 
##' @param n Number of variables to keep. This is really a million dollar
##'   question, so I pass it over to you!
##' @param method Which survival analysis model to use.
##' @return A function that when executed creates a transform function.
##' @seealso transforms
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
coxscore.selection <- function(n, method=c("coxph", "crr")){
    method <- match.arg(method)
    function(x, y, ...){
        if(method == "coxph") y <- as.Surv(y)
        score <- apply(x, 2, switch(method,
            coxph = function(xx){
                with(summary(survival::coxph(y ~ xx)),
                     abs(coefficients[1]/coefficients[4]))
            },
            crr = function(xx){
                with(summary(coxph::crr(y$time, integer.events(y$event), xx)),
                     coef[1] / coef[3])
            }
        ))
        rm(x, y)
        i <- order(score, decreasing=TRUE) <= n
        function(x, ...) x[,i,drop=FALSE]
    }
}

