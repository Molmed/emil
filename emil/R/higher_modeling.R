#' Determine if a performance estimate has stabilized
#' 
#' This function determines if the cumulative mean performance varies less than
#' a fraction of the standard deviation of the estimates.
#' This function is mainly intended for the the soon-to-be-implemented
#' \code{evaluate.until.stable}.
#' 
#' @param x Modeling results as returned from \code{\link{evaluate.modeling}}.
#' @param tail.length The length
#' @param sd.accuracy The accuracy threshold used to determine whether the
#'   cumulative mean performance estimate has stabilized.
#' @return A logical vector wrapped with some attributes used for plotting.
#' @examples
#' proc <- modeling.procedure("qda")
#' perf <- replicate(30, evaluate.modeling(proc, iris[-5], iris$Species),
#'                   simplify=FALSE)
#' stable.mean(perf)
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
stable.mean <- function(x, tail.length=5, sd.accuracy=.1){
    if(all(sapply(x, inherits, "modeling.result"))){
        if(is.multi.proc(x[[1]])){
            estimate <- as.data.frame(t(sapply(x, function(x)
                apply(subtree(x, TRUE, TRUE, "error"), 1, mean))))
        } else {
            estimate <- data.frame(sapply(x,
                function(x) mean(subtree(x, TRUE, "error"))))
        }
    } else {
        estimate <- data.frame(sapply(x, mean))
    }
    estimate.sd <- sapply(estimate, sd)
    cum.mean <- as.data.frame(lapply(estimate, function(x) cumsum(x)/seq_along(x)))
    if(nrow(estimate) < tail.length+1){
        passed <- NULL
        out <- FALSE
    } else {
        passed <- as.data.frame(
            Map(function(x, p, e) abs(x-p) <= e*sd.accuracy,
                cum.mean[nrow(cum.mean)-tail.length:1,], tail(cum.mean, 1), estimate.sd))
        out <- sapply(passed, all)
    }
    structure(out, estimate = estimate, passed=passed,
              sd.accuracy=sd.accuracy, class="stable.mean")
}


#' Learning curve analysis
#' 
#' This function studies the change in permformance as the sizes of the fitting
#' and test sets are varied. In case the studied modeling procedures cannot
#' produce models on the smallest fitting sets, please use
#' \code{.return.errors=TRUE} (see \code{\link{batch.model}}.
#' 
#' @param proc \code{\link{modeling.procedure}}.
#' @param x Dataset descriptors.
#' @param y Response.
#' @param frac Fraction of dataset to hold out, i.e. use as test set. Defaults
#'   20 logarithmically distributed values ranging from all but 5 observations
#'   per class in the largest test set to only 5 observations per class in
#'   the smallest test set.
#' @param nfold How many holdout folds that should be calculated.
#' @param ... Sent to \code{\link{evaluate.modeling}}.
#' @param .verbose Whether to print an activity log. Set to \code{-1} to
#'   also suppress output generated from the procedure's functions.
#' @examples
#' \dontrun{
#' proc <- lapply(c(Linear="lda", Quadratic="qda"), modeling.procedure)
#' lc1 <- learning.curve(proc, iris[-5], iris$Species, frac=7:1/10)
#' 
#' options(emil.max.indent=3)
#' lc2 <- learning.curve(proc, iris[-5], iris$Species, .return.errors=TRUE)
#'
#' require(emilPlots)
#' plot(lc1)
#' }
#' @references Richard O Duda, Peter E Hart, and David G Stork. Pattern
#'   Classification. Wiley, 2nd edition, 2000. ISBN 978-0-471-05669-0.
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
learning.curve <- function(proc, x, y, frac, nfold=100, ..., .verbose=TRUE){
    if(missing(frac)){
        min.frac <- switch(class(y),
            factor = min(5/table(y)),
            5/length(y)
        )
        if(min.frac >= .5) stop("Dataset is too small to do a meaningful learning curve analysis.")
        frac <- 1-exp(seq(log(min.frac), log(1-min.frac), length.out=20))
        frac <- frac[!duplicated(round(frac*length(y)))]
    }

    resamples <- lapply(frac, function(f) resample("holdout", y, frac=f, nfold=nfold))
    counter <- 0
    trace.msg(indent(.verbose, 0), "Learning curve analysis")
    result <- lapply(resamples, function(r){
        counter <<- counter + 1
        trace.msg(indent(.verbose, 1), "Test set fraction %i of %i (%.4g)",
                  counter, length(frac), frac[counter])
        evaluate.modeling(proc, x, y, ..., resample=r, .verbose=indent(.verbose, 2))
    })
    structure(list(n = length(y), frac = frac, result=result),
              class="learning.curve")
}

