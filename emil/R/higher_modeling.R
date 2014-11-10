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
#' This function studies the change in permformance as the size of the training
#' set is varied.
#' 
#' @param proc \code{\link{modeling.procedure}}.
#' @param x Dataset descriptors.
#' @param y Response.
#' @param frac Fraction of dataset to hold out, i.e. use as test set.
#' @param nfold How many holdout folds that should be calculated.
#' @param ... Sent to \code{\link{evaluate.modeling}}.
#' @examples
#' \dontrun{
#' proc <- lapply(c(Linear="lda", Quadratic="qda"), modeling.procedure)
#' lc <- learning.curve(proc, iris[-5], iris$Species)
#' require(emilPlots)
#' plot(lc)
#' }
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
learning.curve <- function(proc, x, y, frac, nfold=100, ...){
    if(missing(frac)){
        frac <- 1-exp(seq(log(.2), log(.95), length.out=20))
        frac <- frac[!duplicated(round(frac*length(y)))]
    }

    resamples <- lapply(frac, function(f) resample("holdout", y, frac=f, nfold=nfold))
    result <- lapply(resamples, function(r){
        evaluate.modeling(proc, x, y, resample=r)
    })
    structure(list(n = length(y), frac = frac, result=result),
              class="learning.curve")
}

