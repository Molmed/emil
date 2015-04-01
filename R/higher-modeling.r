#' Determine if a performance estimate has stabilized
#' 
#' This function determines if the cumulative mean performance varies less than
#' a fraction of the standard deviation of the estimates.
#' This function is mainly intended for the the soon-to-be-implemented
#' \code{evaluate.until.stable}.
#' 
#' @param x Modeling results as returned from \code{\link{evaluate}}.
#' @param tail_length The length
#' @param sd_accuracy The accuracy threshold used to determine whether the
#'   cumulative mean performance estimate has stabilized.
#' @return A logical vector wrapped with some attributes used for plotting.
#' @examples
#' procedure <- modeling_procedure("qda")
#' perf <- replicate(30, evaluate(procedure, iris[-5], iris$Species),
#'                   simplify=FALSE)
#' stable_mean(perf)
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
stable_mean <- function(x, tail_length=5, sd_accuracy=.1){
    if(all(sapply(x, inherits, "modeling_result"))){
        if(is_multi_procedure(x[[1]])){
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
    if(nrow(estimate) < tail_length+1){
        passed <- NULL
        out <- FALSE
    } else {
        passed <- as.data.frame(
            Map(function(x, p, e) abs(x-p) <= e*sd_accuracy,
                cum.mean[nrow(cum.mean)-tail_length:1,], tail(cum.mean, 1), estimate.sd))
        out <- sapply(passed, all)
    }
    structure(out, estimate = estimate, passed=passed,
              sd_accuracy=sd_accuracy, class="stable_mean")
}


