#' Contingency tables
#' 
#' Contingency tables with counts of true positives, negatives etc. for
#' different threshold values.
#' 
#' @param object Modeling result, as produced by \code{\link{batch_model}}.
#' @param y Response vector.
#' @param ... Sent to class methods.
#' @return A data frame with contingency tables for different threshold values.
#'   Each row list the number of true positives (TP), false positives (FP),
#'   false negatives (FN), and true negatives (TN) obtained with
#'   probability >= threshold. Sensitivity, specificity, precision, and recall
#'   are also returned (although sensitivity and recall are in fact the same).
#' @examples
#' procedure <- modeling_procedure("lda")
#' y <- factor(iris$Species == "setosa", levels=c(TRUE, FALSE), labels=c("Setosa", "Other"))
#' cv <- resample("crossvalidation", y, nfold=5, nreplicate=3)
#' perf <- evaluate(procedure, x=iris[-5], y=y, resample=cv)
#' contingency(perf, y, cv)
#' @seealso plot.contingency
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
contingency <- function(object, y, ...){
    UseMethod("contingency")
}
#' @rdname contingency
#' @export
contingency.default <- function(object, y, ...){
    if(is.list(object)){
        lapply(object, contingency, y, ...)
    } else {
        stop("Invalid modeling result")
    }
}
#' @rdname contingency
#' @export
contingency.model <- function(object, y, ...){
    if(!is.factor(y) && length(levels(y)) == 2)
        stop("Contingency can only be computed for binary classification problems.")
    prob <- object$prediction$prob
    if(is.data.frame(prob)) prob <- prob[[1]]
    if(is.matrix(prob)) prob <- prob[,1]

    threshold <- sort(unique(c(prob, Inf)))
    tab <- structure(
        data.frame(
            threshold,
            t(vapply(
                threshold,
                function(thr) table(y, factor(prob >= thr, levels=c(TRUE, FALSE), labels=levels(y))),
                numeric(4)
            ))
        ),
        names = c("threshold", "TP", "FP", "FN", "TN")
    )
    structure(
        transform(tab,
            sensitivity = TP/(TP+FN),
            specificity = TN/(FP+TN),
            precision = TP/(TP+FP),
            recall = TP/(TP+FN)
        ),
        class = c("contingency", "data.frame")
    )
}
#' @param resample Resampling scheme used to obtain \code{object}.
#' @rdname contingency
#' @export
contingency.modeling_result <- function(object, y, resample, ...){
    structure(
        Map(function(obj, res) contingency(obj, y[index_test(res)]),
            object, resample),
        class="contingency.set")
}

