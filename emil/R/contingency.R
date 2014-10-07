##' Contingency tables
##' 
##' Contingency tables with counts of true positives, negatives etc. for
##' different threshold values.
##' 
##' @param object Modeling result, as produced by \code{\link{batch.model}}.
##' @param y Response vector.
##' @param ... Sent to class methods.
##' @return A data frame with contingency tables for different threshold values.
##'   Each row list the number of true positives (TP), false positives (FP),
##'   false negatives (FN), and true negatives (TN) obtained with
##'   probability >= threshold. Sensitivity, specificity, precision, and recall
##'   are also returned (although sensitivity and recall are in fact the same).
##' @examples
##' proc <- modeling.procedure("lda")
##' y <- factor(iris$Species == "setosa", levels=c(TRUE, FALSE), labels=c("Setosa", "Other"))
##' cv <- resample("crossval", y, nfold=5, nrep=3)
##' perf <- evaluate.modeling(proc, x=iris[-5], y=y, resample=cv)
##' contingency(perf, y, cv)
##' @seealso plot.contingency
##' @author Christofer \enc{Bäcklin}{Backlin}
##' @export
contingency <- function(object, y, ...){
    UseMethod("contingency")
}
##' @rdname contingency
##' @export
contingency.default <- function(object, y, ...){
    if(is.list(object)){
        lapply(object, contingency, y, ...)
    } else {
        stop("Invalid modeling result")
    }
}
##' @rdname contingency
##' @export
contingency.model <- function(object, y, ...){
    if(!is.factor(y) && length(levels(y)) == 2)
        stop("Contingency can only be computed for binary classification problems.")
    prob <- object$pred$prob
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
##' @param resample Resampling scheme used to obtain \code{object}.
##' @rdname contingency
##' @export
contingency.modeling.result <- function(object, y, resample, ...){
    structure(
        Map(function(obj, res) contingency(obj, y[index.test(res)]),
            object, resample),
        class="contingency.set")
}

# ##' Plot ROC or precision-recall curves
# ##' 
# ##' This function plots each fold as a separate curve (colored by modeling
# ##' procedure in case of multiple procedures). Average curves must still be
# ##' approximated manually.
# ##' 
# ##' @param object Contingency tables returned from \code{\link{contingency}}.
# ##' @param type What type of plot to produce.
# ##' @return A ggplot object.
# ##' @seealso \code{\link{contingency}}, \code{\link{ggplot}}
# ##' @author Christofer \enc{Bäcklin}{Backlin}
# ##' @export
# plot.contingency.set <- function(object, type=c("roc", "prr")){
#     nice.require("ggplot2")
#     type <- match.arg(type)
#     stack.stats <- function(x, fields, field.names=c("Fold", "Method")){
#         if(inherits(x, "contingency")){
#             x[fields]
#         } else {
#             do.call(
#                 rbind,
#                 Map(function(nam, dat){
#                         d <- data.frame(nam, stack.stats(dat, fields, field.names[-1]))
#                         names(d)[1] <- field.names[1]
#                         d
#                     },
#                     if(is.null(names(x))) seq_along(x) else names(x),
#                     x
#                 )
#             )
#         }
#     }
#     if(type == "roc"){
#         plot.data <- stack.stats(object, c("sensitivity", "specificity"))
#         ggplot(plot.data, aes(y=sensitivity, x=1-specificity)) +
#             geom_path(
#                 if(!"Fold" %in%
#                 if("Method" %in% names(plot.data)) , colour=Method)
#                 else aes(group=Fold)) + 
#             geom_line(data=data.frame(i=0:1), aes(x=i, y=i), linetype="dashed")
#     } else {
#         plot.data <- stack.stats(object, c("precision", "recall"))
#         ggplot(plot.data[complete.cases(plot.data),], aes(y=precision, x=recall)) +
#             geom_path(
#                 if("Method" %in% names(plot.data)) aes(group=Fold, colour=Method)
#                 else aes(group=Fold)) + 
#             geom_hline(yintercept=.5, linetype="dashed")
#     }
# }

