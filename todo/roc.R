#' Plot ROC or precision-recall curves
#' 
#' This function plots each fold as a separate curve (colored by modeling
#' procedure in case of multiple procedures). Average curves must still be
#' approximated manually.
#' 
#' @param object Contingency tables returned from \code{\link{contingency}}.
#' @param type What type of plot to produce.
#' @return A ggplot object.
#' @seealso \code{\link{contingency}}, \code{\link{ggplot}}
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
plot.contingency.set <- function(object, type=c("roc", "prr")){
    nice.require("ggplot2")
    type <- match.arg(type)
    stack.stats <- function(x, fields, field.names=c("Fold", "Method")){
        if(inherits(x, "contingency")){
            x[fields]
        } else {
            do.call(
                rbind,
                Map(function(nam, dat){
                        prediction <- data.frame(nam, stack.stats(dat, fields, field.names[-1]))
                        names(prediction)[1] <- field.names[1]
                        prediction
                    },
                    if(is.null(names(x))) seq_along(x) else names(x),
                    x
                )
            )
        }
    }
    if(type == "roc"){
        plot.data <- stack.stats(object, c("sensitivity", "specificity"))
        ggplot(plot.data, aes(y=sensitivity, x=1-specificity)) +
            geom_path(
                if(!"Fold" %in%
                if("Method" %in% names(plot.data)) , colour=Method)
                else aes(group=Fold)) + 
            geom_line(data=data.frame(i=0:1), aes(x=i, y=i), linetype="dashed")
    } else {
        plot.data <- stack.stats(object, c("precision", "recall"))
        ggplot(plot.data[complete.cases(plot.data),], aes(y=precision, x=recall)) +
            geom_path(
                if("Method" %in% names(plot.data)) aes(group=Fold, colour=Method)
                else aes(group=Fold)) + 
            geom_hline(yintercept=.5, linetype="dashed")
    }
}

