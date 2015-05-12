#' Calculate ROC curves
#'
#' @param result Modeling results, as returned by \code{\link{evaluate}}.
#' @param y True response vector used to create \code{result}.
#' @param resample Resampling scheme used to create \code{result}.
#' @param class The class of interest to create ROC-curves for.
#' @param statistic The name of the statistic (as returned by the prediction
#'   function of the modeling procedure).
#' @return A data frame of class \quote{roc}.
#' @examples
#' # Generate some noisy data
#' my.data <- iris
#' my.data[1:4] <- my.data[1:4] + 2*rnorm(150*4)
#' 
#' # Train and evaluate some classifiers
#' procedure <- list(lda = modeling_procedure("lda"),
#'                   qda = modeling_procedure("qda"))
#' cv <- resample("crossvalidation", iris$Species, nrep=1, nfold=3)
#' result <- evaluate(procedure, my.data[-5], my.data$Species, resample=cv)
#' 
#' # Study the performance
#' select(result, Fold=TRUE, Method=TRUE, Error="error")
#' roc <- roc_curve(result, my.data$Species, cv)
#' plot(roc)
#' @author Christofer \enc{Bäcklin}{Backlin}
#' @export
roc_curve <- function(result, y, resample, class=levels(y), statistic="probability"){
    stopifnot(inherits(result, "modeling_result"))
    stopifnot(is.factor(y))
    if(is.numeric(class)) class <- levels(y)[class]
    roc_fun <- function(stat){
        y_sorted <- y[stat$id][order(stat$Statistic)] == as.character(stat$Class[1])
        data.frame(
            Sensitivity = c(rev(cumsum(rev(y_sorted)))/sum(y_sorted), 0),
            Specificity = c(0, cumsum(!y_sorted)/sum(!y_sorted))
        )
    }
    if(is_multi_procedure(result)){
        stat_table <- select(result, Fold=resample, Method=TRUE, "prediction",
                             statistic, Statistic = class)
    } else {
        stat_table <- select(result, Fold=resample, "prediction",
                             statistic, Statistic = class)
    }
    if(length(class) > 1){
        stat_table <- stat_table %>%
            gather_("Class", "Statistic", class)
    } else {
        stat_table <- stat_table %>% mutate(Class = factor(class))
    }
    stat_table <- stat_table %>%
        group_by_(.dots=intersect(c("Fold", "Method", "Class"), colnames(stat_table))) %>%
        do(roc_fun(.))
    structure(stat_table %>% as.data.frame,
              class=c("roc_curve", "data.frame"))
}

#' @method as.data.table roc_curve
#' @rdname roc_curve
as.data.table.roc_curve <- function(x, ...){
    if(inherits(x, "data.table")) return(x)
    class(x) <- class(x)[-1]
    structure(as.data.table(x), class=c("roc_curve", "data.table"))
}

#' @method as.data.frame roc_curve
#' @rdname roc_curve
as.data.frame.roc_curve <- function(x, ...){
    if(inherits(x, "data.frame")) return(x)
    class(x) <- class(x)[-1]
    structure(as.data.frame(x), class=c("roc_curve", "data.frame"))
}

#' @method plot roc_curve
#' @rdname roc_curve
plot.roc_curve <- function(x, ...){
    nice_require("ggplot2")
    p <- ggplot(x) + 
        geom_segment(x=0, y=0, xend=1, yend=1, linetype="dashed", size=.3) +
        facet_wrap(~Class)
    if("Method" %in% colnames(x)){
        p + geom_path(aes(x=1-Specificity, y=Sensitivity,
                          colour=Method, group=paste(Method, Fold)))
    } else {
        p + geom_path(aes(x=1-Specificity, y=Sensitivity, group=Fold))
    }
}
