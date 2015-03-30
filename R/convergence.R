Plot learning curves

@param x

plot.learning.curve <- function(x, y, ..., summaries=list(Mean = mean, `95-percentile`=function(x) quantile(x, .95))){
    nice.require(c("dplyr", "ggplot2"))
    if(is.multi.proc(x$result[[1]])){
        estimate <- subtree(x$result, TRUE, TRUE, TRUE, "error")
        methods <- names(x$result[[1]][[1]])
        if(is.null(methods))
            methods <- sprintf("Procedure %i", 1:nrow(estimate[[1]]))
        plot.data <- data.frame(
            Performance = unlist(estimate),
            Fraction = rep(x$frac, each=length(estimate[[1]])),
            Method = methods,
            Fold = rep(colnames(estimate[[1]]), each=2)
        )
    } else {
        estimate <- subtree(x$result, TRUE, TRUE, "error")
        plot.data <- data.frame(
            Performance = as.vector(estimate),
            Fraction = rep(x$frac, each=nrow(estimate)),
            Method = "Unnamed procedure",
            Fold = rownames(estimate)
        )
    }
    data.summary <- do.call(rbind, Map(function(method, fun){
        data.frame(Summaries=method,
            summarise(group_by(plot.data, Fraction, Method),
                      Performance = fun(Performance)))
    }, names(summaries), summaries))
    p <- ggplot(plot.data, aes(x=1-Fraction, y=Performance)) + 
        geom_point(colour="grey") +
        geom_line(data=data.summary, aes(colour=Summaries)) +
        xlab("Relative training set size")
    if(is.multi.proc(x$result[[1]]))
        p <- p + facet_wrap(~Method)
    p
}

plot(learning.curve.perf[[group]][[drug]])


plot.stable.mean <- function(x, y, ...){
    nice.require("tidyr")
    p <- attributes(x)
    plot.data <- merge(all.x = TRUE,
        gather(data.frame(p$estimate, Replicate=1:nrow(p$estimate), cumMean=NA),
               Method, Performance, seq_along(p$estimate), -Replicate),
        gather(data.frame(p$passed, Replicate=nrow(p$estimate)-nrow(p$passed):1),
               Method, Passed, seq_along(p$passed), -Replicate)
    )
    cum.mean <- function(x) cumsum(x)/seq_along(x)
    split(plot.data$cumMean, plot.data$Method) <-
        lapply(split(plot.data$Performance, plot.data$Method), cum.mean)
    plot.data$Passed <- factor(plot.data$Passed, c(TRUE, FALSE), c("Passed", "Failed"))

    limits <- transform(summarise(group_by(plot.data, Method),
                                  Mean = mean(Performance), SD = sd(Performance)),
        Upper = Mean + SD*p$sd.accuracy,
        Lower = Mean - SD*p$sd.accuracy)

    ggplot(plot.data, aes(x=Replicate, y=Performance, colour=Method)) +
        geom_hline(data=gather(limits, Limit, Performance, Lower, Upper),
                   aes(yintercept=Performance, colour=Method), linetype="dashed") +
        geom_line(aes(y=cumMean)) +
        supprennWarnings(geom_point(aes(y=cumMean, shape=Passed))) +
        scale_shape_manual("Status", values=c(1,4)) +
        ylab("Cumulative mean performance")
}
