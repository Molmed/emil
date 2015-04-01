
plot.stable.mean <- function(x, y, ...){
    nice.require("tidyr")
    p <- attributes(x)
    plot.data <- merge(all.x = TRUE,
        gather(data.frame(p$estimate, Replicate=1:nrow(p$estimate), cumMean=NA),
               Method, Performance, seq_along(p$estimate), -Replicate),
        gather(data.frame(p$passed, Replicate=nrow(p$estimate)-nrow(p$passed):1),
               Method, Passed, seq_along(p$passed), -Replicate)
    )
    split(plot.data$cumMean, plot.data$Method) <-
        lapply(split(plot.data$Performance, plot.data$Method), cummean)
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
