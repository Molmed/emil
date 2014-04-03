set.seed(123)
y <- outcome(c(3 + rnorm(30), 4 + rnorm(30)),
             rep(c("Event", NA), rep(30, 2)))
x <- rnorm(60) + integer.events(y)

sp <- tune.survival(x, y)
groups <- cut(x, c(-Inf, sp, Inf))
layout(matrix(c(1,1,2,3), 2, 2))
plot(y$time, x, pch=1+7*integer.events(y), col=c("blue", "red")[groups],
     las=1, main="Risk vs time")
segments(-10, sp, 10, sp, lty=3)
legend("topright", c("Event", "No event"), pch=c(8, 1))
plot(survfit(as.Surv(y) ~ rep(0, length(x))),
     conf.int=FALSE, main="Whole cohort", las=1)
plot(survfit(as.Surv(y) ~ as.integer(groups)),
     col=c("blue", "red"), main="Groups", las=1)

