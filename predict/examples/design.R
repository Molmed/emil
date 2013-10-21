y <- factor(runif(100) < .3)
x <- matrix(rnorm(100*12), 100, 12) + .8*(y == "TRUE")
fit <- design("qda", x, y)

y.test <- factor(runif(100) < .3)
x.test <- matrix(rnorm(100*12), 100, 12) + .8*(y.test == "TRUE")
pred <- predict(fit, x.test)
table(true=y.test, predicted=pred$pred)

# Same example with variable standardization
fit <- design("lda", x, y, pre.trans=standard.transform)
pred <- predict(fit, x.test)
table(true=y.test, predicted=pred$pred)

# Example of a custom transformation that translates
# and scales the data to be on the [0,1] interval
fit <- design("lda", x, y,
    pre.trans = function(x, ...){
        lower <- min(x)
        upper <- max(x)
        function(x) (x - lower) / (upper - lower)
    })

