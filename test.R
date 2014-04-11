#library(emil)
source.all("emil/R")
data(iris)

x <- iris[-5]
y <- iris$Species
cv <- resample.crossval(y)
proc <- modeling.procedure("randomForest")
perf <- evaluate.modeling(proc, x, y, test.subset=cv, .checkpoint.dir="dir")






y <- gl(2,30)
x <- sweep(matrix(rnorm(60*10), 60), 1, rep(0:1/3, each=30))
tmp <- rep(c(FALSE, FALSE, FALSE, FALSE, TRUE), 13)
tmx <- rep(c(FALSE,TRUE,FALSE,TRUE,FALSE), c(24,6,24,6,24))
test.subset <- data.frame(tmp[5:64], tmp[4:63], tmp[3:62], tmp[2:61], tmp[1:60],
            tmx[25:84], tmx[19:78], tmx[13:72], tmx[7:66], tmx[1:60])
rm(tmp, tmx)
subset <- TRUE
models <- list(lda=list(pi=list(c(.25, .75), c(.5, .5), c(.75, .25))))
models <- c(list(lda=list()), rep(models, 2))
pre.trans <- function(x, fold){
    list(design=x[na.fill(!fold, FALSE),,drop=FALSE],
         test=x[na.fill(fold, FALSE),,drop=FALSE])
}
error.fun <- function(y, pred) mean(y != pred$pred)
na.fill <- function(x, fill){
    x[is.na(x)] <- fill
    x
}


# Call
pred <- design(x, y, models=models, test.subset, error.fun, pre.trans, TRUE)



