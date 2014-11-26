l <- list(A=list(a=0:2, b=3:4, c=023-22030),
          B=list(a=5:7, b=8:9))
subtree(l, 1:2, "b")
subtree(l, TRUE, mean, "a")

# More practical examples
x <- iris[-5]
y <- iris$Species
cv <- resample("crossval", y, nfold=5, nrep=3)
proc <- modeling.procedure("pamr")

# To illustrate the error handling capacities of subtree we'll introduce some
# spurious errors in the pre-processing function. By setting .return.errors=TRUE
# they wont break the execution, but will instead be return in the results.
perf <- evaluate.modeling(proc, x, y, resample=cv,
    .save=list(vimp=TRUE), .return.errors=TRUE,
    pre.process = function(...){
        if(runif(1) < .3)
            stop("Oh no! Unforseen error!")
        pre.pamr(...)
    }
)
cat(sum(sapply(perf, inherits, "error")),
    "folds did not complete successfully!\n")

# Extract error rates. Since some folds fail it will be an ugly list with both
# numeric estimates and NULL values (for the failed folds).
subtree(perf, TRUE, "error")

# To put it on a more consistent form we can impute the missing error rates
# with NA to allow automatic simplification into a vector (since it requires
# all values to be on the same form, i.e. numeric(1) rather than a mix
# between numeric(1) and NULL as in the previous example).
subtree(perf, TRUE, "error", error.value=as.numeric(NA), warn=-1)

# Sum up variable importance for all classes within each fold and extract.
# Note that the lengths (= 4) must match between the folds for the automatic
# simplification to work.
subtree(perf, TRUE, "vimp", function(x){
    if(is.null(x)){
        rep(NA, 4)
    } else {
        apply(x, 1, sum)
    }
})
