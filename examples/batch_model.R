x <- iris[-5]
y <- iris$Species
proc <- modeling.procedure("lda")
cv <- resample("crossval", y, 4, 4)
perf <- batch.model(proc, x, y, cv, .save=list(pred=TRUE))

# Parallelization on windows
require(parallel)
cl <- makePSOCKcluster(2)
clusterEvalQ(cl, library(emil))
clusterExport(cl, c("proc", "x", "y"))
perf <- parLapply(cl, cv, function(fold)
    batch.model(proc, x, y, resample=fold))
