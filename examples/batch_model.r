x <- iris[-5]
y <- iris$Species
procedure <- modeling_procedure("lda")
cv <- resample("crossvalidation", y, 4, 4)
perf <- batch_model(procedure, x, y, cv, .save=list(pred=TRUE))

# Parallelization on windows
require(parallel)
cl <- makePSOCKcluster(2)
clusterEvalQ(cl, library(emil))
clusterExport(cl, c("procedure", "x", "y"))
perf <- parLapply(cl, cv, function(fold)
    batch_model(procedure, x, y, resample=fold))
