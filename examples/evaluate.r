x <- iris[-5]
y <- iris$Species
procedure <- modeling_procedure("lda")
cv <- resample("crossvalidation", y, 4, 4)
result <- evaluate(procedure, x, y, resample=cv)

# Multiple procedures fitted and tested simultaneously. 
# This is useful when the dataset is large and the splitting takes a long time.
# If you name the elements of the list emil will also name the elements of the
# results object in the same way.
double_procedure <- list(Linear = modeling_procedure("lda"),
                         Quadratic = modeling_procedure("qda"))
result <- evaluate(double_procedure, x, y, resample=cv)

# Multicore parallelization (on a single computer)
result <- evaluate(procedure, x, y, resample=cv, .cores=2)

# Parallelization using a cluster (not limited to a single computer)
# PSOCK is supported on windows too!
require(parallel)
cl <- makePSOCKcluster(2)
clusterEvalQ(cl, library(emil))
clusterExport(cl, c("procedure", "x", "y"))
result <- parLapply(cl, cv, function(fold)
    evaluate(procedure, x, y, resample=fold))

