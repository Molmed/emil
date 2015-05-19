context("Pre-processing")

x <- sweep(matrix(0, 6, 6), 1, 1:6, "+")
na.ind <- arrayInd(sample(length(x)/2, 10), c(6,6))[,2:1]
x[na.ind] <- NA
y <- NULL
fold <- rep(0:1, each=3)

test_that("Median imputation", {
    sets <- pre_impute_median(pre_split(x, y, fold))
    expect_true(all(sets$test$x[na.ind] == 5))
})

test_that("k-NN imputation", {
    sets <- pre_impute_knn(pre_split(x, y, fold), k=1, distance_matrix=dist(x))
    expect_true(all(sets$test$x[na.ind] == 4))
    sets2 <- pre_impute_knn(pre_split(x, y, fold), k=1, distance_matrix="auto")
    expect_identical(sets, sets2)

    x[5,1:3] <- NA
    sets <- pre_impute_knn(pre_split(x, y, fold), k=2, distance_matrix="auto")
    expect_equivalent(sets$fit$x[2,], rep(5,6))
    expect_equivalent(sets$test$x[na.ind], ifelse(na.ind[,2] <= 3, 5, 4.5))
})
