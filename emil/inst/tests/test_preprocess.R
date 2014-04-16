context("Pre-processing")

test_that("Median imputation", {
    x <- sweep(matrix(0, 6, 6), 1, 1:6, "+")
    na.ind <- arrayInd(sample(length(x)/2, 10), c(6,6))[,2:1]
    x[na.ind] <- NA
    sets <- pre.impute.median(x, NULL, rep(c(TRUE, FALSE), each=3))
    expect_true(all(sets$test[na.ind] == 5))
})

test_that("k-NN imputation", {
    x <- sweep(matrix(0, 6, 6), 1, 1:6, "+")
    na.ind <- arrayInd(sample(length(x)/2, 10), c(6,6))[,2:1]
    x[na.ind] <- NA
    sets <- pre.impute.knn(x, NULL, rep(c(TRUE, FALSE), each=3), k=1, distmat="auto")
    expect_true(all(sets$test[na.ind] == 4))

    x[5,] <- NA
    sets <- pre.impute.knn(x, NULL, rep(c(TRUE, FALSE), each=3), k=2, distmat="auto")
    expect_true(all(sets$fit[2,] == 5))
})
