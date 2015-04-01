context("Pre-processing")

test_that("Median imputation", {
    x <- sweep(matrix(0, 6, 6), 1, 1:6, "+")
    na.ind <- arrayInd(sample(length(x)/2, 10), c(6,6))[,2:1]
    x[na.ind] <- NA
    sets <- pre_impute_median(x, NULL, rep(0:1, each=3))
    expect_true(all(sets$test$x[na.ind] == 5))
})

test_that("k-NN imputation", {
    x <- sweep(matrix(0, 6, 6), 1, 1:6, "+")
    na.ind <- arrayInd(round(seq(1, 18, length.out=8)), c(6,6))[,2:1]
    x[na.ind] <- NA
    sets <- pre_impute_knn(x, NULL, rep(0:1, each=3), k=1, distmat="auto")
    expect_true(all(sets$test$x[na.ind] == 4))

    x[5,1:3] <- NA
    sets <- pre_impute_knn(x, NULL, rep(0:1, each=3), k=2, distmat="auto")
    expect_that(sets$fit$x[2,], is_equivalent_to(rep(5,6)))
    expect_that(sets$test$x[na.ind], is_equivalent_to(c(5,5,4.5,5,4.5,5,4.5,4.5)))
})
