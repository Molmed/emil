context("Methods")

test_that("pamr", {
    reset.warn.once()
    expect_warning(fit <- fit.pamr(iris[-5], iris$Species,
            cv=resample.crossval(iris$Species, nfold=5, nrep=1)),
        "use.*pre.pamr.*pre-processing")
    expect_that(fit, is_a("list"))
})

test_that("glmnet", {
    reset.warn.once()
    expect_warning(fit <- fit.glmnet(iris[-5], iris$Species),
                   ".*data set.*matrix.*form.*")
    fit <- fit.glmnet(as.matrix(iris[-5]), iris$Species)
    expect_that(fit, is_a("list"))
    expect_that(fit$glmnet.fit, is_a("glmnet"))
})
