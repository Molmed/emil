context("Resampling schemes")

test_that("repeated holdout", {
    y <- factor(rep(1:2, c(10, 40)))
    ho <- resample("holdout", y, frac=1/5, nfold=5)
    expect_that(ho, is_a("holdout"))
    expect_that(attr(ho[[1]], "param"),
                is_identical_to(list(frac=1/5, nfold=5, balanced=TRUE)))

    ho.tab <- lapply(ho, table, y)
    expect_true(all(sapply(ho.tab[-1], all.equal, ho.tab[[1]])))
})

test_that("cross-validation", {
    y <- factor(rep(1:2, c(10, 40)))
    cv <- resample("crossval", y, nfold=5, nrep=3)
    expect_that(cv, is_a("crossval"))

    cv.tab <- lapply(cv, table, y)
    expect_true(all(sapply(cv.tab[-1], all.equal, cv.tab[[1]])))

    y[1] <- NA
    cv <- resample("crossval", y, nfold=5, nrep=3)
    cv.tab <- lapply(cv, table, y)
    expect_that(range(sapply(cv.tab, "[", 1)), is_equivalent_to(1:2))
    expect_that(range(sapply(cv.tab, "[", 2)), is_equivalent_to(7:8))
    expect_true(all(sapply(cv.tab[-1], function(x) all.equal(x[,2], cv.tab[[1]][,2]))))
})

