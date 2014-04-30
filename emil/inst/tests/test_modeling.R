context("Modeling framework")

test_that("Standard usage", {
    x <- iris[-5]
    y <- iris$Species
    cv <- resample.crossval(y, nfold=3, nrep=2)
    proc <- modeling.procedure("lda")
    perf <- evaluate.modeling(proc, x, y, resample=cv,
                              .save=list(fit=TRUE, pred=TRUE, vimp=FALSE),
                              .verbose=FALSE)

    expect_that(perf, is_a("modeling.result"))
    expect_that(length(perf), is_equivalent_to(length(cv)))
    expect_that(names(perf), is_equivalent_to(names(cv)))
    expect_that(subtree(perf, TRUE, "error"), is_a("numeric"))
    expect_true(all(sapply(perf, function(p) all(c("fit", "pred") %in% names(p)))))

    # Parallelization
    par.perf <- evaluate.modeling(proc, x, y, resample=cv,
                                  .save=list(fit=TRUE, pred=TRUE, vimp=FALSE),
                                  .verbose=FALSE, .parallel.cores=2)
    expect_that(par.perf, is_identical_to(perf))

    # Checkpointing
    if(file.exists("tmp")){
        file.remove(dir("tmp", full.names=TRUE))
        file.remove("tmp")
    }
    check.perf <- evaluate.modeling(proc, x, y, resample=cv,
                                    .save=list(fit=TRUE, pred=TRUE, vimp=FALSE),
                                    .verbose=FALSE, .checkpoint.dir="tmp")
    expect_that(check.perf, is_identical_to(perf))
    expect_true(file.exists("tmp"))
    file.remove(dir("tmp", full.names=TRUE))
    file.remove("tmp")
})
