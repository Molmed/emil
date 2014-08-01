context("Modeling framework")

test_that("modeling procedure", {
    proc1 <- modeling.procedure("pamr")
    proc2 <- modeling.procedure(fit.fun=identity, error.fun=rmse)

    expect_that(names(proc1), is_identical_to(names(proc2)))

    expect_that(proc1$fit.fun, is_a("function"))
    expect_that(proc1$predict.fun, is_a("function"))
    expect_that(proc1$vimp.fun, is_a("function"))
    expect_null(proc1$error.fun)

    expect_that(proc2$fit.fun, is_a("function"))
    expect_that(proc2$predict.fun, is_a("simpleError"))
    expect_that(proc2$vimp.fun, is_a("simpleError"))
    expect_that(proc2$error.fun, is_a("function"))
})

x <- iris[-5]
y <- iris$Species
cv <- resample("crossval", y, nfold=3, nrep=2)
proc <- modeling.procedure("lda")
modeling.FUN <- function(..., xx=x)
    evaluate.modeling(proc, xx, y, resample=cv, ..., .verbose=FALSE)

test_that("Standard usage", {
    perf <- modeling.FUN(.save=list(fit=TRUE, pred=TRUE, vimp=FALSE))

    expect_that(perf, is_a("modeling.result"))
    expect_that(length(perf), is_equivalent_to(length(cv)))
    expect_that(names(perf), is_equivalent_to(names(cv)))
    expect_that(subtree(perf, TRUE, "error"), is_a("numeric"))
    expect_true(all(sapply(perf, function(p) all(c("fit", "pred") %in% names(p)))))
})

test_that("Parallelization", {
    require(parallel)
    if(detectCores() > 1 && .Platform$OS.type != "windows"){
        proc <- modeling.procedure("lda", error.fun = function(...){
            Sys.sleep(0.5)
            list(pid=Sys.getpid(), time=Sys.time())
        })
        modeling.FUN <- function(..., resample=cv[1:4])
            batch.model(proc, x, y, resample=resample, ...)

        seq.time <- system.time( seq.perf <- modeling.FUN() )
        if(.Platform$OS.type == "windows"){
            cl <- makePSOCKcluster(2)
            clusterExport(cl, c("proc", "x", "y", "modeling.FUN", "listify",
                "get.debug.flags", "set.debug.flags", "is.blank", "is.tuned",
                "is.tunable", "error.rate", "trace.msg", "increase",
                "pre.split", "index.fit", "index.test", "na.fill", "fill",
                "nice.require"))
            par.time <- system.time(
                par.perf <- parLapply(cl, cv[1:4], function(fold) modeling.FUN(resample=fold))
            )
            stopCluster(cl)
        } else {
            par.time <- system.time( par.perf <- modeling.FUN(.parallel.cores=2) )
        }

        expect_more_than(seq.time["elapsed"], par.time["elapsed"])
        expect_equal(length(unique(subtree(seq.perf, TRUE, "error", "pid"))), 1)
        expect_equal(length(unique(subtree(par.perf, TRUE, "error", "pid"))), 2)
        expect_more_than(
            diff(range(subtree(seq.perf, T, "error", "time"))),
            diff(range(subtree(par.perf, T, "error", "time")))
        )
    }
})

test_that("Checkpointing", {
    if(file.access(".", 2) == 0){
        tmp.dir <- tempdir()
        perf <- modeling.FUN()
        check.perf <- modeling.FUN(.checkpoint.dir=tmp.dir)

        expect_that(check.perf, is_identical_to(perf))
        expect_true(file.exists(tmp.dir))

        unlink(tmp.dir, recursive=TRUE)
    }
})

test_that("Error handling", {
    x[1] <- NA
    expect_error(modeling.FUN(xx=x, .return.errors=FALSE))
    perf <- modeling.FUN(xx=x, .return.errors=TRUE)
    expect_true(all(sapply(perf, inherits, "error")))
})

