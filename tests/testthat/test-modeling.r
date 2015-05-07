context("Modeling framework")

test_that("modeling procedure", {
    proc1 <- modeling_procedure("randomForest")
    proc2 <- modeling_procedure(fit_fun=identity, error_fun=rmse)

    expect_that(names(proc1), is_identical_to(names(proc2)))

    expect_that(proc1$fit_fun, is_a("function"))
    expect_that(proc1$predict_fun, is_a("function"))
    expect_that(proc1$importance_fun, is_a("function"))
    expect_null(proc1$error_fun)

    expect_that(proc2$fit_fun, is_a("function"))
    expect_that(proc2$predict_fun, is_a("simpleError"))
    expect_that(proc2$importance_fun, is_a("simpleError"))
    expect_that(proc2$error_fun, is_a("function"))
})

x <- iris[-5]
y <- iris$Species
cv <- resample("crossvalidation", y, nfold=3, nreplicate=2)
procedure <- modeling_procedure("lda")
modeling_fun <- function(..., xx=x, .verbose=FALSE)
    evaluate(procedure, xx, y, resample=cv, ..., .verbose=.verbose)

test_that("Standard usage", {
    perf <- modeling_fun(.save=list(fit=TRUE, prediction=TRUE, importance=FALSE))

    expect_that(perf, is_a("modeling_result"))
    expect_that(length(perf), is_equivalent_to(length(cv)))
    expect_that(names(perf), is_equivalent_to(names(cv)))
    expect_that(subtree(perf, TRUE, "error"), is_a("numeric"))
    expect_true(all(sapply(perf, function(p) all(c("fit", "prediction") %in% names(p)))))
})

test_that("Parallelization", {
    require(parallel)
    if(detectCores() > 1 && .Platform$OS.type != "windows"){
        procedure <- modeling_procedure("lda", error_fun = function(...){
            Sys.sleep(0.5)
            list(pid=Sys.getpid(), time=Sys.time())
        })
        modeling_fun <- function(..., resample=cv[1:4])
            batch_model(procedure, x, y, resample=resample, ...)

        seq.time <- system.time( seq.perf <- modeling_fun() )
        if(.Platform$OS.type == "windows"){
            cl <- makePSOCKcluster(2)
            clusterExport(cl, c("procedure", "x", "y", "modeling_fun", "listify",
                "get_debug_flags", "set_debug_flags", "is_blank", "is.tuned",
                "is.tunable", "error.rate", "log.message", "indent",
                "pre.split", "index_fit", "index_test", "na_fill", "fill",
                "nice.require"))
            par.time <- system.time(
                par.perf <- parLapply(cl, cv[1:4], function(fold) modeling_fun(resample=fold))
            )
            stopCluster(cl)
        } else {
            par.time <- system.time( par.perf <- modeling_fun(.cores=2) )
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
        perf <- modeling_fun()
        check.perf <- modeling_fun(.checkpoint_dir=tmp.dir)

        expect_that(check.perf, is_identical_to(perf))
        expect_true(file.exists(tmp.dir))

        unlink(tmp.dir, recursive=TRUE)
    }
})

test_that("Error handling", {
    x[1] <- NA
    expect_error(modeling_fun(xx=x, .return_error=FALSE))
    perf <- modeling_fun(xx=x, .return_error=TRUE, .verbose=-1) # Test verbosity too
    expect_true(all(sapply(perf, inherits, "error")))
})

