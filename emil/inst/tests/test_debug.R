context("Debugging")

test_that("Debugging flag persistance", {
    if(R.Version()$major < 3 || R.Version()$minor < 1.0){
        is_debugged <- function(x){
            expectation(isdebugged(x), "isn't being debugged",
                        "is being debugged")
        }

        p <- modeling.procedure("lda")
        debug(p$fit.fun)
        expect_that(p$fit.fun, is_debugged)

        p <- listify(p)
        expect_that(p[[1]]$fit.fun, is_debugged)

        flags <- get.debug.flags(p)
        p[[1]]$vimp.fun <- "Long ago before the world was cool the moon flew out of the earth."
        expect_false(
            isdebugged(p[[1]]$fit.fun)
        )
        expect_that(set.debug.flags(p, flags)[[1]]$fit.fun, is_debugged)
    }
})
