context("Survival analysis related")

test_that("outcome class", {
    require(survival)
    yo <- outcome(1:10 + .1, rep(c("event", NA), each=5))
    yS <- Surv(1:10 + .1, rep(1:0, each=5))
    expect_that(as.Surv(yo), is_identical_to(yS))
    expect_that(as.outcome(yS), is_identical_to(yo))

    expect_that(integer.events(yo, 0), is_equivalent_to(rep(0, 10)))
    expect_that(integer.events(yo, 4), is_equivalent_to(rep(1:0, c(3,7))))
    expect_that(integer.events(yo, 8), is_equivalent_to(rep(c(1,NA,0), c(5,2,3))))
})
