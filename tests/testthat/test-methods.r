context("Methods")

test_that("pamr", {
  proc <- modeling_procedure(
    method = "pamr",
    parameter = list(cv = list(resample("crossvalidation", iris$Species, nfold=5, nrepeat=1)))
  )
  #reset_notification()
  #expect_message(
    fit <- fit(proc,
      x = iris[-5],
      y = iris$Species
    )
    #, "Use.*pre_pamr.*pre-processing"
  #)
  expect_that(fit, is_a("model"))
  #reset_notification()
})

test_that("glmnet", {
  #reset_notification()
  #expect_message(
    fit <- fit_glmnet(iris[-5], iris$Species)
    #, ".*data set.*matrix.*form.*")
  model <- fit_glmnet(as.matrix(iris[-5]), iris$Species)
  expect_that(fit, is_a("list"))
  expect_that(fit$glmnet.fit, is_a("glmnet"))
  #reset_notification()
})
