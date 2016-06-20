context("Result extractors")

test_that("Parameter tuning", {
    # Simpler uses are covered in the examples section
    procedure <- list(
        RF1 = modeling_procedure("randomForest",
            parameter = list(mtry = c(1, 3),
                             nodesize = c(1, 6))),
        RF2 = modeling_procedure("randomForest",
            parameter = list(maxnodes = c(3, 7)))
    )
    cv <- resample("crossvalidation", y = iris$Species, nfold = 3, nrep = 2)
    options(emil_max_indent = 2)
    model <- fit(procedure, iris[-5], iris$Species, resample=cv)
    result <- evaluate(procedure, iris[-5], iris$Species, resample=cv,
                       .save=c(model=TRUE))
    tuning <- get_tuning(result)
    expect_is(tuning, "data.frame")
    expect_true(all(colnames(tuning) %in% c("error", "fold", "method", "parameter_set", "parameter", "tuning_fold", "value")))
})

