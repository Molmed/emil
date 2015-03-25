context("Helpers")

test_that("Test for blanks", {
    expect_true(is.blank(NA))
    expect_true(is.blank(NULL))
    expect_true(is.blank(""))
    expect_true(is.blank(c()))
    expect_false(is.blank(FALSE))
    expect_true(is.blank(FALSE, false.triggers=TRUE))
    expect_false(is.blank(1))
    expect_false(is.blank(function() 1))
})

test_that("Missing value imputation", {
    x <- c(1:3, NA)
    expect_that(fill(x, 3, 0), is_identical_to(c(1,2,0,NA)))
    expect_that(fill(x, is.na, 0), is_identical_to(na.fill(x, 0)))
})

test_that("dplyr integration", {
    x <- iris[-5]
    y <- iris$Species
    names(y) <- sprintf("orchid%03i", seq_along(y))
    cv <- resample("crossval", y, nfold=3, nrep=2)
    procedures <- list(nsc = modeling.procedure("nsc"),
                       rf = modeling.procedure("randomForest"))
    result <- evaluate.modeling(procedures, x, y, resample=cv)

    # Normal subsetting
    r <- result %>% dplyr::select(Fold = TRUE, Method = TRUE, Error = "error")
    expect_is(r, "data.frame")
    expect_identical(names(r), c("Fold", "Method", "Error"))

    r <- result %>% dplyr::select(Fold = TRUE, Method=c("lda", "nsc"), Error = "error")
    expect_identical(levels(r$Method), "lda") # But not nsc!

    r <- result %>% dplyr::select(Fold = TRUE, "lda", Error = "error")
    expect_identical(names(r), c("Fold", "Error"))
    
    # Resampling
    r <- result %>% dplyr::select(Fold = cv, "lda", "pred", Class="pred")
    expect_is(r, "data.frame")
    expect_is(r$id, "character")
    expect_equal(dim(r %>% spread(Fold, Class)), c(nrow(iris), 1+length(cv)))

    r <- result %>% dplyr::select(Fold = cv[1:3], "lda", "pred", Class="pred")
    expect_equal(levels(r$Fold), names(cv)[1:3])

    # Functions
    r1 <- result %>% dplyr::select(Fold = TRUE, Method = TRUE, Error = "error")
    r2 <- result %>% dplyr::select(Fold = TRUE, Method = TRUE, Accuracy = function(x) 1-x$error)
    expect_equal(r1$Error, 1-r2$Accuracy)

    r <- result %>% dplyr::select(Fold = TRUE, Method = TRUE,
        function(x) data.frame(Error=x$error, Accuarcy = 1-x$error))
    expect_true(all(r$Error + r$Accuracy == 1))
})

test_that("Subtree", {
    l <- list(A=list(a=1:3, b=4:6),
              B=list(a=7:9, b=0:1))
    expect_that(subtree(l, TRUE, "a"), is_a("matrix"))
    expect_that(subtree(l, TRUE, "b"), is_a("list"))
})

