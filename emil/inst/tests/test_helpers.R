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

test_that("Subtree", {
    l <- list(A=list(a=1:3, b=4:6),
              B=list(a=7:9, b=0:1))
    expect_that(subtree(l, TRUE, "a"), is_a("matrix"))
    expect_that(subtree(l, TRUE, "b"), is_a("list"))
})
