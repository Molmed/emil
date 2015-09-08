context("Rcpp function")

test_that("Missing value detection", {
    expect_false(has_na(1:10))
    expect_false(has_na(1L:10L))
    expect_false(has_na(c(TRUE, FALSE)))
    expect_false(has_na(letters))
    expect_false(has_na(gl(2, 5)))
    expect_false(has_na(as.POSIXct(1:10, origin="2000-01-01")))
    expect_false(has_na(1:10 + 1i))
    expect_true(has_na(c(1:4, NA, 6:10)))
    expect_true(has_na(c(1L:7L, NA_integer_, 8L:10L)))
    expect_true(has_na(c(TRUE, FALSE, NA)))
    expect_true(has_na(c(NA_character_, letters)))
    expect_true(has_na(c(gl(2, 5), NA)))
    expect_true(has_na(as.POSIXct(c(1, NA, 3:10), origin="2000-01-01")))
    expect_true(has_na(c(1:3, NA, 5:10) + 1i))
})

test_that("Detection of constants", {
    f <- function(x, type, na.rm) is_constant(as(x, type), na.rm)
    for(p in 1:4){
        x <- as.matrix(do.call(expand.grid, rep(list(c(NA, 0, 1)), p)))
        is.constant <- c(NA, TRUE, FALSE)[sapply(apply(x, 1, table), length) + 1]
        is.constant.without.NA <- ifelse(is.constant,
            ifelse(apply(is.na(x), 1, any), NA, TRUE), FALSE)
        for(my_type in c("character", "complex", "expression", "factor", "integer", "logical", "numeric")){
            expect_identical(apply(x, 1, f, my_type, TRUE), is.constant)
            expect_identical(apply(x, 1, f, my_type, FALSE), is.constant.without.NA)
        }
    }
})

