context("Colors")

test_that("Color generatoin", {
    expect_that(get.colors(1), is_a("character"))
    expect_that(get.colors("red"), is_a("character"))
    expect_that(get.colors(gl(3,3)), is_a("character"))
    expect_that(get.colors("#8888cc", s=1), is_equivalent_to("#0000CC"))
    expect_that(get.colors("#8888cc", s=-1), is_equivalent_to("#CCCCCC"))
    expect_that(get.colors("#8888cc", v=1), is_equivalent_to("#AAAAFF"))
    expect_that(get.colors("#8888cc", v=-1), is_equivalent_to("#000000"))
    expect_that(get.colors("#8888cc", alpha=.5), is_equivalent_to("#8888CC80"))
})

