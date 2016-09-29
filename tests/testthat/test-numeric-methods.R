

context("numeric-methods")

test_that("checkAndTidyWeights method for numeric works", {
    checkAndTidyWeights <- dembase:::checkAndTidyWeights
    target <- Values(array(1.0,
                           dim = c(2, 2),
                           dimnames = list(reg = c("a", "c"),
                               sex = c("f", "m"))))
    ans.obtained <- checkAndTidyWeights(weights = 1,
                                        target = target)
    ans.expected <- as(target, "Counts")
    expect_identical(ans.obtained, ans.expected)
    target <- Values(array(1.0,
                           dim = c(2, 0),
                           dimnames = list(reg = c("a", "c"),
                               sex = character())))
    ans.obtained <- checkAndTidyWeights(weights = 1L,
                                        target = target)
    ans.expected <- as(target, "Counts")
    expect_identical(ans.obtained, ans.expected)
    expect_error(checkAndTidyWeights(weights = c(1, 1),
                                     target = target),
                 "'weights' invalid")
    expect_error(checkAndTidyWeights(weights = 2,
                                     target = target,
                                     nameWeights = "exposure"),
                 "'exposure' invalid")
})

test_that("toDouble works with numeric", {
    expect_identical(toDouble(c(1, 2, 3, NA)),
                     as.double(c(1:3, NA)))
    expect_identical(toDouble(c(1.3, NA)),
                     as.double(c(1.3, NA)))
    expect_identical(toDouble(numeric()),
                     double())
})


test_that("toInteger works with integer", {
    expect_identical(toInteger(c(1:3, NA)),
                     c(1:3, NA))
    expect_identical(toInteger(integer()),
                     integer())
})

test_that("toInteger works with numeric", {
    expect_identical(toInteger(c(1:3, NA)),
                     c(1:3, NA))
    expect_identical(toInteger(double()),
                     integer())
    expect_identical(toInteger(c(1, NA)),
                     c(1L, NA))
    expect_identical(toInteger(1.9, force = TRUE),
                     2L)
    expect_error(toInteger(1.9),
                 "non-integer values")
    expect_error(toInteger(c(1, 1.9, NA)),
                 "non-integer values")
})





