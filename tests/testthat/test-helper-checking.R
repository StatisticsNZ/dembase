
context("helper-checking")
n.test <- 5
test.identity <- FALSE


test_that("checkAndTidyPercentage works", {
    checkAndTidyPercentage <- dembase:::checkAndTidyPercentage
    expect_identical(checkAndTidyPercentage(value = 50L,
                                            name = "percent"),
                     50)
    expect_identical(checkAndTidyPercentage(value = 0,
                                            name = "percent"),
                     0)
    expect_identical(checkAndTidyPercentage(value = 100,
                                            name = "percent"),
                     100)
    expect_identical(checkAndTidyPercentage(value = 93.382,
                                            name = "percent"),
                     93.382)
    expect_error(checkAndTidyPercentage(value = "wrong",
                                        name = "percent"),
                 "'percent' has class \"character\"")
    expect_error(checkAndTidyPercentage(value = c(90, 91),
                                        name = "percent"),
                 "'percent' does not have length 1")
    expect_error(checkAndTidyPercentage(value = NA_integer_,
                                        name = "percent"),
                 "'percent' is missing")
    expect_error(checkAndTidyPercentage(value = 100.0001,
                                        name = "percent"),
                 "'percent' is not between 0 and 100")
    expect_error(checkAndTidyPercentage(value = -32,
                                        name = "percent"),
                 "'percent' is not between 0 and 100")
})



test_that("checkSinglePercent works", {
    checkSinglePercent <- dembase:::checkSinglePercent
    expect_identical(checkSinglePercent(value = "50%",
                                        name = "e1"),
                     NULL)
    expect_identical(checkSinglePercent(value = "0.0001%",
                                        name = "e2"),
                     NULL)
    expect_identical(checkSinglePercent(value = "0%",
                                        name = "e1"),
                     NULL)
    expect_error(checkSinglePercent(value = c("0%", "1%"),
                                    name = "e1"),
                 "'e1' does not have length 1")
    expect_error(checkSinglePercent(value = NA,
                                    name = "e1"),
                 "'e1' is missing")
    expect_error(checkSinglePercent(value = "33",
                                    name = "e1"),
                 "'e1' \\[33\\] is not a valid percentage value")
    expect_error(checkSinglePercent(value = "a33%",
                                    name = "e1"),
                 "'e1' \\[a33%\\] is not a valid percentage value")
    expect_error(checkSinglePercent(value = "33%a",
                                    name = "e1"),
                 "'e1' \\[33%a\\] is not a valid percentage value")
})


