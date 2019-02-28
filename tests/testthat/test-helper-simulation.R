
context("helper-simulation")
n.test <- 5
test.identity <- FALSE


test_that("checkIntervalAndTruthArrayCompatible works", {
    checkIntervalAndTruthArrayCompatible <- dembase:::checkIntervalAndTruthArrayCompatible
    interval <- Counts(array(0:1,
                             dim = c(2, 2, 2),
                             dimnames = list(quantile = c("0.025", "0.975"),
                                             sex = c("F", "M"),
                                             region = 2:1)))
    truth <- Values(array(c(0.1, -0.1, 0.8, 1.1),
                          dim = c(2, 2),
                          dimnames = list(region = 1:2,
                                          sex = c("F", "M"))))
    expect_null(checkIntervalAndTruthArrayCompatible(interval = interval,
                                                      truth = truth))
    ## has same names, apart from quantile
    interval <- Counts(array(0:1,
                             dim = c(2, 2, 2),
                             dimnames = list(quantile = c("0.025", "0.975"),
                                             sex = c("F", "M"),
                                             wrong = 1:2)))
    truth <- Values(array(c(0.1, -0.1, 0.8, 1.1),
                          dim = c(2, 2),
                          dimnames = list(region = 1:2,
                                          sex = c("F", "M"))))
    expect_error(checkIntervalAndTruthArrayCompatible(interval = interval,
                                                      truth = truth),
                 "'interval' and 'truth' have incompatible dimensions : \"quantile\", \"sex\", \"wrong\" vs \"region\", \"sex\"")
    ## dimensions have same length
    interval <- Counts(array(0:1,
                             dim = c(2, 2, 2),
                             dimnames = list(quantile = c("0.025", "0.975"),
                                             sex = c("F", "M"),
                                             region = 1:2)))
    truth <- Values(array(c(0.1, -0.1, 0.8, 1.1),
                          dim = c(3, 2),
                          dimnames = list(region = 1:3,
                                          sex = c("F", "M"))))
    expect_error(checkIntervalAndTruthArrayCompatible(interval = interval,
                                                      truth = truth),
                 "\"region\" dimensions of 'interval' and 'truth' have different lengths")
    ## dimensions have same dimvalues
    interval <- Counts(array(0:1,
                             dim = c(2, 2, 2),
                             dimnames = list(quantile = c("0.025", "0.975"),
                                             sex = c("Female", "Male"),
                                             region = 1:2)))
    truth <- Values(array(c(0.1, -0.1, 0.8, 1.1),
                          dim = c(2, 2),
                          dimnames = list(region = 1:2,
                                          sex = c("F", "M")))) 
    expect_error(checkIntervalAndTruthArrayCompatible(interval = interval,
                                                      truth = truth),
                 "dimscales for \"sex\" dimension of 'interval' and 'truth' incompatible")
})

test_that("checkIntervalAndTruthNumericCompatible works", {
    checkIntervalAndTruthNumericCompatible <- dembase:::checkIntervalAndTruthNumericCompatible
    interval <- Values(array(c(0.1, -0.1),
                          dim = c(1, 2),
                          dimnames = list(region = 1,
                                          quantile = c(0.1, 0.9))))
    expect_null(checkIntervalAndTruthNumericCompatible(truth = 1,
                                                       interval = interval))
    interval <- Values(array(c(0.1, -0.1, 0.8, 1.1),
                          dim = c(2, 2),
                          dimnames = list(region = 1:2,
                                          quantile = c(0.1, 0.9))))
    expect_error(checkIntervalAndTruthNumericCompatible(truth = 1,
                                                        interval = interval),
                 "'truth' is a single number but 'interval' has dimensions \\(other than the \"quantile\" dimension\\) with length not equal to 1")
})

test_that("checkIntervalArray works", {
    checkIntervalArray <- dembase:::checkIntervalArray
    interval <- Counts(array(0:1,
                             dim = c(2, 2, 2),
                             dimnames = list(quantile = c("0.05", "0.95"),
                                             sex = c("F", "M"),
                                             region = 1:2)))
    expect_null(checkIntervalArray(interval))
    ## has quantile dimension
    interval <- Counts(array(0:1,
                             dim = c(2, 2),
                             dimnames = list(sex = c("F", "M"),
                                             region = 1:2)))
    expect_error(checkIntervalArray(interval),
                 "'interval' does not have dimension with dimtype \"quantile\"")
    ## quantile dimension has length 2
    interval <- Counts(array(0:1,
                             dim = c(3, 2, 2),
                             dimnames = list(quantile = c("0.025", "0.5", "0.975"),
                                             sex = c("F", "M"),
                                             region = 1:2)))
    expect_error(checkIntervalArray(interval = interval),
                 "dimension of 'interval' with dimtype \"quantile\" does not have length 2")
})

test_that("checkPointAndTruthCompatible works", {
    checkPointAndTruthCompatible <- dembase:::checkPointAndTruthCompatible
    point <- Values(array(c(0.1, -0.1, 0.8, 1.1),
                          dim = c(2, 2),
                          dimnames = list(region = 1:2,
                                          sex = c("F", "M"))))
    truth <- Values(array(1:4,
                          dim = c(2, 2),
                          dimnames = list(region = 2:1,
                                          sex = c("F", "M"))))
    expect_null(checkPointAndTruthCompatible(point = point,
                                             truth = truth))
    ## have same dimension names
    point <- Values(array(c(0.1, -0.1, 0.8, 1.1),
                          dim = c(2, 2),
                          dimnames = list(region = 1:2,
                                          sex = c("F", "M"))))
    truth <- Values(array(1:4,
                          dim = c(2, 2),
                          dimnames = list(region = 2:1,
                                          wrong = c("F", "M"))))
    expect_error(checkPointAndTruthCompatible(point = point,
                                              truth = truth),
                 "'point' and 'truth' have incompatible dimensions : \"region\", \"sex\" vs \"region\", \"wrong\"")
    ## dimensions have same length
    point <- Values(array(c(0.1, -0.1, 0.8, 1.1),
                          dim = c(2, 2),
                          dimnames = list(region = 1:2,
                                          sex = c("F", "M"))))
    truth <- Values(array(1:6,
                          dim = c(3, 2),
                          dimnames = list(region = 3:1,
                                          sex = c("F", "M"))))
    expect_error(checkPointAndTruthCompatible(point = point,
                                              truth = truth),
                 "\"region\" dimensions of 'point' and 'truth' have different lengths")
    ## dimensions have same dimvalues
    point <- Values(array(c(0.1, -0.1, 0.8, 1.1),
                          dim = c(2, 2),
                          dimnames = list(region = 1:2,
                                          sex = c("F", "M"))))
    truth <- Values(array(1:4,
                          dim = c(2, 2),
                          dimnames = list(region = 2:1,
                                          sex = c("Female", "Male"))))
    expect_error(checkPointAndTruthCompatible(point = point,
                                              truth = truth),
                 "dimscales for \"sex\" dimension of 'point' and 'truth' incompatible")
})

test_that("checkPointArray works", {
    checkPointArray <- dembase:::checkPointArray
    point <- Values(array(c(0.1, -0.1, 0.8, 1.1),
                          dim = c(2, 2),
                          dimnames = list(region = 1:2,
                                          sex = c("F", "M"))))
    expect_null(checkPointArray(point))
    ## no iteration or quantile dimensions
    point <- Values(array(c(0.1, -0.1, 0.8, 1.1),
                          dim = c(2, 2),
                          dimnames = list(iteration = 1:2,
                                          sex = c("F", "M"))))
    expect_error(checkPointArray(point),
                 "'point' has dimension with dimtype \"iteration\"")
    point <- Values(array(c(0.1, -0.1, 0.8, 1.1),
                          dim = c(2, 2),
                          dimnames = list(quantile = c("5%", "90%"),
                                          sex = c("F", "M"))))
    expect_error(checkPointArray(point),
                 "'point' has dimension with dimtype \"quantile\"")
})

test_that("checkTruthArray works", {
    checkTruthArray <- dembase:::checkTruthArray
    truth <- Values(array(c(0.1, -0.1, 0.8, 1.1),
                          dim = c(2, 2),
                          dimnames = list(region = 1:2,
                                          sex = c("F", "M"))))
    expect_null(checkTruthArray(truth))
    ## no iteration or quantile dimensions
    truth <- Values(array(c(0.1, -0.1, 0.8, 1.1),
                          dim = c(2, 2),
                          dimnames = list(iteration = 1:2,
                                          sex = c("F", "M"))))
    expect_error(checkTruthArray(truth),
                 "'truth' has dimension with dimtype \"iteration\"")
    truth <- Values(array(c(0.1, -0.1, 0.8, 1.1),
                          dim = c(2, 2),
                          dimnames = list(quantile = c("5%", "90%"),
                                          sex = c("F", "M"))))
    expect_error(checkTruthArray(truth),
                 "'truth' has dimension with dimtype \"quantile\"")
})

test_that("checkTruthNumeric works", {
    checkTruthNumeric <- dembase:::checkTruthNumeric
    expect_null(checkTruthNumeric(33))
    ## length 1
    expect_error(checkTruthNumeric(1:2),
                 "'truth' is a number but does not have length 1")
})

test_that("getAlphaInterval works", {
    getAlphaInterval <- dembase:::getAlphaInterval
    interval <- Counts(array(0:1,
                             dim = c(2, 2, 2),
                             dimnames = list(quantile = c("0.025", "0.975"),
                                             sex = c("F", "M"),
                                             region = 2:1)))
    expect_identical(getAlphaInterval(interval),
                     0.05)
    interval <- Counts(array(0:1,
                             dim = c(2, 2, 2),
                             dimnames = list(quantile = c("0.025", "0.95"),
                                             sex = c("F", "M"),
                                             region = 2:1)))
    expect_error(getAlphaInterval(interval),
                 "quantiles for 'interval' not symmetric")    
})

test_that("splitLowerUpper works", {
    splitLowerUpper <- dembase:::splitLowerUpper
    interval <- Counts(array(0:1,
                             dim = c(2, 2, 2),
                             dimnames = list(quantile = c("0.025", "0.975"),
                                             sex = c("F", "M"),
                                             region = 2:1)))
    ans.obtained <- splitLowerUpper(interval)
    ans.expected <- list(lower = slab(interval, 1, 1),
                         upper = slab(interval, 1, 2))
    expect_identical(ans.obtained, ans.expected)
})










