

context("numeric-methods")

test_that("extend works with numeric scalar", {
    makeTransform <- dembase:::makeTransform
    y <- Values(array(0,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                                      sex = c("m", "f"))))
    transform <- makeTransform(x = 1, y = y)
    ans.obtained <- extend(object = 0, transform = transform)
    ans.expected <- array(0, dim = c(3, 2))
    expect_identical(ans.obtained, ans.expected)
    y <- Values(array(0L,
                      dim = 3L,
                      dimnames = list(age = c("0-4", "5-9", "10+"))))
    transform <- makeTransform(x = 1, y = y)
    ans.obtained <- extend(object = 0L, transform = transform)
    ans.expected <- array(0L, dim = 3)
    expect_identical(ans.obtained, ans.expected)
    expect_error(extend(1:2, transform),
                 "'object' does not have length 1")    
})


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

test_that("round3 works", {
    for (seed in 1:10) {
        set.seed(seed)
        lambda <- runif(n = 1, min = 0.5, max = 10)
        x <- rpois(n = 100, lambda = lambda)
        x.round <- round3(x)
        expect_true(all(x.round %% 3 == 0))
        expect_true(all(x[x %% 3 == 0L] == x.round[x %% 3 == 0L]))
        x.with.na <- rpois(n = 100, lambda = lambda)
        x.with.na[sample(100, 10)] <- NA
        x.round.with.na <- round3(x.with.na)
        expect_true(all(is.na(x.round.with.na[is.na(x.with.na)])))
    }
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





