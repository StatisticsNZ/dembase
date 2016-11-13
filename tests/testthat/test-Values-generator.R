

context("Values-generator")

test_that("Values creates valid object from valid arguments", {
    a <- array(1:12,
               dim = c(3, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
               sex = c("Male", "Female")))
    expect_is(Values(a), "Values")
    expect_identical(dimtypes(Values(a)),
                     c(age = "age", sex = "sex"))
    expect_identical(dimtypes(Values(a,
                                     dimtypes = c(age = "state"))),
                     c(age = "state", sex = "sex"))
    expect_identical(dimscales(Values(a,
                                      dimtypes = c(age = "state"),
                                      dimscales = c(age = "Categories"))),
                     c(age = "Categories", sex = "Sexes"))
    a <- array(0L,
               dim = c(3, 0),
               dimnames = list(age = c("0-4", "5-9", "10+"),
               sex = NULL))
    expect_is(Values(a), "Values")
    expect_identical(dim(Values(a)), c(3L, 0L))
    expect_identical(dimtypes(Values(a)),
                     c(age = "age", sex = "sex"))
    expect_identical(dimtypes(Values(a, dimtypes = c(age = "state"))),
                     c(age = "state", sex = "sex"))
    expect_identical(dimscales(Values(a, dimtypes = c(age = "state"))),
                     c(age = "Categories", sex = "Sexes"))
    a <- array(1:3,
               dim = 3,
               dimnames = list(age = c("0-4", "5-9", "10+")))
    expect_that(Values(a), is_a("Values"))
    a <- array(1:3,
               dim = 3,
               dimnames = list(c("0-4", "5-9", "10+")))
    expect_error(Values(a), "dimnames do not have names")
    a <-  array(0, dim = 3, dimnames = list(quantile = c(0.025, 0.5, 0.975)))
    expect_true(validObject(Values(a)))
    a <-  array(0, dim = c(1, 2, 1),
                dimnames = list(time = "2001-2005", triangle = c("UPPER", "LOWER"), age = "0-4"))
    expect_true(validObject(Values(a)))
    a <-  array(0, dim = 2, dimnames = list(age = c("0 - 4", "5 - 9")))
    expect_true(validObject(Values(a)))
    d <- data.frame(expand.grid(age = c("0-4", "5+"), sex = c("m", "f"), value = 1:4))
    a <- xtabs(value ~ age + sex, data = d)
    expect_true(validObject(a))
    v <- c("2001" = 80, "2011" = 100)
    expect_true(validObject(v))
    v <- numeric()
    expect_true(validObject(v))
    a <- array(1:4,
               dim = c(2, 2),
               dimnames = list(age = c("50+", "<50"), year = c(2005, 2001)))
    b <- array(4:1,
               dim = c(2, 2),
               dimnames = list(age = c("<50", "50+"), year = c(2001, 2005)))
    expect_identical(Values(a),
                     Values(b))
})


test_that("Values throws error when passed empty array", {
  a <- new("array")
  expect_error(Values(a),
              "'object' does not have dimnames")
})


test_that("Values requires an 'object' argument", {
  expect_error(Values(),
               "argument \"object\" is missing, with no default")
})


test_that("ValuesOne generates Values from valid arguments", {
    ## values and labels both have length 2
    ans.obtained <- ValuesOne(values = 1:2, labels = c("female", "male"), name = "sex")
    ans.expected <- array(1:2, dim = 2, dimnames = list(sex = c("female", "male")))
    ans.expected <- Values(ans.expected)
    expect_identical(ans.obtained, ans.expected)
    ## values and labels both have length 0
    ans.obtained <- ValuesOne(values = numeric(), labels = character(), name = "sex")
    ans.expected <- array(0, dim = 0, dimnames = list(sex = character()))
    ans.expected <- Values(ans.expected)
    expect_identical(ans.obtained, ans.expected)
    ## values has length 1, labels has length 10
    ans.obtained <- ValuesOne(values = 0, labels = 0:9, name = "age")
    ans.expected <- array(0, dim = 10, dimnames = list(age = 0:9))
    ans.expected <- Values(ans.expected)
    expect_identical(ans.obtained, ans.expected)
    ## supply dimtype
    ans.obtained <- ValuesOne(values = 0, labels = 0:9, name = "age", dimtype = "state")
    ans.expected <- array(0, dim = 10, dimnames = list(age = 0:9))
    ans.expected <- Values(ans.expected, dimtypes = c(age = "state"))
    expect_identical(ans.obtained, ans.expected)
    ## supply dimscale
    ans.obtained <- ValuesOne(values = 0, labels = 0:9, name = "age", dimscale = "Points")
    ans.expected <- array(0, dim = 10, dimnames = list(age = 0:9))
    ans.expected <- Values(ans.expected, dimscale = c(age = "Points"))
    expect_identical(ans.obtained, ans.expected)
})

test_that("ValuesOne throws appropriate errors", {
    expect_error(ValuesOne(values = 1:2, labels = c("a", "b", "c"), name = "region"),
                 "length of 'labels' not a multiple of length of 'values'")
    expect_error(ValuesOne(values = 1:4, labels = c("a", "b", "c"), name = "region"),
                 "length of 'values' greater than length of 'labels'")
    expect_error(ValuesOne(values = 1:3, labels = c("a", "b", "c"), name = c("region", "wrong")),
                 "'name' does not have length 1")
    expect_error(ValuesOne(values = 1:3, labels = c("a", "b", "c"), name = "region",
                           dimtype = c("state", "state")),
                 "'dimtype' does not have length 1")
    expect_error(ValuesOne(values = 1:3, labels = c("a", "b", "c"), name = "region",
                           dimscale = c("Categories", "Categories")),
                 "'dimscale' does not have length 1")
})
