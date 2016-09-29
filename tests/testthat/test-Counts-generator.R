

context("Counts-generator")

test_that("Counts creates valid object from valid arguments", {
    a <- array(1:12,
               dim = c(3, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
                   sex = c("Male", "Female")))
    expect_is(Counts(a), "Counts")
    expect_identical(dimtypes(Counts(a)),
                     c(age = "age", sex = "state"))
    expect_identical(dimtypes(Counts(a, dimtypes = c(age = "state"))),
                     c(age = "state", sex = "state"))
    expect_identical(dimscales(Counts(a,
                                      dimtypes = c(age = "state"),
                                      dimscales = c(age = "Categories"))),
                     c(age = "Categories", sex = "Categories"))
    a <- array(0L,
               dim = c(3, 0),
               dimnames = list(age = c("0-4", "5-9", "10+"),
                   sex = NULL))
    expect_is(Counts(a), "Counts")
    expect_identical(dim(Counts(a)), c(3L, 0L))
    expect_identical(dimtypes(Counts(a)),
                     c(age = "age", sex = "state"))
    expect_identical(dimtypes(Counts(a, dimtypes = c(age = "state"))),
                     c(age = "state", sex = "state"))
    expect_identical(dimscales(Counts(a, dimtypes = c(age = "state"))),
                     c(age = "Categories", sex = "Categories"))
    a <- array(1:3,
               dim = 3,
               dimnames = list(age = c("0-4", "5-9", "10+")))
    expect_is(Counts(a), "Counts")
    a <- array(1:3,
               dim = 3,
               dimnames = list(c("0-4", "5-9", "10+")))
    expect_error(Counts(a), "dimnames do not have names")
    d <- data.frame(expand.grid(region = c("a", "b"), year = 2000:2002), count = 1:6)
    a <- xtabs(count ~ year + region, data = d)
    expect_true(validObject(Counts(a, dimscales = c(year = "Points"))))
    a <- c(a = 1, b = 2)
    expect_true(validObject(Counts(a)))
    a <- array(2:1,
               dim = c(2, 2),
               dimnames = list(reg = c("a", "b"), pool = c("ins", "outs")))
    expect_identical(dimtypes(Counts(a)), c(reg = "state", pool = "state"))
    a <- array(1:4,
               dim = c(2, 2),
               dimnames = list(age = c("10+", "0-9"), time = c(2005, 2001)))
    b <- array(4:1,
               dim = c(2, 2),
               dimnames = list(age = c("0-9", "10+"), time = c(2001, 2005)))
    expect_identical(Counts(a),
                     Counts(b))
})


test_that("Counts throws error when passed empty array", {
  a <- new("array")
  expect_error(Counts(a),
              "'object' does not have dimnames")
})


test_that("Counts requires an 'object' argument", {
  expect_error(Counts(),
               "argument \"object\" is missing, with no default")
})

test_that("Counts throws error when a non-zero-length dimension has NULL dimnames", {
    a <- array(0,
               dim = c(3, 2),
               dimnames = list(age = 0:2,
               sex = NULL))
    expect_error(x <- Counts(a),
                 "dimension with no dimnames")
    a <- array(0,
               dim = c(3, 0),
               dimnames = list(age = 0:2,
               sex = NULL))
    x <- Counts(a)
    expect_true(validObject(x))
})

test_that("CountsOne generates Counts from valid arguments", {
    ## values and labels both have length 2
    ans.obtained <- CountsOne(values = 1:2, labels = c("female", "male"), name = "sex")
    ans.expected <- array(1:2, dim = 2, dimnames = list(sex = c("female", "male")))
    ans.expected <- Counts(ans.expected)
    expect_identical(ans.obtained, ans.expected)
    ## values and labels both have length 0
    ans.obtained <- CountsOne(values = numeric(), labels = character(), name = "sex")
    ans.expected <- array(0, dim = 0, dimnames = list(sex = character()))
    ans.expected <- Counts(ans.expected)
    expect_identical(ans.obtained, ans.expected)
    ## values has length 1, labels has length 10
    ans.obtained <- CountsOne(values = 0, labels = 0:9, name = "age")
    ans.expected <- array(0, dim = 10, dimnames = list(age = 0:9))
    ans.expected <- Counts(ans.expected)
    expect_identical(ans.obtained, ans.expected)
    ## supply dimtype
    ans.obtained <- CountsOne(values = 0, labels = 0:9, name = "age", dimtype = "state")
    ans.expected <- array(0, dim = 10, dimnames = list(age = 0:9))
    ans.expected <- Counts(ans.expected, dimtypes = c(age = "state"))
    expect_identical(ans.obtained, ans.expected)
    ## supply dimscale
    ans.obtained <- CountsOne(values = 0, labels = 0:9, name = "age", dimscale = "Points")
    ans.expected <- array(0, dim = 10, dimnames = list(age = 0:9))
    ans.expected <- Counts(ans.expected, dimscale = c(age = "Points"))
    expect_identical(ans.obtained, ans.expected)
})

test_that("CountsOne throws appropriate errors", {
    expect_error(CountsOne(values = 1:2, labels = c("a", "b", "c"), name = "region"),
                 "length of 'labels' not a multiple of length of 'values'")
    expect_error(CountsOne(values = 1:4, labels = c("a", "b", "c"), name = "region"),
                 "length of 'values' greater than length of 'labels'")
    expect_error(CountsOne(values = 1:3, labels = c("a", "b", "c"), name = c("region", "wrong")),
                 "'name' does not have length 1")
    expect_error(CountsOne(values = 1:3, labels = c("a", "b", "c"), name = "region",
                           dimtype = c("state", "state")),
                 "'dimtype' does not have length 1")
    expect_error(CountsOne(values = 1:3, labels = c("a", "b", "c"), name = "region",
                           dimscale = c("Categories", "Categories")),
                 "'dimscale' does not have length 1")
})

