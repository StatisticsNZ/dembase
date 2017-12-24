

context("array-methods")

test_that("collapse works when the transform is identity", {
  collapse <- dembase:::collapse
  a <- array(1L, dim = 5)
  transform <- new("CollapseTransform",
                   dims = 1L,
                   indices = list(rep(1L, 5)),
                   dimBefore = 5L,
                   dimAfter = 1L)
  expect_identical(collapse(object = a, transform = transform),
                   array(5L, dim = 1))
  storage.mode(a) <- "double"
  expect_identical(collapse(object = a, transform = transform),
                   array(5, dim = 1))
})

test_that("collapse works for fancy transformations", {
    collapse <- dembase:::collapse
    a <- array(1:5, dim = 5)
    transform <- new("CollapseTransform",
                     dims = 1L,
                     indices = list(c(0L, 1L, 3L, 1L, 2L)),
                     dimBefore = 5L,
                     dimAfter = 3L)
    expect_identical(collapse(a, transform),
                     array(c(6L, 5L, 3L), dim = 3))
    storage.mode(a) <- "double"
    expect_identical(collapse(a, transform),
                     array(c(6, 5, 3), dim = 3))
    a <- array(1:6, dim = c(3, 2))
    transform <- new("CollapseTransform",
                     dims = 1:2,
                     indices = list(c(2L, 0L, 1L), c(1L, 1L)),
                     dimBefore = c(3L, 2L),
                     dimAfter = 2:1)
    expect_identical(collapse(a, transform),
                     array(c(9L, 5L), dim = 2:1))
    storage.mode(a) <- "double"
    expect_identical(collapse(a, transform),
                     array(c(9, 5), dim = 2:1))
    a <- array(1:6, dim = c(3, 2))
    transform <- new("CollapseTransform",
                     dims = c(1L, 0L),
                     indices = list(c(2L, 1L, 2L), c(1L, 1L)),
                     dimBefore = c(3L, 2L),
                     dimAfter = 2L)
    expect_identical(collapse(a, transform),
                     array(c(7L, 14L), dim = 2L))
    storage.mode(a) <- "double"
    expect_identical(collapse(a, transform),
                     array(c(7, 14), dim = 2L))
})

test_that("collapse works on arrays with 0-length dimensions", {
    collapse <- dembase:::collapse
    a <- new("array")
    transform <- new("CollapseTransform")
    expect_identical(collapse(a, transform),
                     a)
    storage.mode(a) <- "integer"
    expect_identical(collapse(a, transform),
                     a)
    a <- array(0L, dim = c(2, 0))
    transform <- new("CollapseTransform",
                     dims = 1:2,
                     indices = list(1:2, integer()),
                     dimBefore = c(2L, 0L),
                     dimAfter = c(2L, 0L))
    expect_identical(collapse(a, transform),
                     a)
    storage.mode(a) <- "double"
    expect_identical(collapse(a, transform),
                     a)
    a <- array(0, dim = c(2, 0, 3))
    b <- array(0, dim = c(0, 2, 1))
    transform <- new("CollapseTransform",
                     dims = c(3L, 1L, 2L),
                     indices = list(0:1, integer(), 2:0),
                     dimBefore = c(2L, 0L, 3L),
                     dimAfter = c(0L, 2L, 1L))
    expect_identical(collapse(a, transform),
                     b)
    storage.mode(a) <- "integer"
    storage.mode(b) <- "integer"
    expect_identical(collapse(a, transform),
                     b)
})

test_that("collapse works when array contains NAs", {
  collapse <- dembase:::collapse
  a <- array(c(1:4, NA), dim = 5)
  transform <- new("CollapseTransform",
                   dims = 1L,
                   indices = list(c(0L, 1L, 3L, 1L, 2L)),
                   dimBefore = 5L,
                   dimAfter = 3L)
  expect_that(collapse(a, transform),
              is_identical_to(array(c(6L, NA, 3L), dim = 3)))
  a <- array(as.double(c(1:4, NA)), dim = 5)
  transform <- new("CollapseTransform",
                   dims = 1L,
                   indices = list(c(0L, 1L, 3L, 1L, 2L)),
                   dimBefore = 5L,
                   dimAfter = 3L)
  expect_that(collapse(a, transform),
              is_identical_to(array(c(6, NA, 3), dim = 3)))
  a <- array(c(1:2, NA, 4:6), dim = c(3, 2))
  transform <- new("CollapseTransform",
                   dims = 1:2,
                   indices = list(c(2L, 0L, 1L), c(1L, 1L)),
                   dimBefore = 3:2,
                   dimAfter = 2:1)
  expect_that(collapse(a, transform),
              is_identical_to(array(c(NA, 5L), dim = 2:1)))
  a <- array(c(NA, 2:6), dim = c(3, 2))
  transform <- new("CollapseTransform",
                   dims = c(1L, 0L),
                   indices = list(c(2L, 1L, 2L), c(1L, 1L)),
                   dimBefore = 3:2,
                   dimAfter = 2L)
  expect_that(collapse(a, transform),
              is_identical_to(array(c(7L, NA), dim = 2L)))
})

test_that("extend method for arrays works when repeating rows/columns", {
    extend <- dembase:::extend
    a0 <- array(1:4, dim = c(2, 2))
    a1 <- array(c(1L, 2L, 2L, 1L, 2L, 2L, 3L, 4L, 4L), dim = c(3, 3))
    transform <- new("ExtendTransform",
                     dims = 1:2,
                     indices = list(c(1L, 2L, 2L), c(1L, 1L, 2L)),
                     dimBefore = c(2L, 2L),
                     dimAfter = c(3L, 3L))
    expect_identical(extend(a0, transform), a1)
    storage.mode(a0) <- "double"
    storage.mode(a1) <- "double"
    expect_identical(extend(a0, transform), a1)
})

test_that("extend method for arrays works when repeating/dropping rows/columns and transposing", {
    extend <- dembase:::extend
    a0 <- array(1:4, dim = c(2, 2))
    a1 <- array(c(3L, 3L, 4L, 4L, 4L, 4L), dim = c(2, 3))
    transform <- new("ExtendTransform",
                     dims = 2:1,
                     indices = list(c(2L, 2L), c(1L, 2L, 2L)),
                     dimBefore = c(2L, 2L),
                     dimAfter = c(2L, 3L))
    expect_identical(extend(a0, transform), a1)
    storage.mode(a0) <- "double"
    storage.mode(a1) <- "double"
    expect_identical(extend(a0, transform), a1)
})

test_that("extend method for arrays works when adding new dimension", {
    extend <- dembase:::extend
    a0 <- array(1:4, dim = c(2, 2))
    a1 <- array(c(1L, 2L, 1L, 2L, 1L, 2L, 3L, 4L, 3L, 4L, 3L, 4L), dim = c(2, 3, 2))
    transform <- new("ExtendTransform",
                     dims = c(1L, 0L, 2L),
                     indices = list(c(1L, 2L), c(1L, 1L, 1L), c(1L, 2L)),
                     dimBefore = c(2L, 2L),
                     dimAfter = c(2L, 3L, 2L))
    expect_identical(extend(a0, transform), a1)
    storage.mode(a0) <- "double"
    storage.mode(a1) <- "double"
    expect_identical(extend(a0, transform), a1)
})

test_that("extend works on arrays with 0-length dimensions", {
    extend <- dembase:::extend
    a <- new("array")
    transform <- new("ExtendTransform")
    expect_identical(extend(a, transform),
                     a)
    storage.mode(a) <- "integer"
    expect_identical(extend(a, transform),
                     a)
    a <- array(0L, dim = c(2, 0))
    b <- array(0L, dim = c(2, 2, 0))
    transform <- new("ExtendTransform",
                     dims = c(0L, 1:2),
                     indices = list(rep(1L, 2), 2:1, integer()),
                     dimBefore = c(2L, 0L),
                     dimAfter = c(2L, 2L, 0L))
    expect_identical(extend(a, transform),
                     b)
    storage.mode(a) <- "double"
    storage.mode(b) <- "double"
    expect_identical(extend(a, transform),
                     b)
    a <- array(0, dim = c(2, 0, 3))
    b <- array(0, dim = c(0, 2, 1))
    transform <- new("ExtendTransform",
                     dims = c(2L, 1L, 3L),
                     indices = list(integer(), 1:2, 3L),
                     dimBefore = c(2L, 0L, 3L),
                     dimAfter = c(0L, 2L, 1L))
    expect_identical(extend(a, transform),
                     b)
    storage.mode(a) <- "integer"
    storage.mode(b) <- "integer"
    expect_identical(extend(a, transform),
                     b)
    a <- array(1:3, dim = 3) ## length(a) > 0
    b <- array(0L, dim = c(3, 0))
    transform <- new("ExtendTransform",
                     dims = c(1L, 0L),
                     indices = list(1:3, integer()),
                     dimBefore = 3L,
                     dimAfter = c(3L, 0L))
    expect_identical(extend(a, transform = transform), b)
})

test_that("extend works when array contains NAs", {
    extend <- dembase:::extend
    a <- array(c(1L, NA), dim = 2)
    transform <- new("ExtendTransform",
                     dims = 1L,
                     indices = list(c(1L, 1L, 2L, 2L)),
                     dimBefore = 2L,
                     dimAfter = 4L)
    expect_identical(extend(a, transform),
                     array(c(1L, 1L, NA, NA), dim = 4L))
    storage.mode(a) <- "double"
    expect_identical(extend(a, transform),
                     array(c(1, 1, NA, NA), dim = 4L))
    a <- array(c(1L, NA), dim = 2)
    transform <- new("ExtendTransform",
                     dims = c(1L, 0L),
                     indices = list(c(1L, 2L), c(1L, 1L)),
                     dimBefore = 2L,
                     dimAfter = c(2L, 2L))
    expect_identical(extend(a, transform),
                     array(c(1L, NA, 1L, NA), dim = c(2, 2)))
    storage.mode(a) <- "double"
    expect_identical(extend(a, transform),
                     array(c(1, NA, 1, NA), dim = c(2, 2)))
})
           
test_that("round3 works", {
    for (seed in 1:10) {
        set.seed(seed)
        lambda <- runif(n = 1, min = 0.5, max = 10)
        x <- array(rpois(n = 100, lambda = lambda),
                   dim = 100,
                   dimnames = list(reg = 1:100))
        x.round <- round3(x)
        expect_true(all(x.round %% 3 == 0))
        expect_true(all(x[x %% 3 == 0L] == x.round[x %% 3 == 0L]))
        x.with.na <- array(rpois(n = 100, lambda = lambda),
                           dim = 100,
                           dimnames = list(reg = 1:100))
        x.with.na[sample(100, 10)] <- NA
        x.round.with.na <- round3(x.with.na)
        expect_true(all(is.na(x.round.with.na[is.na(x.with.na)])))
    }
})
