

context("Points-methods")

test_that("coercion from Points to Sexes works", {
  expect_error(as(new("Points", dimvalues = c(0.025, 0.5, 0.975)), "Sexes"),
               "labels not valid for dimscale")
  expect_identical(as(new("Points"), "Sexes"),
                   new("Sexes"))
})

test_that("coercion from Points to Triangles works", {
  expect_error(as(new("Points", dimvalues = c(0.025, 0.5, 0.975)), "Triangles"),
               "labels not valid for dimscale")
  expect_identical(as(new("Points"), "Triangles"),
                   new("Triangles"))
})

test_that("coercion from Points to Quantiles works", {
  expect_that(as(new("Points", dimvalues = c(0.025, 0.5, 0.975)), "Quantiles"),
              is_identical_to(new("Quantiles", dimvalues = c(0.025, 0.5, 0.975))))
  expect_that(as(new("Points"), "Quantiles"),
              is_identical_to(new("Quantiles")))
  expect_that(as(new("Points", dimvalues = c(0.025, 0.5, 2)), "Quantiles"),
              throws_error("labels not valid for dimscale"))
})


test_that("coercion from Points to Intervals works", {
  expect_that(as(new("Points", dimvalues = 0:2), "Intervals"),
              is_identical_to(new("Intervals", dimvalues = c(0, 1, 2, 3), isAge = TRUE)))
  expect_that(as(new("Points", dimvalues = c(0.5, 3, 7.5, 12.5)), "Intervals"),
              throws_error("labels not valid for dimscale"))
  expect_that(as(new("Points"), "Intervals"),
              is_identical_to(new("Intervals")))
  expect_that(as(new("Points", dimvalues = c(-1, 0, 1)), "Intervals"),
              is_identical_to(new("Intervals", dimvalues = c(-1, 0, 1, 2))))
})


test_that("coercion from Points to Iterations works", {
  expect_that(as(new("Points", dimvalues = c(1, 2, 3)), "Iterations"),
              is_identical_to(new("Iterations", dimvalues = 1:3)))
  expect_that(as(new("Points", dimvalues = c(0.5, 1.5, 2.5)), "Iterations"),
              throws_error("labels not valid for dimscale"))
  expect_that(as(new("Points"), "Iterations"),
              is_identical_to(new("Iterations")))
})

test_that("dbindDimScales method for Points works", {
    dbindDimScales <- dembase:::dbindDimScales
    e1 <- new("Points", dimvalues = c(0.1, 0.5))
    e2 <- new("Points", dimvalues = 0.9)
    ans.obtained <- dbindDimScales(e1 = e1, e2 = e2, along = "pts")
    ans.expected <- new("Points", dimvalues = c(0.1, 0.5, 0.9))
    expect_identical(ans.obtained, ans.expected)
    e1 <- new("Points", dimvalues = c(0.1, 0.5))
    e2 <- new("Points", dimvalues = 0.5)
    expect_error(dbindDimScales(e1 = e1, e2 = e2, along = "pts"),
                 "\"pts\" dimensions overlap")
})

test_that("length method for DimScale works when used on Points", {
  expect_that(length(new("Points", dimvalues = c(0, 1, 2))),
              is_identical_to(3L))
  expect_that(length(new("Points")),
              is_identical_to(0L))
})

test_that("dimscales method for DimScales works when used on Points", {
  expect_that(dimscales(new("Points", dimvalues = c(0, 1, 2))),
              is_identical_to("Points"))
})

test_that("dimvalues method for DimScales works when used on Points", {
  dimvalues <- dembase:::dimvalues
  expect_identical(dimvalues(new("Points", dimvalues = c(0, 1, 2))),
                   c(0, 1, 2))
  expect_identical(dimvalues(new("Points")),
                   numeric())
})

test_that("e1IsFirstDimScale works", {
    e1IsFirstDimScale <- dembase:::e1IsFirstDimScale
    e1 <- new("Points", dimvalues = c(0, 5, 10))
    e2 <- new("Points", dimvalues = c(15, 20))
    expect_true(e1IsFirstDimScale(e1 = e1, e2 = e2))
    e1 <- new("Points", dimvalues = numeric())
    e2 <- new("Points", dimvalues = numeric())
    expect_true(e1IsFirstDimScale(e1 = e1, e2 = e2))
    e1 <- new("Points", dimvalues = c(10, 15))
    e2 <- new("Points", dimvalues = c(0, 5))
    expect_false(e1IsFirstDimScale(e1 = e1, e2 = e2))
    e1 <- new("Points", dimvalues = c(10, 15))
    e2 <- new("Points", dimvalues = c(0, 5, 11))
    expect_true(e1IsFirstDimScale(e1 = e1, e2 = e2))
})

test_that("stepLengths method for Points works", {
  stepLengths <- dembase:::stepLengths
  expect_that(stepLengths(new("Points", dimvalues = c(1, 2, 4))),
              is_identical_to(c(1, 2)))
})

test_that("incrementDimScale method for Points works", {
    incrementDimScale <- dembase:::incrementDimScale
    x <- new("Points", dimvalues = 2001:2010)
    ans.obtained <- incrementDimScale(x, n = 10)
    ans.expected <- new("Points", dimvalues = 2011:2020)
    expect_identical(ans.obtained, ans.expected)
    x <- new("Points", dimvalues = 2001:2010)
    ans.obtained <- incrementDimScale(x, n = -5)
    ans.expected <- new("Points", dimvalues = 1996:2000)
    expect_identical(ans.obtained, ans.expected)
    x <- new("Points", dimvalues = c(0, 5))
    ans.obtained <- incrementDimScale(x, n = 1)
    ans.expected <- new("Points", dimvalues = 10)
    expect_identical(ans.obtained, ans.expected)
    x <- new("Points", dimvalues = numeric())
    expect_error(incrementDimScale(x, n = 5),
                 "\"along\" dimension has length 0")
    x <- new("Points", dimvalues = c(0, 5, 11))
    expect_error(incrementDimScale(x, n = 5),
                 "points on \"along\" dimension not regularly spaced")
})

test_that("inferDimvalues method for Points works", {
    inferDimvalues <- dembase:::inferDimvalues
    dateToFracYear <- dembase:::dateToFracYear
    expect_that(inferDimvalues(new("Points"), labels = c("1", "2")),
                is_identical_to(c(1, 2)))
    expect_that(inferDimvalues(new("Points"), labels = character()),
                is_identical_to(numeric()))
    expect_that(inferDimvalues(new("Points"), labels = NULL),
                is_identical_to(numeric()))
    expect_that(inferDimvalues(new("Points"), labels = c("2", "1")),
                is_identical_to(c(1, 2)))
    expect_that(inferDimvalues(new("Points"), labels = c("2", "1", "a")),
                is_identical_to(NULL))
    labels <- c("2000-01-01", "2000-01-02", "2000-02-03", "2000-01-30")
    expect_identical(inferDimvalues(new("Points"), labels = labels),
                     dateToFracYear(as.Date(labels)))
    expect_identical(inferDimvalues(new("Points"), labels = "2000-01-01"),
                     2000)
})

test_that("labels method for DimScales works when used on Points", {
    dateToFracYear <- dembase:::dateToFracYear
    expect_identical(labels(new("Points", dimvalues = c(1, 2))),
                     c("1", "2"))
    expect_identical(labels(new("Points", dimvalues = 2000)),
                     "2000")
    expect_identical(labels(new("Points")),
                     character())
    date <- as.Date(c("2000-01-01", "2000-01-02"))
    dv <- dateToFracYear(date)
    ans.obtained <- labels(new("Points", dimvalues = dv))
    ans.expected <- date
    expect_identical(ans.obtained, ans.expected)
    date <- as.Date(c("2000-01-01", "2000-02-01"))
    dv <- dateToFracYear(date)
    ans.obtained <- labels(new("Points", dimvalues = dv))
    ans.expected <- date
    expect_identical(ans.obtained, ans.expected)
    date <- as.Date(c("2000-01-01", "2000-04-01"))
    dv <- dateToFracYear(date)
    ans.obtained <- labels(new("Points", dimvalues = dv))
    ans.expected <- date
    expect_identical(ans.obtained, ans.expected)
})
