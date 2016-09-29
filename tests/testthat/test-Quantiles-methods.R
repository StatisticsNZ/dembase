
context("Quantiles-methods")

test_that("coercion from Quantiles to Triangles works", {
  expect_that(as(new("Quantiles", dimvalues = c(0.025, 0.5, 0.975)), "Triangles"),
              throws_error("labels not valid for dimscale"))
  expect_that(as(new("Quantiles"), "Triangles"),
              is_identical_to(new("Triangles")))
})

test_that("coercion from Quantiles to Points works", {
  expect_that(as(new("Quantiles", dimvalues = c(0.025, 0.5, 0.975)), "Points"),
              is_identical_to(new("Points", dimvalues = c(0.025, 0.5, 0.975))))
  expect_that(as(new("Quantiles"), "Points"),
              is_identical_to(new("Points")))
})

test_that("coercion from Quantiles to Intervals works", {
  expect_that(as(new("Quantiles"), "Intervals"),
              is_identical_to(new("Intervals")))
  expect_that(as(new("Quantiles", dimvalues = c(0.025, 0.5, 0.975)), "Intervals"),
              throws_error("labels not valid for dimscale"))
})

test_that("coercion from Quantiles to Iterations works", {
  expect_that(as(new("Quantiles"), "Iterations"),
              is_identical_to(new("Iterations")))
  expect_that(as(new("Quantiles", dimvalues = c(0.025, 0.5, 0.975)), "Iterations"),
              throws_error("labels not valid for dimscale"))
})

test_that("dbindDimScales method for Quantiles works", {
    dbindDimScales <- dembase:::dbindDimScales
    e1 <- new("Quantiles", dimvalues = c(0.1, 0.5))
    e2 <- new("Quantiles", dimvalues = 0.9)
    ans.obtained <- dbindDimScales(e1 = e1, e2 = e2, along = "quant")
    ans.expected <- new("Quantiles", dimvalues = c(0.1, 0.5, 0.9))
    expect_identical(ans.obtained, ans.expected)
    e1 <- new("Quantiles", dimvalues = c(0.1, 0.5))
    e2 <- new("Quantiles", dimvalues = 0.5)
    expect_error(dbindDimScales(e1 = e1, e2 = e2, along = "quant"),
                 "\"quant\" dimensions overlap")
})

test_that("labels method for Quantiles works", {
    labels <- dembase:::labels
    expect_identical(labels(new("Quantiles", dimvalues = c(0.025, 0.5, 0.975))),
                     c("2.5%", "50%", "97.5%"))
    expect_identical(labels(new("Quantiles", dimvalues = c(0, 1))),
                     c("0%", "100%"))
    expect_identical(labels(new("Quantiles")),
                     character())
})

test_that("inferDimvalues method for Quantiles works", {
  inferDimvalues <- dembase:::inferDimvalues
  expect_that(inferDimvalues(new("Quantiles"), labels = c("2.5%", "50%", "97.5%")),
              is_identical_to(c(0.025, 0.5, 0.975)))
  expect_that(inferDimvalues(new("Quantiles"), labels = c("0.025", "0.5", "0.975")),
              is_identical_to(c(0.025, 0.5, 0.975)))
  expect_that(inferDimvalues(new("Quantiles"), labels = NULL),
              is_identical_to(numeric()))
  expect_that(inferDimvalues(new("Quantiles"), labels = c("0.025", "0.5", "0.975", NA)),
              is_identical_to(NULL))
  expect_that(inferDimvalues(new("Quantiles"), labels = c("0.5", "0.975", "0.025")),
              is_identical_to(NULL))
})


