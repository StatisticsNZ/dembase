
context("Triangles-methods")


test_that("coercion from Triangles to Intervals works", {
  expect_that(as(new("Triangles"), "Intervals"),
              is_identical_to(new("Intervals")))
  expect_that(as(new("Triangles", dimvalues = c("Lower", "Upper")), "Intervals"),
              throws_error("labels not valid for dimscale"))
})


test_that("coercion from Triangles to Points works", {
  expect_that(as(new("Triangles"), "Points"),
              is_identical_to(new("Points")))
  expect_that(as(new("Triangles", dimvalues = c("Lower", "Upper")), "Points"),
              throws_error("labels not valid for dimscale"))
})


test_that("coercion from Triangles to Quantiles works", {
  expect_that(as(new("Triangles"), "Quantiles"),
              is_identical_to(new("Quantiles")))
  expect_that(as(new("Triangles", dimvalues = c("Lower", "Upper")), "Quantiles"),
              throws_error("labels not valid for dimscale"))
})


test_that("coercion from Triangles to Iterations works", {
  expect_that(as(new("Triangles"), "Iterations"),
              is_identical_to(new("Iterations")))
  expect_that(as(new("Triangles", dimvalues = c("TL", "TU")), "Iterations"),
              throws_error("labels not valid for dimscale"))
})


test_that("labels method for Triangles works", {
    labels <- dembase:::labels
    expect_identical(labels(new("Triangles", dimvalues = c("TL", "TU"))),
                     c("TL", "TU"))
    expect_identical(labels(new("Triangles", dimvalues = "Lower")),
                     "Lower")
    expect_identical(labels(new("Triangles")),
                     character())
})


test_that("inferDimvalues method for Triangles works", {
  inferDimvalues <- dembase:::inferDimvalues
  expect_that(inferDimvalues(new("Triangles"), labels = c("Lower", "Upper")),
              is_identical_to(c("Lower", "Upper")))
  expect_that(inferDimvalues(new("Triangles"), labels = c("TL", "TU")),
              is_identical_to(c("TL", "TU")))
  expect_that(inferDimvalues(new("Triangles"), labels = NULL),
              is_identical_to(character()))
  expect_that(inferDimvalues(new("Triangles"), labels = c("Lower", NA)),
              is_identical_to(NULL))
  expect_that(inferDimvalues(new("Triangles"), labels = c("Lower", "TL")),
              is_identical_to(NULL))
})


