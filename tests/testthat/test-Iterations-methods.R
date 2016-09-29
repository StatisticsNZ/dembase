
context("Iterations-methods")


test_that("coercion from Iterations to Points works", {
  expect_that(as(new("Iterations", dimvalues = 1:3), "Points"),
              is_identical_to(new("Points", dimvalues = c(1,2,3))))
  expect_that(as(new("Iterations"), "Points"),
              is_identical_to(new("Points")))
})

test_that("coercion from Iterations to Intervals works", {
  expect_that(as(new("Iterations", dimvalues = 1:3), "Intervals"),
              is_identical_to(new("Intervals", dimvalues = 1:4)))
  expect_that(as(new("Iterations"), "Intervals"),
              is_identical_to(new("Intervals")))
  expect_that(as(new("Iterations", dimvalues = c(1L, 3L)), "Intervals"),
              throws_error("labels not valid for dimscale"))
})

test_that("dbindDimScales works", {
    dbindDimScales <- dembase:::dbindDimScales
    expect_identical(dbindDimScales(e1 = new("Iterations", dimvalues = 1:3),
                                    e2 = new("Iterations", dimvalues = 1:2),
                                    along = "iteration"),
                     new("Iterations", dimvalues = 1:5))
    expect_identical(dbindDimScales(e1 = new("Iterations", dimvalues = 1:3),
                                    e2 = new("Iterations"),
                                    along = "iteration"),
                     new("Iterations", dimvalues = 1:3))
    expect_identical(dbindDimScales(e1 = new("Iterations"),
                                    e2 = new("Iterations"),
                                    along = "iteration"),
                     new("Iterations"))
})

test_that("labels method for Interations works", {
  expect_that(labels(new("Iterations", dimvalues = 1:2)),
              is_identical_to(c("1", "2")))
  expect_that(labels(new("Iterations")),
              is_identical_to(character()))
})

test_that("inferDimvalues method for Iterations works", {
    inferDimvalues <- dembase:::inferDimvalues
    expect_identical(inferDimvalues(new("Iterations"), labels = c("1", "2")),
                     1:2)
    expect_identical(inferDimvalues(new("Iterations"), labels = character()),
                     integer())
    expect_identical(inferDimvalues(new("Iterations"), labels = c("0-4", "5+")),
                     NULL)
    expect_identical(inferDimvalues(new("Iterations"), labels = NULL),
                     integer())
})



