

context("Intervals-methods")


test_that("coercion from Intervals to Sexes works", {
    expect_error(as(new("Intervals", dimvalues = c(0, 1, 2)), "Sexes"),
                 "labels not valid for dimscale")
    expect_identical(as(new("Intervals"), "Sexes"),
                     new("Sexes"))
})

test_that("coercion from Intervals to Triangles works", {
  expect_that(as(new("Intervals", dimvalues = c(0, 1, 2)), "Triangles"),
              throws_error("labels not valid for dimscale"))
  expect_that(as(new("Intervals"), "Triangles"),
              is_identical_to(new("Triangles")))
})


test_that("coercion from Intervals to Points works", {
  expect_that(as(new("Intervals", dimvalues = c(0, 1, 2)), "Points"),
              is_identical_to(new("Points", dimvalues = c(0, 1))))
  expect_that(as(new("Intervals", dimvalues = c(0, 1, Inf)), "Points"),
              throws_error("labels not valid for dimscale"))
  expect_that(as(new("Intervals", dimvalues = c(-Inf, 0, 1)), "Points"),
              throws_error("labels not valid for dimscale"))
  expect_that(as(new("Intervals", dimvalues = c(0, 5)), "Points"),
              throws_error("labels not valid for dimscale"))
  expect_that(as(new("Intervals", dimvalues = c(0, Inf)), "Points"),
              throws_error("labels not valid for dimscale"))
  expect_that(as(new("Intervals"), "Points"),
              is_identical_to(new("Points")))
})


test_that("coercion from Intervals to Quantiles works", {
  expect_that(as(new("Intervals", dimvalues = c(0, 1, 2)), "Quantiles"),
              throws_error("labels not valid for dimscale"))
  expect_that(as(new("Intervals"), "Quantiles"),
              is_identical_to(new("Quantiles")))
})


test_that("coercion from Intervals to Iterations works", {
  expect_that(as(new("Intervals", dimvalues = c(1, 2)), "Iterations"),
              is_identical_to(new("Iterations", dimvalues = 1L)))
  expect_that(as(new("Intervals", dimvalues = c(0, 1, Inf)), "Iterations"),
              throws_error("labels not valid for dimscale"))
  expect_that(as(new("Intervals"), "Iterations"),
              is_identical_to(new("Iterations")))
})

test_that("Extract works", {
    expect_identical(new("Intervals", dimvalues = c(0, 5, 10, Inf))[1:2],
                     new("Intervals", dimvalues = c(0, 5, 10)))
    expect_identical(new("Intervals", dimvalues = c(0, 5, 10, Inf))[2:3],
                     new("Intervals", dimvalues = c(5, 10, Inf)))
    expect_identical(new("Intervals", dimvalues = c(0, 5, 10, Inf))[c(1L, 3L, 0L)],
                     new("Categories", dimvalues = c("0-4", "10+")))
    expect_identical(new("Intervals", dimvalues = c(0, 5, 10, Inf))[c(0L, -2L)],
                     new("Categories", dimvalues = c("0-4", "10+")))
    expect_error(new("Intervals", dimvalues = 0:5)[c(0L, NA)],
                 "'i' has missing values")
    expect_error(new("Intervals", dimvalues = 0:5)[c(0L, 100L)],
                 "'i' has values outside the valid range")
})

test_that("%in% works", {
    assign("%in%", getFromNamespace("%in%", "dembase"))
    x <- new("Intervals", dimvalues = c(0, 1, 5, Inf))
    expect_identical(x %in% c("0", "5+"),
                     new("SubArrayIndices",
                         nms = "x",
                         indices = list(c(TRUE, FALSE, TRUE))))
    expect_identical(x %in% "1-4",
                     new("SubArrayIndices",
                         nms = "x",
                         indices = list(c(FALSE, TRUE, FALSE))))
})

test_that("Compare works", {
    x <- new("Intervals", dimvalues = c(0, 1, 5, 10))
    y <- new("SubArrayIndices", nms = "x", indices = list(c(FALSE, TRUE, TRUE)))
    expect_identical(x > 1, y)
    expect_identical(x >= 1, y)
    expect_identical(x > 0.01, y)
    expect_identical(x >= 0.01, y)
    expect_identical(x != "0", y)
    expect_identical(0.9 < x, y)
    expect_identical(0.001 <= x, y)
    expect_identical("0" != x, y)
    x <- new("Intervals", dimvalues = c(0, 1, 5, Inf))
    y <- new("SubArrayIndices", nms = "x", indices = list(c(TRUE, TRUE, FALSE)))
    z <- new("SubArrayIndices", nms = "x", indices = list(c(TRUE, TRUE, TRUE)))
    expect_identical(x < 10, y)
    expect_identical(x <= 10, y)
    expect_identical(x < 5, y)
    expect_identical(x <= Inf, z)
    expect_identical(x < Inf, z)
    expect_identical(x != "wrong", z)
    expect_identical(5.01 > x, y)
    expect_identical(-1 < x, z)
    expect_identical("wrong" != x, z)
    expect_error(x == 1, "invalid use of '==' operator")
    expect_error(x > "a", "invalid use of '>' operator")
    expect_error(Inf == x, "invalid use of '==' operator")
    expect_error(x < "a", "invalid use of '<' operator")
    x <- new("Intervals", dimvalues = c(2000, 2005, 2010))
    y <- new("SubArrayIndices", nms = "x", indices = list(c(TRUE, FALSE)))
    expect_identical(x == "2001-2005", y)
    expect_identical(x != "2006-2010", y)
    expect_error(x < "2006-2010", "invalid use of '<' operator")
    expect_identical("2001-2005" == x, y)
    expect_identical("2006-2010" != x, y)
    expect_error("2006-2010" < x, "invalid use of '<' operator")
    expect_error(x > x, "attempt to compare two dimscales")
    expect_error(x == x, "attempt to compare two dimscales")
})

test_that("canMakeDimScalePairCompatible works", {
    canMakeDimScalePairCompatible <- dembase:::canMakeDimScalePairCompatible
    e1 <- new("Intervals", dimvalues = c(0, 1, 5, Inf))
    e2 <- new("Intervals", dimvalues = c(0, 5, Inf))
    expect_true(canMakeDimScalePairCompatible(e1 = e1, e2 = e2, isCounts1 = TRUE, isCounts2 = TRUE))
    expect_error(canMakeDimScalePairCompatible(e1 = e1, e2 = e2, isCounts1 = FALSE, isCounts2 = TRUE),
                 "intervals do not align")
    expect_true(canMakeDimScalePairCompatible(e1 = e1, e2 = e2, isCounts1 = TRUE, isCounts2 = FALSE))
    expect_true(canMakeDimScalePairCompatible(e1 = e1, e2 = e2, isCounts1 = FALSE, isCounts2 = FALSE))
    e1 <- new("Intervals", dimvalues = c(0, 1, 5, 10))
    e2 <- new("Intervals", dimvalues = c(1, 5, 8, 10, Inf))
    expect_true(canMakeDimScalePairCompatible(e1 = e1, e2 = e2, isCounts1 = TRUE, isCounts2 = TRUE))
    expect_true(canMakeDimScalePairCompatible(e1 = e1, e2 = e2, isCounts1 = FALSE, isCounts2 = TRUE))
    expect_error(canMakeDimScalePairCompatible(e1 = e1, e2 = e2, isCounts1 = TRUE, isCounts2 = FALSE),
                 "intervals do not align")
    expect_true(canMakeDimScalePairCompatible(e1 = e1, e2 = e2, isCounts1 = FALSE, isCounts2 = FALSE))
    e1 <- new("Intervals")
    e2 <- new("Intervals", dimvalues = c(1, 5, 8, 10, Inf))
    expect_true(canMakeDimScalePairCompatible(e1 = e1, e2 = e2, isCounts1 = TRUE, isCounts2 = TRUE))
    expect_true(canMakeDimScalePairCompatible(e1 = e1, e2 = e2, isCounts1 = FALSE, isCounts2 = TRUE))
    expect_true(canMakeDimScalePairCompatible(e1 = e1, e2 = e2, isCounts1 = TRUE, isCounts2 = FALSE))
    expect_true(canMakeDimScalePairCompatible(e1 = e1, e2 = e2, isCounts1 = FALSE, isCounts2 = FALSE))
    e1 <- new("Intervals", dimvalues = c(0, 1))
    e2 <- new("Intervals", dimvalues = c(5, 8, 10, Inf))
    expect_error(canMakeDimScalePairCompatible(e1 = e1, e2 = e2, isCounts1 = TRUE, isCounts2 = TRUE),
                 "intervals do not align")
    expect_error(canMakeDimScalePairCompatible(e1 = e1, e2 = e2, isCounts1 = FALSE, isCounts2 = TRUE),
                 "intervals do not align")
    expect_error(canMakeDimScalePairCompatible(e1 = e1, e2 = e2, isCounts1 = TRUE, isCounts2 = FALSE),
                 "intervals do not align")
    expect_error(canMakeDimScalePairCompatible(e1 = e1, e2 = e2, isCounts1 = FALSE, isCounts2 = FALSE),
                 "intervals do not align")
    e1 <- new("Intervals", dimvalues = c(0, 3))
    e2 <- new("Intervals", dimvalues = c(1, 8, 10, Inf))
    expect_error(canMakeDimScalePairCompatible(e1 = e1, e2 = e2, isCounts1 = TRUE, isCounts2 = TRUE),
                 "intervals do not align")
    expect_error(canMakeDimScalePairCompatible(e1 = e1, e2 = e2, isCounts1 = FALSE, isCounts2 = TRUE),
                 "intervals do not align")
    expect_error(canMakeDimScalePairCompatible(e1 = e1, e2 = e2, isCounts1 = TRUE, isCounts2 = FALSE),
                 "intervals do not align")
    expect_error(canMakeDimScalePairCompatible(e1 = e1, e2 = e2, isCounts1 = FALSE, isCounts2 = FALSE),
                 "intervals do not align")
})

test_that("canMakeDimScalesCompatible works when 'collapse' is TRUE", {
    canMakeDimScalesCompatible <- dembase:::canMakeDimScalesCompatible
    e1 <- new("Intervals", dimvalues = c(0, 1, 5, Inf))
    e2 <- new("Intervals", dimvalues = c(0, 5, Inf))
    expect_true(canMakeDimScalesCompatible(e1, e2, collapse = TRUE))
    e1 <- new("Intervals", dimvalues = c(0, 1, 5, Inf))
    e2 <- new("Intervals", dimvalues = c(0, 5, Inf))
    expect_true(canMakeDimScalesCompatible(e1, e2,
                                           subset = TRUE,
                                           collapse = TRUE))
    e1 <- new("Intervals", dimvalues = c(-Inf, 0, 1, 5, Inf))
    e2 <- new("Intervals", dimvalues = c(0, 5, Inf))
    expect_error(canMakeDimScalesCompatible(e1, e2, collapse = TRUE),
                 "one dimension starts at -Inf and other starts at 0")
    e1 <- new("Intervals", dimvalues = c(-Inf, 0, 1, 5, Inf))
    e2 <- new("Intervals", dimvalues = c(0, 5, Inf))
    expect_error(canMakeDimScalesCompatible(e1, e2, collapse = TRUE),
                 "one dimension starts at -Inf and other starts at 0")
    e1 <- new("Intervals", dimvalues = c(-Inf, 0, 1, 5, Inf))
    e2 <- new("Intervals", dimvalues = c(0, 5, Inf))
    expect_true(canMakeDimScalesCompatible(e1, e2, subset = TRUE, collapse = TRUE))
    e1 <- new("Intervals", dimvalues = c(0, 1, 5))
    e2 <- new("Intervals", dimvalues = c(0, 2, 5))
    expect_error(canMakeDimScalesCompatible(e1, e2, collapse = TRUE),
                 "one dimension has break \\[2\\] that other does not")
    e1 <- new("Intervals", dimvalues = c(0, 1, 5))
    e2 <- new("Intervals", dimvalues = c(0, 2, 5))
    expect_error(canMakeDimScalesCompatible(e1, e2, subset = TRUE, collapse = TRUE),
                 "one dimension has break \\[2\\] that other does not")
    e1 <- new("Intervals", dimvalues = c(0, 1, 5))
    e2 <- new("Intervals", dimvalues = c(0, 1, 5, Inf))
    expect_error(canMakeDimScalesCompatible(e1, e2, collapse = TRUE),
                 "one dimension ends at 5 and other ends at Inf")
    e1 <- new("Intervals", dimvalues = c(0, 1, 5))
    e2 <- new("Intervals", dimvalues = c(0, 1, 5, Inf))
    expect_error(canMakeDimScalesCompatible(e1, e2, collapse = TRUE, subset = TRUE),
                 "one dimension ends at 5 and other ends at Inf")
    e1 <- new("Intervals")
    e2 <- new("Intervals")
    expect_true(canMakeDimScalesCompatible(e1, e2, collapse = TRUE))
    e1 <- new("Intervals")
    e2 <- new("Intervals")
    expect_true(canMakeDimScalesCompatible(e1, e2, collapse = TRUE, subset = TRUE))
    e1 <- new("Intervals")
    e2 <- new("Intervals", dimvalues = c(0, 5))
    expect_error(canMakeDimScalesCompatible(e1, e2, collapse = TRUE),
                 "one dimension has 1 interval but other has none")
    e1 <- new("Intervals")
    e2 <- new("Intervals", dimvalues = c(0, 5))
    expect_error(canMakeDimScalesCompatible(e1, e2, collapse = TRUE, subset = TRUE),
                 "one dimension has 1 interval but other has none")
})

test_that("canMakeDimScalesCompatible works when 'collapse' is FALSE", {
    canMakeDimScalesCompatible <- dembase:::canMakeDimScalesCompatible
    e1 <- new("Intervals", dimvalues = c(0, 5, Inf))
    e2 <- new("Intervals", dimvalues = c(0, 1, 5, Inf))
    expect_true(canMakeDimScalesCompatible(e1, e2, collapse = FALSE))
    e1 <- new("Intervals", dimvalues = c(0, 5, Inf))
    e2 <- new("Intervals", dimvalues = c(0, 1, 5, Inf))
    expect_true(canMakeDimScalesCompatible(e1, e2, subset = TRUE, collapse = FALSE))
    e1 <- new("Intervals", dimvalues = c(-Inf, 0, 1, 5, Inf))
    e2 <- new("Intervals", dimvalues = c(0, 1, 5, Inf))
    expect_error(canMakeDimScalesCompatible(e1, e2, collapse = FALSE),
                 "one dimension starts at -Inf and other starts at 0")
    e1 <- new("Intervals", dimvalues = c(-Inf, 0, 1, 5, Inf))
    e2 <- new("Intervals", dimvalues = 0:5)
    expect_true(canMakeDimScalesCompatible(e1, e2, collapse = FALSE, subset = TRUE))
    e1 <- new("Intervals", dimvalues = c(-Inf, 0, 1, 5, Inf))
    e2 <- new("Intervals", dimvalues = c(0, 1, 5, Inf))
    expect_error(canMakeDimScalesCompatible(e1, e2, collapse = FALSE),
                 "one dimension starts at -Inf and other starts at 0")
    e1 <- new("Intervals", dimvalues = c(-Inf, 0, 5, Inf))
    e2 <- new("Intervals", dimvalues = c(-5, 0, 5, 10, Inf))
    expect_true(canMakeDimScalesCompatible(e1, e2, subset = TRUE, collapse = FALSE))
    e1 <- new("Intervals", dimvalues = c(0, 1, 5))
    e2 <- new("Intervals", dimvalues = c(0, 2, 5))
    expect_error(canMakeDimScalesCompatible(e1, e2, collapse = FALSE),
                 "one dimension has break \\[1\\] that other does not")
    e1 <- new("Intervals", dimvalues = c(0, 1, 5))
    e2 <- new("Intervals", dimvalues = c(0, 5))
    expect_error(canMakeDimScalesCompatible(e1, e2, subset = TRUE, collapse = FALSE),
                 "one dimension has break \\[1\\] that other does not")
    e1 <- new("Intervals", dimvalues = c(0, 1, 5))
    e2 <- new("Intervals", dimvalues = c(0, 1, 5, Inf))
    expect_error(canMakeDimScalesCompatible(e1, e2, collapse = FALSE),
                 "one dimension ends at 5 and other ends at Inf")
    e1 <- new("Intervals", dimvalues = c(0, 1, 5))
    e2 <- new("Intervals", dimvalues = c(0, 1, 5, Inf))
    expect_error(canMakeDimScalesCompatible(e1, e2, collapse = FALSE, subset = TRUE),
                 "one dimension ends at 5 and other ends at Inf")
    e1 <- new("Intervals")
    e2 <- new("Intervals")
    expect_true(canMakeDimScalesCompatible(e1, e2, collapse = FALSE))
    e1 <- new("Intervals")
    e2 <- new("Intervals")
    expect_true(canMakeDimScalesCompatible(e1, e2, collapse = FALSE, subset = TRUE))
    e1 <- new("Intervals")
    e2 <- new("Intervals", dimvalues = c(0, 5))
    expect_error(canMakeDimScalesCompatible(e1, e2, collapse = FALSE),
                 "one dimension has 1 interval but other has none")
    e1 <- new("Intervals", dimvalues = c(0, 5))
    e2 <- new("Intervals")
    expect_error(canMakeDimScalesCompatible(e1, e2, collapse = FALSE),
                 "one dimension has 1 interval but other has none")
    e1 <- new("Intervals")
    e2 <- new("Intervals", dimvalues = c(0, 5))
    expect_error(canMakeDimScalesCompatible(e1, e2, collapse = FALSE, subset = TRUE),
                 "one dimension has 1 interval but other has none")
})

test_that("collapseDimScale works", {
    collapseDimScale <- dembase:::collapseDimScale
    x <- new("Intervals", dimvalues = c(0, 1, 5, 10))
    y <- new("Intervals", dimvalues = c(0, 5, 10))
    expect_identical(collapseDimScale(x, index = c(1L, 1L, 2L)), y)
    expect_identical(collapseDimScale(x, index = c(1L, 0L, 2L)),
                     new("Categories", dimvalues = c("0", "5-9")))
    expect_identical(collapseDimScale(x, index = 3:1),
                     new("Categories", dimvalues = c("5-9", "1-4", "0")))
    expect_error(collapseDimScale(x, index = c(2L, 2L, 1L)),
                 "'index' has duplicates")
    expect_identical(collapseDimScale(x, index = integer()),
                     new("Intervals"))
    x <- new("Intervals", dimvalues = c(-5, 0, 1, 5, 10, 15, Inf))
    y <- new("Intervals", dimvalues = c(0, 5, 15))
    expect_identical(collapseDimScale(x, index = c(0L, 1L, 1L, 2L, 2L, 0L)), y)
    x <- new("Intervals", dimvalues = c(0, 1, 5, 10, 15, Inf))
    y <- new("Intervals", dimvalues = c(5, 10))
    expect_identical(collapseDimScale(x, index = c(0L, 0L, 1L, 0L, 0L)), y)
    x <- new("Intervals")
    expect_identical(collapseDimScale(x, index = integer()), x)
})

test_that("dbindDimScales works", {
    dbindDimScales <- dembase:::dbindDimScales
    expect_identical(dbindDimScales(e1 = new("Intervals", dimvalues = c(0, 1, 5, 10)),
                                    e2 = new("Intervals", dimvalues = c(10, Inf)),
                                    along = "age"),
                     new("Intervals", dimvalues = c(0, 1, 5, 10, Inf)))
    expect_identical(dbindDimScales(e1 = new("Intervals", dimvalues = c(2010, 2015, 2020)),
                                    e2 = new("Intervals", dimvalues = c(2000, 2005, 2010)),
                                    along = "period"),
                     new("Intervals", dimvalues = c(2000, 2005, 2010, 2015, 2020)))
    expect_error(dbindDimScales(e1 = new("Intervals", dimvalues = c(0, 1, 5, 9)),
                                e2 = new("Intervals", dimvalues = c(10, Inf)),
                                along = "age"),
                 "gap between \"age\" dimensions")
    expect_error(dbindDimScales(e1 = new("Intervals", dimvalues = c(2005, 2015, 2020)),
                                e2 = new("Intervals", dimvalues = c(2000, 2005, 2010)),
                                along = "period"),
                 "\"period\" dimensions overlap")
    expect_error(dbindDimScales(e1 = new("Intervals", dimvalues = c(2015, 2020)),
                                e2 = new("Intervals", dimvalues = c(2000, 2005, 2010)),
                                along = "period"),
                 "gap between \"period\" dimensions")
})

test_that("e1IsFirstDimScale works", {
    e1IsFirstDimScale <- dembase:::e1IsFirstDimScale
    e1 <- new("Intervals", dimvalues = c(0, 5, 10))
    e2 <- new("Intervals", dimvalues = c(10, Inf))
    expect_true(e1IsFirstDimScale(e1 = e1, e2 = e2))
    e1 <- new("Intervals", dimvalues = numeric())
    e2 <- new("Intervals", dimvalues = numeric())
    expect_true(e1IsFirstDimScale(e1 = e1, e2 = e2))
    e1 <- new("Intervals", dimvalues = c(10, Inf))
    e2 <- new("Intervals", dimvalues = c(0, 5, 10))
    expect_false(e1IsFirstDimScale(e1 = e1, e2 = e2))
    e1 <- new("Intervals", dimvalues = c(10, Inf))
    e2 <- new("Intervals", dimvalues = c(0, 5, 11))
    expect_true(e1IsFirstDimScale(e1 = e1, e2 = e2))
})

test_that("extendDimScale works", {
    extendDimScale <- dembase:::extendDimScale
    x <- new("Intervals", dimvalues = c(0, 1, 5, 10))
    y <- new("Intervals", dimvalues = c(0, 1, 5))
    expect_identical(extendDimScale(x, index = c(1L, 2L)), y)
    x <- new("Intervals", dimvalues = c(-5, 0, 1, 5, Inf))
    y <- new("Intervals", dimvalues = c(0, 1, 5, Inf))
    expect_identical(extendDimScale(x, index = 2:4), y)
    x <- new("Intervals", dimvalues = c(0, 1, 5, 10, Inf))
    y <- new("Intervals", dimvalues = c(1, 5, 10, Inf))
    expect_identical(extendDimScale(x, index = 2:4), y)
    x <- new("Intervals", dimvalues = c(0, 5, 10, 15))
    expect_error(extendDimScale(x, index = c(1L, 3L)),
                 "'index' has gaps")
    x <- new("Intervals")
    expect_identical(extendDimScale(x, index = integer()), x)
})

test_that("incrementDimScale method for Intervals works", {
    incrementDimScale <- dembase:::incrementDimScale
    x <- new("Intervals", dimvalues = 2001:2010)
    ans.obtained <- incrementDimScale(x, n = 10)
    ans.expected <- new("Intervals", dimvalues = 2010:2020)
    expect_identical(ans.obtained, ans.expected)
    x <- new("Intervals", dimvalues = 2001:2010)
    ans.obtained <- incrementDimScale(x, n = -5)
    ans.expected <- new("Intervals", dimvalues = 1996:2001)
    expect_identical(ans.obtained, ans.expected)
    x <- new("Intervals", dimvalues = c(0, 5))
    ans.obtained <- incrementDimScale(x, n = 1)
    ans.expected <- new("Intervals", dimvalues = c(5, 10))
    expect_identical(ans.obtained, ans.expected)
    x <- new("Intervals", dimvalues = c(-Inf, 0, 5))
    ans.obtained <- incrementDimScale(x, n = 2)
    ans.expected <- new("Intervals", dimvalues = c(5, 10, 15))
    expect_identical(ans.obtained, ans.expected)
    x <- new("Intervals", dimvalues = c(0, 5, Inf))
    ans.obtained <- incrementDimScale(x, n = -2)
    ans.expected <- new("Intervals", dimvalues = c(-10, -5, 0))
    expect_identical(ans.obtained, ans.expected)
    x <- new("Intervals", dimvalues = numeric())
    expect_error(incrementDimScale(x, n = 5),
                 "\"along\" dimension has length 0")
    x <- new("Intervals", dimvalues = c(-Inf, 0, Inf))
    expect_error(incrementDimScale(x, n = 5),
                 "\"along\" dimension has no finite intervals")
    x <- new("Intervals", dimvalues = c(-5, 0, Inf))
    expect_error(incrementDimScale(x, n = 5),
                 "last interval of \"along\" dimension is open")
    x <- new("Intervals", dimvalues = c(-Inf, -5, 0))
    expect_error(incrementDimScale(x, n = -5),
                 "first interval of \"along\" dimension is open")
    x <- new("Intervals", dimvalues = c(0, 5, 11))
    expect_error(incrementDimScale(x, n = 5),
                 "intervals on \"along\" dimension have varying lengths")
})

test_that("inferDimvalues method for Intervals works", {
  inferDimvalues <- dembase:::inferDimvalues
  expect_identical(inferDimvalues(new("Intervals"), labels = c("1", "2")),
              c(1, 2, 3))
  expect_identical(inferDimvalues(new("Intervals"), labels = character()),
              numeric())
  expect_identical(inferDimvalues(new("Intervals"), labels = c(1, "a")),
                   NULL)
  expect_identical(inferDimvalues(new("Intervals"),
                             labels = c("-5--2", "-1", "0", "1-4", "5+")),
              c(-5, -1, 0, 1, 5, Inf))
  expect_identical(inferDimvalues(new("Intervals"), labels = c("<0", "0-4")),
              c(-Inf, 0, 5))
  expect_identical(inferDimvalues(new("Intervals"), labels = c("<0", "0", "1", "2+")),
              c(-Inf, 0, 1, 2, Inf))
  expect_identical(inferDimvalues(new("Intervals"), labels = c("0-4", "5-9")),
              c(0, 5, 10))
  expect_identical(inferDimvalues(new("Intervals"), labels = c("0-4", "5-9", "10+")),
              c(0, 5, 10, Inf))
  expect_identical(inferDimvalues(new("Intervals"), labels = c("0-5", "5-9", "10+")),
              NULL)
  expect_identical(inferDimvalues(new("Intervals"),
                                  labels = c("2001-2005", "2006-2010")),
              c(2000, 2005, 2010))
  expect_identical(inferDimvalues(new("Intervals"), labels = NULL),
              numeric())
  expect_identical(inferDimvalues(new("Intervals"), labels = "<0"),
                   c(-Inf, 0))
  expect_identical(inferDimvalues(new("Intervals"), labels = "2001+"),
                   c(2000, Inf))
  expect_identical(inferDimvalues(new("Intervals"), labels = "2000.5+"),
                   c(2000.5, Inf))
  expect_identical(inferDimvalues(new("Intervals"),
                                  labels = c("1990-2000.5", "2000.5+")),
                   c(1990, 2000.5, Inf))
  expect_identical(inferDimvalues(new("Intervals"),
                                  labels = as.character(2001:2004)),
                   c(2000, 2001, 2002, 2003, 2004))
  expect_identical(inferDimvalues(new("Intervals"),
                                  labels = c("less than 0", "0 - 29 years", "30 years and over")),
                   c(-Inf, 0, 30, Inf))
  expect_identical(inferDimvalues(new("Intervals"),
                                  labels = c("Jan-2000", "Feb-2000", "Mar-2000")),
                   c(2000, 2000+31/366, 2000+60/366, 2000+91/366))
})

test_that("labels method for Intervals works", {
    labels <- dembase:::labels
    expect_identical(labels(new("Intervals", dimvalues = c(0, 1, 2))),
                     c("0", "1"))
    expect_identical(labels(new("Intervals")),
                     character())
    expect_identical(labels(new("Intervals", dimvalues = c(0, 1, Inf))),
                     c("0", "1+"))
    expect_identical(labels(new("Intervals", dimvalues = c(0, 1, 5, Inf))),
                     c("0", "1-4", "5+"))
    expect_identical(labels(new("Intervals", dimvalues = c(2001, 2006, 2011))),
                     c("2002-2006", "2007-2011"))
    expect_identical(labels(new("Intervals", dimvalues = c(-Inf, 0, 1, Inf))),
                     c("<0", "0", "1+"))
})

test_that("length method for Intervals works", {
  expect_that(length(new("Intervals", dimvalues = c(0, 1, 2))),
              is_identical_to(2L))
  expect_that(length(new("Intervals")),
              is_identical_to(0L))
})

test_that("makeIndices works when 'collapse' is TRUE", {
    makeIndices <- dembase:::makeIndices
    e1 <- new("Intervals", dimvalues = c(0, 1, 2, 5, Inf))
    e2 <- new("Intervals", dimvalues = c(0, 1, 5, Inf))
    expect_identical(makeIndices(e1, e2, collapse = TRUE),
                     c(1L, 2L, 2L, 3L))
    e1 <- new("Intervals", dimvalues = c(-Inf, -5, 0, 1, 2, 5, Inf))
    e2 <- new("Intervals", dimvalues = c(0, 1, 5))
    expect_identical(makeIndices(e1, e2, collapse = TRUE),
                     c(0L, 0L, 1L, 2L, 2L, 0L))
    e1 <- new("Intervals", dimvalues = c(0, 1, 2, 5, Inf))
    e2 <- new("Intervals")
    expect_identical(makeIndices(e1, e2, collapse = TRUE),
                     rep(0L, 4))
    e1 <- new("Intervals")
    e2 <- new("Intervals")
    expect_identical(makeIndices(e1, e2, collapse = TRUE),
                     integer())
})

test_that("makeIndices works when 'collapse' is FALSE", {
    makeIndices <- dembase:::makeIndices
    e1 <- new("Intervals", dimvalues = c(0, 1, 5, Inf))
    e2 <- new("Intervals", dimvalues = c(0, 1, 2, 5, Inf))
    expect_identical(makeIndices(e1, e2, collapse = FALSE),
                     c(1L, 2L, 2L, 3L))
    e1 <- new("Intervals", dimvalues = c(-Inf, 0, 1, 5, Inf))
    e2 <- new("Intervals", dimvalues = c(-Inf, -5, 0, 1, 2, 5, Inf))
    expect_identical(makeIndices(e1, e2, collapse = FALSE),
                     c(1L, 1L, 2L, 3L, 3L, 4L))
    e1 <- new("Intervals", dimvalues = c(-Inf, 0, 1, 5, Inf))
    e2 <- new("Intervals", dimvalues = c(0, 1, 2, 5, 10, Inf))
    expect_identical(makeIndices(e1, e2, collapse = FALSE),
                     c(2L, 3L, 3L, 4L, 4L))
    e1 <- new("Intervals")
    e2 <- new("Intervals")
    expect_identical(makeIndices(e1, e2, collapse = FALSE),
                     integer())
})

test_that("makePairIndices method for Intervals works", {
    makePairIndices <- dembase:::makePairIndices
    e1 <- new("Intervals", dimvalues = c(0, 1, 5, Inf))
    e2 <- new("Intervals", dimvalues = c(0, 1, 2, 5, Inf))
    expect_identical(makePairIndices(e1, e2, isCounts1 = TRUE, isCounts2 = TRUE),
                     list(1:3, c(1L, 2L, 2L, 3L)))
    expect_identical(makePairIndices(e1, e2, isCounts1 = FALSE, isCounts2 = FALSE),
                     list(c(1L, 2L, 2L, 3L), 1:4))
    e1 <- new("Intervals", dimvalues = c(0, 1, 5))
    e2 <- new("Intervals", dimvalues = c(0, 1, 2, 5, Inf))
    expect_identical(makePairIndices(e1, e2, isCounts1 = FALSE, isCounts2 = TRUE),
                     list(c(1:2, 2L), c(1L, 2L, 3L, 0L)))
    expect_identical(makePairIndices(e1, e2, isCounts1 = FALSE, isCounts2 = FALSE),
                     list(c(1L, 2L, 2L), 1:3))
    e1 <- new("Intervals", dimvalues = c(0, 1, 5, Inf))
    e2 <- new("Intervals", dimvalues = c(1, 2, 5, Inf))
    expect_identical(makePairIndices(e1, e2, isCounts1 = TRUE, isCounts2 = TRUE),
                     list(0:2, c(1L, 1L, 2L)))
    expect_identical(makePairIndices(e1, e2, isCounts1 = FALSE, isCounts2 = TRUE),
                     list(c(2L, 2L, 3L), 1:3))
    expect_identical(makePairIndices(e1, e2, isCounts1 = FALSE, isCounts2 = FALSE),
                     list(c(2L, 2L, 3L), 1:3))
    e1 <- new("Intervals", dimvalues = c(0, 1, 5, Inf))
    e2 <- new("Intervals", dimvalues = c(-Inf, -5, 0, 1, 2, 5))
    expect_identical(makePairIndices(e1, e2, isCounts1 = TRUE, isCounts2 = TRUE),
                     list(c(1L, 2L, 0L), c(0L, 0L, 1L, 2L, 2L)))
    expect_identical(makePairIndices(e1, e2, isCounts1 = FALSE, isCounts2 = TRUE),
                     list(c(1L, 2L, 2L), c(0L, 0L, 1L, 2L, 3L)))
    expect_identical(makePairIndices(e1, e2, isCounts1 = FALSE, isCounts2 = FALSE),
                     list(c(1L, 2L, 2L), c(3L, 4L, 5L)))
    e1 <- new("Intervals")
    e2 <- new("Intervals")
    expect_identical(makePairIndices(e1, e2, isCounts1 = TRUE, isCounts2 = TRUE),
                     list(integer(), integer()))
    expect_identical(makePairIndices(e1, e2, isCounts1 = FALSE, isCounts2 = TRUE),
                     list(integer(), integer()))
    expect_identical(makePairIndices(e1, e2, isCounts1 = TRUE, isCounts2 = FALSE),
                     list(integer(), integer()))
    expect_identical(makePairIndices(e1, e2, isCounts1 = FALSE, isCounts2 = FALSE),
                     list(integer(), integer()))
    e1 <- new("Intervals", dimvalues = c(0, 5, Inf))
    e2 <- new("Intervals")
    expect_identical(makePairIndices(e1, e2, isCounts1 = TRUE, isCounts2 = TRUE),
                     list(rep(0L, 2L), integer()))
    expect_identical(makePairIndices(e1, e2, isCounts1 = FALSE, isCounts2 = TRUE),
                     list(integer(), integer()))
    expect_identical(makePairIndices(e1, e2, isCounts1 = TRUE, isCounts2 = FALSE),
                     list(rep(0L, 2), integer()))
    expect_identical(makePairIndices(e1, e2, isCounts1 = FALSE, isCounts2 = FALSE),
                     list(integer(), integer()))
})

test_that("mergeDimScales method for Intervals works", {
    mergeDimScales <- dembase:::mergeDimScales
    e1 <- new("Intervals", dimvalues = c(0, 1, 5, Inf))
    e2 <- new("Intervals", dimvalues = c(0, 1, 2, 5, Inf))
    expect_identical(mergeDimScales(e1, e2), e2)
    mergeDimScales <- dembase:::mergeDimScales
    e1 <- new("Intervals", dimvalues = c(0, 1, 5, 10, Inf))
    e2 <- new("Intervals", dimvalues = c(0, 1, 2, 5, Inf))
    expect_identical(mergeDimScales(e1, e2),
                     new("Intervals", dimvalues = c(0, 1, 2, 5, 10, Inf)))
    expect_identical(mergeDimScales(new("Intervals"), new("Intervals")),
                     new("Intervals"))
    e1 <- new("Intervals", dimvalues = c(1, 5, 10, Inf))
    e2 <- new("Intervals", dimvalues = c(0, 1, 2, 5, 10))
    expect_identical(mergeDimScales(e1, e2),
                     new("Intervals", dimvalues = c(1, 2, 5, 10)))
})

test_that("stepLengths method for Intervals works", {
  stepLengths <- dembase:::stepLengths
  expect_that(stepLengths(new("Intervals", dimvalues = c(1, 2, 4))),
              is_identical_to(c(1, 2)))
  expect_that(stepLengths(new("Intervals", dimvalues = c(0, 5, 10, Inf))),
              is_identical_to(c(5, 5, Inf)))
  expect_that(stepLengths(new("Intervals", dimvalues = c(-Inf, 0, 5, 10, Inf))),
              is_identical_to(c(Inf, 5, 5, Inf)))
  expect_that(stepLengths(new("Intervals")),
              is_identical_to(numeric()))
})


