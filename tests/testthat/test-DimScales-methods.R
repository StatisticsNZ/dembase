
context("DimScale-methods")

test_that("coercion from Categories to Categories works", {
  expect_that(as(new("Categories", dimvalues = c("a", "b")), "Categories"),
              is_identical_to(new("Categories", dimvalues = c("a", "b"))))
  expect_that(as(new("Categories"), "Categories"),
              is_identical_to(new("Categories")))
})


test_that("coercion from Intervals to Categories works", {
  expect_that(as(new("Intervals", dimvalues = c(0, 1, 2), isAge = TRUE), "Categories"),
              is_identical_to(new("Categories", dimvalues = c("0", "1"))))
  expect_that(as(new("Intervals"), "Categories"),
              is_identical_to(new("Categories")))
})


test_that("coercion from Iterations to Categories works", {
  expect_that(as(new("Iterations", dimvalues = 1:3), "Categories"),
              is_identical_to(new("Categories", dimvalues = c("1", "2", "3"))))
  expect_that(as(new("Iterations"), "Categories"),
              is_identical_to(new("Categories")))
})


test_that("coercion from Points to Categories works", {
  expect_that(as(new("Points", dimvalues = c(0.5, 1.5, 2.5)), "Categories"),
              is_identical_to(new("Categories", dimvalues = c("0.5", "1.5", "2.5"))))
  expect_that(as(new("Points"), "Categories"),
              is_identical_to(new("Categories")))
})


test_that("coercion from Quantiles to Categories works", {
  expect_that(as(new("Quantiles", dimvalues = c(0.025, 0.5, 0.975)), "Categories"),
              is_identical_to(new("Categories", dimvalues = c("2.5%", "50%", "97.5%"))))
  expect_that(as(new("Quantiles"), "Categories"),
              is_identical_to(new("Categories")))
})


test_that("coercion from Triangles to Categories works", {
  expect_that(as(new("Triangles", dimvalues = c("Upper", "Lower")), "Categories"),
              is_identical_to(new("Categories", dimvalues = c("Upper", "Lower"))))
  expect_that(as(new("Triangles"), "Categories"),
              is_identical_to(new("Categories")))
})

test_that("Extract works with Categories", {
    expect_identical(new("Categories", dimvalues = c("a", "b", "c"))[1:2],
                     new("Categories", dimvalues = c("a", "b")))
    expect_identical(new("Categories", dimvalues = c("a", "b", "c"))[-2L],
                     new("Categories", dimvalues = c("a", "c")))
    expect_identical(new("Categories", dimvalues = c("a", "b", "c"))[2:0],
                     new("Categories", dimvalues = c("b", "a")))
    expect_error(new("Categories", dimvalues = c("a", "b", "c"))[c(1L, NA)],
                 "'i' has missing values")
    expect_error(new("Categories", dimvalues = c("a", "b", "c"))[c(1L, 100L)],
                 "'i' has values outside the valid range")
    expect_error(new("Categories", dimvalues = c("a", "b", "c"))[c(1L, -1L)],
                 "only 0's may be mixed with negative subscripts")
    expect_identical(new("Categories")[0L],
                     new("Categories"))
    expect_identical(new("Categories")[0L],
                     new("Categories"))
    expect_identical(new("Categories")[integer()],
                     new("Categories"))
    expect_error(new("Categories")[1L],
                 "'i' has values outside the valid range")
})

test_that("Extract works with Points", {
    expect_identical(new("Points", dimvalues = 1:3)[1:2],
                     new("Points", dimvalues = 1:2))
    expect_identical(new("Points", dimvalues = 1:3)[c(0L, -2L)],
                     new("Points", dimvalues = c(1L, 3L)))
    expect_error(new("Points", dimvalues = 1:3)[2:1],
                 "values not strictly increasing")
    expect_error(new("Points", dimvalues = 1:3)[c(1L, NA)],
                 "'i' has missing values")
    expect_error(new("Points", dimvalues = 1:3)[c(1L, 100L)],
                 "'i' has values outside the valid range")
    expect_error(new("Points", dimvalues = 1:3)[c(1L, -1L)],
                 "only 0's may be mixed with negative subscripts")
    expect_identical(new("Points")[0L],
                     new("Points"))
    expect_identical(new("Points")[0L],
                     new("Points"))
    expect_identical(new("Points")[integer()],
                     new("Points"))
    expect_error(new("Points")[1L],
                 "'i' has values outside the valid range")
})

test_that("%in% works with Categories", {
    assign("%in%", getFromNamespace("%in%", "dembase"))
    x <- new("Categories", dimvalues = c("a", "b"))
    expect_identical(x %in% c("a", "b", "c"),
                     new("SubArrayIndices",
                         nms = "x",
                         indices = list(c(TRUE, TRUE))))
    expect_identical(x %in% "b",
                     new("SubArrayIndices",
                         nms = "x",
                         indices = list(c(FALSE, TRUE))))
})

test_that("%in% works with Points", {
    assign("%in%", getFromNamespace("%in%", "dembase"))
    x <- new("Points", dimvalues = 1:3)
    expect_identical(x %in% 2:3,
                     new("SubArrayIndices",
                         nms = "x",
                         indices = list(c(FALSE, TRUE, TRUE))))
    expect_identical(x %in% c("1", "2"),
                     new("SubArrayIndices",
                         nms = "x",
                         indices = list(c(TRUE, TRUE, FALSE))))
})

test_that("Compare works with Categories", {
    x <- new("Categories", dimvalues = c("a", "b", "c"))
    expect_identical(x >= "b",
                     new("SubArrayIndices",
                         nms = "x",
                         indices = list(c(FALSE, TRUE, TRUE))))
    expect_identical("c" > x,
                     new("SubArrayIndices",
                         nms = "x",
                         indices = list(c(TRUE, TRUE, FALSE))))
    expect_error(x == x,
                 "attempt to compare two dimscales")
})

test_that("Compare works with Points", {
    x <- new("Points", dimvalues = 1:3)
    expect_identical(x >= 1,
                     new("SubArrayIndices",
                         nms = "x",
                         indices = list(c(TRUE, TRUE, TRUE))))
    expect_identical(-1 > x,
                     new("SubArrayIndices",
                         nms = "x",
                         indices = list(c(FALSE, FALSE, FALSE))))
    expect_error(x == x,
                 "attempt to compare two dimscales")
})

test_that("canMakeDimScalePairCompatible works with Categories", {
    canMakeDimScalePairCompatible <- dembase:::canMakeDimScalePairCompatible
    x <- new("Categories", dimvalues = c("a", "b", "c"))
    y <- new("Categories", dimvalues = c("a", "b"))
    expect_true(canMakeDimScalePairCompatible(x, y))
    expect_true(canMakeDimScalePairCompatible(new("Categories"), x))
    expect_true(canMakeDimScalePairCompatible(new("Categories"), new("Categories")))
    x <- new("Categories", dimvalues = c("a", "b", "c"))
    y <- new("Categories", dimvalues = c("d", "e"))
    expect_error(canMakeDimScalePairCompatible(x, y),
                "no values in common")
    expect_error(canMakeDimScalePairCompatible(new("Categories"), new("Intervals")),
                "one dimension has dimscale \"Categories\" and other has dimscale \"Intervals\"")
})

test_that("canMakeDimScalePairCompatible works with Points", {
    canMakeDimScalePairCompatible <- dembase:::canMakeDimScalePairCompatible
    x <- new("Points", dimvalues = 1:3)
    y <- new("Points", dimvalues = 3:5)
    expect_true(canMakeDimScalePairCompatible(x, y))
    x <- new("Points", dimvalues = 1:3)
    y <- new("Points", dimvalues = 4:6)
    expect_error(canMakeDimScalePairCompatible(x, y),
                 "no values in common")
    expect_true(canMakeDimScalePairCompatible(x, new("Points")))
    expect_true(canMakeDimScalePairCompatible(new("Points"), new("Points")))
    expect_error(canMakeDimScalePairCompatible(y, new("Intervals")),
                "one dimension has dimscale \"Points\" and other has dimscale \"Intervals\"")
})

test_that("canMakeDimScalesCompatible works with Categories", {
    canMakeDimScalesCompatible <- dembase:::canMakeDimScalesCompatible
    x <- new("Categories", dimvalues = c("a", "b", "c"))
    y <- new("Categories", dimvalues = c("a", "b"))
    expect_error(canMakeDimScalesCompatible(x, y),
                sprintf("one dimension has value \\[%s\\] that other does not",
                        dQuote("c")))
    expect_true(canMakeDimScalesCompatible(x, y, subset = TRUE))
    expect_error(canMakeDimScalesCompatible(y, x),
                sprintf("one dimension has value \\[%s\\] that other does not",
                        dQuote("c")))
    expect_error(canMakeDimScalesCompatible(y, x, subset = TRUE),
                sprintf("one dimension has value \\[%s\\] that other does not",
                        dQuote("c")))
    expect_true(canMakeDimScalesCompatible(x, x))
    expect_error(canMakeDimScalesCompatible(x, new("Categories")),
                 sprintf("one dimension has values \\[%s, %s, %s\\] that other does not",
                         dQuote("a"), dQuote("b"), dQuote("c")))
    expect_true(canMakeDimScalesCompatible(x, new("Categories"), subset = TRUE))
    expect_true(canMakeDimScalesCompatible(new("Categories"), new("Categories")))
    expect_error(canMakeDimScalesCompatible(y, new("Intervals")),
                "one dimension has dimscale \"Categories\" but other has dimscale \"Intervals\"")
})

test_that("canMakeDimScalesCompatible method for DimScales works with Iterations", {
    canMakeDimScalesCompatible <- dembase:::canMakeDimScalesCompatible
    x <- new("Iterations", dimvalues = 1:3)
    y <- new("Iterations", dimvalues = 1:2)
    expect_error(canMakeDimScalesCompatible(x, y),
                 "one dimension has value \\[3\\] that other does not")
    expect_true(canMakeDimScalesCompatible(x, y, subset = TRUE))
    expect_error(canMakeDimScalesCompatible(y, x),
                 "one dimension has value \\[3\\] that other does not")
    expect_error(canMakeDimScalesCompatible(y, x, subset = TRUE),
                 "one dimension has value \\[3\\] that other does not")
    expect_true(canMakeDimScalesCompatible(x, x))
    expect_error(canMakeDimScalesCompatible(x, new("Iterations")),
                 "one dimension has values \\[1, 2, 3\\] that other does not")
    expect_true(canMakeDimScalesCompatible(x, new("Iterations"), subset = TRUE))
    expect_true(canMakeDimScalesCompatible(new("Iterations"), new("Iterations")))
    expect_error(canMakeDimScalesCompatible(y, new("Intervals")),
                 "one dimension has dimscale \"Iterations\" but other has dimscale \"Intervals\"")
})

test_that("canMakeDimScalesCompatible method for DimScales works with Points", {
    canMakeDimScalesCompatible <- dembase:::canMakeDimScalesCompatible
    x <- new("Points", dimvalues = 1:3)
    y <- new("Points", dimvalues = 1:2)
    expect_error(canMakeDimScalesCompatible(x, y),
                "one dimension has value \\[3\\] that other does not")
    expect_true(canMakeDimScalesCompatible(x, y, subset = TRUE))
    expect_error(canMakeDimScalesCompatible(y, x),
                "one dimension has value \\[3\\] that other does not")
    expect_error(canMakeDimScalesCompatible(y, x, subset = TRUE),
                "one dimension has value \\[3\\] that other does not")
    expect_true(canMakeDimScalesCompatible(x, x))
    expect_error(canMakeDimScalesCompatible(x, new("Points")),
                "one dimension has values \\[1, 2, 3\\] that other does not")
    expect_true(canMakeDimScalesCompatible(x, new("Points"), subset = TRUE))
    expect_true(canMakeDimScalesCompatible(new("Points"), new("Points")))
    expect_error(canMakeDimScalesCompatible(y, new("Intervals")),
                 "one dimension has dimscale \"Points\" but other has dimscale \"Intervals\"")
})

test_that("canMakeDimScalesCompatible method for DimScales works with Quantiles", {
    canMakeDimScalesCompatible <- dembase:::canMakeDimScalesCompatible
    x <- new("Quantiles", dimvalues = c(0.1, 0.5, 0.9))
    y <- new("Quantiles", dimvalues = c(0.1, 0.9))
    expect_error(canMakeDimScalesCompatible(x, y),
                 "one dimension has value \\[0.5\\] that other does not")
    expect_true(canMakeDimScalesCompatible(x, y, subset = TRUE))
    expect_error(canMakeDimScalesCompatible(y, x),
                 "one dimension has value \\[0.5\\] that other does not")
    expect_error(canMakeDimScalesCompatible(y, x, subset = TRUE),
                 "one dimension has value \\[0.5\\] that other does not")
    expect_true(canMakeDimScalesCompatible(x, x))
    expect_error(canMakeDimScalesCompatible(x, new("Quantiles")),
                 "one dimension has values \\[0.1, 0.5, 0.9\\] that other does not")
    expect_true(canMakeDimScalesCompatible(x, new("Quantiles"), subset = TRUE))
    expect_true(canMakeDimScalesCompatible(new("Quantiles"), new("Quantiles")))
    expect_error(canMakeDimScalesCompatible(y, new("Intervals")),
                 "one dimension has dimscale \"Quantiles\" but other has dimscale \"Intervals\"")
})

test_that("canMakeDimScalesCompatible method for DimScales works with Triangles", {
    canMakeDimScalesCompatible <- dembase:::canMakeDimScalesCompatible
    x <- new("Triangles", dimvalues = c("Lower", "Upper"))
    y <- new("Triangles", dimvalues = "Lower")
    expect_error(canMakeDimScalesCompatible(x, y),
                 sprintf("one dimension has value \\[%s\\] that other does not",
                         dQuote("Upper")))
    expect_true(canMakeDimScalesCompatible(x, y, subset = TRUE))
    expect_error(canMakeDimScalesCompatible(y, x),
                 sprintf("one dimension has value \\[%s\\] that other does not",
                         dQuote("Upper")))
    expect_error(canMakeDimScalesCompatible(y, x, subset = TRUE),
                 sprintf("one dimension has value \\[%s\\] that other does not",
                         dQuote("Upper")))
    expect_true(canMakeDimScalesCompatible(x, x))
    expect_error(canMakeDimScalesCompatible(x, new("Triangles")),
                 sprintf("one dimension has values \\[%s, %s\\] that other does not",
                         dQuote("Lower"), dQuote("Upper")))
    expect_true(canMakeDimScalesCompatible(x, new("Triangles"), subset = TRUE))
    expect_true(canMakeDimScalesCompatible(new("Triangles"), new("Triangles")))
    expect_error(canMakeDimScalesCompatible(y, new("Intervals")),
                 "one dimension has dimscale \"Triangles\" but other has dimscale \"Intervals\"")
})

test_that("collapseDimScale works", {
    collapseDimScale <- dembase:::collapseDimScale
    x <- new("Categories", dimvalues = c("a", "b", "c"))
    y <- new("Categories", dimvalues = c("c", "a", "b"))
    expect_identical(collapseDimScale(x, index = c(2L, 3L, 1L)), y)
    expect_identical(collapseDimScale(x, index = integer()), new("Categories"))
    collapseDimScale <- dembase:::collapseDimScale
    x <- new("Categories", dimvalues = c("a", "b", "c"))
    y <- new("Categories", dimvalues = c("b", "a"))
    expect_identical(collapseDimScale(x, index = c(2L, 1L, 0L)), y)
    x <- new("Points", dimvalues = 1:3)
    expect_identical(collapseDimScale(x, index = 1:3), x)
    x <- new("Iterations")
    expect_identical(collapseDimScale(x, index = integer()), x)
    x <- new("Categories", dimvalues = c("a", "b", "c"))
    expect_error(collapseDimScale(x, index = c(1L, 1L, 2L)))
})

test_that("dbindDimScales works", {
    dbindDimScales <- dembase:::dbindDimScales
    expect_identical(dbindDimScales(new("Categories", dimvalues = c("a", "b", "c")),
                                    new("Categories", dimvalues = c("d", "e")),
                                    along = "region"),
                     new("Categories", dimvalues = c("a", "b", "c", "d", "e")))
    expect_error(dbindDimScales(new("Categories", dimvalues = c("a", "b", "c")),
                                    new("Categories", dimvalues = c("d", "c")),
                                    along = "region"),
                 sprintf("\"region\" dimensions both have value %s",
                         dQuote("c")))
    expect_identical(dbindDimScales(new("Categories", dimvalues = "Net"),
                                    new("Triangles", dimvalues = c("Lower", "Upper")),
                                    along = "region"),
                     new("Categories", dimvalues = c("Net", "Lower", "Upper")))
})

test_that("e1IsFirstDimScale works", {
    e1IsFirstDimScale <- dembase:::e1IsFirstDimScale
    e1 <- new("Categories", dimvalues = c("a", "b", "c"))
    e2 <- new("Categories", dimvalues = c("d", "e", "f"))
    expect_true(e1IsFirstDimScale(e1 = e1, e2 = e2))
    e1 <- new("Iterations")
    e2 <- new("Iterations")
    expect_true(e1IsFirstDimScale(e1 = e1, e2 = e2))
})

test_that("extendDimScale works", {
    extendDimScale <- dembase:::extendDimScale
    x <- new("Categories", dimvalues = c("a", "b", "c"))
    y <- new("Categories", dimvalues = c("b", "c", "a"))
    expect_identical(extendDimScale(x, index = c(2L, 3L, 1L)), y)
    extendDimScale <- dembase:::extendDimScale
    x <- new("Categories", dimvalues = c("a", "b", "c"))
    y <- new("Categories", dimvalues = c("b", "a"))
    expect_identical(extendDimScale(x, index = c(2L, 1L)), y)
    x <- new("Points", dimvalues = 1:3)
    expect_identical(extendDimScale(x, index = 1:3), x)
    x <- new("Iterations")
    expect_identical(extendDimScale(x, index = integer()), x)
})

test_that("incrementDimScale works", {
    incrementDimScale <- dembase:::incrementDimScale
    x <- new("Categories", dimvalues = c("a", "b", "c"))
    expect_error(incrementDimScale(x),
                 "'n' argument can only be used when \"along\" dimension has numeric dimscale \\(eg \"Intervals\" or \"Points\"")
})

test_that("makeIndices method for Categories works", {
    makeIndices <- dembase:::makeIndices
    e1 <- new("Categories", dimvalues = c("a", "b"))
    e2 <- new("Categories", dimvalues = c("b", "a"))
    expect_identical(makeIndices(e1, e2, collapse = TRUE),
                     2:1)
    expect_identical(makeIndices(e1, e2, collapse = FALSE),
                     2:1)
    e1 <- new("Categories", dimvalues = c("a", "b", "c"))
    e2 <- new("Categories", dimvalues = c("a", "b"))
    expect_identical(makeIndices(e1, e2, collapse = TRUE),
                     c(1L, 2L, 0L))
    expect_identical(makeIndices(e1, e2, collapse = FALSE),
                     c(1L, 2L))
    e1 <- new("Categories", dimvalues = c("c", "a", "b"))
    e2 <- new("Categories", dimvalues = c("a", "b"))
    expect_identical(makeIndices(e1, e2, collapse = TRUE),
                     c(0L, 1L, 2L))
    expect_identical(makeIndices(e1, e2, collapse = FALSE),
                     c(2L, 3L))
    e1 <- new("Categories")
    e2 <- new("Categories")
    expect_identical(makeIndices(e1, e2, collapse = FALSE),
                     integer())
    expect_identical(makeIndices(e1, e2, collapse = TRUE),
                     integer())
})

test_that("makeIndices method for Points works", {
    makeIndices <- dembase:::makeIndices
    e1 <- new("Points", dimvalues = 1:2)
    e2 <- new("Points", dimvalues = 1:2)
    expect_identical(makeIndices(e1, e2, collapse = TRUE),
                     1:2)
    expect_identical(makeIndices(e1, e2, collapse = FALSE),
                     1:2)
    e1 <- new("Points", dimvalues = 1:3)
    e2 <- new("Points", dimvalues = 1:2)
    expect_identical(makeIndices(e1, e2, collapse = TRUE),
                     c(1L, 2L, 0L))
    expect_identical(makeIndices(e1, e2, collapse = FALSE),
                     c(1L, 2L))
    e1 <- new("Points")
    e2 <- new("Points")
    expect_identical(makeIndices(e1, e2, collapse = TRUE),
                     integer())
})

test_that("makeMissingAgeTimeDimScale works with age and time both DimScales - no triangles", {
    makeMissingAgeTimeDimScale <- dembase:::makeMissingAgeTimeDimScale
    ans.obtained <- makeMissingAgeTimeDimScale(age = new("Intervals", dimvalues = c(0, 1, 2), isAge = TRUE),
                                               time = new("Points", dimvalues = c(0, 1)))
    ans.expected <- new("Intervals", dimvalues = c(-2, -1, 0, 1))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- makeMissingAgeTimeDimScale(age = new("Intervals", dimvalues = c(0, 5)),
                                               time = new("Intervals", dimvalues = c(2000, 2005, 2010)))
    ans.expected <- new("Intervals", dimvalues = c(1995, 2000, 2005, 2010))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- makeMissingAgeTimeDimScale(age = new("Intervals", dimvalues = c(2, 3)),
                                               time = new("Points", dimvalues = c(0, 1)))
    ans.expected <- new("Intervals", dimvalues = c(-3, -2, -1))
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeMissingAgeTimeDimScale works with age and time both DimScales - with triangles", {
    makeMissingAgeTimeDimScale <- dembase:::makeMissingAgeTimeDimScale
    ans.obtained <- makeMissingAgeTimeDimScale(age = new("Intervals", dimvalues = c(0, 1, 2), isAge = TRUE),
                                               time = new("Points", dimvalues = c(0, 1)),
                                               triangle = new("Triangles", dimvalues = c("Lower", "Upper")))
    ans.expected <- new("Intervals", dimvalues = c(-2, -1, 0, 1))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- makeMissingAgeTimeDimScale(age = new("Intervals", dimvalues = c(0, 5)),
                                               time = new("Intervals", dimvalues = c(2000, 2005, 2010)),
                                               triangle = new("Triangles", dimvalues = c("Lower", "Upper")))
    ans.expected <- new("Intervals", dimvalues = c(1995, 2000, 2005, 2010))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- makeMissingAgeTimeDimScale(age = new("Intervals", dimvalues = c(2, 3)),
                                               time = new("Points", dimvalues = c(0, 1)),
                                               triangle = new("Triangles", dimvalues = c("Lower", "Upper")))
    ans.expected <- new("Intervals", dimvalues = c(-3, -2, -1))
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeMissingAgeTimeDimScale works with age and cohort both Intervals - no triangles", {
    makeMissingAgeTimeDimScale <- dembase:::makeMissingAgeTimeDimScale
    ans.obtained <- makeMissingAgeTimeDimScale(age = new("Intervals", dimvalues = c(0, 1, 2), isAge = TRUE),
                                               cohort = new("Intervals", dimvalues = c(2000, 2001)))
    ans.expected <- new("Points", dimvalues = c(2001, 2002))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- makeMissingAgeTimeDimScale(age = new("Intervals", dimvalues = c(0, 5)),
                                               cohort = new("Intervals", dimvalues = c(2000, 2005, 2010)))
    ans.expected <- new("Points", dimvalues = c(2005, 2010))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- makeMissingAgeTimeDimScale(age = new("Intervals", dimvalues = c(10, 15)),
                                               cohort = new("Intervals", dimvalues = c(2000, 2005, 2010)))
    ans.expected <- new("Points", dimvalues = c(2015, 2020))
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeMissingAgeTimeDimScale works with age and cohort both Intervals - with triangles", {
    makeMissingAgeTimeDimScale <- dembase:::makeMissingAgeTimeDimScale
    ans.obtained <- makeMissingAgeTimeDimScale(age = new("Intervals", dimvalues = c(0, 1, 2), isAge = TRUE),
                                               cohort = new("Intervals", dimvalues = c(2000, 2001)),
                                               triangle = new("Triangles", dimvalues = c("Lower", "Upper")))
    ans.expected <- new("Intervals", dimvalues = c(2000, 2001, 2002, 2003))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- makeMissingAgeTimeDimScale(age = new("Intervals", dimvalues = c(0, 5)),
                                               cohort = new("Intervals", dimvalues = c(2000, 2005, 2010)),
                                               triangle = new("Triangles", dimvalues = c("Lower", "Upper")))
    ans.expected <- new("Intervals", dimvalues = c(2000, 2005, 2010, 2015))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- makeMissingAgeTimeDimScale(age = new("Intervals", dimvalues = c(10, 15)),
                                               cohort = new("Intervals", dimvalues = c(2000, 2005, 2010)),
                                               triangle = new("Triangles", dimvalues = c("Lower", "Upper")))
    ans.expected <- new("Intervals", dimvalues = c(2010, 2015, 2020, 2025))
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeMissingAgeTimeDimScale works with age Points and cohort Intervals", {
    makeMissingAgeTimeDimScale <- dembase:::makeMissingAgeTimeDimScale
    ans.obtained <- makeMissingAgeTimeDimScale(age = new("Points", dimvalues = c(0, 1, 2)),
                                               cohort = new("Intervals", dimvalues = c(2000, 2001)))
    ans.expected <- new("Intervals", dimvalues = c(2000, 2001, 2002, 2003))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- makeMissingAgeTimeDimScale(age = new("Points", dimvalues = 5),
                                               cohort = new("Intervals", dimvalues = c(2000, 2005, 2010)))
    ans.expected <- new("Intervals", dimvalues = c(2005, 2010, 2015))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- makeMissingAgeTimeDimScale(age = new("Points", dimvalues = c(10, 15)),
                                               cohort = new("Intervals", dimvalues = c(2000, 2005, 2010)))
    ans.expected <- new("Intervals", dimvalues = c(2010, 2015, 2020, 2025))
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeMissingAgeTimeDimScale works with time and cohort both Intervals - no triangles", {
    makeMissingAgeTimeDimScale <- dembase:::makeMissingAgeTimeDimScale
    ans.obtained <- makeMissingAgeTimeDimScale(time = new("Intervals", dimvalues = c(2000, 2001, 2002, 2003)),
                                               cohort = new("Intervals", dimvalues = c(2000, 2001)))
    ans.expected <- new("Points", dimvalues = c(0, 1, 2))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- makeMissingAgeTimeDimScale(time = new("Intervals", dimvalues = c(2005, 2010)),
                                               cohort = new("Intervals", dimvalues = c(2000, 2005, 2010)))
    ans.expected <- new("Points", dimvalues = c(0, 5))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- makeMissingAgeTimeDimScale(time = new("Intervals", dimvalues = c(2010, 2015)),
                                               cohort = new("Intervals", dimvalues = c(2000, 2005, 2010)))
    ans.expected <- new("Points", dimvalues = c(5, 10))
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeMissingAgeTimeDimScale works with time and cohort both Intervals - with triangles", {
    makeMissingAgeTimeDimScale <- dembase:::makeMissingAgeTimeDimScale
    ans.obtained <- makeMissingAgeTimeDimScale(time = new("Intervals", dimvalues = c(2000, 2001, 2002, 2003)),
                                               cohort = new("Intervals", dimvalues = c(2000, 2001)),
                                               triangle = new("Triangles", dimvalues = c("Lower", "Upper")))
    ans.expected <- new("Intervals", dimvalues = c(0, 1, 2, 3), isAge = TRUE)
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- makeMissingAgeTimeDimScale(time = new("Intervals", dimvalues = c(2005, 2010)),
                                               cohort = new("Intervals", dimvalues = c(2000, 2005, 2010)),
                                               triangle = new("Triangles", dimvalues = c("Lower", "Upper")))
    ans.expected <- new("Intervals", dimvalues = c(0, 5, 10), isAge = TRUE)
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- makeMissingAgeTimeDimScale(time = new("Intervals", dimvalues = c(2010, 2015)),
                                               cohort = new("Intervals", dimvalues = c(2000, 2005, 2010)),
                                               triangle = new("Triangles", dimvalues = c("Lower", "Upper")))
    ans.expected <- new("Intervals", dimvalues = c(0, 5, 10, 15))
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeMissingAgeTimeDimScale works with time Points and cohort Intervals", {
    makeMissingAgeTimeDimScale <- dembase:::makeMissingAgeTimeDimScale
    ans.obtained <- makeMissingAgeTimeDimScale(time = new("Points", dimvalues = c(2001, 2002, 2003)),
                                               cohort = new("Intervals", dimvalues = c(2000, 2001)))
    ans.expected <- new("Intervals", dimvalues = c(0, 1, 2, 3), isAge = TRUE)
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- makeMissingAgeTimeDimScale(time = new("Points", dimvalues = c(2005, 2010)),
                                               cohort = new("Intervals", dimvalues = c(2000, 2005, 2010)))
    ans.expected <- new("Intervals", dimvalues = c(0, 5, 10), isAge = TRUE)
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- makeMissingAgeTimeDimScale(time = new("Points", dimvalues = c(2010, 2015)),
                                               cohort = new("Intervals", dimvalues = c(2000, 2005, 2010)))
    ans.expected <- new("Intervals", dimvalues = c(0, 5, 10, 15))
    expect_identical(ans.obtained, ans.expected)
})










    

          

    

          

    

          


test_that("makePairIndices method for Categories works", {
    makePairIndices <- dembase:::makePairIndices
    e1 <- new("Categories", dimvalues = c("a", "b"))
    e2 <- new("Categories", dimvalues = c("b", "a"))
    expect_identical(makePairIndices(e1, e2, isCounts1 = TRUE, isCounts2 = TRUE),
                     list(1:2, 2:1))
    expect_identical(makePairIndices(e1, e2, isCounts1 = FALSE, isCounts2 = FALSE),
                     list(1:2, 2:1))
    e1 <- new("Categories", dimvalues = c("a", "b", "c"))
    e2 <- new("Categories", dimvalues = c("b", "c", "a"))
    expect_identical(makePairIndices(e1, e2, isCounts1 = TRUE, isCounts2 = FALSE),
                     list(1:3, c(3L, 1L, 2L)))
    expect_identical(makePairIndices(e1, e2, isCounts1 = FALSE, isCounts2 = FALSE),
                     list(1:3, c(3L, 1L, 2L)))
    e1 <- new("Categories")
    e2 <- new("Categories")
    expect_identical(makePairIndices(e1, e2, isCounts1 = TRUE, isCounts2 = TRUE),
                     list(integer(), integer()))
    e1 <- new("Categories")
    e2 <- new("Categories", dimvalues = c("a", "b"))
    expect_identical(makePairIndices(e1, e2, isCounts1 = FALSE, isCounts2 = TRUE),
                     list(integer(), c(0L, 0L)))
    e1 <- new("Categories", dimvalues = c("a", "b"))
    e2 <- new("Categories", dimvalues = c("c", "a", "b"))
    expect_identical(makePairIndices(e1, e2, isCounts1 = TRUE, isCounts2 = TRUE),
                     list(1:2, c(0L, 1:2)))
    expect_identical(makePairIndices(e1, e2, isCounts1 = FALSE, isCounts2 = TRUE),
                     list(1:2, c(0L, 1:2)))
    expect_identical(makePairIndices(e1, e2, isCounts1 = TRUE, isCounts2 = FALSE),
                     list(1:2, 2:3))
    expect_identical(makePairIndices(e1, e2, isCounts1 = FALSE, isCounts2 = FALSE),
                     list(1:2, 2:3))
})

test_that("makePairIndices method for Points works", {
    makePairIndices <- dembase:::makePairIndices
    e1 <- new("Points", dimvalues = 1:2)
    e2 <- new("Points", dimvalues = 1:2)
    expect_identical(makePairIndices(e1, e2, isCounts1 = TRUE, isCounts2 = FALSE),
                     list(1:2, 1:2))
    e1 <- new("Points")
    e2 <- new("Points", dimvalues = 1:2)
    expect_identical(makePairIndices(e1, e2, isCounts1 = TRUE, isCounts2 = TRUE),
                     list(integer(), rep(0L, 2)))
    expect_identical(makePairIndices(e1, e2, isCounts1 = FALSE, isCounts2 = TRUE),
                     list(integer(), rep(0L, 2)))
    expect_identical(makePairIndices(e1, e2, isCounts1 = TRUE, isCounts2 = FALSE),
                     list(integer(), integer()))
    expect_identical(makePairIndices(e1, e2, isCounts1 = FALSE, isCounts2 = FALSE),
                     list(integer(), integer()))
    e1 <- new("Points", dimvalues = 1:2)
    e2 <- new("Points", dimvalues = 2:3)
    expect_identical(makePairIndices(e1, e2, isCounts1 = TRUE, isCounts2 = TRUE),
                     list(0:1, 1:0))
    expect_identical(makePairIndices(e1, e2, isCounts1 = FALSE, isCounts2 = TRUE),
                     list(2L, 1:0))
    expect_identical(makePairIndices(e1, e2, isCounts1 = TRUE, isCounts2 = FALSE),
                     list(0:1, 1L))
    expect_identical(makePairIndices(e1, e2, isCounts1 = FALSE, isCounts2 = FALSE),
                     list(2L, 1L))
})

test_that("mergeDimScales method for Categories works", {
    mergeDimScales <- dembase:::mergeDimScales
    e1 <- new("Categories", dimvalues = c("a", "b"))
    e2 <- new("Categories", dimvalues = c("b", "a"))
    expect_identical(mergeDimScales(e1, e2), e1)
    e1 <- new("Categories")
    e2 <- new("Categories")
    expect_identical(mergeDimScales(e1, e2), e1)
    e1 <- new("Categories", dimvalues = c("c", "a", "b"))
    e2 <- new("Categories", dimvalues = c("b", "a", "d"))
    expect_identical(mergeDimScales(e1, e2),
                     new("Categories", dimvalues = c("a", "b")))
})

test_that("mergeDimScales method for Points works", {
    mergeDimScales <- dembase:::mergeDimScales
    e1 <- new("Points", dimvalues = 1:2)
    e2 <- new("Points", dimvalues = 1:2)
    expect_identical(mergeDimScales(e1, e2), e1)
    e1 <- new("Points")
    e2 <- new("Points")
    expect_identical(mergeDimScales(e1, e2), e1)
    e1 <- new("Points", dimvalues = 1:10)
    e2 <- new("Points", dimvalues = c(5, 10))
    expect_identical(mergeDimScales(e1, e2), e2)
})

test_that("stepLengths raises error when applied to inappropriate dimscales", {
  stepLengths <- dembase:::stepLengths
  expect_that(stepLengths(new("Categories", dimvalues = c("a", "b"))),
              throws_error("step lengths not defined for dimscale \"Categories\""))
  expect_that(stepLengths(new("Iterations", dimvalues = 1:4)),
              throws_error("step lengths not defined for dimscale \"Iterations\""))
  expect_that(stepLengths(new("Quantiles", dimvalues = c(0.025, 0.5, 0.975))),
              throws_error("step lengths not defined for dimscale \"Quantiles\""))
  expect_that(stepLengths(new("Triangles", dimvalues = c("Lower", "Upper"))),
              throws_error("step lengths not defined for dimscale \"Triangles\""))
})














