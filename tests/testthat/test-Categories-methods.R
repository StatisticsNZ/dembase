
context("Categories-methods")

test_that("coercion from Categories to Sexes works", {
    expect_identical(as(new("Categories", dimvalues = c("Male", "Female")), "Sexes"),
                     new("Sexes", dimvalues = c("Male", "Female")))
    expect_identical(as(new("Categories", dimvalues = "females"), "Sexes"),
                     new("Sexes", dimvalues = "females"))
    expect_identical(as(new("Categories"), "Sexes"),
                     new("Sexes"))
    expect_error(as(new("Categories", dimvalues = c("a", "b")), "Sexes"),
                 "labels not valid for dimscale")
})

test_that("coercion from Categories to Triangles works", {
  expect_that(as(new("Categories", dimvalues = c("Upper", "Lower")), "Triangles"),
              is_identical_to(new("Triangles", dimvalues = c("Upper", "Lower"))))
  expect_that(as(new("Categories", dimvalues = "TL"), "Triangles"),
              is_identical_to(new("Triangles", dimvalues = "TL")))
  expect_that(as(new("Categories"), "Triangles"),
              is_identical_to(new("Triangles")))
  expect_that(as(new("Categories", dimvalues = c("a", "b")), "Triangles"),
              throws_error("labels not valid for dimscale"))
})

test_that("coercion from Categories to Intervals works", {
  expect_that(as(new("Categories", dimvalues = c("0-4", "5-9")), "Intervals"),
              is_identical_to(new("Intervals", dimvalues = c(0, 5, 10), isAge = TRUE)))
  expect_that(as(new("Categories"), "Intervals"),
              is_identical_to(new("Intervals")))
  expect_that(as(new("Categories", dimvalues = c("a", "b")), "Intervals"),
              throws_error("labels not valid for dimscale"))
})


test_that("coercion from Categories to Points works", {
  expect_that(as(new("Categories", dimvalues = c("0", "1", "2")), "Points"),
              is_identical_to(new("Points", dimvalues = c(0, 1, 2))))
  expect_that(as(new("Categories", dimvalues = c("0-4", "5+")), "Points"),
              throws_error("labels not valid for dimscale"))
  expect_that(as(new("Categories"), "Points"),
              is_identical_to(new("Points")))
  expect_that(as(new("Categories", dimvalues = c("a", "b")), "Points"),
              throws_error("labels not valid for dimscale"))
})


test_that("coercion from Categories to Quantiles works", {
  expect_that(as(new("Categories", dimvalues = c("2.5%", "50%", "97.5%")), "Quantiles"),
              is_identical_to(new("Quantiles", dimvalues = c(0.025, 0.5, 0.975))))
  expect_that(as(new("Categories", dimvalues = c("0.05", "0.95")), "Quantiles"),
              is_identical_to(new("Quantiles", dimvalues = c(0.05, 0.95))))
  expect_that(as(new("Categories"), "Quantiles"),
              is_identical_to(new("Quantiles")))
  expect_that(as(new("Categories", dimvalues = c("a", "b")), "Quantiles"),
              throws_error("labels not valid for dimscale"))
})

test_that("coercion from Categories to Iterations works", {
  expect_that(as(new("Categories", dimvalues = c("1", "2", "3")), "Iterations"),
              is_identical_to(new("Iterations", dimvalues = 1:3)))
  expect_that(as(new("Categories", dimvalues = c("0", "1", "2", "3")), "Iterations"),
              throws_error("labels not valid for dimscale"))
  expect_that(as(new("Categories", dimvalues = c("0-4", "5+")), "Iterations"),
              throws_error("labels not valid for dimscale"))
  expect_that(as(new("Categories", dimvalues = c("1", "Inf")), "Iterations"),
              throws_error("labels not valid for dimscale"))
  expect_that(as(new("Categories"), "Iterations"),
              is_identical_to(new("Iterations")))
})

test_that("canMakeDimScalesCompatible works", {
    canMakeDimScalesCompatible <- dembase:::canMakeDimScalesCompatible
    Concordance <- classconc::Concordance
    translate <- classconc::translate
    ## no concordance
    x <- new("Categories", dimvalues = c("a", "b", "c"))
    y <- new("Categories", dimvalues = c("c", "b"))
    expect_true(canMakeDimScalesCompatible(x = x, y = y, subset = TRUE, concordance = NULL))
    expect_error(canMakeDimScalesCompatible(x = x, y = y, subset = FALSE, concordance = NULL),
                 sprintf("one dimension has value \\[%s\\] that other does not",
                         dQuote("a")))
    x <- new("Categories", dimvalues = c("a", "b", "c"))
    y <- new("Categories", dimvalues = "c")
    expect_true(canMakeDimScalesCompatible(x = x, y = y, subset = TRUE, concordance = NULL))
    expect_error(canMakeDimScalesCompatible(x = x, y = y, subset = FALSE, concordance = NULL),
                 sprintf("one dimension has values \\[%s, %s\\] that other does not",
                         dQuote("a"), dQuote("b")))
    ## has concordance, collapse = TRUE
    x <- new("Categories", dimvalues = c("a", "b", "c", "d"))
    y <- new("Categories", dimvalues = c("XX", "YY"))
    conc <- Concordance(data.frame(from = c("a", "b", "c", "d"), to = c("XX", "XX", "YY", "YY")))
    expect_true(canMakeDimScalesCompatible(x = x, y = y, subset = TRUE,
                                           collapse = TRUE, concordance = conc))
    expect_true(canMakeDimScalesCompatible(x = x, y = y, subset = FALSE,
                                           collapse = TRUE, concordance = conc))
    x <- new("Categories", dimvalues = c("a", "b", "c", "d"))
    y <- new("Categories", dimvalues = "XX")
    conc <- Concordance(data.frame(from = c("a", "b", "c", "d"), to = c("XX", "XX", "YY", "YY")))
    expect_true(canMakeDimScalesCompatible(x = x, y = y, subset = TRUE,
                                           collapse = TRUE, concordance = conc))
    expect_error(canMakeDimScalesCompatible(x = x, y = y, subset = FALSE,
                                            collapse = TRUE, concordance = conc),
                 sprintf("one dimension has value \\[%s\\] that other does not",
                         dQuote("YY")))
    x <- new("Categories", dimvalues = c("a", "b", "c", "d"))
    y <- new("Categories", dimvalues = "XX")
    conc <- Concordance(data.frame(from = c("a", "b", "c"), to = c("XX", "XX", "YY")))
    expect_error(canMakeDimScalesCompatible(x = x, y = y, subset = TRUE,
                                            collapse = TRUE, concordance = conc),
                 sprintf("value not found in classification \"from\" : %s",
                         dQuote("d")))
    ## has concordance, collapse = FALSE
    x <- new("Categories", dimvalues = c("XX", "YY"))
    y <- new("Categories", dimvalues = c("a", "b", "c", "d"))
    conc <- Concordance(data.frame(from = c("a", "b", "c", "d"), to = c("XX", "XX", "YY", "YY")))
    expect_true(canMakeDimScalesCompatible(x = x, y = y, subset = TRUE,
                                           collapse = FALSE, concordance = conc))
    expect_true(canMakeDimScalesCompatible(x = x, y = y, subset = FALSE,
                                           collapse = FALSE, concordance = conc))
    x <- new("Categories", dimvalues = "XX")
    y <- new("Categories", dimvalues = c("a", "b", "c", "d"))
    conc <- Concordance(data.frame(from = c("a", "b", "c", "d"), to = c("XX", "XX", "YY", "YY")))
    expect_error(canMakeDimScalesCompatible(x = x, y = y, subset = TRUE,
                                            collapse = FALSE, concordance = conc),
                 sprintf("one dimension has value \\[%s\\] that other does not",
                         dQuote("YY")))
    expect_error(canMakeDimScalesCompatible(x = x, y = y, subset = FALSE,
                                            collapse = FALSE, concordance = conc),
                 sprintf("one dimension has value \\[%s\\] that other does not",
                         dQuote("YY")))
    x <- new("Categories", dimvalues = "XX")
    y <- new("Categories", dimvalues = c("a", "b", "c", "d"))
    conc <- Concordance(data.frame(from = c("a", "b", "c"), to = c("XX", "XX", "YY")))
    expect_error(canMakeDimScalesCompatible(x = x, y = y, subset = TRUE,
                                            collapse = FALSE, concordance = conc),
                 sprintf("value not found in classification \"from\" : %s",
                         dQuote("d")))
})    
                
test_that("labels method for Categories works", {
    labels <- dembase:::labels
    expect_identical(labels(new("Categories", dimvalues = c("a", "b"))),
                     c("a", "b"))
    expect_identical(labels(new("Categories")),
                     character())
})


test_that("inferDimvalues method for Categories works", {
  inferDimvalues <- dembase:::inferDimvalues
  expect_that(inferDimvalues(new("Categories"), labels = c("a", "b")),
              is_identical_to(c("a", "b")))
  expect_that(inferDimvalues(new("Categories"), labels = character()),
              is_identical_to(character()))
  expect_that(inferDimvalues(new("Categories"), labels = NULL),
              is_identical_to(character()))
  expect_that(inferDimvalues(new("Categories"), labels = c("a", NA)),
              is_identical_to(NULL))
})

test_that("makeIndices method for Categories works with concordances", {
    makeIndices <- dembase:::makeIndices
    Concordance <- classconc::Concordance
    translate <- classconc::translate
    ## collapse is TRUE
    x <- new("Categories", dimvalues = c("a", "b", "c", "d"))
    y <- new("Categories", dimvalues = c("A", "B"))
    conc <- Concordance(data.frame(from = c("a", "b", "c", "d"), to = c("A", "B", "B", "C")))
    ans.obtained <- makeIndices(x = x, y = y, collapse = TRUE, concordance = conc)
    ans.expected <- c(1L, 2L, 2L, 0L)
    expect_identical(ans.obtained, ans.expected)
    ## collapse is FALSE
    x <- new("Categories", dimvalues = c("A", "B", "C"))
    y <- new("Categories", dimvalues = c("d", "a", "c", "b"))
    conc <- Concordance(data.frame(from = c("a", "b", "c", "d"), to = c("A", "B", "B", "C")))
    ans.obtained <- makeIndices(x = x, y = y, collapse = FALSE, concordance = conc)
    ans.expected <- c(3L, 1L, 2L, 2L)
    expect_identical(ans.obtained, ans.expected)
})






