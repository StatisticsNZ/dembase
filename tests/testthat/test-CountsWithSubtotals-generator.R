

context("CountsWithSubtotals-generator")
n.test <- 5
test.identity <- FALSE


test_that("attachSubtotals works with object of class Counts", {
    metadata <- dembase:::metadata
    makeTransform <- dembase:::makeTransform
    makeCollapseTransformExtra <- dembase:::makeCollapseTransformExtra
    Concordance <- classconc::Concordance
    ## whole subarray missing
    x <- Counts(array(1:60,
                      dim = 5:3,
                      dimnames = list(a = 1:5, b = 1:4, c = 1:3)))
    x[1:20] <- NA
    subtotals <- CountsOne(values = 50, labels = "1", name = "c")
    ans.obtained <- attachSubtotals(x, subtotals = subtotals)
    ans.expected <- new("CountsWithSubtotals",
                        x,
                        subtotals = as.integer(subtotals),
                        subtotalsNet = as.integer(subtotals),
                        metadataSubtotals = metadata(subtotals),
                        transformSubtotals = makeCollapseTransformExtra(makeTransform(x = x, y = subtotals, subset = TRUE)))
    expect_identical(ans.obtained, ans.expected)
    ## one value missing
    x <- Counts(array(1:60,
                      dim = 5:3,
                      dimnames = list(a = 1:5, b = 1:4, c = 1:3)))
    x[1] <- NA
    subtotals <- CountsOne(values = 220, labels = "1", name = "c")
    ans.obtained <- attachSubtotals(x, subtotals = subtotals)
    ans.expected <- new("CountsWithSubtotals",
                        x,
                        subtotals = as.integer(subtotals),
                        subtotalsNet = as.integer(subtotals) - sum(x[2:20]),
                        metadataSubtotals = metadata(subtotals),
                        transformSubtotals = makeCollapseTransformExtra(makeTransform(x = x, y = subtotals, subset = TRUE)))
    expect_identical(ans.obtained, ans.expected)
    ## three values missing
    x <- Counts(array(1:15,
                      dim = c(5, 3),
                      dimnames = list(age = 0:4, reg = c("a", "b", "c"))),
                dimscales = c(age = "Intervals"))
    x[1:3] <- NA
    subtotals <- Counts(array(10, dim = c(1, 1), dimnames = list(age = "0-3", reg = "a")))
    ans.obtained <- attachSubtotals(x, subtotals = subtotals)
    ans.expected <- new("CountsWithSubtotals",
                        x,
                        subtotals = as.integer(subtotals),
                        subtotalsNet = as.integer(subtotals) - 4L,
                        metadataSubtotals = metadata(subtotals),
                        transformSubtotals = makeCollapseTransformExtra(makeTransform(x = x, y = subtotals, subset = TRUE)))
    expect_identical(ans.obtained, ans.expected)
    ## more complicated transform
    x <- Counts(array(1:15,
                      dim = c(5, 3),
                      dimnames = list(age = 0:4, reg = c("a", "b", "c"))),
                dimscales = c(age = "Intervals"))
    x[c(1:4, 6:9)] <- NA
    subtotals <- Counts(array(10:13,
                              dim = c(2, 2),
                              dimnames = list(reg = c("a", "b"),
                                  age = c("0-1", "2-3"))))
    ans.obtained <- attachSubtotals(x, subtotals = subtotals)
    ans.expected <- new("CountsWithSubtotals",
                        x,
                        subtotals = as.integer(subtotals),
                        subtotalsNet = as.integer(subtotals),
                        metadataSubtotals = metadata(subtotals),
                        transformSubtotals = makeCollapseTransformExtra(makeTransform(x = x, y = subtotals, subset = TRUE)))
    expect_identical(ans.obtained, ans.expected)
    ## concordance
    x <- Counts(array(1:15,
                      dim = c(5, 3),
                      dimnames = list(age = 0:4, reg = c("a", "b", "c"))))
    x[c(1:4, 6:9)] <- NA
    subtotals <- Counts(array(10:11,
                              dim = c(1, 2),
                              dimnames = list(reg = "A",
                                  age = c("0-1", "2-3"))))
    conc <- Concordance(data.frame(from = c("a", "b", "c"), to = c("A", "A", "B")))
    concordances <- list(reg = conc)
    ans.obtained <- attachSubtotals(x, subtotals = subtotals, concordance = concordances)
    ans.expected <- new("CountsWithSubtotals",
                        x,
                        subtotals = as.integer(subtotals),
                        subtotalsNet = as.integer(subtotals),
                        metadataSubtotals = metadata(subtotals),
                        transformSubtotals = makeCollapseTransformExtra(makeTransform(x = x, y = subtotals,
                            subset = TRUE, concordances = concordances)))
    expect_identical(ans.obtained, ans.expected)
})

test_that("attachSubtotals throws appropriate errors with subtotals of class Counts", {
    x <- Counts(array(1:60,
                      dim = 5:3,
                      dimnames = list(a = 1:5, b = 1:4, c = 1:3)))
    x[1:20] <- NA
    subtotals <- CountsOne(values = 50, labels = "1", name = "c")
    x <- attachSubtotals(x, subtotals = subtotals)
    ## 'object' has no "iteration" or "quantile" dimensions
    x.wrong <- addDimension(x, name = "quantile", labels = "50%", dimtype = "quantile")
    expect_error(attachSubtotals(x.wrong, subtotals),
                 "'object' has dimension with dimtype \"quantile\"")
    ## 'object' has only integer values
    x.wrong <- x
    x.wrong[1] <- 1.1
    expect_error(attachSubtotals(x.wrong, subtotals = subtotals),
                 "'object' has non-integer values")
    ## 'object' has no negative values
    x.wrong <- x
    x.wrong[1] <- -1L
    expect_error(attachSubtotals(x.wrong, subtotals = subtotals),
                 "'object' has negative values")
    ## subtotals has no "iteration" or "quantile" dimensions
    subtotals.wrong <- addDimension(subtotals, name = "iteration", labels = "1", dimtype = "iteration")
    expect_error(attachSubtotals(x, subtotals.wrong),
                 "'subtotals' has dimension with dimtype \"iteration\"")
    ## 'subtotals' has no missing values
    subtotals.wrong <- subtotals
    subtotals.wrong[1] <- NA
    expect_error(attachSubtotals(x, subtotals.wrong),
                 "'subtotals' has missing values")
    ## 'subtotals' has only integer values
    subtotals.wrong <- subtotals
    subtotals.wrong[1] <- 1.1
    expect_error(attachSubtotals(x, subtotals.wrong),
                 "'subtotals' has non-integer values")
    ## 'subtotals' has no negative values
    subtotals.wrong <- subtotals
    subtotals.wrong[1] <- -1
    expect_error(attachSubtotals(x, subtotals.wrong),
                 "'subtotals' has negative values")
    ## transform not one-to-one
    x <- Counts(array(1:60,
                      dim = 5:3,
                      dimnames = list(a = 1:5, b = 1:4, c = 1:3)))
    x[1:5] <- NA
    subtotals <- Counts(array(1:5,
                              dim = c(5, 1, 1),
                              dimnames = list(a = 1:5, b = 1, c = 1)))
    expect_error(attachSubtotals(x, subtotals = subtotals),
                 "'object' has one-to-one relationship with 'subtotals'")
})

test_that("attachSubtotals raises error with object of class Values", {
    x <- Counts(array(1:60,
                      dim = 5:3,
                      dimnames = list(a = 1:5, b = 1:4, c = 1:3)))
    subtotals <- ValuesOne(values = 500, labels = "1", name = "c")
    expect_error(attachSubtotals(x, subtotals = subtotals),
                 "'subtotals' has class \"Values\"")
})


test_that("attachSubtotals works with object of class numeric", {
    metadata <- dembase:::metadata
    makeTransform <- dembase:::makeTransform
    makeCollapseTransformExtra <- dembase:::makeCollapseTransformExtra
    Concordance <- classconc::Concordance
    ## whole subarray missing
    x <- Counts(array(1:60,
                      dim = 5:3,
                      dimnames = list(a = 1:5, b = 1:4, c = 1:3)))
    x[1:20] <- NA
    subtotals <- sum(x, na.rm = TRUE) + 5
    ans.obtained <- attachSubtotals(x, subtotals = subtotals)
    ans.expected <- new("CountsWithSubtotals",
                        x,
                        subtotals = as.integer(subtotals),
                        subtotalsNet = 5L,
                        metadataSubtotals = NULL,
                        transformSubtotals = makeCollapseTransformExtra(makeTransform(x = x, y = subtotals, subset = TRUE)))
    expect_identical(ans.obtained, ans.expected)
    ## one value missing
    x <- Counts(array(1:60,
                      dim = 5:3,
                      dimnames = list(a = 1:5, b = 1:4, c = 1:3)))
    x[1] <- NA
    subtotals <- 2500
    ans.obtained <- attachSubtotals(x, subtotals = subtotals)
    ans.expected <- new("CountsWithSubtotals",
                        x,
                        subtotals = as.integer(subtotals),
                        subtotalsNet = 2500L - sum(x[2:60]),
                        metadataSubtotals = NULL,
                        transformSubtotals = makeCollapseTransformExtra(makeTransform(x = x, y = subtotals,
                            subset = FALSE)))
    expect_identical(ans.obtained, ans.expected)
    ## three values missing
    x <- Counts(array(1:15,
                      dim = c(5, 3),
                      dimnames = list(age = 0:4, reg = c("a", "b", "c"))),
                dimscales = c(age = "Intervals"))
    x[1:3] <- NA
    subtotals <- 200
    ans.obtained <- attachSubtotals(x, subtotals = subtotals)
    ans.expected <- new("CountsWithSubtotals",
                        x,
                        subtotals = as.integer(subtotals),
                        subtotalsNet = 200L - sum(x, na.rm = T),
                        metadataSubtotals = NULL,
                        transformSubtotals = makeCollapseTransformExtra(makeTransform(x = x, y = subtotals, subset = TRUE)))
    expect_identical(ans.obtained, ans.expected)
    ## concordance
    x <- Counts(array(1:15,
                      dim = c(5, 3),
                      dimnames = list(age = 0:4, reg = c("a", "b", "c"))),
                dimscales = c(age = "Intervals"))
    subtotals <- sum(x[c(1:4, 6:9, 11:14)])
    x[c(1:4, 6:9)] <- NA
    conc <- Concordance(data.frame(from = c("a", "b", "c"), to = c("A", "A", "B")))
    concordances <- list(reg = conc)
    expect_warning(attachSubtotals(x,
                                   subtotals = subtotals,
                                   concordance = concordances),
                   "'concordances' argument ignored when 'subtotals' has class \"integer\"")
})

test_that("attachSubtotals throws appropriate errors with subtotals of class numeric", {
    x <- Counts(array(1:60,
                      dim = 5:3,
                      dimnames = list(a = 1:5, b = 1:4, c = 1:3)))
    x[1:20] <- NA
    subtotals <- 2500
    x <- attachSubtotals(x, subtotals = subtotals)
    ## subtotals has length 1
    expect_error(attachSubtotals(x, 1:2),
                 "'subtotals' does not have length 1")
    ## 'subtotals' has no missing values
    expect_error(attachSubtotals(x, as.numeric(NA)),
                 "'subtotals' is missing")
    ## 'subtotals' has only integer values
    expect_error(attachSubtotals(x, 1.1),
                 "'subtotals' is not an integer")
    ## 'subtotals' has no negative values
    expect_error(attachSubtotals(x, -1),
                 "'subtotals' is negative")
    ## 'subtotals' at least as large as collapsed values
    expect_error(attachSubtotals(x, subtotals = 3),
                 "'subtotals' is less than the sum of 'object'")
})


