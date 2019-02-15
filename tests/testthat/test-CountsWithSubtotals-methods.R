
context("CountsWithSubtotals-methods")
n.test <- 5
test.identity <- FALSE


test_that("Extract works as expected", {
    x <- Counts(array(1:6, dim = 2:3, dimnames = list(sex = c("f", "m"), age = 0:2)))
    x[1,] <- NA
    subtotals <- CountsOne(values = 10, labels = "f", name = "sex")
    X <- attachSubtotals(x, subtotals = subtotals)
    expect_identical(X[1,], x[1,])
    expect_identical(X[2:1, 2:1], x[2:1, 2:1])
    expect_identical(X[,], x[,])
    expect_identical(X[TRUE, FALSE, drop = TRUE], x[TRUE, FALSE, drop = TRUE])
})

test_that("impute works with CountsWithSubtotals", {
    collapse <- dembase:::collapse
    x <- Counts(array(1:15,
                      dim = c(5, 3),
                      dimnames = list(age = 0:4, reg = c("a", "b", "c"))),
                dimscales = c(age = "Intervals"))
    x[c(1:4, 6:7)] <- NA
    subtotals <- Counts(array(c(10, 25),
                              dim = c(1, 2),
                              dimnames = list(reg = "A",
                                  age = c("0-1", "2-3"))))
    conc <- Concordance(data.frame(from = c("a", "b", "c"), to = c("A", "A", "B")))
    concordances <- list(reg = conc)
    x <- attachSubtotals(x, subtotals = subtotals, concordance = concordances)
    transform <- x@transformSubtotals
    ## without max
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- impute(x)
        expect_equal(as.integer(collapse(ans.obtained@.Data, transform)),
                     as.integer(subtotals))
    }
    ## with max
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- impute(x, max = (1:15) + 1L)
        expect_equal(as.integer(collapse(ans.obtained@.Data, transform)),
                     as.integer(subtotals))
        expect_true(all(ans.obtained <= (1:15) + 1L))
    }
})
