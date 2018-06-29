
context("Concordance-generator")

test_that("Concordance creates objects of class ManyToOne from valid inputs", {
    x <- cbind(c1 = c("a", "b", "c", "c"), c2 = c("x", "y", "x", "x"))
    expect_identical(Concordance(x),
                     new("ManyToOne",
                         classifications = c("c1", "c2"),
                         values = cbind(c("a", "b", "c"), c("x", "y", "x"))))
    x <- data.frame(c1 = c("a", "b", "c", "c"), c2 = c("x", "y", "x", "x"))
    expect_identical(Concordance(x),
                     new("ManyToOne",
                         classifications = c("c1", "c2"),
                         values = cbind(c("a", "b", "c"), c("x", "y", "x"))))
    x <- data.frame(c2 = c("x", "y", "x", "x"), c1 = c("a", "b", "c", "c"))
    expect_identical(Concordance(x),
                     new("ManyToOne",
                         classifications = c("c1", "c2"),
                         values = cbind(c("a", "b", "c"), c("x", "y", "x"))))
})

test_that("Concordance creates objects of class OneToOne from valid inputs", {
    x <- cbind(c1 = c("a", "b", "c", "a"), c2 = c("x", "y", "z", "x"))
    expect_identical(Concordance(x),
                     new("OneToOne",
                         classifications = c("c1", "c2"),
                         values = cbind(c("a", "b", "c"), c("x", "y", "z"))))
    x <- data.frame(c2 = c("x", "y", "z", "x"), c1 = c("a", "b", "c", "a"))
    expect_identical(Concordance(x),
                     new("OneToOne",
                         classifications = c("c2", "c1"),
                         values = cbind(c("x", "y", "z"), c("a", "b", "c"))))
    x <- matrix(nrow = 0, ncol = 2, dimnames = list(NULL, c("c1", "c2")))
    expect_identical(Concordance(x),
                     new("OneToOne",
                         classifications = c("c1", "c2"),
                         values = matrix(character(), nrow = 0, ncol = 2)))
})

test_that("Concordance throws appropriate errors", {
    x <- array(1:27, dim = rep(3, 3))
    expect_error(Concordance(x),
                 "does not have two dimensions")
    x <- matrix(1:27, nrow = 9)
    expect_error(Concordance(x),
                 "does not have two columns")
    x <- matrix(1:20, nrow = 10)
    expect_error(Concordance(x),
                 "does not have colnames")
    x <- matrix(1:20, nrow = 10)
    colnames(x) <- c(NA, "b")
    expect_error(Concordance(x),
                 "colnames have missing values")
    x <- matrix(1:20, nrow = 10)
    colnames(x) <- c("b", "b")
    expect_error(Concordance(x),
                 "colnames have duplicates")
    x <- matrix(1:20, nrow = 10)
    colnames(x) <- c("b", "")
    expect_error(Concordance(x),
                 "colnames have blanks")
    x <- data.frame(c2 = c("x", "y", "z", "x"), c1 = c("a", "b", "c", NA))
    expect_error(Concordance(x),
                 "missing values")
    x <- data.frame(c2 = c("x", "y", "y"), c1 = c("a", "b", "a"))
    expect_error(Concordance(x),
                 "relationship neither one-to-one nor many-to-one")
})
