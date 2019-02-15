
context("Concordance-methods")

test_that("as.matrix works", {
    m <- cbind(c1 = c("a", "b"), c2 = c("x", "x"))
    expect_identical(as.matrix(Concordance(m)), m)
})

test_that("as.data.frame works", {
    d <- data.frame(c1 = c("a", "b"), c2 = c("x", "x"))
    expect_identical(as.data.frame(Concordance(d)), d)
})

test_that("as.list works", {
    d <- data.frame(c1 = c("c", "b", "b"), c2 = c("A", "C", "B"))
    conc <- Concordance(d)
    ans.obtained <- as.list(conc)
    ans.expected <- list(c = "A", b = c("C", "B"))
    expect_identical(ans.obtained, ans.expected)
    d <- data.frame(c1 = c("c", "c", "c"), c2 = c("A", "C", "B"))
    conc <- Concordance(d)
    ans.obtained <- as.list(conc)
    ans.expected <- list(c = c("A", "C", "B"))
    expect_identical(ans.obtained, ans.expected)
})    

test_that("classificationFrom works", {
    d <- data.frame(c1 = c("a", "b"), c2 = c("x", "x"))
    x <- Concordance(d)
    expect_identical(classificationFrom(x), "c1")
})

test_that("classifications works", {
    d <- data.frame(c1 = c("a", "b"), c2 = c("x", "x"))
    x <- Concordance(d)
    expect_identical(classifications(x), c("c1", "c2"))
})

test_that("classifications<- works", {
    d <- data.frame(c1 = c("a", "b"), c2 = c("x", "x"))
    x <- Concordance(d)
    classifications(x) <- c("a1", "a2")
    expect_identical(classifications(x), c("a1", "a2"))
    classifications(x)[1] <- "A1"
    expect_identical(classifications(x), c("A1", "a2"))
    expect_error(classifications(x)[2] <- "A1",
                 "'classifications' has duplicates")
})

test_that("classificationTo works", {
    d <- data.frame(c1 = c("a", "b"), c2 = c("x", "x"))
    x <- Concordance(d)
    expect_identical(classificationTo(x), "c2")
})

test_that("codes works", {
    d <- data.frame(c1 = c("a", "b"), c2 = c("x", "x"))
    x <- Concordance(d)
    expect_identical(codes(x, classification = "c1"),
                     c("a", "b"))
    expect_error(codes(x, classification = c("c1", "c1")),
                 "'classification' does not have length 1")
    expect_error(codes(x, classification = NA),
                 "'classification' is missing")
    expect_error(codes(x, classification = "wrong"),
                 "'classification' outside valid range")
})

test_that("codesAmbiguous works", {
    d <- data.frame(c1 = c("a", "b"), c2 = c("x", "x"))
    x <- Concordance(d)
    expect_false(codesAmbiguous(x))
    d <- data.frame(c1 = c("a", "b"), c2 = c("x", "a"))
    x <- Concordance(d)
    expect_true(codesAmbiguous(x))
    d <- data.frame(c1 = character(), c2 = character())
    x <- Concordance(d)
    expect_false(codesAmbiguous(x))
})

test_that("translate works with ManyToOne and to missing", {
    d <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "x", "y"))
    x <- Concordance(d)
    expect_identical(translate(c("b", "a", "c", "a"), concordance = x),
                     c("x", "x", "y", "x"))
    expect_identical(translate(character(), concordance = x),
                     character())
    expect_error(translate(c("b", "a", "c", "wrong"), concordance = x),
                 sprintf("value not found in classification \"c1\" : %s", dQuote("wrong")))
    expect_error(translate(c("b", "a", "c", "wrong1", "wrong2"), concordance = x),
                 sprintf("values not found in classification \"c1\" : %s, %s",
                         dQuote("wrong1"), dQuote("wrong2")))
})

test_that("translate works with ManyToOne and to missing", {
    d <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "x", "y"))
    x <- Concordance(d)
    expect_identical(translate(c("b", "a", "c", "a"), concordance = x, to = "c2"),
                     c("x", "x", "y", "x"))
    expect_error(translate(c("b", "a", "c"), concordance = x, to = "wrong"),
                 "invalid value for 'to'")
})

test_that("translate works with OneToOne and to supplied", {
    d <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "y", "z"))
    x <- Concordance(d)
    expect_identical(translate(c("b", "a", "c", "a"), concordance = x, to = "c2"),
                     c("y", "x", "z", "x"))
    expect_identical(translate(c("y", "x", "z", "x"), concordance = x, to = "c1"),
                     c("b", "a", "c", "a"))
    expect_identical(translate(character(), concordance = x, to = "c1"),
                     character())
    expect_error(translate(c("b", "a", "c"), concordance = x, to = c("c2", "c2")),
                 "'to' does not have length 1")
    expect_error(translate(c("b", "a", "c"), concordance = x, to = "wrong"),
                 "'to' outside valid range")
    expect_error(translate(c("b", "a", "c"), concordance = x, to = NA),
                 "'to' outside valid range")
    expect_error(translate(c("b", "a", "c", "wrong"), concordance = x, to = "c2"),
                 sprintf("value not found in classification \"c1\" : %s", dQuote("wrong")))
    expect_error(translate(c("b", "a", "c", "wrong1", "wrong2"), concordance = x, to = "c2"),
                 sprintf("values not found in classification \"c1\" : %s, %s",
                         dQuote("wrong1"), dQuote("wrong2")))
})

test_that("translate works with OneToOne and to is NULL", {
    d <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "y", "z"))
    x <- Concordance(d)
    expect_identical(translate(c("b", "a", "c", "a"), concordance = x, to = NULL),
                     c("y", "x", "z", "x"))
    expect_identical(translate(c("y", "x", "z", "x"), concordance = x, to = NULL),
                     c("b", "a", "c", "a"))
    d <- data.frame(c1 = c("a", "b"), c2 = c("a", "b"))
    x <- Concordance(d)
    expect_identical(translate(c("b", "a"), concordance = x, to = NULL),
                     c("b", "a"))
    d <- data.frame(c1 = c("a", "b"), c2 = c("b", "a"))
    x <- Concordance(d)
    expect_error(translate(c("b", "a"), concordance = x, to = NULL),
                 "\"c1\" and \"c2\" both contain all values in 'object'")
    d <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "y", "z"))
    x <- Concordance(d)
    expect_error(translate(c("a", "wrong"), concordance = x, to = NULL),
                 "neither \"c1\" nor \"c2\" contain all values in 'object'")
})

test_that("translate works with OneToOne and to is missing", {
    d <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "y", "z"))
    x <- Concordance(d)
    expect_identical(translate(c("b", "a", "c", "a"), concordance = x),
                     c("y", "x", "z", "x"))})

test_that("translate works with data frames", {
    d <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "y", "z"))
    x <- Concordance(d)
    expect_identical(translate(c("b", "a", "c", "a"), concordance = d),
                     translate(c("b", "a", "c", "a"), concordance = x))
    expect_identical(translate(c("b", "a", "c", "a"), concordance = d, to = "c2"),
                     translate(c("b", "a", "c", "a"), concordance = x, to = "c2"))
    d <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "x", "y"))
    x <- Concordance(d)
    expect_identical(translate(c("b", "a", "c", "a"), concordance = d),
                     translate(c("b", "a", "c", "a"), concordance = x))
})

test_that("translate works with data frames", {
    d <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "y", "z"))
    x <- Concordance(d)
    expect_identical(translate(c("b", "a", "c", "a"), concordance = d),
                     translate(c("b", "a", "c", "a"), concordance = x))
    expect_identical(translate(c("b", "a", "c", "a"), concordance = d, to = "c2"),
                     translate(c("b", "a", "c", "a"), concordance = x, to = "c2"))
    d <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "x", "y"))
    x <- Concordance(d)
    expect_identical(translate(c("b", "a", "c", "a"), concordance = d),
                     translate(c("b", "a", "c", "a"), concordance = x))
    d <- data.frame(c1 = c("a", "b", "c"), c2 = c("x", "x", "y"), c3 = 1:3)
    expect_error(translate(c("b", "a", "c", "a"), concordance = d),
                 "could not make concordance from 'concordance' : does not have two columns")
})

test_that("translate works with matrices", {
    m <- cbind(c1 = c("a", "b", "c"), c2 = c("x", "y", "z"))
    x <- Concordance(m)
    expect_identical(translate(c("b", "a", "c", "a"), concordance = m),
                     translate(c("b", "a", "c", "a"), concordance = x))
    expect_identical(translate(c("b", "a", "c", "a"), concordance = m, to = "c2"),
                     translate(c("b", "a", "c", "a"), concordance = x, to = "c2"))
    m <- cbind(c1 = c("a", "b", "c"), c2 = c("x", "x", "y"))
    x <- Concordance(m)
    expect_identical(translate(c("b", "a", "c", "a"), concordance = m),
                     translate(c("b", "a", "c", "a"), concordance = x))
    m <- cbind(c1 = c("a", "b", "c"), c2 = c("x", "x", "y"), c3 = 1:3)
    expect_error(translate(c("b", "a", "c", "a"), concordance = m),
                 "could not make concordance from 'concordance' : does not have two columns")
})




