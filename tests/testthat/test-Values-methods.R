
context("Values-methods")

test_that("coercion to data.frame works", {
    a <- array(1:6,
               dim = c(3, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
               sex = c("Male", "Female")))
    x <- Values(a)
    expect_identical(as(x, "data.frame"),
                     as.data.frame.table(a, responseName = "value"))
    a <- array(0L,
               dim = c(3, 0),
               dimnames = list(age = c("0-4", "5-9", "10+"),
               sex = NULL))
    x <- Values(a)
    expect_identical(as(x, "data.frame"),
                     data.frame(age = factor(), sex = factor(), value = integer()))
})

test_that("Ops works when e2 is Counts", {
    x <- Values(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(reg = c("a", "b", "c"),
                      sex = c("m", "f"))))
    y <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(reg = c("a", "b"),
                      sex = c("f", "m"))))
    z1 <- Counts(array(c(1:2, 4:5) * c(3:4, 1:2),
                       dim = c(2, 2),
                       dimnames = list(reg = c("a", "b"),
                       sex = c("m", "f"))))
    z2 <- array(c(FALSE, FALSE, TRUE, TRUE),
                dim = c(2, 2),
                dimnames = list(reg = c("a", "b"),
                sex = c("m", "f")))
    z3 <- array(TRUE,
                dim = c(2, 2),
                dimnames = list(reg = c("a", "b"),
                sex = c("m", "f")))
    expect_identical(x * y, z1)
    expect_identical(x / y, x / as(y, "Values"))
    expect_identical(x * t(y), x * y)
    expect_identical(x > y, z2)
    expect_identical(x | y, z3)
    x <- Values(array(0,
                      dim = c(0, 2),
                      dimnames = list(reg = NULL, sex = c("m", "f"))))
    y <- Counts(array(0,
                      dim = c(0, 2),
                      dimnames = list(reg = NULL, sex = c("f", "m"))))
    z1 <- Counts(array(0,
                       dim = c(0, 2),
                       dimnames = list(reg = NULL, sex = c("m", "f"))))
    z2 <- array(NA,
                dim = c(0, 2),
                dimnames = list(reg = NULL, sex = c("m", "f")))
    expect_identical(x * y, z1)
    expect_identical(x < y, z2)
    x <- Values(array(1:2,
                      dim = c(2, 2),
                      dimnames = list(sex = c("f", "m"),
                      iteration = 1:2)))
    y <- Counts(array(2:1,
                      dim = 2,
                      dimnames = list(sex = c("m", "f"))))
    z <- Counts(array(c(1L, 4L),
                      dim = c(2, 2),
                      dimnames = list(sex = c("f", "m"),
                      iteration = 1:2)))
    expect_identical(x * y, z)
})

test_that("Ops works when e2 is Values", {
    x <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(reg = c("a", "b"),
                      sex = c("m", "f"))))
    y <- Values(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(reg = c("a", "b"),
                      sex = c("f", "m"))))
    z1 <- Counts(array(c(3L, 8L),
                       dim = c(2, 2),
                       dimnames = list(reg = c("a", "b"),
                       sex = c("m", "f"))))
    z2 <- Counts(array(c(1/3, 2/4, 3/1, 4/2),
                       dim = c(2, 2),
                       dimnames = list(reg = c("a", "b"),
                       sex = c("m", "f"))))
    z3 <- array(c(FALSE, FALSE, TRUE, TRUE),
                dim = c(2, 2),
                dimnames = list(reg = c("a", "b"),
                sex = c("m", "f")))
    z4 <- array(TRUE,
                dim = c(2, 2),
                dimnames = list(reg = c("a", "b"),
                sex = c("m", "f")))
    expect_identical(x * y, z1)
    expect_identical(x / y, z2)
    expect_identical(x + y, as(x, "Counts") + y)
    expect_identical(x * t(y), x * y)
    expect_identical(x > y, z3)
    expect_identical(x | y, z4)
    x <- Counts(array(0,
                      dim = c(0, 2),
                      dimnames = list(reg = NULL, sex = c("m", "f"))))
    y <- Values(array(0,
                      dim = c(0, 2),
                      dimnames = list(reg = NULL, sex = c("f", "m"))))
    z1 <- Counts(array(0,
                       dim = c(0, 2),
                       dimnames = list(reg = NULL, sex = c("m", "f"))))
    z2 <- array(NA,
                dim = c(0, 2),
                dimnames = list(reg = NULL, sex = c("m", "f")))
    expect_identical(x * y, x)
    expect_identical(x / y, z1)
    expect_identical(x < y, z2)
})

test_that("Ops works when e2 is numeric", {
    a <- array(rnorm(4), dim = c(2, 2), dimnames = list(sex = c("f", "m"), reg = c("a", "b")))
    x <- Values(a)
    expect_identical(x * 1:4, Values(a * 1:4))
    expect_identical(x > 0, a > 0)
    expect_identical(x == 0, a == 0)
    x <- Values(array(1:2, dim = 2, dimnames = list(quantile = c("10%", "90%"))))
    expect_identical(x * 1L, x)
    expect_identical(x | 1, as(x, "array") | 1)
    expect_error(x - 1, "dimension \"quantile\" has dimtype \"quantile\"")
})

test_that("Ops works when e1 is numeric", {
    a <- array(rnorm(4), dim = c(2, 2), dimnames = list(sex = c("f", "m"), iter = 1:2))
    x <- Values(a)
    expect_identical(-1 * x, Values(-1 * a))
    expect_identical(-1 * x, -x)
    expect_identical(0 <= x, 0 <= a)
    expect_identical(0 | x, 0 | a)
    x <- Values(array(1:2, dim = 2, dimnames = list(quantile = c("10%", "90%"))))
    expect_identical(0L + x, x)
    expect_identical(0 & x, 0 & as(x, "array"))
    expect_error(1 + x, "dimension \"quantile\" has dimtype \"quantile\"")
})

test_that("Ops works when e2 is an array", {
    a <- array(rnorm(4), dim = c(2, 2), dimnames = list(sex = c("f", "m"), iter = 1:2))
    x <- Values(a)
    expect_identical(x * abs(a), Values(a * abs(a)))
    expect_identical(x ^ a, Values(a ^ a))
    expect_identical(x | a, a | a)
    expect_identical(x > a^2, a > a^2)
    a <- array(1:2, dim = 2, dimnames = list(quantile = c("10%", "90%")))
    x <- Values(a)
    expect_error(x * a, "dimension \"quantile\" has dimtype \"quantile\"")
    expect_identical(x | a, a | a)
})

test_that("Ops works when e1 is an array", {
    a <- array(1:2, dim = c(2, 2), dimnames = list(sex = c("f", "m"), iter = 1:2))
    x <- Values(a)
    b <- array(1L, dim = 2, dimnames = list(sex = c("f", "m")))
    expect_identical(x * b, x)
    expect_identical(a ^ x, Values(a ^ a))
    expect_identical(a == x, a == a)
    expect_identical(x^2 < a, a^2 < a)
    a <- array(1:2, dim = 2, dimnames = list(quantile = c("10%", "90%")))
    x <- Values(a)
    expect_error(a * x, "dimension \"quantile\" has dimtype \"quantile\"")
    expect_identical(a == x, a == a)
})

test_that("Ops works with tables and xtabs", {
    d <- data.frame(expand.grid(sex = c("m", "f"), reg = 1:2), value = rnorm(4))
    xt <- xtabs(value ~ sex + reg, d)
    tab <- table(d$sex, d$reg)
    x <- Values(xt)
    expect_identical(x * xt, Values(xt * xt))
    expect_identical(tab + x, Values(as(x, "array") + tab))
    expect_identical(x | xt, as(xt | xt, "array"))
})

test_that("addDimension works when all arguments supplied", {
    a1 <- array(1:6,
                dim = c(3, 2),
                dimnames = list(age = c("0-4", "5-9", "10+"),
                sex = c("Male", "Female")))
    a2 <- array(1:6,
                dim = c(3, 2, 3),
                dimnames = list(age = c("0-4", "5-9", "10+"),
                sex = c("Male", "Female"),
                region = c("Region 1", "Region 2", "Region 3")))
    x1 <- Values(a1)
    x2 <- Values(a2)
    expect_identical(addDimension(x1,
                                  name = "region",
                                  labels = c("Region 1", "Region 2", "Region 3"),
                                  scale = rep(1L, 3),
                                  dimtype = "state",
                                  dimscale = "Categories"),
                     x2)
})

test_that("addDimension works when dimtype not supplied", {
    a1 <- array(1:6,
                dim = c(3, 2),
                dimnames = list(age = c("0-4", "5-9", "10+"),
                sex = c("Male", "Female")))
    a2 <- array(1:6,
                dim = c(3, 2, 3),
                dimnames = list(age = c("0-4", "5-9", "10+"),
                sex = c("Male", "Female"),
                region = c("Region 1", "Region 2", "Region 3")))
    x1 <- Values(a1)
    x2 <- Values(a2)
    expect_identical(addDimension(x1,
                                  name = "region",
                                  labels = c("Region 1", "Region 2", "Region 3"),
                                  scale = rep(1L, 3),
                                  dimscale = "Categories"),
                     x2)
})

test_that("addDimension works when dimtype and dimscale not supplied", {
    a1 <- array(1:6,
                dim = c(3, 2),
                dimnames = list(age = c("0-4", "5-9", "10+"),
                sex = c("Male", "Female")))
    a2 <- array(1:6,
                dim = c(3, 2, 3),
                dimnames = list(age = c("0-4", "5-9", "10+"),
                sex = c("Male", "Female"),
                region = c("Region 1", "Region 2", "Region 3")))
    x1 <- Values(a1)
    x2 <- Values(a2)
    expect_identical(addDimension(x1,
                                  name = "region",
                                  labels = c("Region 1", "Region 2", "Region 3"),
                                  scale = rep(1L, 3)),
                     x2)
})

test_that("addDimension works when scale, dimtype, and dimscale not supplied", {
    a1 <- array(1:6,
                dim = c(3, 2),
                dimnames = list(age = c("0-4", "5-9", "10+"),
                sex = c("Male", "Female")))
    a2 <- array(1:6,
                dim = c(3, 2, 3),
                dimnames = list(age = c("0-4", "5-9", "10+"),
                sex = c("Male", "Female"),
                region = c("Region 1", "Region 2", "Region 3")))
    x1 <- Values(a1)
    x2 <- Values(a2)
    expect_identical(addDimension(x1,
                                  name = "region",
                                  labels = c("Region 1", "Region 2", "Region 3")),
                     x2)
})

test_that("addDimension works when 'after' supplied", {
    a <- array(1:6,
               dim = c(3, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
               sex = c("Male", "Female")))
    x <- Values(a)
    expect_identical(addDimension(x,
                                  name = "region",
                                  labels = c("Region 1", "Region 2", "Region 3"),
                                  after = 2),
                     addDimension(x,
                                  name = "region",
                                  labels = c("Region 1", "Region 2", "Region 3")))
    expect_identical(addDimension(x,
                                  name = "region",
                                  labels = c("Region 1", "Region 2", "Region 3"),
                                  after = "age"),
                     aperm(addDimension(x,
                                        name = "region",
                                        labels = c("Region 1", "Region 2", "Region 3")),
                           perm = c(1, 3, 2)))
})

test_that("addDimension works when two dimensions added", {
    a1 <- array(1:6,
                dim = c(3, 2),
                dimnames = list(age = c("0-4", "5-9", "10+"),
                sex = c("Male", "Female")))
    a2 <- array(1:6,
                dim = c(3, 2, 3, 3),
                dimnames = list(age = c("0-4", "5-9", "10+"),
                sex = c("Male", "Female"),
                reg_orig = c("Region 1", "Region 2", "Region 3"),
                reg_dest = c("Region 1", "Region 2", "Region 3")))
    a3 <- array(as.integer(a1 %o% 1:3 %o% 2:4),
                dim = dim(a2),
                dimnames = dimnames(a2))
    a4 <- array(as.integer(a1 %o% 1:3 %o% 1:3),
                dim = dim(a2),
                dimnames = dimnames(a2))
    x1 <- Values(a1)
    x2 <- Values(a2)
    x3 <- Values(a3)
    x4 <- Values(a4)
    expect_identical(addDimension(x1,
                                  name = c("reg_orig", "reg_dest"),
                                  labels = c("Region 1", "Region 2", "Region 3")),
                     x2)
    expect_identical(addDimension(x1,
                                  name = c("reg_orig", "reg_dest"),
                                  labels = list(c("Region 1", "Region 2", "Region 3"),
                                  c("Region 1", "Region 2", "Region 3"))),
                     x2)
    expect_identical(addDimension(x1,
                                  name = c("reg_orig", "reg_dest"),
                                  labels = c("Region 1", "Region 2", "Region 3"),
                                  scale = list(1:3, 2:4)),
                     x3)
    expect_identical(addDimension(x1,
                                  name = c("reg_orig", "reg_dest"),
                                  labels = c("Region 1", "Region 2", "Region 3"),
                                  scale = 1:3),
                     x4)
    expect_identical(addDimension(x1,
                                  name = c("reg_orig", "reg_dest"),
                                  labels = c("Region 1", "Region 2", "Region 3"),
                                  after = "age"),
                     aperm(x2, c(1, 3, 4, 2)))
})

test_that("addDimension works with 0-length dimensions", {
  a1 <- array(0,
              dim = c(3, 0),
              dimnames = list(age = c("0-4", "5-9", "10+"),
                sex = NULL))
  a2 <- array(0,
              dim = c(3, 0, 3),
              dimnames = list(age = c("0-4", "5-9", "10+"),
                sex = NULL,
                region = c("Region 1", "Region 2", "Region 3")))
  x1 <- Values(a1)
  x2 <- Values(a2)
  expect_that(addDimension(x1,
                           name = "region",
                           labels = c("Region 1", "Region 2", "Region 3")),
              is_identical_to(x2))
})

test_that("addDimension throws appropriate errors", {
  a <- array(1:6,
             dim = c(3, 2),
             dimnames = list(age = c("0-4", "5-9", "10+"),
               sex = c("Male", "Female")))
  x <- Values(a)
  expect_error(addDimension(x,
                           name = "region",
                           labels = list(c("Region 1", "Region 2", "Region 3"),
                             c("Region 1", "Region 2", "Region 3"))),
              "'labels' has more elements than 'name'")
  expect_error(addDimension(x,
                           name = "region",
                           labels = c("Region 1", "Region 2", "Region 3"),
                           scale = list(1:3, 1)),
              "'scale' has more elements than 'name'")
  expect_error(addDimension(x,
                           name = "region",
                           labels = c("Region 1", "Region 2", "Region 3"),
                           dimtype = rep("state", 2)),
              "'dimtype' has more elements than 'name'")
  expect_error(addDimension(x,
                           name = "region",
                           labels = c("Region 1", "Region 2", "Region 3"),
                           dimscale = rep("state", 2)),
              "'dimscale' has more elements than 'name'")
  expect_error(addDimension(x,
                           name = "reg_orig",
                           labels = list(c("Region 1", "Region 2", "Region 3"))),
              "dimension \"reg_orig\" lacks pair")
  expect_error(addDimension(x,
                            name = "region",
                            labels = c("Region 1", "Region 2"),
                            after = 1:2),
               "'after' does not have length 1")
  expect_error(addDimension(x,
                            name = "region",
                            labels = c("Region 1", "Region 2"),
                            after = "wrong"),
               "'after' outside valid range")
  expect_error(addDimension(x,
                            name = "region",
                            labels = c("Region 1", "Region 2"),
                            after = NA),
               "'after' outside valid range")
  expect_error(addDimension(x,
                            name = "region",
                            labels = c("Region 1", "Region 2"),
                            after = 3),
               "'after' outside valid range")
})

test_that("as.data.frame works", {
    a <- array(1:12,
               dim = c(3, 2, 2),
               dimnames = list(period = 2000:2002,
               sex = c("Male", "Female"),
               region = c("a", "b")))
    x <- Values(a, dimscales = c(period = "Points"))
    df <- data.frame(expand.grid(period = as.numeric(2000:2002),
                                 sex = c("Male", "Female"),
                                 region = c("a", "b")),
                     value = 1:12)
    expect_identical(as.data.frame(x, direction = "long", midpoints = TRUE),
                     df)
    a <- array(1:12,
               dim = c(3, 2, 2),
               dimnames = list(period = 2000:2002,
               sex = c("Male", "Female"),
               region = c("a", "b")))
    x <- Values(a, dimscales = c(period = "Intervals"))
    df <- data.frame(expand.grid(period = c(1999.5, 2000.5, 2001.5),
                                 sex = c("Male", "Female"),
                                 region = c("a", "b")),
                     value = 1:12)
    expect_identical(as.data.frame(x, direction = "long", midpoints = TRUE),
                     df)
    a <- array(0L,
               dim = c(0, 2),
               dimnames = list(age = NULL,
               sex = c("Male", "Female")))
    x <- Values(a)
    expect_identical(as.data.frame(x, direction = "long"),
                     data.frame(age = factor(), sex = factor(), value = integer()))
    a <- array(1:2, dim = c(2, 1), dimnames = list(age = c("0-4", "5+"), period = "2001-2010"))
    d <- data.frame(age = c("0-4", "5+"), period = c(2005, 2005), value = 1:2)
    x <- Values(a)
    expect_identical(as.data.frame(x, midpoints = "period", direction = "long"), d)
    a <- array(1:2, dim = c(2, 1), dimnames = list(age = c("0-4", "5+"), period = "2001-2010"))
    d <- data.frame(age = c("0-4", "5+"), period = c(2005, 2005), val = 1:2)
    x <- Values(a)
    expect_identical(as.data.frame(x, responseName = "val",
                                   midpoints = "period", direction = "long"), d)
})

test_that("canMakeCompatible works in simple cases", {
    canMakeCompatible <- dembase:::canMakeCompatible
    canMakePairCompatible <- dembase:::canMakePairCompatible
    x <- Values(array(0,
                      dim = c(2, 2),
                      dimnames = list(age = c("0-4", "5+"),
                          sex = c("m", "f"))))
    y <- Values(array(0,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          sex = c("f", "m"))))
    expect_true(canMakeCompatible(x, y))
    expect_true(canMakeCompatible(x, t(x)))
    expect_error(canMakeCompatible(y, x),
                 paste("\"age\" dimensions have incompatible dimscales :",
                       "one dimension has break \\[10\\] that other does not"))
    x.wrong <- x
    names(x.wrong)[1] <- "wrong"
    expect_error(canMakeCompatible(x, x.wrong),
                 sprintf("one object has dimension \\[%s\\] that other does not",
                         dQuote("age")))
    x.wrong <- x
    dimtypes(x.wrong)[1] <- "state"
    expect_error(canMakeCompatible(x, x.wrong),
                 sprintf("%s dimensions have different dimtypes : %s versus %s",
                         dQuote("age"), dQuote("age"), dQuote("state")))
    x <- Values(array(0, dim = 3, dimnames = list(region = c("a", "b", "c"))))
    y <- Values(array(0, dim = 2, dimnames = list(region = c("a", "c"))))
    expect_error(canMakeCompatible(x, y),
                 sprintf("\"region\" dimensions have incompatible dimscales : one dimension has value \\[%s\\] that other does not",
                         dQuote("b")))
    expect_true(canMakeCompatible(x, y, subset = TRUE))
    x <- Values(array(0,
                      dim = c(3, 3),
                      dimnames = list(region = c("a", "b", "c"),
                          quantile = c("2.5%", "50%", "97.5%"))))
    y <- Values(array(0, dim = 3, dimnames = list(region = c("a", "b", "c"))))
    expect_error(canMakeCompatible(x, y),
                 "dimension \"quantile\" has dimtype \"quantile\"")
    expect_error(canMakeCompatible(y, x),
                 "dimension \"quantile\" has dimtype \"quantile\"")
    x <- Values(array(1:2, dim = 2, dimnames = list(sex = c("f", "m"))))
    y <- Values(array(1:2, dim = 2, dimnames = list(age = c("0-4", "5+"))))
    expect_true(canMakePairCompatible(e1 = x, e2 = y))
})

test_that("canMakeCompatible where e2 is Values works with 0-length dimensions", {
    canMakeCompatible <- dembase:::canMakeCompatible
    x <- Values(array(0,
                      dim = c(2, 0),
                      dimnames = list(age = c("0-4", "5+"),
                      sex = NULL)))
    y <- Values(array(0,
                      dim = c(2, 2),
                      dimnames = list(age = c("0-4", "5+"),
                      sex = c("f", "m"))))
    expect_error(canMakeCompatible(x, y),
                 sprintf("\"sex\" dimensions have incompatible dimscales : one dimension has values \\[%s, %s\\] that other does not",
                         dQuote("f"), dQuote("m")))
    expect_error(canMakeCompatible(y, x),
                 sprintf("\"sex\" dimensions have incompatible dimscales : one dimension has values \\[%s, %s\\] that other does not",
                         dQuote("f"), dQuote("m")))
})

test_that("canMakeCompatible works with Iterations", {
    canMakeCompatible <- dembase:::canMakeCompatible
    x <- Values(array(0,
                      dim = 2,
                      dimnames = list(age = c("0-4", "5+"))))
    y <- Values(array(0,
                      dim = c(2, 2),
                      dimnames = list(age = c("0-4", "5+"), sim = 1:2)))
    expect_true(canMakeCompatible(x, y))
    expect_true(canMakeCompatible(y, x))
})

test_that("canMakeOrigDestParentChildCompatible works", {
    canMakeOrigDestParentChildCompatible <- dembase:::canMakeOrigDestParentChildCompatible
    ## simple, orig-dest only
    x <- Values(array(0,
                      dim = c(3, 2, 2, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          region_orig = c("a", "b"),
                          region_dest = c("a", "b"),
                          time = c("2001-2005", "2006-2010"))))
    y <- Counts(array(0,
                      dim = c(3, 2, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          region = c("a", "b"),
                          time = c("2001-2005", "2006-2010"))))
    expect_true(canMakeOrigDestParentChildCompatible(x = x, y = y, subset = TRUE))
    ## orig-dest only; need to permute, and extend
    x <- Values(array(0,
                      dim = c(2, 3, 2),
                      dimnames = list(time = c("2001-2005", "2006-2010"),
                          region_orig = c("a", "b", "c"),
                          region_dest = c("c", "b"))))
    y <- Counts(array(0,
                      dim = c(3, 2, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          region = c("a", "b"),
                          time = c("2001-2005", "2006-2010"))))
    expect_true(canMakeOrigDestParentChildCompatible(x = x, y = y, subset = TRUE))
    ## orig-dest and parent-child; need to subset and permute
    x <- Values(array(0,
                      dim = c(2, 2, 3, 2, 3, 2),
                      dimnames = list(time = c("2001-2005", "2006-2010"),
                          eth_child = 1:2,
                          eth_parent = 3:1,
                          age = c("0-9", "10+"),
                          region_orig = c("a", "b", "c"),
                          region_dest = c("c", "b"))))
    y <- Counts(array(0,
                      dim = c(3, 2, 2, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          eth = 2:1,
                          region = c("a", "b"),
                          time = c("2001-2005", "2006-2010"))))
    expect_true(canMakeOrigDestParentChildCompatible(x = x, y = y, subset = TRUE))
    ## y has parent dimension
    x <- Counts(array(0,
                      dim = c(2, 2, 3, 3, 3, 2),
                      dimnames = list(time = c("2001-2005", "2006-2010"),
                          eth_child = 1:2,
                          eth_parent = 3:1,
                          age = c("0-4", "5-9", "10+"),
                          region_orig = c("a", "b", "c"),
                          region_dest = c("c", "b"))))
    y <- Counts(array(0,
                      dim = c(3, 2, 2, 2, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          eth_parent = 2:1,
                          eth_child = 1:2,
                          region = c("a", "b"),
                          time = c("2001-2005", "2006-2010"))))
    expect_error(canMakeOrigDestParentChildCompatible(x = x, y = y, subset = TRUE),
                 "'y' has dimension with dimtype \"parent\"")
})

test_that("canMakePairCompatible works in simple cases", {
    canMakePairCompatible <- dembase:::canMakePairCompatible
    x <- Values(array(0,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-14", "15-19"),
                                      sex = c("m", "f"))))
    y <- Values(array(0,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10-19"),
                                      sex = c("f", "m"))))
    expect_true(canMakePairCompatible(x, y))
    expect_true(canMakePairCompatible(y, x))
    x <- Values(array(0,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-14", "15-19"),
                                      sex = c("m", "f"))))
    y <- Counts(array(0,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10-19"),
                                      sex = c("f", "m"))))
    expect_error(canMakePairCompatible(x, y),
                 paste("\"age\" dimensions have incompatible dimscales :",
                       "intervals do not align"))
    x <- Values(array(0,
                      dim = c(4, 2),
                      dimnames = list(age = c("0-4", "5-14", "15-19", "20+"),
                                      sex = c("m", "f"))),
                dimtypes = c(sex = "state"))
    y <- Values(array(0,
                      dim = c(2, 3),
                      dimnames = list(age = c("5-9", "10-19"),
                                      sex = c("f", "m", "other"))),
                dimtypes = c(sex = "state"))
    expect_true(canMakePairCompatible(x, y))
    expect_true(canMakePairCompatible(y, x))
    x <- Values(array(0,
                      dim = c(4, 2),
                      dimnames = list(age = c("0-4", "5-14", "15-19", "20+"),
                                      sex = c("m", "f"))),
                dimtypes = c(sex = "state"))
    y <- Counts(array(0,
                      dim = c(2, 3),
                      dimnames = list(age = c("5-9", "10-19"),
                                      sex = c("f", "m", "other"))),
                dimtypes = c(sex = "state"))
    expect_error(canMakePairCompatible(x, y),
                 "\"age\" dimensions have incompatible dimscales : intervals do not align")
    expect_error(canMakePairCompatible(y, x),
                 "\"age\" dimensions have incompatible dimscales : intervals do not align")
})

test_that("canMakePairCompatible works in with 0-length dimensions", {
    canMakePairCompatible <- dembase:::canMakePairCompatible
    x <- Values(array(0,
                      dim = c(3, 0),
                      dimnames = list(age = c("0-4", "5-14", "15-19"), sex = NULL)))
    y <- Values(array(0,
                      dim = c(2, 0),
                      dimnames = list(age = c("0-14", "15-19"), region = NULL)))
    expect_true(canMakePairCompatible(x, y))
    x <- Values(array(0,
                      dim = 2,
                      dimnames = list(age = c("0-14", "15-19"))))
    y <- Counts(array(0,
                      dim = c(2, 0),
                      dimnames = list(age = c("0-14", "15-19"), region = NULL)))
    expect_true(canMakePairCompatible(x, y))
})

test_that("canMakePairCompatible works with Iterations", {
    canMakePairCompatible <- dembase:::canMakePairCompatible
    x <- Values(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-14", "15-19"), sex = c("f", "m"))))
    y <- Values(array(1:6,
                      dim = c(2, 3),
                      dimnames = list(age = c("0-14", "15-19"), sim = 1:3)))
    expect_true(canMakePairCompatible(x, y))
    expect_true(canMakePairCompatible(y, x))
    x <- Values(array(1:6,
                      dim = c(2, 3),
                      dimnames = list(age = c("0-14", "15-19"), sim = 1:3)))
    y <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-14", "15-19"), sex = c("f", "m"))))
    expect_error(canMakePairCompatible(x, y, allowCopyIterDim = FALSE),
                 "one object has dimension with dimtype \"iteration\" but other does not")
})

test_that("canMakePairCompatible works when e2 is an array", {
    canMakePairCompatible <- dembase:::canMakePairCompatible
    e1 <- Values(array(0,
                       dim = c(3, 2, 3),
                       dimnames = list(age = c("0-4", "5-9", "10+"),
                       sex = c("m", "f"),
                       sim = 1:3)))
    e2 <- array(1:6,
                dim = c(3, 2),
                dimnames = list(age = c("0-4", "5-9", "10+"),
                sex = c("m", "f")))
    expect_true(canMakePairCompatible(e1, e2))
    expect_error(canMakePairCompatible(e1, t(e2)),
                 "non-conformable arrays")
})

test_that("canMakePairCompatible works when e1 is an array", {
    canMakePairCompatible <- dembase:::canMakePairCompatible
    e1 <- array(1:6,
                dim = c(3, 2),
                dimnames = list(age = c("0-4", "5-9", "10+"),
                sex = c("m", "f")))
    e2 <- Values(array(0,
                       dim = c(3, 2),
                       dimnames = list(age = c("0-4", "5-9", "10+"),
                       sex = c("m", "f"))))
    expect_true(canMakePairCompatible(e1, e2))
    e1 <- array(1:6,
                dim = c(3, 2),
                dimnames = list(age = c("0-4", "5-9", "10+"),
                quantile = c("10%", "90%")))
    e2 <- Values(array(0,
                       dim = c(3, 2),
                       dimnames = list(age = c("0-4", "5-9", "10+"),
                       quantile = c(0.1, 0.9))))
    expect_true(canMakePairCompatible(e1, e2))
})

test_that("canMakeSharedDimScalesCompatible works", {
    Concordance <- classconc::Concordance
    canMakeSharedDimScalesCompatible <- dembase:::canMakeSharedDimScalesCompatible
    ## not using concordances
    e1 <- Values(array(0,
                       dim = c(3, 2),
                       dimnames = list(age = c("0-4", "5-9", "10+"),
                       sex = c("m", "f"))))
    e2 <- Values(array(0,
                       dim = c(2, 2),
                       dimnames = list(age = c("0-9", "10+"),
                           sex = c("f", "m"))))
    concordances <- list(age = NULL, sex = NULL)
    expect_error(canMakeSharedDimScalesCompatible(e1, e2, concordances = concordances),
                 paste("\"age\" dimensions have incompatible dimscales :",
                       "one dimension has break \\[5\\] that other does not"))
    e1 <- Values(array(0,
                       dim = c(3, 2),
                       dimnames = list(age = c("0-4", "5-9", "10+"),
                       sex = c("m", "f"))))
    e2 <- Values(array(0,
                       dim = c(2, 2),
                       dimnames = list(age = c("0-4", "5-9"),
                           sex = c("f", "m"))))
    concordances <- list(age = NULL, sex = NULL)
    expect_true(canMakeSharedDimScalesCompatible(x = e1, y = e2, subset = TRUE,
                                                 concordances = concordances))
    expect_error(canMakeSharedDimScalesCompatible(x = e1, y = e2,
                                                  concordances = concordances),
                 paste("\"age\" dimensions have incompatible dimscales :",
                       "one dimension ends at Inf and other ends at 10"))
    ## with concordances
    e1 <- Values(array(1:4,
                       dim = c(2, 2),
                       dimnames = list(age = c("0-4", "5+"),
                           region = c("A", "B"))))
    e2 <- Values(array(1:9,
                       dim = c(3, 3),
                       dimnames = list(age = c("0-4", "5-9", "10+"),
                       region = c("a", "b", "c"))))
    conc <- Concordance(data.frame(from = c("a", "b", "c"), to = c("A", "B", "B")))
    concordances <- list(age = NULL, region = conc)
    expect_true(canMakeSharedDimScalesCompatible(x = e1, y = e2, subset = TRUE,
                                                 concordances = concordances))
    expect_true(canMakeSharedDimScalesCompatible(x = e1, y = e2, subset = FALSE,
                                                 concordances = concordances))
})

test_that("collapse works", {
    collapse <- dembase:::collapse
    x <- Values(array(0,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = c("m", "f"))))
    transform <- new("CollapseTransform",
                     dims = 1:2,
                     indices = list(c(1:2, 0L), 1:2),
                     dimBefore = c(3L, 2L),
                     dimAfter = c(2L, 2L))
    expect_identical(collapse(x, transform = transform),
                     x[1:2,])
    expect_identical(collapse(x, transform = transform),
                     x[1:2,])
    transform <- new("CollapseTransform",
                     dims = c(1L, 0L),
                     indices = list(1:3, c(1L, 1L)),
                     dimBefore = c(3L, 2L),
                     dimAfter = 3L)
    expect_error(collapse(x, transform = transform),
                 "attempt to collapse elements of object with class \"Values\"")
})

test_that("collapseCategories method for Values works with old, new", {
    x <- Counts(array(1:6,
                      dim = 3:2,
                      dimnames = list(reg = c("a", "b", "c"),
                      sex = c("m", "f"))))
    y <- Counts(array(c(4L, 2L, 10L, 5L),
                      dim = c(2, 2),
                      dimnames = list(reg = c("d", "b"),
                          sex = c("m", "f"))))
    w <- Counts(array(6:1,
                      dim = 3:2,
                      dimnames = list(reg = c("a", "b", "c"),
                      sex = c("m", "f"))))
    vals <- x / w
    w.col <- collapseCategories(w, dim = "reg", old = c("a", "c"), new = "d")
    expect_identical(collapseCategories(vals, dimension = "reg",
                                        old = c("a", "c"), new = "d",
                                        weights = w),
                     y / w.col)
    x <- Values(array(c(Inf, 2, 3, 4, 5, 6, 7, 8, 9),
                      dim = c(3, 3),
                      dimnames = list(eth_parent = c("a", "b", "c"),
                      eth_child = c("a", "b", "c"))))
    y <- Values(array(c(Inf, 2.5, 5.5, 7),
                      dim = c(2, 2),
                      dimnames = list(eth_parent = c("a", "d"),
                      eth_child = c("a", "d"))))
    expect_identical(collapseCategories(x, dimension = "eth",
                                        old = c("b", "c"), new = "d",
                                        weights = 1),
                     y)
    x <- Counts(array(c(NA, 2, 3, 4, 5, 6, 7, 8, 9),
                      dim = c(3, 3),
                      dimnames = list(reg1 = c("a", "b", "c"),
                          reg2 = c("a", "b", "c"))))
    w <- Counts(array(1:3,
                      dim = 3,
                      dimnames = list(reg1 = c("a", "b", "c"))))
    w.expand <- Counts(array(1:3,
                             dim = c(3, 3),
                             dimnames = list(reg1 = c("a", "b", "c"),
                                 reg2 = c("a", "b", "c"))))
    vals <- x / w.expand
    y <- collapseCategories(x, old = c("b", "c"), new = "d") /
        collapseCategories(w.expand, old = c("b", "c"), new = "d")
    expect_identical(collapseCategories(vals, old = c("b", "c"), new = "d",
                                        weights = w),
                     y)
    expect_error(collapseCategories(x, old = c("b", "wrong"), new = "d"),
                 "cannot collapse categories for dimension \"reg1\" : value \"wrong\" not found")
})

test_that("collapseCategories method for Values works with concordance", {
    Concordance <- classconc::Concordance
    x <- Counts(array(1:6,
                      dim = 3:2,
                      dimnames = list(reg = c("a", "b", "c"),
                      sex = c("m", "f"))))
    y <- Counts(array(c(4L, 2L, 10L, 5L),
                      dim = c(2, 2),
                      dimnames = list(reg = c("d", "b"),
                          sex = c("m", "f"))))
    w <- Counts(array(6:1,
                      dim = 3:2,
                      dimnames = list(reg = c("a", "b", "c"),
                      sex = c("m", "f"))))
    vals <- x / w
    conc <- Concordance(data.frame(c1 = c("a", "b", "c"),
                                    c2 = c("d", "b", "d")))
    w.col <- collapseCategories(w, dim = "reg", concordance = conc)
    expect_identical(collapseCategories(vals, dimension = "reg",
                                        concordance = conc,
                                        weights = w),
                     y / w.col)
    x <- Values(array(1:6,
                      dim = 3:2,
                      dimnames = list(reg = c("a", "b", "c"),
                      sex = c("m", "f"))))
    y <- Values(array(1:6,
                      dim = 3:2,
                      dimnames = list(reg = c("A", "B", "C"),
                      sex = c("m", "f"))))
    w <- Counts(array(6:1,
                      dim = 3:2,
                      dimnames = list(reg = c("a", "b", "c"),
                      sex = c("m", "f"))))
    conc <- Concordance(data.frame(c1 = c("a", "b", "c"),
                                   c2 = c("A", "B", "C")))
    expect_identical(collapseCategories(x,
                                        dimension = "reg",
                                        concordance = conc,
                                        weights = w),
                     y)
    conc.wrong <- Concordance(data.frame(c1 = c("a", "b", "wrong"),
                                         c2 = c("A", "B", "C")))
    expect_error(collapseCategories(x,
                                    dimension = "reg",
                                    concordance = conc.wrong,
                                    weights = w),
                 "problem translating dimension \"reg\"")
    x <- Values(array(c(Inf, 2, 3, 4, 5, 6, 7, 8, 9),
                      dim = c(3, 3),
                      dimnames = list(eth_parent = c("a", "b", "c"),
                      eth_child = c("a", "b", "c"))))
    y <- Values(array(c(Inf, 2.5, 5.5, 7),
                      dim = c(2, 2),
                      dimnames = list(eth_parent = c("a", "d"),
                      eth_child = c("a", "d"))))
    conc <- Concordance(data.frame(c1 = c("a", "b", "c"),
                                    c2 = c("a", "d", "d")))
    expect_identical(collapseCategories(x, dimension = "eth",
                                        concordance = conc,
                                        weights = 1),
                     y)
    x <- Counts(array(c(NA, 2, 3, 4, 5, 6, 7, 8, 9),
                      dim = c(3, 3),
                      dimnames = list(reg1 = c("a", "b", "c"),
                          reg2 = c("a", "b", "c"))))
    w <- Counts(array(1:3,
                      dim = 3,
                      dimnames = list(reg1 = c("a", "b", "c"))))
    w.expand <- Counts(array(1:3,
                             dim = c(3, 3),
                             dimnames = list(reg1 = c("a", "b", "c"),
                                 reg2 = c("a", "b", "c"))))
    vals <- x / w.expand
    conc <- Concordance(data.frame(c1 = c("a", "b", "c"),
                                    c2 = c("d", "b", "d")))
    y <- collapseCategories(x, conc = conc) /
        collapseCategories(w.expand, conc = conc)
    expect_identical(collapseCategories(vals, conc = conc,
                                        weights = w),
                     y)
    vals <- Values(array(c(NA, 2, 3, 4, 5, 6, 7, 8, 9),
                      dim = c(3, 3),
                      dimnames = list(reg1 = c("a", "b", "wrong"),
                          reg2 = c("a", "b", "c"))))
    w <- Counts(array(1:3,
                      dim = 3,
                      dimnames = list(reg1 = c("a", "b", "wrong"))))
    conc <- Concordance(data.frame(c1 = c("a", "b", "c"),
                                    c2 = c("d", "b", "d")))    
    expect_error(collapseCategories(vals, conc = conc, weights = w),
                 "cannot collapse categories for dimension \"reg1\" : value \"wrong\" not found in classification 'c1'")
})

test_that("collapseDimension works when weights supplied", {
    counts1 <- Counts(array(rpois(n = 6, lambda = 10),
                            dim = c(3, 2),
                            dimnames = list(age = c("0-4", "5-9", "10+"),
                            sex = c("Male", "Female"))))
    weights1 <- Counts(array(rpois(n = 6, lambda = 5) + 1,
                             dim = c(3, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                             sex = c("Male", "Female"))))
    values1 <- counts1 / weights1
    counts2 <- collapseDimension(counts1, dimension = "sex")
    weights2 <- collapseDimension(weights1, dimension = "sex")
    values2 <- counts2 / weights2
    expect_identical(collapseDimension(values1, dimension = "sex", weights = weights1),
                     values2)
    expect_identical(collapseDimension(values1, dimension = 2, weights = weights1),
                     values2)
    expect_identical(collapseDimension(values1, margin = "age", weights = weights1),
                     values2)
    expect_identical(collapseDimension(values1, margin = 1, weights = weights1),
                     values2)
    expect_identical(collapseDimension(values1, dimension = "sex", weights = t(weights1)),
                     values2)
    expect_error(collapseDimension(values1, dimension = "wrong", weights = t(weights1)),
                 sprintf("subscript %s outside valid range",
                         dQuote("wrong")))
    counts1 <- Counts(array(rpois(n = 12, lambda = 10),
                            dim = c(2, 2, 3),
                            dimnames = list(eth_parent = c("a", "b"),
                            eth_child = c("a", "b"),
                            age = c("0-4", "5-9", "10+"))))
    weights1 <- Counts(array(rpois(n = 12, lambda = 100) + 1,
                             dim = c(2, 2, 3),
                             dimnames = list(eth_parent = c("a", "b"),
                             eth_child = c("a", "b"),
                             age = c("0-4", "5-9", "10+"))))
    values1 <- counts1 / weights1
    counts2 <- collapseDimension(counts1, dimension = "eth")
    weights2 <- collapseDimension(weights1, dimension = "eth")
    values2 <- counts2 / weights2
    expect_identical(collapseDimension(values1, dimension = "eth", weights = weights1),
                     values2)
    expect_identical(collapseDimension(values1,
                                       dimension = c("age", "eth"),
                                       weights = weights1),
                     sum(values1 * weights1) / sum(weights1))
    counts1 <- Counts(array(rpois(n = 12, lambda = 10),
                            dim = c(2, 2, 3),
                            dimnames = list(eth_parent = c("a", "b"),
                            eth_child = c("a", "b"),
                            age = c("0-4", "5-9", "10+"))))
    weights1 <- Counts(array(rpois(n = 27, lambda = 100) + 1,
                             dim = c(3, 3, 3),
                             dimnames = list(eth_parent = c("a", "b", "extra"),
                             eth_child = c("a", "b", "extra"),
                             age = c("0-4", "5-9", "10+"))))
    weights1.trimmed <- subarray(weights1, eth_parent != "extra" & eth_child != "extra")
    values1 <- counts1 / weights1.trimmed
    counts2 <- collapseDimension(counts1, dimension = "eth")
    weights2 <- collapseDimension(weights1.trimmed, dimension = "eth")
    values2 <- counts2 / weights2
    expect_identical(collapseDimension(values1, dimension = "eth", weights = weights1),
                     values2)
    values <- Values(array(1:2,
                           dim = 2,
                           dimnames = list(region = c("a", "b"))))
    weights <- Counts(array(c(1, -1),
                            dim = 2,
                            dimnames = list(region = c("a", "b"))))
    expect_error(collapseDimension(values, dimension = "region", weights = weights),
                 "'weights' has negative values")
    values <- Values(array(1:4,
                           dim = c(2, 2, 1),
                           dimnames = list(age = c("0-4", "5+"), reg = c("a", "b"), sex = "f")))
    weights <- Counts(array(4:1,
                            dim = c(2, 2),
                            dimnames = list(age = c("0-4", "5+"), reg = c("a", "b"))))
    ans <- Values(array(c((1 * 4 + 2 * 3) / 7, (3 * 2 + 4 * 1) / 3),
                        dim = 2,
                        dimnames = list(reg = c("a", "b"))))
    expect_identical(collapseDimension(values, dimension = c("age", "sex"), weights = weights),
                     ans)
    values <- Values(array(rpois(n = 6, lambda = 10),
                           dim = c(3, 2),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                               sex = c("Male", "Female"))))
    weights1 <- Counts(array(1:3,
                             dim = 3,
                             dimnames = list(age = c("0-4", "5-9", "10+"))))
    weights2 <- Counts(array(1:3,
                             dim = c(3, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 sex = c("Male", "Female"))))
    expect_identical(collapseDimension(values, dimension = "age", weights = weights1),
                     collapseDimension(values, dimension = "age", weights = weights2))
    expect_identical(collapseDimension(values, dimension = 2, weights = weights1),
                     collapseDimension(values, dimension = 2, weights = weights2))
})

test_that("collapseDimension works when weights not supplied", {
    x <- Values(array(rnorm(n = 6),
                      dim = c(3, 2, 1, 1),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          sex = c("Male", "Female"),
                          reg = "a",
                          eth = "b")))
    expect_identical(collapseDimension(x, dimension = c("reg", "eth")),
                     drop(x))
    expect_identical(collapseDimension(x, margin = c("age", "sex")),
                     drop(x))
    expect_error(collapseDimension(x, dimension = c("sex", "age")),
                 "'weights' is missing")
    expect_identical(collapseDimension(x, margin = c("eth", "age", "sex", "reg")),
                     aperm(x, perm = c(4, 1, 2, 3)))
    expect_error(collapseDimension(x, margin = "sex"),
                 "'weights' is missing")
    x <- Values(array(3,
                      dim = c(1, 1, 1, 1),
                      dimnames = list(age = "0-4",
                          sex = "Male",
                          reg = "a",
                          eth = "b")))
    expect_identical(collapseDimension(x, dimension = c("age", "sex", "reg", "eth")),
                     3)    
})

test_that("collapseIntervals works", {
    x <- Values(array(1:6,
                      dim = c(2, 3),
                      dimnames = list(sex = c("m", "f"),
                          age = c("0-4", "5-9", "10+"))))
    w <- Counts(array(6:1,
                      dim = c(2, 3),
                      dimnames = list(sex = c("m", "f"),
                          age = c("0-4", "5-9", "10+"))))
    expect_identical(collapseIntervals(x, dimension = "age", breaks = 10, weights = w),
                     collapseIntervals(x * w, dimension = "age", breaks = 10) /
                     collapseIntervals(w, dimension = "age", breaks = 10))
    x <- Values(array(1:6,
                      dim = c(2, 3),
                      dimnames = list(sex = c("m", "f"),
                          age = c("0-4", "5-9", "10+"))))
    w1 <- as(x / x, "Counts")
    expect_identical(collapseIntervals(x, dimension = "age", breaks = 10, weights = 1),
                     collapseIntervals(x, dimension = "age", breaks = 10,
                                       weights = w1))
    w.wrong <- w
    w.wrong[1] <- -1
    expect_error(collapseIntervals(x, dimension = "age", breaks = 10,
                                   weights = w.wrong),
                 "'weights' has negative values")
    expect_error(collapseIntervals(x, dimension = "age", breaks = 5,
                                   weights = 2),
                 "'weights' invalid")
    ## 'weights = 1' works as expected
    x <- Values(array(1:6,
                      dim = c(2, 3),
                      dimnames = list(sex = c("m", "f"),
                          age = c("0-4", "5-9", "10+"))))
    w <- Counts(array(1,
                      dim = c(2, 3),
                      dimnames = list(sex = c("m", "f"),
                          age = c("0-4", "5-9", "10+"))))
    expect_identical(collapseIntervals(x, dim = "age", old = c("5-9", "10+"),
                                       weights = 1),
                     collapseIntervals(x, dim = "age", old = c("5-9", "10+"),
                                       weights = w))
    ## not all dimension specified for weights
    x <- Values(array(1:6,
                      dim = c(2, 3),
                      dimnames = list(sex = c("m", "f"),
                          age = c("0-4", "5-9", "10+"))))
    w1 <- Counts(array(1:2,
                      dim = 2,
                      dimnames = list(sex = c("m", "f"))))
    w2 <- Counts(array(1:2,
                      dim = c(2, 3),
                      dimnames = list(sex = c("m", "f"),
                          age = c("0-4", "5-9", "10+"))))
    expect_identical(collapseIntervals(x, dim = "age", old = c("5-9", "10+"),
                                       weights = w1),
                     collapseIntervals(x, dim = "age", old = c("5-9", "10+"),
                                       weights = w2))
})

test_that("collapseOrigDest works", {
    x <- Values(array(1:9,
                      dim = c(3, 3),
                      dimnames = list(reg_orig = c("a", "b", "c"),
                      reg_dest = c("a", "b", "c"))))
    w <- Counts(array(10:18,
                      dim = c(3, 3),
                      dimnames = list(reg_orig = c("a", "b", "c"),
                      reg_dest = c("a", "b", "c"))))
    expect_identical(collapseOrigDest(x, weights = w),
                     collapseOrigDest(x * w) / collapseOrigDest(w))
})

test_that("dbind2 works", {
    dbind2 <- dembase:::dbind2
    ## along reg_dest dimension
    x <- Values(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(reg_orig = c("a", "b"), reg_dest = c("a", "b"))))
    y <- Values(array(5:6,
                      dim = c(2, 1),
                      dimnames = list(reg_orig = c("a", "b"), reg_dest = "c")))
    ans.obtained <- dbind2(e1 = x, e2 = y, name1 = "x", name2 = "y", along = "reg_dest", dimtypeAlong = "destination")
    ans.expected <- Values(array(1:6,
                                 dim = c(2, 3),
                                 dimnames = list(reg_orig = c("a", "b"), reg_dest = c("a", "b", "c"))))
    expect_identical(ans.obtained, ans.expected)
    ## along reg_dest dimension, 'y' has extra dimension not in 'x'
    x <- Values(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(reg_orig = c("a", "b"), reg_dest = c("a", "b"))))
    y <- Values(array(5:12,
                      dim = c(2, 2, 2),
                      dimnames = list( reg_orig = c("a", "b"),
                          reg_dest = c("a", "b"), eth = c("a", "b"))))
    ans.obtained <- dbind2(e1 = x, e2 = y, name1 = "x", name2 = "y", along = "eth", dimtypeAlong = "state")
    ans.expected <- Values(array(1:12,
                                 dim = c(2, 2, 3),
                                 dimnames = list(reg_orig = c("a", "b"), reg_dest = c("a", "b"),
                                     eth = c("x", "a", "b"))))
    expect_identical(ans.obtained, ans.expected)
    ## zero-length dimension
    x <- Values(array(0L,
                      dim = c(0, 1),
                      dimnames = list(reg = NULL, sex = "m")))
    y <- Values(array(0L,
                      dim = c(0, 1),
                      dimnames = list(reg = NULL, sex = "m")))
    ans.obtained <- dbind2(e1 = x, e2 = y, name1 = "x", name2 = "y", along = "eth", dimtypeAlong = "state")
    ans.expected <- Values(array(0L,
                                 dim = c(0, 1, 2),
                                 dimnames = list(reg = NULL, sex = "m", eth = c("x", "y"))))
    expect_identical(ans.obtained, ans.expected)
    ## attempt to dbind Values and Counts
    x <- Values(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(reg_orig = c("a", "b"), reg_dest = c("a", "b"))))
    y <- Counts(array(5:6,
                      dim = c(2, 1),
                      dimnames = list(reg_orig = c("a", "b"), reg_dest = "c")))
    expect_error(dbind2(e1 = x, e2 = y, name1 = "x", name2 = "y", along = "reg_dest", dimtypeAlong = "state"),
                 "cannot combine object of class \"Values\" with object of class \"Counts\"")
    ## repeat age group
    x <- Values(array(1:3,
                      dim = c(3, 1),
                      dimnames = list(age = c(0, 1, "2+"), region = "a")))
    y <- Values(array(1:2,
                      dim = c(2, 1),
                      dimnames = list(age = c(0, "1+"), region = "b")))
    ans.obtained <- dbind2(e1 = x, e2 = y, name1 = "x", name2 = "y", along = "region", dimtypeAlong = "state")
    ans.expected <- Values(array(c(1:3, 1:2, 2L),
                                 dim = c(3, 2),
                                 dimnames = list(age = c(0, "1", "2+"), region = c("a", "b"))))
    expect_identical(ans.obtained, ans.expected)
    ## objects in wrong order
    x <- Values(array(3:6,
                      dim = c(2, 2),
                      dimnames = list(age = c("5-9", "10+"), reg = c("a", "b"))))
    y <- Values(array(1:2,
                      dim = c(1, 2),
                      dimnames = list(age = "0-4", reg = c("a", "b"))))
    ans.obtained <- dbind2(x, y, name1 = "x", name2 = "y", along = "age", dimtypeAlong = "age")
    ans.expected <- Values(array(c(1L, 2L, 3L, 5L, 4L, 6L),
                                 dim = c(2, 3),
                                 dimnames = list(reg = c("a", "b"),
                                     age = c("0-4", "5-9", "10+"))))
    expect_identical(ans.obtained, ans.expected)
})

test_that("expandCategories method for Values works with old, new", {
    object <- Values(array(c(4L, 2L, 10L, 5L),
                           dim = c(2, 2),
                           dimnames = list(reg = c("d", "b"),
                               sex = c("m", "f"))))
    ans.obtained <- expandCategories(object, dim = "reg", old = "d", new = c("a", "c"))
    ans.expected <- Values(array(c(4L, 4L, 2L, 10L, 10L, 5L),
                                 dim = 3:2,
                                 dimnames = list(reg = c("a", "c", "b"),
                                     sex = c("m", "f"))))
    expect_identical(ans.obtained, ans.expected)
    object <- Values(array(c(Inf, 2.5, 5.5, 7),
                           dim = c(2, 2),
                           dimnames = list(eth_parent = c("a", "d"),
                               eth_child = c("a", "d"))))
    ans.obtained <- expandCategories(object, dim = "eth", old = "d", new = c("b", "c"))
    ans.expected <- Values(array(c(Inf, 2.5, 2.5, 5.5, 7, 7, 5.5, 7, 7),
                                 dim = c(3, 3),
                                 dimnames = list(eth_parent = c("a", "b", "c"),
                                     eth_child = c("a", "b", "c"))))
    expect_identical(ans.obtained, ans.expected)
    object <- ValuesOne(values = c(-1, 0, NA), labels = c("A", "B", "C"), name = "reg")
    ans.obtained <- expandCategories(object, dim = "reg", old = "C", new = c("D", "E"))
    ans.expected <- ValuesOne(values = c(-1, 0, NA, NA),
                              labels = c("A", "B", "D", "E"),
                              name = "reg")
    expect_identical(ans.obtained, ans.expected)
    object <- ValuesOne(values = 1:2, labels = c("b", "d"), name = "reg")
    weights <- ValuesOne(values = 1:3, labels = c("a", "b", "c"), name = "reg")
    expect_warning(expandCategories(object, old = "d", new = c("a", "c"),
                                    weights = weights),
                   "'weights' argument ignored")
    object <- ValuesOne(values = 1:2, labels = c("b", "a"), name = "reg")
    expect_error(expandCategories(object, old = "wrong", new = c("d", "e")),
                 "cannot expand category for dimension \"reg\" : value \"wrong\" not found")
})

test_that("expandCategories method for Values works with concordance", {
    Concordance <- classconc::Concordance
    object <- Values(array(c(4L, 2L, 10L, 5L),
                           dim = c(2, 2),
                           dimnames = list(reg = c("d", "b"),
                               sex = c("m", "f"))))
    conc <- Concordance(data.frame(c1 = c("a", "b", "c"),
                                   c2 = c("d", "b", "d")))
    ans.obtained <- expandCategories(object, dim = "reg", conc = conc)
    ans.expected <- Values(array(c(4L, 2L, 4L, 10L, 5L, 10L),
                                 dim = 3:2,
                                 dimnames = list(reg = c("a", "b", "c"),
                                     sex = c("m", "f"))))
    expect_identical(ans.obtained, ans.expected)
    object <- Values(array(c(Inf, 2.5, 5.5, 7),
                           dim = c(2, 2),
                           dimnames = list(eth_parent = c("a", "d"),
                               eth_child = c("a", "d"))))
    conc <- Concordance(data.frame(c1 = c("a", "b", "c"),
                                    c2 = c("a", "d", "d")))
    ans.obtained <- expandCategories(object, dim = "eth", conc = conc)
    ans.expected <- Values(array(c(Inf, 2.5, 2.5, 5.5, 7, 7, 5.5, 7, 7),
                                 dim = c(3, 3),
                                 dimnames = list(eth_parent = c("a", "b", "c"),
                                     eth_child = c("a", "b", "c"))))
    expect_identical(ans.obtained, ans.expected)
    object <- ValuesOne(values = c(-1, 0, NA), labels = c("A", "B", "C"), name = "reg")
    conc <- Concordance(data.frame(c1 = c("a", "b", "c", "d", "e", "f"),
                                   c2 = c("C", "C", "B", "A", "B", "D")))
    ans.obtained <- expandCategories(object, dim = "reg", conc = conc)
    ans.expected <- ValuesOne(values = c(NA, NA, 0, -1, 0),
                              labels = c("a", "b", "c", "d", "e"),
                              name = "reg")
    expect_identical(ans.obtained, ans.expected)
    object <- ValuesOne(values = 1:2, labels = c("b", "d"), name = "reg")
    weights <- ValuesOne(values = 1:3, labels = c("a", "b", "c"), name = "reg")
    conc <- Concordance(data.frame(c1 = c("a", "b", "c"),
                                   c2 = c("d", "b", "d")))
    expect_warning(expandCategories(object, conc = conc, weights = weights),
                   "'weights' argument ignored")
    object <- ValuesOne(values = 1:2, labels = c("b", "wrong"), name = "reg")
    conc <- Concordance(data.frame(c1 = c("a", "b", "c"),
                                   c2 = c("d", "b", "d")))
    expect_error(expandCategories(object, conc = conc),
                 "cannot expand categories for dimension \"reg\" : value \"wrong\" not found in classification 'c2'")
})

test_that("growth works when 'within' is NULL", {
    a <- array(1:12,
               dim = c(4, 3),
               dimnames = list(region = 1:4, age = c(5, 10, 25)))
    x <- Values(a)
    v <- colMeans(x)
    start <- v[[1]]
    end <- v[[3]]
    expect_identical(growth(x, along = "age"),
                     (end/start)^(1/20) - 1)
    expect_identical(growth(x, along = "age", type = "linear"),
                     (end - start) / 20)
    distance <- c(5, 10, 25)
    expect_identical(growth(x, along = "age", type = "l", method = "lm"),
                     coef(lm(v ~ distance))[["distance"]])
    expect_identical(growth(x, along = "age", method = "lm"),
                     exp(coef(lm(log(v) ~ distance))[["distance"]]) - 1)
})

test_that("growth works when 'within' is non-NULL", {
    a <- array(1:27,
               dim = c(3, 3, 3),
               dimnames = list(region = 1:3, age = c("0-4", "5-9", "10+"),
                   time = c(2000, 2005, 2010)))
    x <- Values(a)
    w <- Counts(array(1, dim = dim(a), dimnames = dimnames(a)))
    expect_identical(growth(x, within = c("region", "age"), type = "l"),
                     Values(apply(a, 1:2, function(x) (x[3] - x[1]) / 10)))
    b <- apply(a, 2:3, mean)
    age <- apply(b, 1, function(x) (x[3] - x[1]) / 10)
    expect_identical(growth(x, within = "age", type = "l"),
                     Values(age))
    expect_identical(growth(x, within = 2:1),
                     growth(x, within = c("age", "region")))
    expect_identical(growth(x, along = "age", type = "l"),
                     growth(x, along = 2.0, type = "l"))
})

test_that("growth works when 'within' is .", {
    a <- array(1:27,
               dim = c(3, 3, 3),
               dimnames = list(region = 1:3, age = c("0-4", "5-9", "10+"),
                   time = c(2000, 2005, 2010)))
    x <- Values(a)
    expect_identical(growth(x, along = "age", within = "."),
                     growth(x, along = "age", within = c("region", "time")))
})

test_that("growth works when 'within' is orig-dest", {
    a <- array(1:27,
               dim = c(3, 3, 3),
               dimnames = list(reg_orig = 1:3, reg_dest = 1:3,
                   time = c(2000, 2005, 2010)))
    x <- Values(a)
    expect_identical(growth(x, along = "time", within = "reg"),
                     growth(x, along = "time", within = c("reg_orig", "reg_dest")))
})

test_that("growth works with interations", {
    a <- array(1:27,
               dim = c(3, 3, 3),
               dimnames = list(region = 1:3, iteration = 1:3,
                   time = c(2000, 2005, 2010)))
    x <- Values(a)
    expect_identical(growth(x, within = "region"),
                     growth(x, within = c("region", "iteration")))
})

test_that("growth works with values and weights", {
    a <- array(rpois(27, lambda = 10),
               dim = c(3, 3, 3),
               dimnames = list(region = 1:3, age = c("0-4", "5-9", "10+"),
                   time = c(2000, 2005, 2010)))
    x <- Values(a)
    w <- array(1,
               dim = c(3, 3, 3),
               dimnames = list(region = 1:3, age = c("0-4", "5-9", "10+"),
                   time = c(2000, 2005, 2010)))
    w <- Counts(w)
    expect_equal(growth(x, within = "region", type = "exp", weights = w),
                 growth(x, within = "region", type = "exp"))
    a <- array(rpois(27, lambda = 10),
               dim = c(3, 3, 3),
               dimnames = list(region = 1:3, age = c("0-4", "5-9", "10+"),
                   time = c(2000, 2005, 2010)))
    x <- Values(a)
    w <- array(1:27,
               dim = c(3, 3, 3),
               dimnames = list(region = 1:3, age = c("0-4", "5-9", "10+"),
                   time = c(2000, 2005, 2010)))
    w <- Counts(w)
    expect_equal(growth(x, within = "region", type = "exp", weights = w),
                 growth(collapseDimension(x, dim = "age", weights = w),
                        within = "region", type = "exp"))
})

test_that("growth throws appropriate errors", {
    x <- Values(array(rpois(27, lambda = 10),
                      dim = c(3, 3, 3),
                      dimnames = list(region = 1:3, quantile = c("1%", "50%", "99%"),
                          time = c(2000, 2005, 2010))))
    expect_error(growth(x),
                 "dimension with dimtype \"quantile\"")
    x <- Values(array(rpois(27, lambda = 10),
                      dim = c(3, 3, 3),
                      dimnames = list(region = 1:3, age = c("0-4", "5-9", "10+"),
                          time = c(2000, 2010, 2020))))
    expect_error(growth(x, along = 1:2),
                 "'along' does not have length 1")
    expect_error(growth(x, along = NA),
                 "'along' is missing")
    expect_error(growth(x, along = 100),
                 "'along' outside valid range")
    expect_error(growth(x, along = 1.5),
                 "'along' outside valid range")
    expect_error(growth(x, along = "wrong"),
                 "'along' outside valid range")
    x <- Values(array(rpois(9, lambda = 10),
                      dim = c(3, 3, 1),
                      dimnames = list(region = 1:3, age = c("0-4", "5-9", "10+"),
                          time = 2000)),
                dimscales = c(time = "Points"))
    expect_error(growth(x),
                 paste("cannot calculate growth along dimension \"time\" because",
                       "dimension has length 1"))
    x <- Values(array(rpois(27, lambda = 10),
                      dim = c(3, 3, 3),
                      dimnames = list(region = 1:3, age = c("0-4", "5-9", "10+"),
                          time = c(2000, 2010, 2020))))
    expect_error(growth(x, within = NA),
                 "'within' has missing values")
    expect_error(growth(x, within = c("age", "age")),
                 "'within' has duplicates")
    expect_error(growth(x, within = c("age", "wrong")),
                 "'within' outside valid range")
    expect_error(growth(x, within = c(1, 100)),
                 "'within' outside valid range")
    expect_error(growth(x, within = 3, along = 3),
                 "dimension \"time\" included in 'along' and 'within'")
    x <- Values(array(rpois(27, lambda = 10),
                      dim = c(3, 3, 3),
                      dimnames = list(region = 1:3, iteration = 1:3,
                          time = c(2000, 2010, 2020))))
    expect_error(growth(x, within = 3, along = 2),
                 "'along' dimension \\[\"iteration\"\\] has dimtype \"iteration\"")
    x <- Values(array(rpois(27, lambda = 10),
                      dim = c(3, 3, 3),
                      dimnames = list(region = 1:3, age = c("0-4", "5-9", "10+"),
                          time = c(2000, 2010, 2020))))
    expect_error(growth(x, within = 3, along = 1),
                 "'along' dimension \\[\"region\"\\] has dimscale \"Categories\"")
    expect_error(growth(x, within = 3, method = "wrong"),
                 sprintf("'arg' should be one of %s",
                         paste(dQuote(c("endpoints", "lm")), collapse = ", ")))
    expect_error(growth(x, within = 3, type = "wrong"),
                 sprintf("'arg' should be one of %s",
                         paste(dQuote(c("exponential", "linear")), collapse = ", ")))
    x <- Values(array(rpois(27, lambda = 10),
                      dim = c(3, 3, 3),
                      dimnames = list(region = 1:3, iteration = 1:3,
                          time = c(2000, 2010, 2020))))
    w <- Values(array(1,
                      dim = c(3, 3, 3),
                      dimnames = list(region = 1:3, iteration = 1:3,
                          time = c(2000, 2010, 2020))))
    expect_error(growth(x, within = "region", weights = w),
                 "'weights' has class \"Values\"")
})

test_that("makeCompatible works", {
    makeCompatible <- dembase:::makeCompatible
    x <- ValuesOne(1:3, labels = c("a", "b", "c"), name = "reg")
    y <- CountsOne(1:3, labels = c("b", "c", "a"), name = "reg")
    ans.obtained <- makeCompatible(x, y)
    ans.expected <- x[c(2,3,1)]
    expect_identical(ans.obtained, ans.expected)
    x <- Values(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = c("m", "f"))))
    y <- Values(array(0,
                      dim = c(2, 2, 2),
                      dimnames = list(age = c("5-9", "10+"),
                      sex = c("f", "m"),
                      region = c("a", "b"))))
    z <- Values(array(c(5:6, 2:3),
                      dim = c(2, 2, 2),
                      dimnames = list(age = c("5-9", "10+"),
                      sex = c("f", "m"),
                      region = c("a", "b"))))
    expect_identical(makeCompatible(x, y, subset = TRUE), z)
    x <- Values(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(age = c("0-9", "10+"),
                      sex = c("m", "f"))))
    y <- Values(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = c("m", "f"))))
    z <- Values(array(c(1L, 1L, 2L, 3L, 3L, 4L),
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = c("m", "f"))))
    expect_identical(makeCompatible(x, y), z)
    x <- Values(array(0,
                      dim = c(0, 2),
                      dimnames = list(age = NULL,
                      sex = c("m", "f"))))
    y <- Values(array(0,
                      dim = c(0, 2),
                      dimnames = list(age = NULL,
                      sex = c("f", "m"))))
    z <- Values(array(0,
                      dim = c(0, 2),
                      dimnames = list(age = NULL,
                      sex = c("f", "m"))))
    expect_identical(makeCompatible(x, y), z)
})


test_that("makeOrigDestParentChildCompatible works", {
    makeOrigDestParentChildCompatible <- dembase:::makeOrigDestParentChildCompatible
    ## simple, orig-dest only
    x <- Values(array(0,
                      dim = c(3, 2, 2, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          region_orig = c("a", "b"),
                          region_dest = c("a", "b"),
                          time = c("2001-2005", "2006-2010"))))
    y <- Counts(array(0,
                      dim = c(3, 2, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          region = c("a", "b"),
                          time = c("2001-2005", "2006-2010"))))
    ans.obtained <- makeOrigDestParentChildCompatible(x = x, y = y, subset = TRUE)
    ans.expected <- x
    expect_identical(ans.obtained, ans.expected)
    ## orig-dest only; need to permute, and extend
    x <- Values(array(0,
                      dim = c(2, 3, 3),
                      dimnames = list(time = c("2001-2005", "2006-2010"),
                          region_orig = c("a", "b", "c"),
                          region_dest = c("a", "b", "c"))))
    y <- Counts(array(0,
                      dim = c(3, 2, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          region = c("a", "b"),
                          time = c("2001-2005", "2006-2010"))))
    ans.obtained <- makeOrigDestParentChildCompatible(x = x, y = y, subset = TRUE)
    ans.expected <- Values(array(0,
                                 dim = c(3, 2, 2, 2),
                                 dimnames = list(age = c("0-4", "5-9", "10+"),
                                     region_orig = c("a", "b"),
                                     region_dest = c("a", "b"),
                                     time = c("2001-2005", "2006-2010"))))
    expect_identical(ans.obtained, ans.expected)
    ## orig-dest and parent-child; need to subset and permute
    x <- Values(array(0,
                      dim = c(2, 3, 3, 2, 3, 3),
                      dimnames = list(time = c("2001-2005", "2006-2010"),
                          eth_child = 3:1,
                          eth_parent = 3:1,
                          age = c("0-9", "10+"),
                          region_orig = c("a", "b", "c"),
                          region_dest = c("a", "b", "c"))))
    y <- Counts(array(0,
                      dim = c(3, 2, 2, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          eth = 2:1,
                          region = c("c", "b"),
                          time = c("2001-2005", "2006-2010"))))
    ans.obtained <- makeOrigDestParentChildCompatible(x = x, y = y, subset = TRUE)
    ans.expected <- Values(array(0,
                                 dim = c(3, 2, 2, 2, 2, 2),
                                 dimnames = list(age = c("0-4", "5-9", "10+"),
                                     eth_parent = 2:1,
                                     eth_child = 2:1,
                                     region_orig = c("c", "b"),
                                     region_dest = c("c", "b"),
                                     time = c("2001-2005", "2006-2010"))))
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOrigDestParentChildTransform works", {
    makeOrigDestParentChildTransform <- dembase:::makeOrigDestParentChildTransform
    ## simple, orig-dest only
    x <- Values(array(0,
                      dim = c(3, 2, 2, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          region_orig = c("a", "b"),
                          region_dest = c("a", "b"),
                          time = c("2001-2005", "2006-2010"))))
    y <- Counts(array(0,
                      dim = c(3, 2, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          region = c("a", "b"),
                          time = c("2001-2005", "2006-2010"))))
    ans.obtained <- makeOrigDestParentChildTransform(x = x, y = y, subset = TRUE)
    ans.expected <- new("ExtendTransform",
                        dims = 1:4,
                        indices = list(1:3, 1:2, 1:2, 1:2),
                        dimBefore = c(3L, 2L, 2L, 2L),
                        dimAfter = c(3L, 2L, 2L, 2L))
    expect_identical(ans.obtained, ans.expected)
    ## orig-dest only; need to permute, and extend
    x <- Values(array(0,
                      dim = c(2, 3, 3),
                      dimnames = list(time = c("2001-2005", "2006-2010"),
                          region_orig = c("a", "b", "c"),
                          region_dest = c("a", "b", "c"))))
    y <- Counts(array(0,
                      dim = c(3, 2, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          region = c("a", "b"),
                          time = c("2001-2005", "2006-2010"))))
    ans.obtained <- makeOrigDestParentChildTransform(x = x, y = y, subset = TRUE)
    ans.expected <- new("ExtendTransform",
                        dims = c(0L, 2L, 3L, 1L),
                        indices = list(rep(1L, 3), 1:2,  1:2, 1:2),
                        dimBefore = c(2L, 3L, 3L),
                        dimAfter = c(3L, 2L, 2L, 2L))
    expect_identical(ans.obtained, ans.expected)
    ## orig-dest and parent-child; need to subset and permute
    x <- Values(array(0,
                      dim = c(2, 3, 3, 2, 3, 3),
                      dimnames = list(time = c("2001-2005", "2006-2010"),
                          eth_child = 3:1,
                          eth_parent = 3:1,
                          age = c("0-9", "10+"),
                          region_orig = c("a", "b", "c"),
                          region_dest = c("a", "b", "c"))))
    y <- Counts(array(0,
                      dim = c(3, 2, 2, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          eth = 2:1,
                          region = c("c", "b"),
                          time = c("2001-2005", "2006-2010"))))
    ans.obtained <- makeOrigDestParentChildTransform(x = x, y = y, subset = TRUE)
    ans.expected <- new("ExtendTransform",
                        dims = c(4L, 3:2, 5:6, 1L),
                        indices = list(c(1L, 1L, 2L), 2:3, 2:3, 3:2, 3:2, 1:2),
                        dimBefore = c(2L, 3L, 3L, 2L, 3L, 3L),
                        dimAfter = c(3L, rep(2L, 5)))
    expect_identical(ans.obtained, ans.expected)
})


test_that("makePairCompatible works when e2 is Counts", {
    makePairCompatible <- dembase:::makePairCompatible
    x <- ValuesOne(values = 1:3, labels = c("a", "b", "c"), name = "reg")
    y <- CountsOne(values = 1:3, labels = c("b", "c", "a"), name = "reg")
    expect_identical(makePairCompatible(e1 = x, e2 = y),
                     list(x, y[c(3,1,2)]))
    x0 <- Values(array(1:18,
                       dim = c(2, 3, 3),
                       dimnames = list(time = c("1996-2005", "2006-2010"),
                           eth_parent = c("b", "a", "c"),
                           eth_child = c("b", "a", "c"))))
    y0 <- Counts(array(1:24,
                       dim = c(2, 2, 3, 2),
                       dimnames = list(eth_parent = c("a", "b"),
                           eth_child = c("a", "b"),
                           time = c("1996-2000", "2001-2005", "2006-2010"),
                           sex = c("m", "f"))))
    x1 <- Values(array(as.integer(c(1, 1, 2, 3, 3, 4, 7, 7, 8, 9, 9, 10)),
                       dim = c(3, 2, 2, 2),
                       dimnames = list(time = c("1996-2000", "2001-2005", "2006-2010"),
                           eth_parent = c("b", "a"),
                           eth_child = c("b", "a"),
                           sex = c("m", "f"))))
    y1 <- aperm(y0, perm = c("time", "eth_parent", "eth_child", "sex"))
    y1 <- y1[,2:1,2:1,]
    expect_identical(makePairCompatible(x0, y0), list(x1, y1))
    x <- Values(array(1,
                      dim = 2,
                      dimnames = list(region = c("a", "b"))))
    y <- Counts(array(1,
                      dim = c(2, 1),
                      dimnames = list(region = c("a", "b"),
                      iteration = 1)))
    expect_identical(makePairCompatible(x, y), list(as(y, "Values"), y))
})

test_that("makePairCompatible works when e2 is Values", {
    makePairCompatible <- dembase:::makePairCompatible
    x <- ValuesOne(values = 1:3, labels = c("a", "b", "c"), name = "reg")
    y <- ValuesOne(values = 1:3, labels = c("b", "c", "a"), name = "reg")
    expect_identical(makePairCompatible(e1 = x, e2 = y),
                     list(x, y[c(3,1,2)]))
    x <- Values(array(0,
                      dim = c(2, 2, 4),
                      dimnames = list(eth_parent = c("a", "b"),
                      eth_child = c("a", "b"),
                      time = c("1996-2000", "2001-2005", "2006-2010", "2011-2016"))))
    y <- Values(array(0,
                      dim = c(2, 2, 2),
                      dimnames = list(time = c("1996-2005", "2006-2010"),
                      eth_parent = c("b", "a"),
                      eth_child = c("b", "a"))))
    expect_identical(makePairCompatible(x, y),
                     list(x[,,1:3], x[,,1:3]))
    x <- Values(array(1:2, dim = 2, dimnames = list(sex = c("f", "m"))))
    y <- Values(array(3:4, dim = 2, dimnames = list(age = c("0-4", "5+"))))
    xx <- Values(array(1:2, dim = c(2, 2), dimnames = list(sex = c("f", "m"), age = c("0-4", "5+"))))
    yy <- Values(array(c(3L, 3L, 4L, 4L),
                       dim = c(2, 2),
                       dimnames = list(sex = c("f", "m"), age = c("0-4", "5+"))))
    expect_identical(makePairCompatible(e1 = x, e2 = y),
                     list(xx, yy))
})

test_that("makePairTransformsDbind works", {
    makePairTransformsDbind <- dembase:::makePairTransformsDbind
    e1 <- Values(array(1:2,
                       dim = c(2, 1),
                       dimnames = list(age = c("0-4", "5+"), sex = "f")))
    e2 <- Values(array(3:4,
                       dim = c(2, 1),
                       dimnames = list(age = c("0-4", "5+"), sex = "m")))
    ans.obtained <- makePairTransformsDbind(e1 = e1, e2 = e2, along = "sex")
    ans.expected <- list(new("ExtendTransform",
                             dims = 1:2,
                             indices = list(1:2, 1L),
                             dimBefore = 2:1,
                             dimAfter = 2:1),
                         new("ExtendTransform",
                             dims = 1:2,
                             indices = list(1:2, 1L),
                             dimBefore = 2:1,
                             dimAfter = 2:1))
    expect_identical(ans.obtained, ans.expected)
    e1 <- Values(array(1:6,
                       dim = c(3, 2, 1),
                       dimnames = list(age = c("0-4", "5-9", "10+"), sex = c("f", "m"), region = "a")))
    e2 <- Values(array(5:8,
                       dim = c(2, 2, 1),
                       dimnames = list(age = c("0-4", "5+"), sex = c("m", "f"), region = "b")))
    ans.obtained <- makePairTransformsDbind(e1 = e1, e2 = e2, along = "region")
    ans.expected <- list(new("ExtendTransform",
                             dims = 1:3,
                             indices = list(1:3, 1:2, 1L),
                             dimBefore = c(3L, 2L, 1L),
                             dimAfter = c(3L, 2L, 1L)),
                         new("ExtendTransform",
                             dims = 1:3,
                             indices = list(c(1:2, 2L), 2:1, 1L),
                             dimBefore = c(2L, 2L, 1L),
                             dimAfter = c(3L, 2L, 1L)))
    expect_identical(ans.obtained, ans.expected)
    e1 <- Values(array(1:4,
                       dim = c(2, 2),
                       dimnames = list(age = c("0-4", "5-9"), sex = c("f", "m"))))
    e2 <- Values(array(1:4,
                       dim = c(2, 2),
                       dimnames = list(sex = c("m", "f"), age = c("10-14", "15+"))))
    ans.obtained <- makePairTransformsDbind(e1 = e1, e2 = e2, along = "age")
    ans.expected <- list(new("ExtendTransform",
                             dims = 2:1,
                             indices = list(1:2, 1:2),
                             dimBefore = c(2L, 2L),
                             dimAfter = c(2L, 2L)),
                         new("ExtendTransform",
                             dims = 1:2,
                             indices = list(2:1, 1:2),
                             dimBefore = c(2L, 2L),
                             dimAfter = c(2L, 2L)))
    expect_identical(ans.obtained, ans.expected)
    e1 <- Values(array(1:4,
                       dim = c(2, 2),
                       dimnames = list(time = c("2001-2005", "2006-2010"),
                           iteration = 1:2)))
    e2 <- Values(array(1:8,
                       dim = c(2, 2, 2),
                       dimnames = list(sex = c("m", "f"),
                           time = c("2001-2005", "2006-2010"),
                           iteration = 1:2)))
    ans.obtained <- makePairTransformsDbind(e1 = e1, e2 = e2, along = "iteration")
    ans.expected <- list(new("ExtendTransform",
                             dims = c(1L, 0L, 2L),
                             indices = list(1:2, c(1L, 1L), 1:2),
                             dimBefore = c(2L, 2L),
                             dimAfter = c(2L, 2L, 2L)),
                         new("ExtendTransform",
                             dims = c(2L, 1L, 3L),
                             indices = list(1:2, 1:2, 1:2),
                             dimBefore = c(2L, 2L, 2L),
                             dimAfter = c(2L, 2L, 2L)))
    expect_identical(ans.obtained, ans.expected)
    e1 <- Values(array(0,
                       dim = c(0, 2),
                       dimnames = list(time = character(),
                           iteration = 1:2)),
                 dimscales = c(time = "Points"))
    e2 <- Values(array(0,
                       dim = c(3, 0),
                       dimnames = list(iteration = 1:3, time = character())),
                 dimscales = c(time = "Points"))
    ans.obtained <- makePairTransformsDbind(e1 = e1, e2 = e2, along = "iteration")
    ans.expected <- list(new("ExtendTransform",
                             dims = 1:2,
                             indices = list(integer(), 1:2),
                             dimBefore = c(0L, 2L),
                             dimAfter = c(0L, 2L)),
                         new("ExtendTransform",
                             dims = 2:1,
                             indices = list(integer(), 1:3),
                             dimBefore = c(3L, 0L),
                             dimAfter = c(0L, 3L)))
    expect_identical(ans.obtained, ans.expected)
    e1 <- Values(array(1:8,
                       dim = c(2, 4),
                       dimnames = list(age = c("0-4", "5+"),
                           year = c(2000, 2005, 2010, 2015))))
    e2 <- Values(array(1:3,
                       dim = c(3, 1),
                       dimnames = list(age = c("0-4", "5-9", "10+"),
                           year = 1995)),
                 dimscales = c(year = "Points"))
    ans.obtained <- makePairTransformsDbind(e1, e2, along = "year")
    ans.expected <- list(new("ExtendTransform",
                             dims = 1:2,
                             indices = list(c(1:2, 2L), 1:4),
                             dimBefore = c(2L, 4L),
                             dimAfter = c(3L, 4L)),
                         new("ExtendTransform",
                             dims = 1:2,
                             indices = list(1:3, 1L),
                             dimBefore = c(3L, 1L),
                             dimAfter = c(3L, 1L)))
    expect_identical(ans.obtained, ans.expected)
    e1 <- Values(array(1,
                       dim = c(1, 1),
                       dimnames = list(age = "-5--1",
                           year = 1995)),
                 dimscales = c(year = "Points"))
    e2 <- Values(array(1:3,
                       dim = c(3, 1),
                       dimnames = list(age = c("0-4", "5-9", "10+"),
                           year = 1995)),
                 dimscales = c(year = "Intervals"))
    expect_error(makePairTransformsDbind(e1, e2, along = "age"),
                 "\"year\" dimensions have incompatible dimscales")
})

test_that("makePairTransforms method for Values and Counts works", {
    makePairTransforms <- dembase:::makePairTransforms
    x <- Values(array(0,
                      dim = c(2, 2),
                      dimnames = list(age = c("0-4", "5+"),
                                      sex = c("f", "m"))),
                dimtypes = c(sex = "state"))
    y <- Counts(array(0,
                      dim = c(3, 3),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                                      sex = c("m", "f", "o"))),
                dimtypes = c(sex = "state"))
    expect_identical(makePairTransforms(x, y),
                     list(new("ExtendTransform",
                              dims = 1:2,
                              indices = list(c(1L, 2L, 2L), 1:2),
                              dimBefore = c(2L, 2L),
                              dimAfter = c(3L, 2L)),
                          new("CollapseTransform",
                              dims = 1:2,
                              indices = list(1:3, c(2:1, 0L)),
                              dimBefore = c(3L, 3L),
                              dimAfter = c(3L, 2L))))
    x <- Values(array(1:3,
                      dim = 3,
                      dimnames = list(age = c("0-9", "10-14", "15+"))))
    y <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10-14"),
                                      sex = c("m", "f"))))
    expect_identical(makePairTransforms(x, y),
                     list(new("ExtendTransform",
                              dims = c(1L, 0L),
                              indices = list(c(1L, 1L, 2L), c(1L, 1L)),
                              dimBefore = 3L,
                              dimAfter = c(3L, 2L)),
                          new("CollapseTransform",
                              dims = 1:2,
                              indices = list(1:3, 1:2),
                              dimBefore = c(3L, 2L),
                              dimAfter = c(3L, 2L))))
    x <- Values(array(0,
                      dim = c(3, 0),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                                      sex = NULL)))
    y <- Counts(array(0,
                      dim = c(0, 2),
                      dimnames = list(sex = NULL, age = c("0-4", "5-9"))))
    expect_identical(makePairTransforms(x, y),
                     list(new("ExtendTransform",
                              dims = 1:2,
                              indices = list(1:2, integer()),
                              dimBefore = c(3L, 0L),
                              dimAfter = c(2L, 0L)),
                          new("CollapseTransform",
                              dims = 2:1,
                              indices = list(integer(), 1:2),
                              dimBefore = c(0L, 2L),
                              dimAfter = c(2L, 0L))))
    x <- Values(array(1,
                      dim = c(1, 1),
                      dimnames = list(sex = "f", iter = 1)))
    y <- Counts(array(1,
                      dim = c(1, 2),
                      dimnames = list(sex = "f", iter = 1:2)))
    expect_identical(makePairTransforms(x, y),
                     list(new("ExtendTransform",
                              dims = 1:2,
                              indices = list(1L, 1L),
                              dimBefore = c(1L, 1L),
                              dimAfter = c(1L, 1L)),
                          new("CollapseTransform",
                              dims = 1:2,
                              indices = list(1L, c(1L, 0L)),
                              dimBefore = c(1L, 2L),
                              dimAfter = c(1L, 1L))))
    x <- Values(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                                      sex = c("f", "m"))),
                dimtypes = c(sex = "state"))
    y <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(sex = c("f", "m", "o"), age = c("0-4", "5-9"))),
                dimtypes = c(sex = "state"))
    expect_identical(makePairTransforms(x, y),
                     list(new("ExtendTransform",
                              dims = 1:2,
                              indices = list(1:2, 1:2),
                              dimBefore = c(3L, 2L),
                              dimAfter = c(2L, 2L)),
                          new("CollapseTransform",
                              dims = 2:1,
                              indices = list(c(1L, 2L, 0L), 1:2),
                              dimBefore = c(3L, 2L),
                              dimAfter = c(2L, 2L))))
})

test_that("makePairTransforms method for Values works", {
    makePairTransforms <- dembase:::makePairTransforms
    x <- Values(array(0,
                      dim = c(2, 2, 2),
                      dimnames = list(age = c("0-4", "5+"),
                      sex = c("m", "f"),
                      region = c("a", "b"))))
    y <- Values(array(0,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = c("f", "m"))))
    expect_identical(makePairTransforms(x, y),
                     list(new("ExtendTransform",
                              dims = 1:3,
                              indices = list(c(1L, 2L, 2L), 1:2, 1:2),
                              dimBefore = c(2L, 2L, 2L),
                              dimAfter = c(3L, 2L, 2L)),
                          new("ExtendTransform",
                              dims = c(1L, 2L, 0L),
                              indices = list(1:3, 2:1, rep(1L, 2)),
                              dimBefore = c(3L, 2L),
                              dimAfter = c(3L, 2L, 2L))))
    x <- Values(array(0,
                      dim = c(0, 2),
                      dimnames = list(age = NULL,
                      sex = c("m", "f"))))
    y <- Values(array(0,
                      dim = c(3, 0),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = NULL)))
    expect_identical(makePairTransforms(x, y),
                     list(new("ExtendTransform",
                              dims = 1:2,
                              indices = list(integer(), integer()),
                              dimBefore = c(0L, 2L),
                              dimAfter = c(0L, 0L)),
                          new("ExtendTransform",
                              dims = 1:2,
                              indices = list(integer(), integer()),
                              dimBefore = c(3L, 0L),
                              dimAfter = c(0L, 0L))))
    x <- Values(array(0,
                      dim = c(0, 2),
                      dimnames = list(age = NULL,
                      sex = c("m", "f"))))
    y <- Values(array(0,
                      dim = c(0, 2),
                      dimnames = list(age = NULL,
                      sex = c("f", "m"))))
    expect_identical(makePairTransforms(x, y),
                     list(new("ExtendTransform",
                              dims = 1:2,
                              indices = list(integer(), 1:2),
                              dimBefore = c(0L, 2L),
                              dimAfter = c(0L, 2L)),
                          new("ExtendTransform",
                              dims = 1:2,
                              indices = list(integer(), 2:1),
                              dimBefore = c(0L, 2L),
                              dimAfter = c(0L, 2L))))
    x <- Values(array(1:12,
                      dim = c(2, 3, 2),
                      dimnames = list(age = c("0-4", "5+"),
                      sex = c("o", "m", "f"),
                      region = c("a", "b"))),
                dimtypes = c(sex = "state"))
    y <- Values(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                                      sex = c("f", "m"))),
                dimtypes = c(sex = "state"))
    expect_identical(makePairTransforms(x, y),
                     list(new("ExtendTransform",
                              dims = 1:3,
                              indices = list(c(1L, 2L, 2L), 2:3, 1:2),
                              dimBefore = c(2L, 3L, 2L),
                              dimAfter = c(3L, 2L, 2L)),
                          new("ExtendTransform",
                              dims = c(1L, 2L, 0L),
                              indices = list(1:3, 2:1, rep(1L, 2)),
                              dimBefore = c(3L, 2L),
                              dimAfter = c(3L, 2L, 2L))))
})

test_that("makeTransform method for Values works without concordances", {
    makeTransform <- dembase:::makeTransform
    x <- Values(array(0,
                      dim = c(2, 2),
                      dimnames = list(age = c("0-4", "5+"),
                      sex = c("m", "f"))))
    y <- Values(array(0,
                      dim = c(3, 2, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = c("f", "m"),
                      region = c("a", "b"))))
    expect_identical(makeTransform(x, y),
                     new("ExtendTransform",
                         dims = c(1:2, 0L),
                         indices = list(c(1L, 2L, 2L), 2:1, c(1L, 1L)),
                         dimBefore = c(2L, 2L),
                         dimAfter = c(3L, 2L, 2L)))
    x <- Values(array(0,
                      dim = c(2, 2),
                      dimnames = list(age = c("0-9", "10+"),
                      sex = c("m", "f"))))
    y <- Values(array(0,
                      dim = c(2, 3, 2),
                      dimnames = list(reg = c("a", "b"),
                      age = c("0-8", "9", "10+"),
                      sex = c("f", "m"))))
    expect_identical(makeTransform(x, y),
                     new("ExtendTransform",
                         dims = c(0L, 1L, 2L),
                         indices = list(c(1L, 1L), c(1L, 1L, 2L), c(2L, 1L)),
                         dimBefore = c(2L, 2L),
                         dimAfter = c(2L, 3L, 2L)))
    x <- Values(array(0,
                      dim = c(2, 0),
                      dimnames = list(age = c("0-4", "5+"),
                      sex = NULL)))
    y <- Values(array(0,
                      dim = c(0, 1),
                      dimnames = list(sex = NULL, age = "0-4")))
    expect_identical(makeTransform(x, y, subset = TRUE),
                     new("ExtendTransform",
                         dims = c(2L, 1L),
                         indices = list(integer(), 1L),
                         dimBefore = c(2L, 0L),
                         dimAfter = c(0L, 1L)))
})

test_that("makeTransform method for Values works when y has class DemographicArray and using concordances", {
    makeTransform <- dembase:::makeTransform
    Concordance <- classconc::Concordance
    x <- Values(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(reg = c("A", "B"),
                          sex = c("f", "m"))))
    y <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(reg = c("a", "b", "c"),
                          sex = c("m", "f"))))
    conc <- Concordance(data.frame(from = c("a", "b", "c"), to = c("A", "A", "B")))
    concordances = list(reg = conc)
    ans.obtained <- makeTransform(x, y, subset = TRUE, concordances = concordances)
    ans.expected <- new("ExtendTransform",
                        dims = 1:2,
                        indices = list(c(1L, 1L, 2L), 2:1),
                        dimBefore = c(2L, 2L),
                        dimAfter = c(3L, 2L))
    expect_identical(ans.obtained, ans.expected)
    x <- Values(array(1:8,
                      dim = c(2, 2, 2),
                      dimnames = list(reg_orig= c("A", "B"),
                          sex = c("m", "f"),
                          reg_dest = c("A", "B"))))
    y <- Counts(array(1:18,
                      dim = c(3, 2, 3),
                      dimnames = list(reg_orig= c("a", "b", "c"),
                          sex = c("m", "f"),
                          reg_dest = c("a", "b", "c"))))
    conc <- Concordance(data.frame(from = c("a", "b", "c"), to = c("A", "A", "B")))
    concordances = list(reg = conc)
    ans.obtained <- makeTransform(x, y, subset = FALSE, concordances = concordances)
    ans.expected <- new("ExtendTransform",
                        dims = 1:3,
                        indices = list(c(1L, 1L, 2L), 1:2, c(1L, 1L, 2L)),
                        dimBefore = c(2L, 2L, 2L),
                        dimAfter = c(3L, 2L, 3L))
    expect_identical(ans.obtained, ans.expected)
})


test_that("makeTransform method for Values works when y has class numeric", {
    makeTransform <- dembase:::makeTransform
    x <- Counts(array(0,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = c("m", "f"))))
    expect_identical(makeTransform(x = x, y = 1),
                     new("CollapseTransform",
                         dims = c(1L, 0L),
                         indices = list(c(1L, 1L, 1L), c(1L, 1L)),
                         dimBefore = c(3L, 2L),
                         dimAfter = 1L))
    x <- Counts(array(0,
                      dim = 3L,
                      dimnames = list(age = c("0-4", "5-9", "10+"))))
    expect_identical(makeTransform(x = x, y = 1),
                     new("CollapseTransform",
                         dims = 1L,
                         indices = list(c(1L, 1L, 1L)),
                         dimBefore = 3L,
                         dimAfter = 1L))
    expect_error(makeTransform(x, 1:2),
                 "'y' has class \"integer\" but does not have length 1")
    x <- Counts(array(0,
                      dim = 0L,
                      dimnames = list(age = character())))
    expect_error(makeTransform(x, 1),
                 "'x' has length 0")    
})

test_that("tfr works", {
    tfr <- dembase:::tfr
    x <- Values(array(runif(n = 3 * 2 * 2 * 3 * 3),
                      dim = c(3, 2, 2, 3, 3),
                      dimnames = list(age = c("15-19", "20-24", "25-29"),
                          sex = c("f", "m"),
                          triangle = c("TL", "TU"),
                          time = c("2001-2005", "2006-2010", "2011-2015"),
                          region = c("a", "b", "c"))))
    ans.obtained <- tfr(x)
    ans.expected <- collapseDimension(Counts(2.5 * x), margin = c("time", "region"))
    expect_equal(ans.obtained, ans.expected)
    x <- Values(array(runif(n = 2 * 3 * 2),
                      dim = c(2, 3, 2),
                      dimnames = list(gender = c("f", "m"),
                          age = c("15-19", "20-24", "25-34"),
                          time = c("2001-2005", "2006-2010"))))
    ans.obtained <- tfr(x)
    ans.expected <- collapseDimension(Counts(rep(c(5, 5, 10), each = 2) * x),
                                      margin = "time")
    expect_equal(ans.obtained, ans.expected)
    x <- Values(array(runif(n = 2 * 3 * 2),
                      dim = c(2, 3, 2),
                      dimnames = list(sexx = c("f", "m"),
                          age = c("15-19", "20-24", "25-34"),
                          time = c("2001-2005", "2006-2010"))),
                dimtypes = c(sexx = "sex"))
    x[1] <- NA
    ans.obtained <- tfr(x)
    ans.expected <- collapseDimension(Counts(rep(c(5, 5, 10), each = 2) * x),
                                      margin = "time")
    expect_equal(ans.obtained, ans.expected)
    x <- Values(array(-1,
                      dim = c(2, 3, 2),
                      dimnames = list(sexx = c("f", "m"),
                          age = c("15-19", "20-24", "25-34"),
                          time = c("2001-2005", "2006-2010"))),
                dimtypes = c(sexx = "sex"))
    expect_error(tfr(x),
                 "negative values")
    expect_error(tfr(ValuesOne(integer(), labels = character(), name = "age")),
                 "'object' has length 0")
})

test_that("dplot works", {
    data <- Values(array(rexp(n = 36),
                      dim = c(3, 3, 4),
                      dimnames = list(reg_orig = c("a", "b", "c"),
                      reg_dest = c("a", "b", "c"),
                      age = c("0-4", "5-9", "10-14", "15+"))))
    weights <- Counts(array(rpois(n = 36, lambda = 20),
                      dim = c(3, 3, 4),
                      dimnames = list(reg_orig = c("a", "b", "c"),
                      reg_dest = c("a", "b", "c"),
                      age = c("0-4", "5-9", "10-14", "15+"))))
    p <- dplot(value ~ age | reg_orig + reg_dest, data = data, weights = weights)
    expect_is(p, "trellis")
    p <- dplot(~ reg_dest, data = data, weights = weights)
    expect_is(p, "trellis")
    p <- dplot(log(value) ~ reg_orig, data = data, weights = weights)
    expect_is(p, "trellis")
    p <- dplot(value + log(value) ~ reg_orig, data = data, weights = weights)
    expect_is(p, "trellis")
    f <- function(df, w) dplot(value ~ reg_orig, data = df, weights = w)
    p <- f(data, weights)
    expect_is(p, "trellis")
    p <- dplot(value ~ reg_dest, data = data, weights = weights, subarray = age > 5)
    expect_is(p, "trellis")
    p <- dplot(value ~ reg_dest, data = data, weights = weights, subarray = age == "0-4")
    expect_is(p, "trellis")
    p <- dplot(value ~ age | reg_dest + reg_orig, data = data)
    expect_is(p, "trellis")
    expect_error(dplot(value ~ age | reg_dest, data = data),
                 sprintf("need to collapse %s dimension but 'weights' argument not supplied",
                         dQuote("reg_orig")))
    expect_error(dplot(value ~ age, data = data),
                 sprintf("need to collapse %s, %s dimensions but 'weights' argument not supplied",
                         dQuote("reg_orig"), dQuote("reg_dest")))
    data <- Values(array(rexp(n = 36),
                      dim = c(3, 3, 4),
                      dimnames = list(reg_orig = c("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                                      "b", "c"),
                      reg_dest = c("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", "b", "c"),
                      age = c("0-4", "5-9", "10-14", "15+"))))
    p <- dplot(value ~ age,
      data = data,
      midpoints = TRUE,
      type = "l",
      subarray = ((reg_orig == "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa") &
                  (reg_dest == "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa") &
                  (age < 80)))
    expect_is(p, "trellis")
    lambda <- Values(array(c(10, 15, 20, 5, 10, 15),
                    dim = c(3, 2),
                    dimnames = list(age = c("0-4", "5-9", "10+"), sex = c("f", "m"))))
    x <- Values(array(replicate(n = 100, rpois(n = 6, lambda = lambda)),
                      dim = c(dim(lambda), 100),
                      dimnames = c(dimnames(lambda), list(iteration = 1:100))))
    w <- Counts(array(1:6, dim = dim(lambda), dimnames = dimnames(lambda)))
    p <- dplot( ~ age, data = x, weights = w)
    expect_is(p, "trellis")
    p <- dplot( ~ age, data = x, midpoints = TRUE, weights = w)
    expect_is(p, "trellis")
    p <- dplot( ~ age, data = x, midpoints = TRUE, weights = w,
               overlay = list(values = lambda, col = "red"))
    expect_is(p, "trellis")
    p <- dplot( ~ age | sex, data = x, weights = w)
    expect_is(p, "trellis")
    p <- dplot( ~ age | sex, data = x, weights = w, probs = c(0.025, 0.2, 0.8, 0.975))
    expect_is(p, "trellis")
    p <- dplot( ~ age | sex, data = x, weights = w, probs = c(0.025, 0.2, 0.5, 0.8, 0.975),
               overlay = list(values = lambda + 3, col = "red"))
    expect_is(p, "trellis")
    p <- dplot( ~ age | sex, data = x, weights = w, probs = c(0.025, 0.2, 0.5, 0.8, 0.975),
               overlay = list(values = lambda + 3, col = "red"), midpoints = TRUE)
    expect_is(p, "trellis")
    p <- dplot( ~ age, groups = sex, data = x, weights = w, alpha = 0.5)
    expect_is(p, "trellis")
    x.with.missing <- x
    x.with.missing[1] <- NA
    p <- dplot( ~ age, data = x.with.missing, weights = w, na.rm = TRUE)
})





























