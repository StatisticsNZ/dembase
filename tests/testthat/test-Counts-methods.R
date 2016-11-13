

context("Counts-methods")
n.test <- 5
test.identity <- FALSE

test_that("coercion to data.frame works", {
    a <- array(1:6,
               dim = c(3, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
               sex = c("Male", "Female")))
    x <- Counts(a)
    expect_identical(as(x, "data.frame"),
                     as.data.frame.table(a, responseName = "count"))
    a <- array(0L,
               dim = c(0, 2),
               dimnames = list(age = NULL,
               sex = c("Male", "Female")))
    x <- Counts(a)
    expect_identical(as(x, "data.frame"),
                     data.frame(age = factor(), sex = factor(), count = integer()))
    a <- array(1:6,
               dim = c(3, 2),
               dimnames = list(year = c(2000, 2010, 2015),
               sex = c("Male", "Female")))
    x <- Counts(a)
    expect_identical(as(x, "data.frame"),
                     data.frame(expand.grid(year = c(2000, 2010, 2015),
                                            sex = c("Male", "Female")),
                                count = 1:6))
})

test_that("Ops works when e2 is Counts", {
    x <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(reg = c("a", "b"),
                      sex = c("m", "f"))))
    y <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(reg = c("a", "b"),
                      sex = c("f", "m"))))
    z1 <- Counts(array(c(4L, 6L, 4L, 6L),
                       dim = c(2, 2),
                       dimnames = list(reg = c("a", "b"),
                       sex = c("m", "f"))))
    z2 <- Values(array(c(1/3, 2/4, 3/1, 4/2),
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
    z5 <- Values(array((1:4) * c(3:4, 1:2),
                       dim = c(2, 2),
                       dimnames = list(reg = c("a", "b"),
                           sex = c("m", "f"))))
    expect_identical(x + y, z1)
    expect_identical(x / y, z2)
    expect_identical(x - t(y), x - y)
    expect_identical(x > y, z3)
    expect_identical(x | y, z4)
    expect_identical(x * y, z5)
    x <- Counts(array(0,
                      dim = c(0, 2),
                      dimnames = list(reg = NULL, sex = c("m", "f"))))
    y <- Counts(array(0,
                      dim = c(0, 2),
                      dimnames = list(reg = NULL, sex = c("f", "m"))))
    z1 <- Values(array(0,
                       dim = c(0, 2),
                       dimnames = list(reg = NULL, sex = c("m", "f"))))
    z2 <- array(NA,
                dim = c(0, 2),
                dimnames = list(reg = NULL, sex = c("m", "f")))
    expect_identical(x + y, x)
    expect_identical(x /y, z1)
    expect_identical(x < y, z2)
    x <- Counts(array(1:2,
                      dim = 2,
                      dimnames = list(sex = c("m", "f"))))
    y <- Counts(array(2:3,
                      dim = c(2, 2),
                      dimnames = list(sex = c("m", "f"), iter = 1:2)))
    z <- Counts(array(c(3L, 5L),
                      dim = c(2, 2),
                      dimnames = list(sex = c("m", "f"), iter = 1:2)))
    expect_identical(x + y, z)
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
    z1 <- Counts(array(c(3L, 8L, 3L, 8L),
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
    expect_identical(x + y, as(x, "Values") + y)
    expect_identical(x / y, z2)
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
    x <- Counts(array(1:2,
                      dim = 2,
                      dimnames = list(sex = c("m", "f"))))
    y <- Values(array(2:3,
                      dim = c(2, 2),
                      dimnames = list(sex = c("m", "f"), iter = 1:2)))
    z <- Counts(array(c(2L, 6L),
                      dim = c(2, 2),
                      dimnames = list(sex = c("m", "f"), iter = 1:2)))
    expect_identical(x * y, z)
    x <- Counts(array(1:3,
                      dim = 3,
                      dimnames = list(sex = c("m", "f", "o"))),
                dimtypes = c(sex = "state"))
    y <- Values(array(1:2,
                      dim = 2,
                      dimnames = list(sex = c("f", "m"))),
                dimtypes = c(sex = "state"))
    z <- Counts(array((1:2) * (2:1),
                      dim = 2,
                      dimnames = list(sex = c("m", "f"))),
                dimtypes = c(sex = "state"))
    expect_identical(x * y, z)
})

test_that("Ops works when e2 is numeric", {
    a <- array(1:6,
               dim = 2:3,
               dimnames = list(time = c(2000, 2005),
               ethnicity = c("a", "b", "c")))
    x <- Counts(a)
    expect_identical(x / 22, Counts(a / 22))
    expect_identical(x %% -1.3, Counts(a %% -1.3))
    expect_identical(x > 4, a > 4)
    expect_identical(x & 1, a & 1)
    a <- array(1:3, dim = 3, dimnames = list(quantile = c("1%", "50%", "99%")))
    x <- Counts(a)
    expect_identical(x * 3, Counts(a * 3))
    expect_identical(x / 3, Counts(a / 3))
    expect_identical(x < 2, a < 2)
    expect_identical(x != 2, a != 2)
    expect_error(x * 1:3,
                 "dimension \"quantile\" has dimtype \"quantile\"")
    expect_error(x * -Inf,
                 "dimension \"quantile\" has dimtype \"quantile\"")
    expect_error(x + 1,
                 "dimension \"quantile\" has dimtype \"quantile\"")
    expect_error(x / -1,
                 "dimension \"quantile\" has dimtype \"quantile\"")
    a <- array(1:6, dim = c(3, 2), dimnames = list(sim = 1:3, sex = c("f", "m")))
    x <- Counts(a)
    expect_identical(x * 1:3, Counts(a * 1:3))
    a <- array(1:4,
               dim = c(2, 2),
               dimnames = list(eth_parent = c("a", "b"),
               eth_child = c("a", "b")))
    x <- Counts(a)
    expect_identical(x^4, Counts(a^4))
    expect_identical(x - 22, Counts(a - 22))
    expect_identical(x <= 1, a <= 1)
    expect_identical(x == 1:2, a == 1:2)
    a <- array(1:3, dim = 3, dimnames = list(quantile = c("1%", "50%", "99%")))
    x <- Counts(a)
    expect_identical(x * 3, Counts(a * 3))
    expect_identical(x / 3, Counts(a / 3))
    expect_identical(x ^ 22, Counts(a ^ 22))
    expect_identical(x / 1L, x * 1.0)
    expect_identical(x > 1, a > 1)
    expect_error(x * 1:3,
                 "dimension \"quantile\" has dimtype \"quantile\"")
    expect_error(x * -1,
                 "dimension \"quantile\" has dimtype \"quantile\"")
    expect_error(x / -1.1,
                 "dimension \"quantile\" has dimtype \"quantile\"")
})

test_that("Ops works when e1 is numeric", {
    a <- array(1:4,
               dim = c(2, 2),
               dimnames = list(eth_parent = c("a", "b"),
               eth_child = c("a", "b")))
    x <- Counts(a)
    expect_identical(4^x, Counts(4^a))
    expect_identical(1:4 - x, Counts(1:4 - a))
    expect_identical(1 == x, 1 == a)
    expect_identical(2:1 < x, 2:1 < a)
    a <- array(1:3, dim = 3, dimnames = list(quantile = c("1%", "50%", "99%")))
    x <- Counts(a)
    expect_identical(3 *x, Counts(3 * a))
    expect_identical(3 == x, 3 == a)
    expect_identical(0 < x, 0 < a)
    expect_error((1:3) * x,
                 "dimension \"quantile\" has dimtype \"quantile\"")
    expect_error(33 + x,
                 "dimension \"quantile\" has dimtype \"quantile\"")
    expect_error(33 / x,
                 "dimension \"quantile\" has dimtype \"quantile\"")
    a <- array(1:6, dim = c(3, 2), dimnames = list(sim = 1:3, sex = c("f", "m")))
    x <- Counts(a)
    expect_identical(x * 1:3, Counts(a * 1:3))
    expect_identical(x == 1:3, a == 1:3)
})

test_that("Ops works when e1 is an array", {
    a <- array(1:6,
               dim = 2:3,
               dimnames = list(time = c(2000, 2005),
               ethnicity = c("a", "b", "c")))
    x <- Counts(a)
    expect_identical(a / x, Counts(a / a))
    expect_identical(a > x, a > a)
    expect_identical(unname(a) %/% x, Counts(unname(a) %/% a))
    expect_identical(unname(a) == x, unname(a) == a)
    expect_identical(unname(a) > x, unname(a) > a)
    x <- Counts(array(1:3, dim = 3, dimnames = list(quantile = c("1%", "50%", "99%"))))
    expect_error(a * x,
                 "dimension \"quantile\" has dimtype \"quantile\"")
    x <- Counts(array(1:6, dim = c(3, 2), dimnames = list(sim = 1:3, sex = c("f", "m"))))
    a <- array(1:2, dimnames = list(sex = NULL))
    b <- array(rep(1:2, each = 3), dim = c(3, 2))
    expect_identical(a / x, b / x)
    expect_identical(a == x, b == x)
    expect_identical(a <= x, b <= x)
})

test_that("Ops works when e2 is an array", {
    a <- array(1:6,
               dim = 2:3,
               dimnames = list(time = c(2000, 2005),
               ethnicity = c("a", "b", "c")))
    x <- Counts(a)
    expect_identical(x / a, Counts(a / a))
    expect_identical(x %% unname(a), Counts(a %% unname(a)))
    x <- Counts(array(1:3, dim = 3, dimnames = list(quantile = c("1%", "50%", "99%"))))
    expect_error(x * as(x, "array"),
                 "dimension \"quantile\" has dimtype \"quantile\"")
    x <- Counts(array(1:6, dim = c(3, 2), dimnames = list(sim = 1:3, sex = c("f", "m"))))
    a <- array(1:2, dimnames = list(sex = NULL))
    b <- array(rep(1:2, each = 3), dim = c(3, 2))
    expect_identical(x * a, x * b)
    expect_identical(x > a, x > b)
    expect_identical(x == a, x == b)
})

test_that("Ops works with tables and xtabs", {
    x <- Counts(array(1,
                      dim = c(2, 3),
                      dimnames = list(gender = c("m", "f"), year = 2000:2002)),
                dimscales = c(year = "Points"))
    d <- as.data.frame(x, direction = "long")
    xt <- xtabs(count ~ gender + year, d)
    tab <- table(d$gender, d$year)
    expect_identical(x * xt, x * as(x, "array"))
    expect_identical(xt + x, x + x)
    expect_identical(tab / x, as(tab, "array") / x)
    expect_identical(x %% tab, x %% as(tab, "array"))
    expect_identical(x > tab, x > as(tab, "array"))
})

test_that("addDimension works", {
    x <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(age = c("0-4", "5+"), sex = c("f", "m"))))
    y <- Counts(array(1:4,
                      dim = c(2, 2, 2),
                      dimnames = list(age = c("0-4", "5+"), sex = c("f", "m"), iteration = 1:2)))
    expect_identical(addDimension(x, name = "iteration", labels = 1:2),
                     y)
    x <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(age = c("0-4", "5+"), sex = c("f", "m"))))
    y <- Counts(array(1:4,
                      dim = c(2, 1, 1, 2),
                      dimnames = list(age = c("0-4", "5+"), reg = "a", eth = "b", sex = c("f", "m"))))
    expect_identical(addDimension(x, name = c("reg", "eth"), labels = list("a", "b"), after = 1),
                     y)
})

test_that("as.data.frame works", {
    a <- array(1:12,
               dim = c(3, 2, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
               sex = c("Male", "Female"),
               region = c("a", "b")))
    b <- array(1:12,
               dim = c(3, 2, 2),
               dimnames = list(age = c("2.5", "7.5", "12.5"),
               sex = c("Male", "Female"),
               region = c("a", "b")))
    d <- data.frame(expand.grid(age = c(2.5, 7.5, 12.5),
                                 sex = c("Male", "Female"),
                                 region = c("a", "b")),
                     count = 1:12)
    x <- Counts(a)
    expect_identical(as.data.frame(x), as.data.frame(a))
    expect_identical(as.data.frame(x, midpoints = TRUE), as.data.frame(b))
    expect_identical(as.data.frame(x, midpoints = "age"), as.data.frame(b))
    expect_identical(as.data.frame(x, midpoints = 1), as.data.frame(b))
    expect_identical(as.data.frame(x, direction = "long", midpoints = TRUE), d)
    expect_identical(as.data.frame(x, direction = "long", midpoints = "age"), d)
    a <- array(0L,
               dim = c(0, 2),
               dimnames = list(age = NULL,
               sex = c("Male", "Female")))
    x <- Counts(a)
    expect_identical(as.data.frame(x),
                     as.data.frame(a))
    expect_identical(as.data.frame(x, direction = "long"),
                     as(x, "data.frame"))
    a <- array(1:4,
               dim = c(2, 2),
               dimnames = list(age = c("0-4", "5+"),
                   period = c("2001-2005", "2006-2010")))
    b <- array(1:4,
               dim = c(2, 2),
               dimnames = list(age = c("0-4", "5+"), period = c("2002.5", "2007.5")))
    d <- data.frame(age = c("0-4", "5+", "0-4", "5+"),
                    period = c(2002.5, 2002.5, 2007.5, 2007.5),
                    count = 1:4)
    x <- Counts(a)
    expect_identical(as.data.frame(x), as.data.frame(a))
    expect_identical(as.data.frame(x, midpoints = "period"), as.data.frame(b))
    expect_identical(as.data.frame(x, midpoints = "period", direction = "long"), d)
    x <- Counts(array(1:6,
                      dim = c(2, 3),
                      dimnames = list(sex = c("f", "m"), quantile = c("0%", "50%", "100%"))))
    df <- as.data.frame(x, direction = "long")
    expect_identical(levels(df$quantile), c("0%", "50%", "100%"))
    x <- Counts(array(1:6,
                      dim = c(2, 3),
                      dimnames = list(sex = c("f", "m"), quantile = c("0%", "50%", "100%"))))
    df <- as.data.frame(x, responseName = "Count", direction = "long")
    expect_identical(names(df), c("sex", "quantile", "Count"))
})


test_that("canMakeCompatible works in simple cases", {
    canMakeCompatible <- dembase:::canMakeCompatible
    x <- Counts(array(0,
                      dim = 3:2,
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = c("Male", "Female"))))
    y <- Counts(array(0,
                      dim = 3,
                      dimnames = list(age = c("0-4", "5-9", "10+"))))
    expect_true(canMakeCompatible(x, t(x)))
    expect_true(canMakeCompatible(x, y))
    expect_error(canMakeCompatible(y, x),
                 sprintf("one object has dimension \\[%s\\] that other does not",
                         dQuote("sex")))
    x <- Counts(array(0,
                      dim = 3,
                      dimnames = list(iter = 1:3)),
                dimtypes = c(iter = "time"),
                dimscales = c(iter = "Points"))
    y <- Counts(array(0,
                      dim = 3,
                      dimnames = list(iter = 1:3)),
                dimtypes = c(iter = "age"))
    expect_error(canMakeCompatible(x, y),
                 sprintf("%s dimensions have different dimtypes : %s versus %s",
                         dQuote("iter"), dQuote("time"), dQuote("age")))
    x <- Counts(array(0,
                      dim = 3:2,
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = c("Male", "Female"))))
    y <- Counts(array(0,
                      dim = c(2, 2),
                      dimnames = list(age = c("0-4", "5+"),
                      sex = c("Male", "Female"))))
    expect_true(canMakeCompatible(x, y))
    expect_error(canMakeCompatible(y, x),
                 paste("\"age\" dimensions have incompatible dimscales :",
                       "one dimension has break \\[10\\] that other does not"))
    expect_error(canMakeCompatible(y, x, subset = TRUE),
                 paste("\"age\" dimensions have incompatible dimscales :",
                       "one dimension has break \\[10\\] that other does not"))
    x <- Counts(array(0,
                      dim = c(3, 2, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      reg_orig = c("a", "b"),
                      reg_dest = c("a", "b"))))
    y <- Counts(array(0,
                      dim = 3,
                      dimnames = list(age = c("0-4", "5-9", "10+"))))
    expect_true(canMakeCompatible(x, y))
    expect_error(canMakeCompatible(y, x),
                 sprintf("one object has dimensions \\[%s, %s\\] that other does not",
                         dQuote("reg_orig"), dQuote("reg_dest")))
})

test_that("canMakeCompatible works with concordances", {
    canMakeCompatible <- dembase:::canMakeCompatible
    Concordance <- classconc::Concordance
    x <- Counts(array(1:9,
                      dim = c(3, 3),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      region = c("a", "b", "c"))))
    y <- Counts(array(1:6,
                      dim = 3:2,
                      dimnames = list(age = c("0-4", "5-9", "10+"), region = c("X", "Y"))))
    conc <- Concordance(data.frame(from = c("a", "b", "c"), to = c("X", "X", "Y")))
    concordances <- list(region = conc)
    expect_true(canMakeCompatible(x, y, concordances = concordances))
    x <- Counts(array(1:9,
                      dim = c(3, 3),
                      dimnames = list(reg_orig = c("a", "b", "c"),
                          reg_dest = c("a", "c", "b"))))
    y <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(reg_orig = c("A", "B"), 
                          reg_dest = c("A", "B"))))
    conc <- Concordance(data.frame(from = c("a", "b", "c"), to = c("A", "B", "B")))
    concordances <- list(reg_orig = conc, reg_dest = conc)
    expect_true(canMakeCompatible(x, y, concordances = concordances))
    x <- Counts(array(1:9,
                      dim = c(3, 3),
                      dimnames = list(reg_orig = c("a", "b", "c"),
                          reg_dest = c("a", "c", "b"))))
    y <- Counts(array(1,
                      dim = c(1, 1),
                      dimnames = list(reg_orig = "A",
                          reg_dest = "A")))
    conc <- Concordance(data.frame(from = c("a", "b", "c"), to = c("A", "B", "B")))
    concordances <- list(reg_orig = conc, reg_dest = conc)
    expect_error(canMakeCompatible(x, y, concordances = concordances),
                 sprintf("one dimension has value \\[%s\\] that other does not",
                         dQuote("B")))
})

test_that("canMakeCompatible works with dimensions with length 0", {
    canMakeCompatible <- dembase:::canMakeCompatible
    x <- Counts(array(0,
                      dim = c(3, 0),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = character())))
    y <- Counts(array(0,
                      dim = 3,
                      dimnames = list(age = c("0-4", "5-9", "10+"))))
    expect_true(canMakeCompatible(x, x))
    expect_error(canMakeCompatible(x, y),
                 sprintf("one object has dimension \\[%s\\] with length 0 that other does not",
                         dQuote("sex")))
    expect_error(canMakeCompatible(y, x),
                 sprintf("one object has dimension \\[%s\\] that other does not",
                         dQuote("sex")))
    x <- Counts(array(0,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = c("Male", "Female"))))
    y <- Counts(array(0,
                      dim = c(3, 0),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = character())))
    expect_error(canMakeCompatible(x, y),
                 sprintf("\"sex\" dimensions have incompatible dimscales : one dimension has values \\[%s, %s\\] that other does not",
                         dQuote("Male"), dQuote("Female")))
})

test_that("canMakeOrigDestParentChildCompatible works", {
    canMakeOrigDestParentChildCompatible <- dembase:::canMakeOrigDestParentChildCompatible
    ## simple, orig-dest only
    x <- Counts(array(0,
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
    ## orig-dest only; need to subset and permute
    x <- Counts(array(0,
                      dim = c(2, 3, 3, 2),
                      dimnames = list(time = c("2001-2005", "2006-2010"),
                          age = c("0-4", "5-9", "10+"),
                          region_orig = c("a", "b", "c"),
                          region_dest = c("c", "b"))))
    y <- Counts(array(0,
                      dim = c(3, 2, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          region = c("a", "b"),
                          time = c("2001-2005", "2006-2010"))))
    expect_true(canMakeOrigDestParentChildCompatible(x = x, y = y, subset = TRUE))
    ## orig-dest and parent-child; need to subset and permute
    x <- Counts(array(0,
                      dim = c(2, 2, 3, 3, 3, 2),
                      dimnames = list(time = c("2001-2005", "2006-2010"),
                          eth_child = 1:2,
                          eth_parent = 3:1,
                          age = c("0-4", "5-9", "10+"),
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

test_that("canMakeCompatible works with dimtype Iteration", {
    canMakeCompatible <- dembase:::canMakeCompatible
    x <- Counts(array(1:2,
                      dim = 2,
                      dimnames = list(sex = c("f", "m"))))
    y <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(sim = 1:3, sex = c("f", "m"))))
    expect_true(canMakeCompatible(x, y))
    expect_error(canMakeCompatible(y, x),
                 "dimension \"sim\" has dimtype \"iteration\" and cannot be collapsed")
    expect_true(canMakeCompatible(y, t(y)))
    expect_error(canMakeCompatible(x, y, allowCopyIterDim = FALSE),
               sprintf("one object has dimension \\[%s\\] that other does not",
                       dQuote("sim")))
})

test_that("canMakePairCompatible works in simple cases", {
    canMakePairCompatible <- dembase:::canMakePairCompatible
    ## 'y' is permuted, collapsed version of 'x'
    x <- Counts(array(0,
                      dim = 3:2,
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = c("Male", "Female"))))
    y <- Counts(array(0,
                      dim = 3,
                      dimnames = list(age = c("0-4", "5-9", "10+"))))
    expect_true(canMakePairCompatible(x, t(x)))
    expect_true(canMakePairCompatible(x, y))
    expect_true(canMakePairCompatible(y, x))
    ## some subsetting needed
    x <- Counts(array(0,
                      dim = c(2, 2),
                      dimnames = list(age = c("0-4", "5-9"),
                      sex = c("Male", "Female"))))
    y <- Counts(array(0,
                      dim = 3,
                      dimnames = list(age = c("0-4", "5-9", "10+"))))
    expect_true(canMakePairCompatible(x, t(x)))
    expect_true(canMakePairCompatible(x, y))
    expect_true(canMakePairCompatible(y, x))
    ## incompatible dimtypes    
    x <- Counts(array(0,
                      dim = c(3, 1),
                      dimnames = list(iter = 1:3, sex = "f")),
                dimtypes = c(iter = "iteration", sex = "state"))
    y <- Counts(array(0,
                      dim = c(3, 1),
                      dimnames = list(iter = 1:3, sex = "f")),
                dimtypes = c(iter = "age", sex = "state"))
    expect_error(canMakePairCompatible(x, y),
                 sprintf("%s dimensions have different dimtypes : %s versus %s",
                         dQuote("iter"), dQuote("iteration"), dQuote("age")))
    ## values and counts
    x <- Counts(array(0,
                      dim = 3:2,
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                                      sex = c("Male", "Female"))),
                dimtypes = c(sex = "state"))
    y <- Values(array(0,
                      dim = c(2, 3),
                      dimnames = list(age = c("0-4", "5+"),
                                      sex = c("Male", "Female", "Other"))),
                dimtypes = c(sex = "state"))
    expect_true(canMakePairCompatible(x, y))
    expect_true(canMakePairCompatible(y, x))
    ## orig-dest dimensions
    x <- Counts(array(0,
                      dim = c(3, 2, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      reg_orig = c("a", "b"),
                      reg_dest = c("a", "b"))))
    y <- Counts(array(0,
                      dim = 3,
                      dimnames = list(age = c("0-4", "5-9", "10+"))))
    expect_true(canMakePairCompatible(x, y))
    expect_true(canMakePairCompatible(y, x))
    ## need to subset
    x <- Counts(array(0,
                      dim = c(2, 3),
                      dimnames = list(age = c("0-4", "5-9"),
                                      sex = c("Male", "Female", "Other"))),
                dimtypes = c(sex = "state"))
    y <- Counts(array(0,
                      dim = 3:2,
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                                      sex = c("Male", "Female"))),
                dimtypes = c(sex = "state"))
    expect_true(canMakePairCompatible(x, y))
    expect_true(canMakePairCompatible(y, x))
    ## need to expand vaues intervals
    x <- Counts(array(0,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = c("Female", "Male"))))
    y <- Values(array(0,
                      dim = c(2, 2),
                      dimnames = list(age = c("0-4", "5+"),
                      sex = c("Male", "Female"))))
    expect_true(canMakePairCompatible(x, y))
    ## values too detailed
    x <- Values(array(0,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = c("Female", "Male"))))
    y <- Counts(array(0,
                      dim = c(2, 2),
                      dimnames = list(age = c("0-4", "5+"),
                      sex = c("Male", "Female"))))
    expect_error(canMakePairCompatible(x, y),
                 paste("\"age\" dimensions have incompatible dimscales :",
                       "intervals do not align"))
})

test_that("canMakePairCompatible works with dimensions with length 0", {
    canMakePairCompatible <- dembase:::canMakePairCompatible
    x <- Counts(array(0,
                      dim = c(3, 0),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = character())))
    y <- Counts(array(0,
                      dim = 3,
                      dimnames = list(age = c("0-4", "5-9", "10+"))))
    expect_true(canMakePairCompatible(x, x))
    expect_true(canMakePairCompatible(x, y))
    expect_true(canMakePairCompatible(y, x))
    x <- Counts(array(0,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = c("Male", "Female"))))
    y <- Counts(array(0,
                      dim = c(3, 0),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = character())))
    expect_true(canMakePairCompatible(x, y))
    x <- Counts(array(0,
                      dim = c(0, 2),
                      dimnames = list(sex = NULL, age = c("0-4", "5-9"))))
    y <- Values(array(1,
                      dim = 1,
                      dimnames = list(age = "0-9")))
    expect_true(canMakePairCompatible(e1 = x, e2 = y))
})

test_that("canMakePairCompatible works when e2 is array", {
    canMakePairCompatible <- dembase:::canMakePairCompatible
    a <- array(1:4,
               dim = c(2, 2),
               dimnames = list(reg_orig  = c("a", "b"), reg_dest = c("a", "b")))
    x <- Counts(a)
    expect_true(canMakePairCompatible(x, a))
    expect_error(canMakePairCompatible(x, t(a)),
                 "names of dimensions do not match \\[\"reg_orig\" versus \"reg_dest\"\\]")
    a <- array(1:4,
               dim = c(2, 2),
               dimnames = list(reg_orig  = c("a", "b"), reg_dest = c("a", "b")))
    b <- array(1:4,
               dim = c(2, 2, 3),
               dimnames = list(reg_orig  = c("a", "b"),
               reg_dest = c("a", "b"),
               sim = 1:3))
    x <- Counts(b)
    expect_true(canMakePairCompatible(x, a))
    x <- aperm(x, c("sim", "reg_orig", "reg_dest"))
    expect_true(canMakePairCompatible(x, a))
})

test_that("canMakePairCompatible works when e1 is array", {
    canMakePairCompatible <- dembase:::canMakePairCompatible
    a <- array(1:4,
               dim = c(2, 2),
               dimnames = list(age = c("0-4", "5+"),
                   region = c("a", "b")))
    x <- Counts(a)
    expect_true(canMakePairCompatible(a, x))
    expect_true(canMakePairCompatible(t(x), t(a)))
    a <- array(1:4,
               dim = c(2, 2),
               dimnames = list(reg_orig  = c("a", "b"), reg_dest = c("a", "b")))
    b <- array(1:4,
               dim = c(2, 2, 3),
               dimnames = list(reg_orig  = c("a", "b"),
                   reg_dest = c("a", "b"),
                   sim = 1:3))
    x <- Counts(b)
    expect_true(canMakePairCompatible(a, x))
    expect_true(canMakePairCompatible(unname(a), x))
    names(dimnames(a)) <- NULL
    expect_true(canMakePairCompatible(unname(a), x))
})

test_that("canMakePairCompatible works with dimtype Iteration", {
    canMakePairCompatible <- dembase:::canMakePairCompatible
    x <- Counts(array(1:2,
                      dim = 2,
                      dimnames = list(sex = c("f", "m"))))
    y <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(sim = 1:3, sex = c("f", "m"))))
    expect_true(canMakePairCompatible(x, y))
    expect_true(canMakePairCompatible(y, x))
    x <- Counts(array(1:2,
                      dim = c(3, 2),
                      dimnames = list(sim = 1:3, age = c("0-4", "5-9"))))
    y <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(sim = 1:3, sex = c("f", "m"))))
    expect_error(canMakePairCompatible(x, y),
                 "no dimensions in common \\(apart from dimension with dimtype \"iteration\"\\)")
    x <- Counts(array(1:2,
                      dim = c(3, 2),
                      dimnames = list(sim = 1:3, sex = c("m", "f"))))
    y <- Counts(array(1:2,
                      dim = 2,
                      dimnames = list(sex = c("f", "m"))))
    expect_true(canMakePairCompatible(x, y))
    expect_error(canMakePairCompatible(x, y, allowCopyIterDim = FALSE),
                 "one object has dimension with dimtype \"iteration\" but other does not")
    y <- as(y, "Values")
    expect_true(canMakePairCompatible(x, y))
    x <- Counts(array(1:2,
                      dim = 2,
                      dimnames = list(age = c("0-4", "5+"))))
    y <- Values(array(1:2,
                      dim = c(2, 3),
                      dimnames = list(age = c("0-4", "5+"), sim = 1:3)))
    expect_true(canMakePairCompatible(x, y))
    expect_error(canMakePairCompatible(x, y, allowCopyIterDim = FALSE),
                "one object has dimension with dimtype \"iteration\" but other does not")
})

test_that("collapse works", {
    collapse <- dembase:::collapse
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = c("m", "f"))))
    y <- Counts(array(c(4L, 11L, 1L, 5L),
                      dim = c(2, 2),
                      dimnames = list(age = c("0-4", "5+"),
                      sex = c("f", "m"))))
    transform <- new("CollapseTransform",
                     dims = 1:2,
                     indices = list(c(1L, 2L, 2L), 2:1),
                     dimBefore = c(3L, 2L),
                     dimAfter = c(2L, 2L))
    expect_identical(collapse(x, transform = transform), y)
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = c("m", "f"))))
    y <- Counts(array(c(1L, 3L, 4L, 6L),
                      dim = c(2, 2),
                      dimnames = list(age = c("0-4", "10+"),
                      sex = c("m", "f"))),
                dimtypes = c(age = "state"))
    transform <- new("CollapseTransform",
                     dims = 1:2,
                     indices = list(c(1L, 0L, 2L), 1:2),
                     dimBefore = c(3L, 2L),
                     dimAfter = c(2L, 2L))
    expect_identical(collapse(x, transform = transform), y)
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = c("m", "f"))))
    y <- Counts(array(c(2L, 1L),
                      dim = 2,
                      dimnames = list(age = c("5-9", "0-4"))),
                dimtypes = c(age = "state"))
    transform <- new("CollapseTransform",
                     dims = c(1L, 0L),
                     indices = list(c(2L, 1L, 0L), c(1L, 0L)),
                     dimBefore = c(3L, 2L),
                     dimAfter = 2L)
    expect_identical(collapse(x, transform = transform), y)
    x <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(eth_parent = c("a", "b"),
                      eth_child = c("a", "b"))))
    y <- Counts(array(c(4L, 6L),
                      dim = 2,
                      dimnames = list(eth = c("a", "b"))))
    transform <- new("CollapseTransform",
                     dims = c(1L, 0L),
                     indices = list(c(1L, 2L), c(1L, 1L)),
                     dimBefore = c(2L, 2L),
                     dimAfter = 2L)
    expect_identical(collapse(x, transform = transform), y)
    x <- Counts(array(0,
                      dim = c(2, 0),
                      dimnames = list(region = c("a", "b"), age = NULL)))
    y <- Counts(array(0,
                      dim = c(0, 1),
                      dimnames = list(age = NULL, region = "a")))
    transform <- new("CollapseTransform",
                     dims = 2:1,
                     indices = list(c(1L, 0L), integer()),
                     dimBefore = c(2L, 0L),
                     dimAfter = c(0L, 1L))
    expect_identical(collapse(x, transform = transform), y)
    x <- Counts(array(1:2,
                      dim = c(2, 2),
                      dimnames = list(region = c("a", "b"), quantile = c("1%", "99%"))))
    y <- Counts(array(1:2,
                      dim = 2,
                      dimnames = list(region = c("a", "b"))))
    transform <- new("CollapseTransform",
                     dims = c(1L, 0L),
                     indices = list(1:2, c(1L, 0L)),
                     dimBefore = c(2L, 2L),
                     dimAfter = 2L)
    expect_identical(collapse(x, transform = transform),
                     y)
    transform <- new("CollapseTransform",
                     dims = c(1L, 0L),
                     indices = list(1:2, c(1L, 1L)),
                     dimBefore = c(2L, 2L),
                     dimAfter = 2L)
    expect_error(collapse(x, transform = transform),
                 "attempt to aggregate cells when there is a dimension with dimtype \"quantile\"")
    transform <- new("CollapseTransform",
                     dims = c(1L, 2L),
                     indices = list(1:2, c(1L, 1L)),
                     dimBefore = c(2L, 2L),
                     dimAfter = 2:1)
    expect_error(collapse(x, transform = transform),
                 "attempt to aggregate cells when there is a dimension with dimtype \"quantile\"")
    transform <- new("CollapseTransform",
                     dims = c(1L, 2L),
                     indices = list(c(1L, 1L), 1:2),
                     dimBefore = c(2L, 2L),
                     dimAfter = 1:2)
    expect_error(collapse(x, transform = transform),
                 "attempt to aggregate cells when there is a dimension with dimtype \"quantile\"")
    x <- Counts(array(1:2,
                      dim = c(2, 2),
                      dimnames = list(region = c("a", "b"), iteration = 1:2)))
    y <- Counts(array(1:2,
                      dim = 2,
                      dimnames = list(region = c("a", "b"))))
    transform <- new("CollapseTransform",
                     dims = c(1L, 0L),
                     indices = list(1:2, c(1L, 0L)),
                     dimBefore = c(2L, 2L),
                     dimAfter = 2L)
    expect_identical(collapse(x, transform = transform),
                     y)
    transform <- new("CollapseTransform",
                     dims = c(1L, 2L),
                     indices = list(1:2, c(1L, 1L)),
                     dimBefore = c(2L, 2L),
                     dimAfter = 2:1)
    expect_error(collapse(x, transform = transform),
                 "attempt to collapse cells across iterations")
    x <- Counts(array(1:2,
                      dim = c(2, 2),
                      dimnames = list(region = c("a", "b"), iteration = 1:2)))
    transform <- new("CollapseTransform",
                     dims = c(1L, 0L),
                     indices = list(1:2, c(1L, 1L)),
                     dimBefore = c(2L, 2L),
                     dimAfter = 2L)
    expect_error(collapse(x, transform = transform),
                 "attempt to collapse cells across iterations")
    x <- Counts(array(1:2,
                      dim = c(2, 2),
                      dimnames = list(region = c("a", "b"), pool = c("Ins", "Outs"))))
    y <- Counts(array(1:2,
                      dim = c(2, 1),
                      dimnames = list(region = c("a", "b"), pool = "Ins")),
                dimtypes = c(pool = "state"))
    transform <- new("CollapseTransform",
                     dims = 1:2,
                     indices = list(1:2, c(1L, 0L)),
                     dimBefore = c(2L, 2L),
                     dimAfter = 2:1)
    expect_identical(collapse(x, transform = transform),
                     y)
})

test_that("canMakeSharedDimScalesCompatible works", {
    Concordance <- classconc::Concordance
    canMakeSharedDimScalesCompatible <- dembase:::canMakeSharedDimScalesCompatible
    ## no concordances
    e1 <- Counts(array(0,
                       dim = c(3, 2),
                       dimnames = list(age = c("0-4", "5-9", "10+"),
                       sex = c("m", "f"))))
    e2 <- Counts(array(0,
                       dim = c(2, 2),
                       dimnames = list(age = c("0-9", "10+"),
                           sex = c("f", "m"))))
    concordances <- list(age = NULL, sex = NULL)
    expect_true(canMakeSharedDimScalesCompatible(e1, e2, concordances = concordances))
    e1 <- Counts(array(0,
                       dim = c(3, 2),
                       dimnames = list(age = c("0-4", "5-9", "10+"),
                       sex = c("m", "f"))))
    e2 <- Counts(array(0,
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
    e1 <- Counts(array(1:9,
                       dim = c(3, 3),
                       dimnames = list(age = c("0-4", "5-9", "10+"),
                       region = c("a", "b", "c"))))
    e2 <- Counts(array(1:4,
                       dim = c(2, 2),
                       dimnames = list(age = c("0-4", "5+"),
                           region = c("A", "B"))))
    conc <- Concordance(data.frame(from = c("a", "b", "c"), to = c("A", "B", "B")))
    concordances <- list(age = NULL, region = conc)
    expect_true(canMakeSharedDimScalesCompatible(x = e1, y = e2, subset = TRUE,
                                                 concordances = concordances))
    expect_true(canMakeSharedDimScalesCompatible(x = e1, y = e2, subset = FALSE,
                                                 concordances = concordances))
})

test_that("checkAndTidyWeights method for Counts works", {
    checkAndTidyWeights <- dembase:::checkAndTidyWeights
    ## same number of dimensions
    weights <- Counts(array(1L,
                            dim = 3:2,
                            dimnames = list(reg = c("a", "b", "c"),
                                sex = c("f", "m"))))
    target <- Values(array(1L,
                           dim = c(2, 2),
                           dimnames = list(reg = c("a", "c"),
                               sex = c("f", "m"))))
    ans.obtained <- checkAndTidyWeights(weights = weights,
                                        target = target)
    ans.expected <- as(target, "Counts")
    expect_identical(ans.obtained, ans.expected)
    ## target has extra dimension
    weights <- Counts(array(1L,
                            dim = 3,
                            dimnames = list(reg = c("a", "b", "c"))))
    target <- Values(array(1L,
                           dim = c(2, 2),
                           dimnames = list(reg = c("a", "c"),
                               sex = c("f", "m"))))
    ans.obtained <- checkAndTidyWeights(weights = weights,
                                        target = target)
    ans.expected <- as(target, "Counts")
    expect_identical(ans.obtained, ans.expected)
    ## target has 0-length dimension
    weights <- Counts(array(1L,
                            dim = 3:2,
                            dimnames = list(reg = c("a", "b", "c"),
                                sex = c("f", "m"))))
    target <- Values(array(1L,
                           dim = c(2, 0),
                           dimnames = list(reg = c("a", "c"),
                               sex = character())))
    ans.obtained <- checkAndTidyWeights(weights = weights,
                                        target = target)
    ans.expected <- as(target, "Counts")
    expect_identical(ans.obtained, ans.expected)
    ## weights has missing
    weights <- Counts(array(c(1L, NA),
                            dim = 3:2,
                            dimnames = list(reg = c("a", "b", "c"),
                                sex = c("f", "m"))))
    target <- Values(array(1L,
                           dim = c(2, 2),
                           dimnames = list(reg = c("a", "c"),
                               sex = c("f", "m"))))
    expect_error(checkAndTidyWeights(weights = weights,
                                     target = target),
                 "'weights' has missing values")
    expect_error(checkAndTidyWeights(weights = weights,
                                     target = target,
                                     nameWeights = "exposure"),
                 "'exposure' has missing values")
    ans.obtained <- checkAndTidyWeights(weights = weights,
                                        target = target,
                                        allowNA = TRUE)
    ans.expected <- Counts(array(c(1L, 1L, NA, NA),
                                 dim = c(2, 2),
                                 dimnames = list(reg = c("a", "c"),
                                     sex = c("f", "m"))))
    expect_identical(ans.obtained, ans.expected)
    ## weights has interations, but object does not
    weights <- Counts(array(1L,
                            dim = 3:2,
                            dimnames = list(reg = c("a", "b", "c"),
                                iteration = 1:2)))
    target <- Counts(array(1L,
                           dim = 3,
                           dimnames = list(reg = c("a", "b", "c"))))
    expect_error(checkAndTidyWeights(weights = weights, target = target),
                 "'object' and 'weights' not compatible: dimension \"iteration\" has dimtype \"iteration\" and cannot be collapsed")
    ## weights not compatible
    weights <- Counts(array(1L,
                            dim = 3:2,
                            dimnames = list(reg = c("a", "b", "wrong"),
                                sex = c("f", "m"))))
    target <- Values(array(1L,
                           dim = c(2, 2),
                           dimnames = list(reg = c("a", "c"),
                               sex = c("f", "m"))))
    expect_error(checkAndTidyWeights(weights = weights,
                                     target = target),
                 "'object' and 'weights' not compatible")
    expect_error(checkAndTidyWeights(weights = weights,
                                     target = target,
                                     nameTarget = "x"),
                 "'x' and 'weights' not compatible")
})

test_that("collapseCategories method for Counts works with old, new", {
    x <- Counts(array(1:6,
                      dim = 3:2,
                      dimnames = list(reg = c("a", "b", "c"),
                      sex = c("m", "f"))))
    y <- Counts(array(c(4L, 2L, 10L, 5L),
                      dim = c(2, 2),
                      dimnames = list(reg = c("d", "b"),
                      sex = c("m", "f"))))
    expect_identical(collapseCategories(x, dimension = "reg",
                                        old = c("a", "c"), new = "d"),
                     y)
    x <- Counts(array(1:6,
                      dim = 3:2,
                      dimnames = list(reg = c("a", "b", "c"),
                      time = c("2001", "2006"))))
    y <- Counts(array(c(4L, 2L, 10L, 5L),
                      dim = c(2, 2),
                      dimnames = list(reg = c("d", "b"),
                          time = c("2001", "2006"))))
    expect_identical(collapseCategories(x, old = c("a", "c"), new = "d"),
                     y)
    x <- Counts(array(c(Inf, 2, 3, 4, 5, 6, 7, 8, 9),
                      dim = c(3, 3),
                      dimnames = list(eth_parent = c("a", "b", "c"),
                      eth_child = c("a", "b", "c"))))
    y <- Counts(array(c(Inf, 5, 11, 28),
                      dim = c(2, 2),
                      dimnames = list(eth_parent = c("a", "d"),
                      eth_child = c("a", "d"))))
    expect_identical(collapseCategories(x, dimension = "eth",
                                        old = c("b", "c"), new = "d"),
                     y)
    x <- Counts(array(c(NA, 2, 3, 4, 5, 6, 7, 8, 9),
                      dim = c(3, 3),
                      dimnames = list(reg1 = c("a", "b", "c"),
                      reg2 = c("a", "b", "c"))))
    y <- Counts(array(c(NA, 5, 11, 28),
                      dim = c(2, 2),
                      dimnames = list(reg1 = c("a", "d"),
                      reg2 = c("a", "d"))))
    expect_identical(collapseCategories(x, old = c("b", "c"), new = "d"),
                     y)
    x <- Counts(array(c(NA, 2, 3, 4, 5, 6, 7, 8, 9),
                      dim = c(3, 3),
                      dimnames = list(reg1 = c("a", "b", "c"),
                      reg2 = c("a", "b", "c"))))
    expect_warning(collapseCategories(x, dimension = "reg1",
                                      old = c("a", "b"), new = "A",
                                      weights = x),
                   "'weights' argument ignored")
    expect_error(collapseCategories(x, old = c("b", "wrong"), new = "d"),
                 "cannot collapse categories for dimension \"reg1\" : value \"wrong\" not found")
})

test_that("collapseCategories method for Counts works with concordances", {
    Concordance <- classconc::Concordance
    x <- Counts(array(1:6,
                      dim = 3:2,
                      dimnames = list(reg = c("a", "b", "c"),
                      sex = c("m", "f"))))
    y <- Counts(array(c(4L, 2L, 10L, 5L),
                      dim = c(2, 2),
                      dimnames = list(reg = c("d", "b"),
                          sex = c("m", "f"))))
    conc <- Concordance(data.frame(v1 = c("a", "b", "c"),
                                   v2 = c("d", "b", "d")))
    expect_identical(collapseCategories(x, dimension = "reg",
                                        concordance = conc),
                     y)
    x <- Counts(array(c(Inf, 2, 3, 4, 5, 6, 7, 8, 9),
                      dim = c(3, 3),
                      dimnames = list(eth_parent = c("a", "b", "c"),
                      eth_child = c("a", "b", "c"))))
    y <- Counts(array(c(Inf, 5, 11, 28),
                      dim = c(2, 2),
                      dimnames = list(eth_parent = c("a", "d"),
                          eth_child = c("a", "d"))))
    conc <- Concordance(data.frame(v1 = c("a", "b", "c"),
                                   v2 = c("a", "d", "d")))
    expect_identical(collapseCategories(x, dimension = "eth",
                                        concordance = conc),
                     y)
    x <- Counts(array(c(Inf, 2, 3, 4, 5, 6, 7, 8, 9),
                      dim = c(3, 3),
                      dimnames = list(eth_parent = c("a", "b", "c"),
                      eth_child = c("a", "b", "c"))))
    y <- Counts(array(c(Inf, 2, 3, 4, 5, 6, 7, 8, 9),
                      dim = c(3, 3),
                      dimnames = list(eth_parent = c("A", "B", "C"),
                      eth_child = c("A", "B", "C"))))
    conc <- Concordance(data.frame(v1 = c("a", "b", "c"),
                                   v2 = c("A", "B", "C")))
    expect_identical(collapseCategories(x, dimension = "eth",
                                        concordance = conc),
                     y)
    x <- Counts(array(c(NA, 2, 3, 4, 5, 6, 7, 8, 9),
                      dim = c(3, 3),
                      dimnames = list(reg1 = c("a", "b", "c"),
                      reg2 = c("a", "b", "c"))))
    conc <- Concordance(data.frame(v1 = c("a", "b", "c"),
                                   v2 = c("a", "d", "d")))
    expect_warning(collapseCategories(x, dimension = "reg1",
                                      concordance = conc,
                                      weights = x),
                   "'weights' argument ignored")
    x <- Counts(array(c(NA, 2, 3, 4, 5, 6, 7, 8, 9),
                      dim = c(3, 3),
                      dimnames = list(reg_orig = c("a", "b", "c"),
                      reg_dest = c("a", "b", "c"))))
    y <- Counts(array(c(NA, 5, 11, 28),
                      dim = c(2, 2),
                      dimnames = list(reg1 = c("a", "d"),
                      reg2 = c("a", "d"))))
    conc <- Concordance(data.frame(v1 = c("a", "b", "wrong"),
                                   v2 = c("a", "d", "d")))
    expect_error(collapseCategories(x, concordance = conc),
                 "cannot collapse categories for dimension \"reg_orig\"")
})

test_that("collapseDimension works", {
    a1 <- array(as.numeric(1:6),
                dim = c(3, 2),
                dimnames = list(age = c("0-4", "5-9", "10+"),
                sex = c("Male", "Female")))
    x1 <- Counts(a1)
    a2 <- array(c(5, 7, 9),
                dim = 3,
                dimnames = list(age = c("0-4", "5-9", "10+")))
    x2 <- Counts(a2)
    expect_identical(collapseDimension(x1, margin = 1),
                     x2)
    expect_identical(collapseDimension(x1, margin = 1:2),
                     x1)
    expect_identical(collapseDimension(x1, dimension = character()),
                     x1)
    expect_identical(collapseDimension(x1, margin = c("age", "sex")),
                     x1)
    expect_identical(collapseDimension(x1, dimension = integer()),
                     x1)
    expect_identical(collapseDimension(x1, margin = "age"),
                     x2)
    expect_error(collapseDimension(x1, margin = c(1, NA)),
                 "'subscript' has missing values")
    expect_error(collapseDimension(x1, margin = 4),
                 sprintf("subscript %s outside valid range", sQuote('4')))
    expect_error(collapseDimension(x1, margin = "wrong"),
                 sprintf("subscript %s outside valid range",
                         dQuote("wrong")))
    expect_error(collapseDimension(x1, margin = c(1, 1)),
                 "'subscript' contains duplicates")
    expect_error(collapseDimension(x1, dimension = 1, margin = 2),
                 "has 'dimension' and 'margin' arguments")
    expect_error(collapseDimension(x1),
                 "no 'dimension' or 'margin' arguments")
    expect_error(collapseDimension(x1, margin = 1, weights = x1),
                 "weights cannot be used when 'object' has class \"Counts\"")
    a1 <- array(1,
                dim = c(2, 2, 3),
                dimnames = list(eth_parent = c("a", "b"),
                eth_child = c("a", "b"),
                age = c("0-4", "5-9", "10+")))
    x1 <- Counts(a1)
    a2 <- array(2,
                dim = c(2, 3),
                dimnames = list(eth = c("a", "b"),
                age = c("0-4", "5-9", "10+")))
    x2 <- Counts(a2)
    expect_identical(collapseDimension(x1, dimension = "eth_child"),
                     x2)
    expect_identical(collapseDimension(x2, dimension = "eth"),
                     collapseDimension(x2, margin = "age"))
    expect_identical(collapseDimension(x2, dimension = c("age", "eth")),
                     sum(x2))
    x <- Counts(array(1:2, dim = 2, dimnames = list(sex = c("f", "m"))))
    expect_identical(collapseDimension(x, dimension = "sex"), 3L)
    x <- Counts(array(1:2, dim = c(2, 3), dimnames = list(sex = c("f", "m"), sim = 1:3)))
    expect_error(collapseDimension(x, dimension = "sim"),
                 paste("attempt to collapse dimension with dimtype \"iteration\"",
                       "\\(consider using function 'collapseIterations' instead\\)"))
    x <- Counts(array(1,
                      dim = c(2, 3),
                      dimnames = list(sex = c("f", "m"),
                      quantile = c("1%", "50%", "99%"))))
    expect_error(collapseDimension(x, dimension = "sex"),
                 "dimension with dimtype \"quantile\"")
})

test_that("collapseDimension works like aperm when all dims in 'margin'", {
    x <- Counts(array(1,
                      dim = c(2, 3),
                      dimnames = list(sex = c("f", "m"),
                      age = c("0-4", "5-9", "10+"))))
    expect_identical(collapseDimension(x, margin = c("age", "sex")),
                     aperm(x, perm = 2:1))
    expect_identical(collapseDimension(x, margin = 1:2),
                     x)
})

test_that("collapseDimension adds iteration dimension to margin if not supplied", {
    x <- Counts(array(1,
                      dim = c(2, 3, 3),
                      dimnames = list(sex = c("f", "m"),
                          age = c("0-4", "5-9", "10+"),
                          iteration = 1:3)))
    expect_identical(collapseDimension(x, margin = "age"),
                     collapseDimension(x, margin = c("age", "iteration")))
    expect_identical(collapseDimension(x, margin = character()),
                     collapseDimension(x, margin = 3L))
})

test_that("collapseIntervals works using breaks argument", {
    x <- Counts(array(1:6,
                      dim = c(2, 3),
                      dimnames = list(sex = c("m", "f"), age = c("0-4", "5-9", "10+"))))
    y <- Counts(array(c(4L,6L,5L,6L),
                      dim = c(2, 2),
                      dimnames = list(sex = c("m", "f"), age = c("0-9", "10+"))))
    expect_identical(collapseIntervals(x, dimension = "age", breaks = c(0, 10, Inf)),
                     y)
    expect_identical(collapseIntervals(x, dimension = "age", breaks = c(0, 10)),
                     y)
    expect_identical(collapseIntervals(x, dimension = "age", breaks = 10),
                     y)
    expect_identical(collapseIntervals(x, dimension = 2, breaks = c(0, 10, Inf)),
                     y)
    expect_identical(collapseIntervals(x, dimension = 2, breaks = c(0, 10, Inf),
                                       width = NULL, old = NULL),
                     y)
    x <- Counts(array(1:10,
                      dim = c(2, 5),
                      dimnames = list(sex = c("m", "f"), period = 2001:2005)),
                dimscales = c(period = "Intervals"))
    y <- Counts(array(c(4L,6L,21L,24L),
                      dim = c(2, 2),
                      dimnames = list(sex = c("m", "f"),
                          period = c("2001-2002", "2003-2005"))))
    expect_identical(collapseIntervals(x, dimension = "period", breaks = c(2000, 2002, 2005)),
                     y)
    expect_identical(collapseIntervals(x, dimension = "period", breaks = 2002),
                     y)
    expect_error(collapseIntervals(x, dimension = 1:2, breaks = 2002),
                 "'dimension' does not have length 1")
    expect_error(collapseIntervals(x, dimension = 3, breaks = 2002),
                 sprintf("subscript %s outside valid range", sQuote("3")))
    expect_error(collapseIntervals(x, dimension = 1, breaks = 2002),
                 "dimension \"sex\" has dimscale \"Sexes\"")
    expect_error(collapseIntervals(x, dimension = 2, breaks = c(2002, NA)),
                 "'breaks' has missing values")
    expect_error(collapseIntervals(x, dimension = 2, breaks = c(2002, 2001)),
                 "'breaks' not increasing")
    expect_error(collapseIntervals(x, dimension = 2, breaks = c(2002, 2002.5)),
                 "no existing break at value 2002.5")
    expect_error(collapseIntervals(x, dimension = 2, breaks = c(2002, 2002.5, 2002.75)),
                 "no existing breaks at values 2002.5, 2002.75")
    x <- Counts(array(1:10,
                      dim = c(2, 5),
                      dimnames = list(quantile = c("20%", "80%"), period = 2000:2004)),
                dimscales = c(period = "Intervals"))
    expect_error(collapseIntervals(x, dimension = 2, breaks = 2002),
                 "dimension with dimtype \"quantile\"")
})

test_that("collapseIntervals works using width argument", {
    x <- Counts(array(1:6,
                      dim = c(2, 3),
                      dimnames = list(sex = c("m", "f"), age = c("0-4", "5-9", "10+"))))
    y <- Counts(array(c(4L,6L,5L,6L),
                      dim = c(2, 2),
                      dimnames = list(sex = c("m", "f"), age = c("0-9", "10+"))))
    expect_identical(collapseIntervals(x, dimension = "age", width = 10),
                     y)
    expect_identical(collapseIntervals(x, dimension = "age", width = 5),
                     x)
    expect_identical(collapseIntervals(x, dimension = "age", breaks = NULL,
                                       width = 5, old = NULL),
                     x)
    expect_error(collapseIntervals(x, dimension = "age", width = 1:2),
                 "'width' does not have length 1")
    expect_error(collapseIntervals(x, dimension = "age", width = -1),
                 "'width' is non-positive")
    expect_error(collapseIntervals(x, dimension = "age", width = 6),
                 "'width' \\[6\\] is not a divisor of difference between lowest and highest finite breaks \\[10\\]")
    x <- Counts(array(1:2,
                      dim = c(2, 1),
                      dimnames = list(sex = c("m", "f"), age = c("0+"))))
    expect_identical(collapseIntervals(x, dimension = "age", width = 5),
                     x)
})

test_that("collapseIntervals works using 'old' argument", {
    x <- Counts(array(1:6,
                      dim = c(2, 3),
                      dimnames = list(sex = c("m", "f"), age = c("0-4", "5-9", "10+"))))
    y <- Counts(array(c(4L,6L,5L,6L),
                      dim = c(2, 2),
                      dimnames = list(sex = c("m", "f"), age = c("0-9", "10+"))))
    expect_identical(collapseIntervals(x, dimension = "age", old = c("0-4", "5-9")),
                     y)
    expect_identical(collapseIntervals(x, dimension = "age", breaks = NULL,
                                       width = NULL, old = c("0-4", "5-9")),
                     y)
    x <- Counts(array(1:10,
                      dim = c(2, 5),
                      dimnames = list(sex = c("m", "f"), period = 2000:2004)),
                dimscales = c(period = "Intervals"))
    y <- Counts(array(c(1:4,21L,24L),
                      dim = c(2, 3),
                      dimnames = list(sex = c("m", "f"),
                          period = c("2000", "2001", "2002-2004"))))
    expect_identical(collapseIntervals(x, dimension = "period", old = c("2002", "2003", "2004")),
                     y)
    expect_identical(collapseIntervals(x, dimension = "period", old = 2002:2004),
                     y)
    expect_identical(collapseIntervals(x, dimension = "period", breaks = NULL,
                                       width = NULL, old = 2002:2004),
                     y)
    expect_error(collapseIntervals(x, dimension = 2, old = character()),
                 "'old' has length 0")
    expect_error(collapseIntervals(x, dimension = 2, old = "wrong"),
                 sprintf("value in 'old' \\[%s\\] not found in dimension \"period\"", dQuote("wrong")))
    expect_error(collapseIntervals(x, dimension = 2, old = c("wrong1", "wrong2")),
                 sprintf("values in 'old' \\[%s\\] not found in dimension \"period\"",
                         paste(dQuote(c("wrong1", "wrong2")), collapse = ", ")))
    expect_error(collapseIntervals(x, dimension = 2, old = c("2001", "2004")),
                 "elements of 'old' are not consecutive")
})

test_that("collapseIntervals throws error when too many arguments used", {
    x <- Counts(array(1:6,
                      dim = c(2, 3),
                      dimnames = list(sex = c("m", "f"), age = c("0-4", "5-9", "10+"))))
    expect_error(collapseIntervals(x, dimension = 2, breaks = 5, old = "0-4"),
                 "unable to find an inherited method")
    expect_error(collapseIntervals(x, dimension = 2, breaks = 5, width = 5),
                 "unable to find an inherited method")
})

test_that("collapseIntervals throws error when weights used", {
    x <- Counts(array(1:6,
                      dim = c(2, 3),
                      dimnames = list(sex = c("m", "f"), age = c("0-4", "5-9", "10+"))))
    expect_error(collapseIntervals(x, dimension = 2, breaks = 5, weights = x),
                 "weights cannot be used when 'object' has class \"Counts\"")
})

test_that("collapseOrigDest works", {
    x <- Counts(array(1:9,
                      dim = c(3, 3),
                      dimnames = list(reg_orig = c("a", "b", "c"),
                      reg_dest = c("a", "b", "c"))))
    y <- Counts(array(c(11L, 10L, 9L, 5L, 10L, 15L),
                      dim = c(3, 2),
                      dimnames = list(reg = c("a", "b", "c"),
                          direction = c("Out", "In"))))
    y <- Pool(y, between = "reg", direction = "direction")
    z <- Counts(array(c(-6L, 0L, 6L),
                      dim = 3,
                      dimnames = list(reg = c("a", "b", "c"))))
    z <- Net(z, between = "reg")
    expect_identical(collapseOrigDest(x, to = "pool"), y)
    expect_identical(collapseOrigDest(x, base = "reg", to = "pool"), y)
    expect_error(collapseOrigDest(x, base = "wrong"),
                 "'base' outside valid range")
    expect_error(collapseOrigDest(x, base = c("reg", "wrong")),
                 "'base' outside valid range")
    expect_identical(collapseOrigDest(x, to = "net"), z)
    expect_identical(collapseOrigDest(x), z)
    expect_error(collapseOrigDest(z),
                 "no dimensions with dimtypes \"origin\" or \"destination\"")
    x <- Counts(array(1:18,
                      dim = c(3, 2, 3),
                      dimnames = list(reg_dest = c("b", "c", "a"),
                      age = c("0-4", "5+"),
                          reg_orig = c("a", "b", "c"))))
    y <- x[c(3, 1, 2), , ]
    y[slice.index(y, 1) == slice.index(y, 3)] <- 0L
    y <- dbind(Out = collapseDimension(y, dimension = "reg_dest"),
               In = collapseDimension(y, dimension = "reg_orig"),
               along = "direction")
    y <- Pool(y, between = "reg", direction = "direction")
    z <- Counts(array(c(21L, -3L, -18L,
                        21L, -3L, -18L),
                      dim = c(3, 2),
                      dimnames = list(reg = c("a", "b", "c"), age = c("0-4", "5+"))))
    z <- Net(z, between = "reg")
    expect_identical(xx <- collapseOrigDest(x, to = "pool"), y)
    expect_identical(collapseOrigDest(x, to = "net"), z)
    x <- Counts(array(1:81,
                      dim = c(3, 3, 2, 2),
                      dimnames = list(reg_orig = c("a", "b", "c"),
                      reg_dest = c("a", "b", "c"),
                      eth_orig = c("x", "y"),
                      eth_dest = c("x", "y"))))
    y <- x
    y[slice.index(y, 1) == slice.index(y, 2)] <- 0L
    y[slice.index(y, 3) == slice.index(y, 4)] <- 0L
    y <- dbind(Out = collapseDimension(y, dimension = c("reg_dest", "eth_dest")),
               In = collapseDimension(y, dimension = c("reg_orig", "eth_orig")),
               along = "direction")
    y <- Pool(y, between = c("reg", "eth"), direction = "direction")
    z <- Counts(array(c(-24L, -18L, -12L, 12L, 18L, 24L),
                      dim = c(3, 2),
                      dimnames = list(reg = c("a", "b", "c"), eth = c("x", "y"))))
    z <- Net(z, between = c("reg", "eth"))
    w <- Counts(array(c(-6L, 0L, 6L,
                        -6L, 0L, 6L,
                        -6L, 0L, 6L,
                        -6L, 0L, 6L),
                      dim = c(3, 2, 2),
                      dimnames = list(
                      reg = c("a", "b", "c"),
                      eth_orig = c("x", "y"),
                      eth_dest = c("x", "y"))))
    expect_identical(collapseOrigDest(x, to = "pool"), y)
    expect_identical(collapseOrigDest(x, base = "reg", to = "net"), w)
    expect_identical(collapseOrigDest(x), z)
    x <- Counts(array(1:12,
                      dim = c(2, 2, 3),
                      dimnames = list(reg_dest = c("b", "c"),
                      age = c("0-4", "5+"),
                      reg_orig = c("a", "b", "c"))))
    y <- dbind(x,
               Counts(array(0L,
                            dim = c(1, 2, 3),
                            dimnames = list(reg_dest = "a",
                            age = c("0-4", "5+"),
                            reg_orig = c("a", "b", "c")))),
               along = "reg_dest")
    expect_identical(collapseOrigDest(x),
                     collapseOrigDest(y))
    x <- Counts(array(1:4,
                      dim = c(2, 1, 2),
                      dimnames = list(reg_dest = c("b", "c"),
                          age = "0-4",
                          reg_orig = c("b", "c"))))
    ans.obtained <- collapseOrigDest(x, to = "pool")
    ans.expected <- Counts(array(c(2L, 3L, 3L, 2L),
                                 dim = c(1, 2, 2),
                                 dimnames = list(age = "0-4",
                                     reg = c("b", "c"),
                                     direction = c("Out", "In"))))
    ans.expected <- Pool(ans.expected, between = "reg", direction = "direction")
    expect_identical(ans.obtained, ans.expected)
})

test_that("dbind2 works", {
    dbind2 <- dembase:::dbind2
    ## along "state" dimension
    x <- Counts(array(1:2,
                      dim = c(2, 1),
                      dimnames = list(age = c("0-4", "5+"), sex = "f")))
    y <- Counts(array(3:4,
                      dim = c(2, 1),
                      dimnames = list(age = c("0-4", "5+"), sex = "m")))
    ans.obtained <- dbind2(e1 = x, e2 = y, name1 = "x", name2 = "y", along = "sex",
                           dimtypeAlong = "sex")
    ans.expected <- Counts(array(1:4,
                                 dim = c(2, 2),
                                 dimnames = list(age = c("0-4", "5+"), sex = c("f", "m"))))
    expect_identical(ans.obtained, ans.expected)
    ## along overlapping age dimension
    expect_error(dbind2(x, y, name1 = "x", name2 = "y", along = "age",
                        dimtypeAlong = "age"),
                 "\"age\" dimensions overlap")
    ## adding new 'region' dimension
    x <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(age = c("0-4", "5+"), sex = c("f", "m"))))
    y <- Counts(array(5:8,
                      dim = c(2, 2),
                      dimnames = list(age = c("0-4", "5+"), sex = c("f", "m"))))
    ans.obtained <- dbind2(x, y, name1 = "x", name2 = "y", along = "region",
                           dimtypeAlong = "state")
    ans.expected <- Counts(array(1:8,
                                 dim = c(2, 2, 2),
                                 dimnames = list(age = c("0-4", "5+"),
                                     sex = c("f", "m"), region = c("x", "y"))))
    expect_identical(ans.obtained, ans.expected)
    ## attempt to dbind Counts and Values
    x <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(age = c("0-4", "5-9"), sex = c("f", "m"))))
    y <- Values(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(age = c("10-14", "15+"), sex = c("f", "m"))))
    expect_error(dbind2(e1 = x, e2 = y, name1 = "x", name2 = "y", along = "age",
                        dimtypeAlong = "age"),
                 "cannot combine object of class \"Counts\" with object of class \"Values\"")
    ## gap in time dimensions
    x <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(time = c("2001-2005", "2006-2010"), sex = c("f", "m"))))
    y <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(time = c("2016-2020", "2021-2025"), sex = c("f", "m"))))
    expect_error(dbind2(x, y, name1 = "x", name2 = "y", along = "time", dimtypeAlong = "time"),
                 "gap between \"time\" dimensions")
    ## zero-length dimension    
    x <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(time = c("2001-2005", "2006-2010"), sex = c("f", "m"))))
    y <- Counts(array(0,
                      dim = c(0, 2),
                      dimnames = list(time = NULL, sex = c("f", "m"))),
                dimscales = c(time = "Intervals"))
    ans.obtained <- dbind2(x, y, name1 = "x", name2 = "y", along = "time", dimtypeAlong = "time")
    ans.expected <- toDouble(t(x))
    expect_identical(ans.obtained, ans.expected)
    ## reset iterations
    x <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(time = c("2001-2005", "2006-2010"),
                          iteration = 1:2)))
    y <- Counts(array(1:4,
                      dim = c(2, 4),
                      dimnames = list(time = c("2001-2005", "2006-2010"),
                          iteration = 1:4)))
    ans.obtained <- dbind2(x, x, along = "iteration", dimtypeAlong = "time")
    ans.expected <- y
    expect_identical(ans.obtained, ans.expected)
    ## reset iterations
    x <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(time = c("2001-2005", "2006-2010"),
                          iteration = 1:2)))
    y <- Counts(array(5:6,
                      dim = 2,
                      dimnames = list(time = c("2001-2005", "2006-2010"))))
    ans.obtained <- dbind2(x, y, along = "iteration", dimtypeAlong = "iteration")
    ans.expected <- Counts(array(1:6,
                                 dim = c(2, 3),
                                 dimnames = list(time = c("2001-2005", "2006-2010"),
                                     iteration = 1:3)))
    expect_identical(ans.obtained, ans.expected)
    ## add iterations
    x <- Counts(array(1:2,
                      dim = 2,
                      dimnames = list(time = c("2001-2005", "2006-2010"))))
    y <- Counts(array(3:6,
                      dim = c(2, 2),
                      dimnames = list(time = c("2001-2005", "2006-2010"),
                          iteration = 1:2)))
    ans.obtained <- dbind2(x, y, along = "iteration", dimtypeAlong = "iteration")
    ans.expected <- Counts(array(1:6,
                                 dim = c(2, 3),
                                 dimnames = list(time = c("2001-2005", "2006-2010"),
                                     iteration = 1:3)))
    expect_identical(ans.obtained, ans.expected)
    ## put in right order
    x <- Counts(array(1:12,
                      dim = c(3, 4),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          year = c(2000, 2005, 2010, 2015))))
    y <- Counts(array(1:3,
                      dim = c(3, 1),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          year = 1995)),
                dimscales = c(year = "Points"))
    ans.obtained <- dbind2(x, y, along = "year", dimtypeAlong = "time")
    ans.expected <- Counts(array(c(1:3, 1:12),
                                 dim = c(3, 5),
                                 dimnames = list(age = c("0-4", "5-9", "10+"),
                                     year = c(1995, 2000, 2005, 2010, 2015))))
    expect_identical(ans.obtained, ans.expected)
    ## put in right order
    x <- Counts(array(1:3,
                      dim = c(3, 1),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          year = 1995)),
                dimscales = c(year = "Points"))
    y <- Counts(array(1,
                      dim = c(1, 1),
                      dimnames = list(age = "-5--1",
                          year = 1995)),
                dimscales = c(year = "Points"))
    ans.obtained <- dbind2(x, y, along = "age", dimtypeAlong = "age")
    ans.expected <- Counts(array(c(1, 1:3),
                                 dim = c(1, 4),
                                 dimnames = list(year = 1995, age = c("-5--1", "0-4", "5-9", "10+"))),
                           dimscales = c(year = "Points"))
    expect_identical(ans.obtained, ans.expected)
    ## error message from incompatible dimscales
    x <- Counts(array(1,
                      dim = c(1, 1),
                      dimnames = list(age = "-5--1",
                          year = 1995)),
                dimscales = c(year = "Points"))
    y <- Counts(array(1:3,
                      dim = c(3, 1),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          year = 1995)),
                dimscales = c(year = "Intervals"))
    expect_error(dbind2(x, y, along = "age", dimtypeAlong = "age"),
                 "\"year\" dimensions have incompatible dimscales")
    ## put in right order and collapse categories
    x <- Counts(array(1:2,
                      dim = c(2, 1),
                      dimnames = list(age = c("0-4", "5+"),
                      year = "2001-2005")))
    y <- Counts(array(1:3,
                      dim = c(3, 1),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          year = "1996-2000")))
    ans.obtained <- dbind2(x, y, along = "year", dimtypeAlong = "time")
    ans.expected <- Counts(array(c(1L, 5L, 1:2),
                                 dim = c(2, 2),
                                 dimnames = list(age = c("0-4", "5+"),
                                     year = c("1996-2000", "2001-2005"))))
    expect_identical(ans.obtained, ans.expected)
})


test_that("expandCategories method for Counts works with concordances when means is FALSE", {
    Concordance <- classconc::Concordance
    object <- Counts(array(c(4L, 2L, 10L, 5L),
                           dim = c(2, 2),
                           dimnames = list(reg = c("d", "b"),
                               sex = c("m", "f"))))
    conc <- Concordance(data.frame(v1 = c("a", "b", "c"),
                                   v2 = c("d", "b", "d")))
    ans.obtained <- expandCategories(object,
                                     dimension = "reg",
                                     concordance = conc,
                                     weights = 1)
    expect_identical(collapseCategories(ans.obtained,
                                        dimension = "reg",
                                        concordance = conc),
                     object)
    weights <- Counts(array(1:3,
                            dim = 3,
                            dimnames = list(reg = c("a", "b", "c"))))
    ans.obtained <- expandCategories(object,
                                     dimension = "reg",
                                     concordance = conc,
                                     weights = weights,
                                     n = 5)
    for (i in 1:5) {
        expect_identical(collapseCategories(ans.obtained,
                                            dimension = "reg",
                                            concordance = conc)[,,i],
                         object)
    }
    uncollapsed <- Counts(array(1:8,
                                dim = c(2, 4),
                                dimnames = list(sex = c("m", "f"),
                                    reg = c("a", "b", "c", "d"))))
    conc <- Concordance(data.frame(v1 = c("a", "b", "c", "d"),
                                   v2 = c("B", "B", "A", "B")))
    collapsed <- collapseCategories(uncollapsed,
                                    dimension = "reg",
                                    concordance = conc)
    ans.obtained <- expandCategories(collapsed,
                                     dimension = "reg",
                                     concordance = conc,
                                     weights = 1)
    expect_identical(collapseCategories(ans.obtained,
                                        dimension = "reg",
                                        concordance = conc),
                     collapsed)
    x <- Counts(array(c(Inf, 2, 3, 4, 5, 6, 7, 8, 9),
                      dim = c(3, 3),
                      dimnames = list(eth_parent = c("a", "b", "c"),
                          eth_child = c("a", "b", "c"))))
    y <- Counts(array(c(Inf, 5, 11, 28),
                      dim = c(2, 2),
                      dimnames = list(eth_parent = c("a", "d"),
                          eth_child = c("a", "d"))))
    conc <- Concordance(data.frame(v1 = c("a", "b", "c"),
                                   v2 = c("a", "d", "d")))
    expect_identical(collapseCategories(x, dimension = "eth",
                                        concordance = conc),
                     y)
})

test_that("expandCategories method for Counts works with concordances when means is TRUE", {
    Concordance <- classconc::Concordance
    object <- Counts(array(c(4L, 2L, 10L, 5L),
                           dim = c(2, 2),
                           dimnames = list(reg = c("d", "b"),
                               sex = c("m", "f"))))
    conc <- Concordance(data.frame(v1 = c("a", "b", "c"),
                                   v2 = c("d", "b", "d")))
    ans.obtained <- expandCategories(object,
                                     dimension = "reg",
                                     concordance = conc,
                                     weights = 1,
                                     means = TRUE)
    expect_equal(collapseCategories(ans.obtained,
                                    dimension = "reg",
                                    concordance = conc),
                 object)
    weights <- Counts(array(1:3,
                            dim = 3,
                            dimnames = list(reg = c("a", "b", "c"))))
    ans.obtained <- expandCategories(object,
                                     dimension = "reg",
                                     concordance = conc,
                                     weights = weights,
                                     n = 5)
    for (i in 1:5) {
        expect_equal(collapseCategories(ans.obtained,
                                        dimension = "reg",
                                        concordance = conc)[,,i],
                     object)
    }
    uncollapsed <- Counts(array(1:8,
                                dim = c(2, 4),
                                dimnames = list(sex = c("m", "f"),
                                    reg = c("a", "b", "c", "d"))))
    conc <- Concordance(data.frame(v1 = c("a", "b", "c", "d"),
                                   v2 = c("B", "B", "A", "B")))
    collapsed <- collapseCategories(uncollapsed,
                                    dimension = "reg",
                                    concordance = conc)
    ans.obtained <- expandCategories(collapsed,
                                     dimension = "reg",
                                     concordance = conc,
                                     means = TRUE,
                                     weights = 1)
    expect_equal(collapseCategories(ans.obtained,
                                    dimension = "reg",
                                    concordance = conc),
                 collapsed)
    x <- Counts(array(c(Inf, 2, 3, 4, 5, 6, 7, 8, 9),
                      dim = c(3, 3),
                      dimnames = list(eth_parent = c("a", "b", "c"),
                          eth_child = c("a", "b", "c"))))
    y <- Counts(array(c(Inf, 5, 11, 28),
                      dim = c(2, 2),
                      dimnames = list(eth_parent = c("a", "d"),
                          eth_child = c("a", "d"))))
    conc <- Concordance(data.frame(v1 = c("a", "b", "c"),
                                   v2 = c("a", "d", "d")))
    expect_equal(collapseCategories(x, dimension = "eth",
                                    concordance = conc),
                 y)
})

test_that("exposure works when 'triangles' is FALSE", {
    ## object has time and age; not regular
    x <- Counts(array(1:24,
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"),
                          time = c(2000, 2001, 2005),
                          age = c("0-4", "5-9", "10-14", "15+"))))
    ans.obtained <- exposure(x)
    ans.expected <- as(x, "array")
    ans.expected <- 0.5 * (ans.expected[,-3,] + ans.expected[,-1,]) * c(1, 1, 4, 4)
    dimnames(ans.expected)[[2]] <- c("2001", "2002-2005")
    ans.expected <- Counts(ans.expected)
    expect_identical(ans.obtained, ans.expected)
    ## object has time and age; regular
    x <- Counts(array(1:24,
                      dim = c(3, 2, 4),
                      dimnames = list(time = c(2000, 2005, 2010),
                          sex = c("f", "m"),
                          age = c("0-4", "5-9", "10-14", "15+"))))
    ans.obtained <- exposure(x)
    ans.expected <- as(x, "array")
    ans.expected <- 2.5 * (ans.expected[-3,,] + ans.expected[-1,,])
    dimnames(ans.expected)[[1]] <- c("2001-2005", "2006-2010")
    ans.expected <- Counts(ans.expected)
    expect_identical(ans.obtained, ans.expected)
    ## object has time no age; regular
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(time = c(2000, 2005, 2010),
                          sex = c("f", "m"))))
    ans.obtained <- exposure(x)
    ans.expected <- as(x, "array")
    ans.expected <- 2.5 * (ans.expected[-3,] + ans.expected[-1,])
    dimnames(ans.expected)[[1]] <- c("2001-2005", "2006-2010")
    ans.expected <- Counts(ans.expected)
    expect_identical(ans.obtained, ans.expected)
    ## object has age, no time; regular
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c(0, 5, 10),
                          sex = c("f", "m"))))
    ans.obtained <- exposure(x)
    ans.expected <- as(x, "array")
    ans.expected <- 2.5 * (ans.expected[-3,] + ans.expected[-1,])
    dimnames(ans.expected)[[1]] <- c("0-4", "5-9")
    ans.expected <- Counts(ans.expected)
    expect_identical(ans.obtained, ans.expected)
    ## object has length 0
    x <- Counts(array(0L,
                      dim = c(2, 4, 0),
                      dimnames = list(cohort = c("2001-2005", "2006-2010"),
                          age = c(0, 5, 10, 15),
                          region = character())))
    ans.obtained <- exposure(x)
    ans.expected <- Counts(array(numeric(),
                                 dim = c(2, 3, 0),
                                 dimnames = list(cohort = c("2001-2005", "2006-2010"),
                                     age = c("0-4", "5-9", "10-14"),
                                     region = character())))
    expect_identical(ans.obtained, ans.expected)
})

test_that("exposure works when 'triangles' is TRUE", {
    x <- Counts(array(1:24,
                      dim = c(3, 2, 4),
                      dimnames = list(time = c(2000, 2005, 2010),
                          sex = c("f", "m"),
                          age = c("0-4", "5-9", "10-14", "15+"))))
    ans.obtained <- exposure(x, triangles = TRUE)
    lower <- 2.5 * x@.Data[2:3,,]
    upper <- 2.5 * x@.Data[1:2,,]
    ans.expected <- Counts(array(c(lower, upper),
                                 dim = c(2, 2, 4, 2),
                                 dimnames = list(time = c("2001-2005", "2006-2010"),
                                     sex = c("f", "m"),
                                     age = c("0-4", "5-9", "10-14", "15+"),
                                     triangle = c("TL", "TU"))))
    expect_identical(ans.obtained, ans.expected)
})

test_that("exposure throws appropriate errors", {
    ## dimtypes and dimscales
    x <- Counts(array(1:24,
                      dim = c(3, 2, 4),
                      dimnames = list(time = c(2000, 2005, 2010),
                          sex = c("f", "m"),
                          age = c(0, 5, 10, 15))))
    expect_error(exposure(x),
                 "dimension with dimtype \"time\" has dimscale \"Points\" and dimension with dimtype \"age\" has dimscale \"Points\"")
    x <- Counts(array(1:24,
                      dim = c(3, 2, 4),
                      dimnames = list(time = c("2001-2005", "2006-2010", "2011-2015"),
                          sex = c("f", "m"),
                          region = 1:4)))
    expect_error(exposure(x),
                 "dimension with dimtype \"time\" has dimscale \"Intervals\"")
    x <- Counts(array(1:24,
                      dim = c(3, 2, 4),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          sex = c("f", "m"),
                          region = 1:4)))
    expect_error(exposure(x),
                 "dimension with dimtype \"age\" has dimscale \"Intervals\"")
    x <- Counts(array(1:24,
                      dim = c(3, 2, 4),
                      dimnames = list(ethnicity = 1:3,
                          sex = c("f", "m"),
                          region = 1:4)))
    expect_error(exposure(x),
                 "no dimensions with dimtype \"time\" or \"age\"")
    ## dimension lengths
    x <- Counts(array(1:8,
                      dim = c(1, 2, 4),
                      dimnames = list(time = 2000,
                          sex = c("f", "m"),
                          region = 1:4)),
                dimscales = c(time = "Points"))
    expect_error(exposure(x),
                 "dimension with dimtype \"time\" has length 1")
    x <- Counts(array(0,
                      dim = c(0, 2, 4),
                      dimnames = list(age = integer(),
                          sex = c("f", "m"),
                          region = 1:4)),
                dimscales = c(age = "Points"))
    expect_error(exposure(x),
                 "dimension with dimtype \"age\" has length 0")
})

test_that("growth works when 'within' is NULL", {
    a <- array(1:12,
               dim = c(4, 3),
               dimnames = list(region = 1:4, age = c(5, 10, 25)))
    x <- Counts(a)
    v <- colSums(x)
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
    x <- Counts(a)
    expect_identical(growth(x, within = c("region", "age"), type = "l"),
                     Counts(apply(a, 1:2, function(x) (x[3] - x[1]) / 10)))
    b <- collapseDimension(x, dimension = "region")
    age <- apply(b, 1, function(x) (x[3] - x[1]) / 10)
    expect_identical(growth(x, within = "age", type = "l"),
                     Counts(age))
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
    x <- Counts(a)
    expect_identical(growth(x, along = "age", within = "."),
                     growth(x, along = "age", within = c("region", "time")))
})

test_that("growth works when 'within' is orig-dest", {
    a <- array(1:27,
               dim = c(3, 3, 3),
               dimnames = list(reg_orig = 1:3, reg_dest = 1:3,
                   time = c(2000, 2005, 2010)))
    x <- Counts(a)
    expect_identical(growth(x, along = "time", within = "reg"),
                     growth(x, along = "time", within = c("reg_orig", "reg_dest")))
})

test_that("growth works with interations", {
    a <- array(1:27,
               dim = c(3, 3, 3),
               dimnames = list(region = 1:3, iteration = 1:3,
                   time = c(2000, 2005, 2010)))
    x <- Counts(a)
    expect_identical(growth(x, within = "region"),
                     growth(x, within = c("region", "iteration")))
})

test_that("growth gives warning when supplied weights", {
    a <- array(rpois(27, lambda = 10),
               dim = c(3, 3, 3),
               dimnames = list(region = 1:3, age = c("0-4", "5-9", "10+"),
                   time = c(2000, 2005, 2010)))
    x <- Counts(a)
    b <- array(1,
               dim = c(3, 3, 3),
               dimnames = list(region = 1:3, age = c("0-4", "5-9", "10+"),
                   time = c(2000, 2005, 2010)))
    w <- Counts(b)
    expect_warning(growth(x, within = "region", weights = w),
                   "'weights' ignored when 'object' has class \"Counts\"")
})

test_that("growth throws appropriate errors", {
    x <- Counts(array(rpois(27, lambda = 10),
                      dim = c(3, 3, 3),
                      dimnames = list(region = 1:3, quantile = c("1%", "50%", "99%"),
                          time = c(2000, 2005, 2010))))
    expect_error(growth(x),
                 "dimension with dimtype \"quantile\"")
    x <- Counts(array(rpois(27, lambda = 10),
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
    x <- Counts(array(rpois(9, lambda = 10),
                      dim = c(3, 3, 1),
                      dimnames = list(region = 1:3, age = c("0-4", "5-9", "10+"),
                          time = 2000)),
                dimscales = c(time = "Points"))
    expect_error(growth(x),
                 paste("cannot calculate growth along dimension \"time\" because",
                       "dimension has length 1"))
    x <- Counts(array(rpois(27, lambda = 10),
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
    x <- Counts(array(rpois(27, lambda = 10),
                      dim = c(3, 3, 3),
                      dimnames = list(region = 1:3, iteration = 1:3,
                          time = c(2000, 2010, 2020))))
    expect_error(growth(x, within = 3, along = 2),
                 "'along' dimension \\[\"iteration\"\\] has dimtype \"iteration\"")
    x <- Counts(array(rpois(27, lambda = 10),
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
})

test_that("makeCompatible works", {
    makeCompatible <- dembase:::makeCompatible
    x <- Counts(array(1:6,
                      dim = 3:2,
                      dimnames = list(reg = c("a", "b", "c"),
                      sex = c("m", "f"))))
    y <- Counts(array(c(4L, 2L, 10L, 5L),
                      dim = c(2, 2),
                      dimnames = list(reg = c("a", "b"),
                      sex = c("f", "m"))))
    expect_error(makeCompatible(x, y),
                 sprintf("\"reg\" dimensions have incompatible dimscales : one dimension has value \\[%s\\] that other does not",
                         dQuote("c")))
    expect_identical(makeCompatible(x, y, subset = TRUE),
                     Counts(array(c(4L, 5L, 1L, 2L),
                                  dim = c(2, 2),
                                  dimnames = list(reg = c("a", "b"), sex = c("f", "m")))))
    expect_error(makeCompatible(y, x, subset = TRUE),
                 sprintf("\"reg\" dimensions have incompatible dimscales : one dimension has value \\[%s\\] that other does not",
                         dQuote("c")))
    x <- Counts(array(0,
                      dim = c(0, 2),
                      dimnames = list(reg = NULL, sex = c("m", "f"))))
    y <- Counts(array(0,
                      dim = c(0, 2),
                      dimnames = list(reg = NULL, sex = c("f", "m"))))
    expect_identical(makeCompatible(x, y), y)
    x <- Counts(array(0,
                      dim = c(0, 2),
                      dimnames = list(reg = NULL, sex = c("m", "f"))))
    y <- Counts(array(0,
                      dim = 2,
                      dimnames = list(sex = c("f", "m"))))
    expect_error(makeCompatible(x, y),
                 sprintf("one object has dimension \\[%s\\] with length 0 that other does not",
                         dQuote("reg")))
    expect_error(makeCompatible(x, y, subset = TRUE),
                 sprintf("one object has dimension \\[%s\\] with length 0 that other does not",
                         dQuote("reg")))
    expect_error(makeCompatible(y, x),
                 sprintf("one object has dimension \\[%s\\] that other does not",
                         dQuote("reg")))
    x <- Counts(array(1:6,
                      dim = 3:2,
                      dimnames = list(reg = c("a", "b", "c"),
                      sex = c("m", "f"))))
    y <- Counts(array(1:6,
                      dim = c(3, 2, 3),
                      dimnames = list(reg = c("a", "b", "c"),
                      sex = c("m", "f"),
                      sim = 1:3)))
    expect_identical(makeCompatible(x, y), y)
})


test_that("makeOrigDestParentChildCompatible works", {
    makeOrigDestParentChildCompatible <- dembase:::makeOrigDestParentChildCompatible
    ## simple, orig-dest only
    x <- Counts(array(0,
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
    ## orig-dest only; need to subset and permute
    x <- Counts(array(0,
                      dim = c(2, 3, 3, 3),
                      dimnames = list(time = c("2001-2005", "2006-2010"),
                          age = c("0-4", "5-9", "10+"),
                          region_dest = c("a", "b", "c"),
                          region_orig = c("a", "b", "c"))))
    y <- Counts(array(0,
                      dim = c(3, 2, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          region = c("a", "b"),
                          time = c("2001-2005", "2006-2010"))))
    ans.obtained <- makeOrigDestParentChildCompatible(x = x, y = y, subset = TRUE)
    ans.expected <- Counts(array(0,
                                 dim = c(3, 2, 2, 2),
                                 dimnames = list(age = c("0-4", "5-9", "10+"),
                                     region_orig = c("a", "b"),
                                     region_dest = c("a", "b"),
                                     time = c("2001-2005", "2006-2010"))))
    expect_identical(ans.obtained, ans.expected)
    ## orig-dest and parent-child; need to subset and permute
    x <- Counts(array(0,
                      dim = c(2, 2, 2, 3, 3, 3),
                      dimnames = list(time = c("2001-2005", "2006-2010"),
                          eth_child = 1:2,
                          eth_parent = 1:2,
                          age = c("0-4", "5-9", "10+"),
                          region_orig = c("a", "b", "c"),
                          region_dest = c("a", "b", "c"))))
    y <- Counts(array(0,
                      dim = c(3, 2, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          eth = 2:1,
                          time = c("2001-2005", "2006-2010"))))
    ans.obtained <- makeOrigDestParentChildCompatible(x = x, y = y, subset = TRUE)
    ans.expected <- Counts(array(0,
                                 dim = c(3, 2, 2, 2),
                                 dimnames = list(age = c("0-4", "5-9", "10+"),
                                     eth_parent = 2:1,
                                     eth_child = 2:1,
                                     time = c("2001-2005", "2006-2010"))))
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeOrigDestParentChildTransform works", {
    makeOrigDestParentChildTransform <- dembase:::makeOrigDestParentChildTransform
    ## simple, orig-dest only
    x <- Counts(array(0,
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
    ans.expected <- new("CollapseTransform",
                        dims = 1:4,
                        indices = list(1:3, 1:2, 1:2, 1:2),
                        dimBefore = c(3L, 2L, 2L, 2L),
                        dimAfter = c(3L, 2L, 2L, 2L))
    expect_identical(ans.obtained, ans.expected)
    ## orig-dest only; need to subset and permute
    x <- Counts(array(0,
                      dim = c(2, 3, 3, 3),
                      dimnames = list(time = c("2001-2005", "2006-2010"),
                          age = c("0-4", "5-9", "10+"),
                          region_dest = c("a", "b", "c"),
                          region_orig = c("a", "b", "c"))))
    y <- Counts(array(0,
                      dim = c(3, 2, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          region = c("a", "b"),
                          time = c("2001-2005", "2006-2010"))))
    ans.obtained <- makeOrigDestParentChildTransform(x = x, y = y, subset = TRUE)
    ans.expected <- new("CollapseTransform",
                        dims = c(4L, 1L, 3L, 2L),
                        indices = list(1:2, 1:3, c(1:2, 0L), c(1:2, 0L)),
                        dimBefore = c(2L, 3L, 3L, 3L),
                        dimAfter = c(3L, 2L, 2L, 2L))
    expect_identical(ans.obtained, ans.expected)
    ## orig-dest and parent-child; need to subset and permute
    x <- Counts(array(0,
                      dim = c(2, 2, 2, 3, 3, 3),
                      dimnames = list(time = c("2001-2005", "2006-2010"),
                          eth_child = 1:2,
                          eth_parent = 1:2,
                          age = c("0-4", "5-9", "10+"),
                          region_orig = c("a", "b", "c"),
                          region_dest = c("a", "b", "c"))))
    y <- Counts(array(0,
                      dim = c(3, 2, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          eth = 2:1,
                          time = c("2001-2005", "2006-2010"))))
    ans.obtained <- makeOrigDestParentChildTransform(x = x, y = y, subset = TRUE)
    ans.expected <- new("CollapseTransform",
                        dims = c(4L, 3L, 2L, 1L, 0L, 0L),
                        indices = list(1:2, 2:1, 2:1, 1:3, rep(1L, 3), rep(1L, 3)),
                        dimBefore = c(2L, 2L, 2L, 3L, 3L, 3L),
                        dimAfter = c(3L, 2L, 2L, 2L))
    expect_identical(ans.obtained, ans.expected)
})


test_that("makePairCompatible works when e2 is Counts", {
    makePairCompatible <- dembase:::makePairCompatible
    x0 <- Counts(array(1:4,
                       dim = c(2, 2),
                       dimnames = list(period = c("2001-2005", "2006-2010"),
                       sex = c("m", "f"))))
    y0 <- Counts(array(1:2,
                       dim = c(1, 2),
                       dimnames = list(period = "2001-2010",
                       sex = c("f", "m"))))
    x1 <- Counts(array(c(3L, 7L),
                       dim = c(1, 2),
                       dimnames = list(period = "2001-2010",
                       sex = c("m", "f"))))
    y1 <- Counts(array(2:1,
                       dim = c(1, 2),
                       dimnames = list(period = "2001-2010",
                       sex = c("m", "f"))))
    expect_identical(makePairCompatible(x0, y0), list(x1, y1))
    x <- Counts(array(0,
                      dim = c(0, 2),
                      dimnames = list(reg = NULL, sex = c("m", "f"))))
    y <- Counts(array(0,
                      dim = c(0, 2),
                      dimnames = list(reg = NULL, sex = c("f", "m"))))
    expect_identical(makePairCompatible(x, y), list(x, x))
    x <- Counts(array(0,
                      dim = c(0, 2),
                      dimnames = list(reg = NULL, sex = c("m", "f"))))
    y <- Counts(array(0,
                      dim = 2,
                      dimnames = list(sex = c("f", "m"))))
    expect_identical(makePairCompatible(x, y), list(x, x))
    x <- Counts(array(0,
                      dim = c(2, 2),
                      dimnames = list(reg_orig = c("a", "b"),
                      reg_dest = c("a", "b"))))
    y <- Counts(array(0,
                      dim = c(2, 2, 2),
                      dimnames = list(reg_orig = c("a", "b"),
                      reg_dest = c("a", "b"),
                      iteration = 1:2)))
    expect_identical(makePairCompatible(x, y), list(y, y))
    x <- Counts(array(0,
                      dim = c(2, 2, 3),
                      dimnames = list(reg_orig = c("a", "b"),
                          reg_dest = c("a", "b"),
                          iteration = 1:3)))
    y <- Counts(array(0,
                      dim = c(2, 2, 2),
                      dimnames = list(reg_orig = c("a", "b"),
                          reg_dest = c("a", "b"),
                          iteration = 1:2)))
    expect_identical(makePairCompatible(x, y), list(y, y))
})

test_that("makePairCompatible works when e2 is Values", {
    makePairCompatible <- dembase:::makePairCompatible
    x0 <- Counts(array(1:4,
                       dim = c(2, 2),
                       dimnames = list(period = c("2001-2005", "2006-2010"),
                           sex = c("m", "f"))))
    y0 <- Values(array(1:2,
                       dim = c(1, 2),
                       dimnames = list(period = "2001-2010",
                           sex = c("f", "m"))))
    y1 <- Values(array(c(2L, 2L, 1L, 1L),
                       dim = c(2, 2),
                       dimnames = list(period = c("2001-2005", "2006-2010"),
                           sex = c("m", "f"))))
    expect_identical(makePairCompatible(x0, y0), list(x0, y1))
    x <- Counts(array(0,
                      dim = c(0, 2),
                      dimnames = list(reg = NULL, sex = c("m", "f"))))
    y <- Values(array(0,
                      dim = c(0, 2),
                      dimnames = list(reg = NULL, sex = c("f", "m"))))
    expect_identical(makePairCompatible(x, y), list(x, as(x, "Values")))
    x <- Counts(array(0,
                      dim = c(0, 2),
                      dimnames = list(reg = NULL, sex = c("m", "f"))))
    y <- Values(array(0,
                      dim = 2,
                      dimnames = list(sex = c("f", "m"))))
    expect_identical(makePairCompatible(x, y), list(x, as(x, "Values")))
    x <- Counts(array(0,
                      dim = c(2, 2),
                      dimnames = list(reg_orig = c("a", "b"),
                          reg_dest = c("a", "b"))))
    y <- Values(array(0,
                      dim = c(2, 2, 2),
                      dimnames = list(reg_orig = c("a", "b"),
                          reg_dest = c("a", "b"),
                          iteration = 1:2)))
    expect_identical(makePairCompatible(x, y), list(as(y, "Counts"), y))
    x0 <- Counts(array(0,
                       dim = c(3, 2),
                       dimnames = list(reg_orig = c("a", "b", "c"),
                           reg_dest = c("a", "b"))))
    y0 <- Values(array(0,
                       dim = c(2, 3, 2),
                       dimnames = list(reg_orig = c("a", "b"),
                           reg_dest = c("a", "b", "c"),
                           iteration = 1:2)))
    x1 <- Counts(array(0,
                       dim = c(2, 2, 2),
                       dimnames = list(reg_orig = c("a", "b"),
                           reg_dest = c("a", "b"),
                           iteration = 1:2)))
    y1 <- Values(array(0,
                       dim = c(2, 2, 2),
                       dimnames = list(reg_orig = c("a", "b"),
                           reg_dest = c("a", "b"),
                           iteration = 1:2)))
    expect_identical(makePairCompatible(x0, y0), list(x1, y1))
})

test_that("makePairTransforms method for Counts and Counts works", {
    makePairTransforms <- dembase:::makePairTransforms
    x <- Counts(array(0,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = c("m", "f"))))
    y <- Counts(array(0,
                      dim = c(2, 2),
                      dimnames = list(age = c("0-4", "5+"),
                      sex = c("f", "m"))))
    expect_identical(makePairTransforms(x, y),
                     list(new("CollapseTransform",
                              dims = 1:2,
                              indices = list(c(1L, 2L, 2L), 1:2),
                              dimBefore = c(3L, 2L),
                              dimAfter = c(2L, 2L)),
                          new("CollapseTransform",
                              dims = 1:2,
                              indices = list(c(1L, 2L), 2:1),
                              dimBefore = c(2L, 2L),
                              dimAfter = c(2L, 2L))))
    x <- Counts(array(0,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = c("m", "f"))))
    y <- Counts(array(0,
                      dim = 2,
                      dimnames = list(age = c("0-9", "10+"))))
    expect_identical(makePairTransforms(x, y),
                     list(new("CollapseTransform",
                              dims = c(1L, 0L),
                              indices = list(c(1L, 1L, 2L), c(1L, 1L)),
                              dimBefore = c(3L, 2L),
                              dimAfter = 2L),
                          new("CollapseTransform",
                              dims = 1L,
                              indices = list(1:2),
                              dimBefore = 2L,
                              dimAfter = 2L)))
    x <- Counts(array(0,
                      dim = c(3, 0),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = NULL)))
    y <- Counts(array(0,
                      dim = c(0, 2),
                      dimnames = list(sex = NULL, age = c("0-4", "5+"))))
    expect_identical(makePairTransforms(x, y),
                     list(new("CollapseTransform",
                              dims = 1:2,
                              indices = list(c(1L, 2L, 2L), integer()),
                              dimBefore = c(3L, 0L),
                              dimAfter = c(2L, 0L)),
                          new("CollapseTransform",
                              dims = 2:1,
                              indices = list(integer(), 1:2),
                              dimBefore = c(0L, 2L),
                              dimAfter = c(2L, 0L))))
    x <- Counts(array(1,
                      dim = 1,
                      dimnames = list(sex = "f")))
    y <- Counts(array(1,
                      dim = c(1, 2),
                      dimnames = list(sex = "f", iter = 1:2)))
    expect_error(makePairTransforms(x, y),
                 "one object has dimension with dimtype \"iteration\" but other does not")
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                                      sex = c("f", "m"))),
                dimtype = c(sex = "state"))
    y <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(sex = c("f", "m", "o"), age = c("0-4", "5+"))),
                dimtype = c(sex = "state"))
    expect_identical(makePairTransforms(x, y),
                     list(new("CollapseTransform",
                              dims = 1:2,
                              indices = list(c(1L, 2L, 2L), 1:2),
                              dimBefore = c(3L, 2L),
                              dimAfter = c(2L, 2L)),
                          new("CollapseTransform",
                              dims = 2:1,
                              indices = list(c(1:2, 0L), 1:2),
                              dimBefore = c(3L, 2L),
                              dimAfter = c(2L, 2L))))
})

test_that("makePairTransforms method for Counts and Values works", {
    makePairTransforms <- dembase:::makePairTransforms
    x <- Counts(array(0,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                                      sex = c("m", "f"))),
                dimtype = c(sex = "state"))
    y <- Values(array(0,
                      dim = c(2, 3),
                      dimnames = list(age = c("0-4", "5+"),
                                      sex = c("f", "m", "o"))),
                dimtype = c(sex = "state"))
    expect_identical(makePairTransforms(x, y),
                     list(new("CollapseTransform",
                              dims = 1:2,
                              indices = list(1:3, 1:2),
                              dimBefore = c(3L, 2L),
                              dimAfter = c(3L, 2L)),
                          new("ExtendTransform",
                              dims = 1:2,
                              indices = list(c(1L, 2L, 2L), 2:1),
                              dimBefore = c(2L, 3L),
                              dimAfter = c(3L, 2L))))
    x <- Counts(array(1:8,
                      dim = c(4, 2),
                      dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                      sex = c("m", "f"))))
    y <- Values(array(1:2,
                      dim = 2,
                      dimnames = list(age = c("0-9", "10-14"))))
    expect_identical(makePairTransforms(x, y),
                     list(new("CollapseTransform",
                              dims = 1:2,
                              indices = list(c(1:3, 0L), 1:2),
                              dimBefore = c(4L, 2L),
                              dimAfter = c(3L, 2L)),
                          new("ExtendTransform",
                              dims = c(1L, 0L),
                              indices = list(c(1L, 1L, 2L), c(1L, 1L)),
                              dimBefore = 2L,
                              dimAfter = c(3L, 2L))))
    x <- Counts(array(0,
                      dim = c(3, 0),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = NULL)))
    y <- Values(array(0,
                      dim = c(0, 2),
                      dimnames = list(sex = NULL, age = c("0-4", "5+"))))
    expect_identical(makePairTransforms(x, y),
                     list(new("CollapseTransform",
                              dims = 1:2,
                              indices = list(c(1L, 2L, 3L), integer()),
                              dimBefore = c(3L, 0L),
                              dimAfter = c(3L, 0L)),
                          new("ExtendTransform",
                              dims = 2:1,
                              indices = list(c(1:2, 2L),integer()),
                              dimBefore = c(0L, 2L),
                              dimAfter = c(3L, 0L))))
    x <- Counts(array(1,
                      dim = c(1, 1),
                      dimnames = list(sex = "f", iter = 1)))
    y <- Values(array(1,
                      dim = c(1, 2),
                      dimnames = list(sex = "f", iter = 1:2)))
    expect_identical(makePairTransforms(x, y),
                     list(new("CollapseTransform",
                              dims = 1:2,
                              indices = list(1L, 1L),
                              dimBefore = c(1L, 1L),
                              dimAfter = c(1L, 1L)),
                          new("ExtendTransform",
                              dims = 1:2,
                              indices = list(1L, 1L),
                              dimBefore = c(1L, 2L),
                              dimAfter = c(1L, 1L))))
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                                      sex = c("f", "m"))),
                dimtype = c(sex = "state"))
    y <- Values(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(sex = c("f", "m", "o"), age = c("0-4", "5+"))),
                dimtype = c(sex = "state"))
    expect_identical(makePairTransforms(x, y),
                     list(new("CollapseTransform",
                              dims = 1:2,
                              indices = list(1:3, 1:2),
                              dimBefore = c(3L, 2L),
                              dimAfter = c(3L, 2L)),
                          new("ExtendTransform",
                              dims = 2:1,
                              indices = list(c(1L, 2L, 2L), 1:2),
                              dimBefore = c(3L, 2L),
                              dimAfter = c(3L, 2L))))
})

test_that("makePairTransformsDbind works", {
    makePairTransformsDbind <- dembase:::makePairTransformsDbind
    e1 <- Counts(array(1:2,
                       dim = c(2, 1),
                       dimnames = list(age = c("0-4", "5+"), sex = "f")))
    e2 <- Counts(array(3:4,
                       dim = c(2, 1),
                       dimnames = list(age = c("0-4", "5+"), sex = "m")))
    ans.obtained <- makePairTransformsDbind(e1 = e1, e2 = e2, along = "sex")
    ans.expected <- list(new("CollapseTransform",
                             dims = 1:2,
                             indices = list(1:2, 1L),
                             dimBefore = 2:1,
                             dimAfter = 2:1),
                         new("CollapseTransform",
                             dims = 1:2,
                             indices = list(1:2, 1L),
                             dimBefore = 2:1,
                             dimAfter = 2:1))
    expect_identical(ans.obtained, ans.expected)
    e1 <- Counts(array(1:4,
                       dim = c(2, 2, 1),
                       dimnames = list(age = c("0-4", "5+"), sex = c("f", "m"), region = "a")))
    e2 <- Counts(array(5:8,
                       dim = c(2, 2, 1),
                       dimnames = list(age = c("0-4", "5+"), sex = c("f", "m"), region = "b")))
    ans.obtained <- makePairTransformsDbind(e1 = e1, e2 = e2, along = "region")
    ans.expected <- list(new("CollapseTransform",
                             dims = 1:3,
                             indices = list(1:2, 1:2, 1L),
                             dimBefore = c(2L, 2L, 1L),
                             dimAfter = c(2L, 2L, 1L)),
                         new("CollapseTransform",
                             dims = 1:3,
                             indices = list(1:2, 1:2, 1L),
                             dimBefore = c(2L, 2L, 1L),
                             dimAfter = c(2L, 2L, 1L)))
    expect_identical(ans.obtained, ans.expected)
    e1 <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(age = c("0-4", "5-9"), sex = c("f", "m"))))
    e2 <- Counts(array(1:4,
                       dim = c(2, 2),
                       dimnames = list(sex = c("m", "f"), age = c("10-14", "15+"))))
    ans.obtained <- makePairTransformsDbind(e1 = e1, e2 = e2, along = "age")
    ans.expected <- list(new("CollapseTransform",
                             dims = 2:1,
                             indices = list(1:2, 1:2),
                             dimBefore = c(2L, 2L),
                             dimAfter = c(2L, 2L)),
                         new("CollapseTransform",
                             dims = 1:2,
                             indices = list(2:1, 1:2),
                             dimBefore = c(2L, 2L),
                             dimAfter = c(2L, 2L)))
    expect_identical(ans.obtained, ans.expected)
    e1 <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(time = c("2001-2005", "2006-2010"),
                          iteration = 1:2)))
    e2 <- Counts(array(1:8,
                      dim = c(2, 2, 2),
                       dimnames = list(sex = c("m", "f"),
                           time = c("2001-2005", "2006-2010"),
                           iteration = 1:2)))
    ans.obtained <- makePairTransformsDbind(e1 = e1, e2 = e2, along = "iteration")
    ans.expected <- list(new("CollapseTransform",
                             dims = 1:2,
                             indices = list(1:2, 1:2),
                             dimBefore = c(2L, 2L),
                             dimAfter = c(2L, 2L)),
                         new("CollapseTransform",
                             dims = 0:2,
                             indices = list(c(1L, 1L), 1:2, 1:2),
                             dimBefore = c(2L, 2L, 2L),
                             dimAfter = c(2L, 2L)))
    expect_identical(ans.obtained, ans.expected)
    e1 <- Counts(array(0,
                      dim = c(0, 2),
                      dimnames = list(time = character(),
                          iteration = 1:2)),
                 dimscales = c(time = "Intervals"))
    e2 <- Counts(array(0,
                      dim = c(3, 0),
                       dimnames = list(iteration = 1:3, time = character())),
                 dimscales = c(time = "Intervals"))
    ans.obtained <- makePairTransformsDbind(e1 = e1, e2 = e2, along = "iteration")
    ans.expected <- list(new("CollapseTransform",
                             dims = 1:2,
                             indices = list(integer(), 1:2),
                             dimBefore = c(0L, 2L),
                             dimAfter = c(0L, 2L)),
                         new("CollapseTransform",
                             dims = 2:1,
                             indices = list(1:3, integer()),
                             dimBefore = c(3L, 0L),
                             dimAfter = c(0L, 3L)))
    expect_identical(ans.obtained, ans.expected)
    e1 <- Counts(array(1:8,
                      dim = c(2, 4),
                      dimnames = list(age = c("0-4", "5+"),
                          year = c(2000, 2005, 2010, 2015))))
    e2 <- Counts(array(1:3,
                      dim = c(3, 1),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          year = 1995)),
                 dimscales = c(year = "Points"))
    ans.obtained <- makePairTransformsDbind(e1, e2, along = "year")
    ans.expected <- list(new("CollapseTransform",
                             dims = 1:2,
                             indices = list(1:2, 1:4),
                             dimBefore = c(2L, 4L),
                             dimAfter = c(2L, 4L)),
                         new("CollapseTransform",
                             dims = 1:2,
                             indices = list(c(1L, 2L, 2L), 1L),
                             dimBefore = c(3L, 1L),
                             dimAfter = c(2L, 1L)))
    expect_identical(ans.obtained, ans.expected)
    e1 <- Counts(array(1,
                      dim = c(1, 1),
                      dimnames = list(age = "-5--1",
                          year = 1995)),
                dimscales = c(year = "Points"))
    e2 <- Counts(array(1:3,
                      dim = c(3, 1),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          year = 1995)),
                 dimscales = c(year = "Intervals"))
    expect_error(makePairTransformsDbind(e1, e2, along = "age"),
                 "\"year\" dimensions have incompatible dimscales")
})

test_that("makeTransform method for Counts works when y has class DemographicArray and no concordances", {
    Concordance <- classconc::Concordance
    makeTransform <- dembase:::makeTransform
    x <- Counts(array(0,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = c("m", "f"))))
    y <- Counts(array(0,
                      dim = c(2, 2),
                      dimnames = list(age = c("0-4", "5+"),
                      sex = c("f", "m"))))
    expect_identical(makeTransform(x, y),
                     new("CollapseTransform",
                         dims = 1:2,
                         indices = list(c(1L, 2L, 2L), 2:1),
                         dimBefore = c(3L, 2L),
                         dimAfter = c(2L, 2L)))
    x <- Counts(array(0,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = c("m", "f"))))
    y <- Counts(array(0,
                      dim = 2,
                      dimnames = list(age = c("0-9", "10+"))))
    expect_identical(makeTransform(x, y),
                     new("CollapseTransform",
                         dims = c(1L, 0L),
                         indices = list(c(1L, 1L, 2L), c(1L, 1L)),
                         dimBefore = c(3L, 2L),
                         dimAfter = 2L))
    x <- Counts(array(0,
                      dim = c(3, 0),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = NULL)))
    y <- Counts(array(0,
                      dim = c(0, 2),
                      dimnames = list(sex = NULL, age = c("0-4", "5-9"))))
    expect_identical(makeTransform(x, y, subset = TRUE),
                     new("CollapseTransform",
                         dims = c(2L, 1L),
                         indices = list(c(1L, 2L, 0L), integer()),
                         dimBefore = c(3L, 0L),
                         dimAfter = c(0L, 2L)))
    x <- Counts(array(0,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                      sex = c("m", "f"))))
    y <- Counts(array(0,
                      dim = c(2, 2, 2),
                      dimnames = list(age = c("0-4", "5+"),
                      sex = c("f", "m"),
                      sim = 1:2)))
    expect_error(makeTransform(x, y),
                 sprintf("one object has dimension \\[%s\\] that other does not",
                         dQuote("sim")))
})

test_that("makeTransform method for Counts works when y has class DemographicArray and using concordances", {
    Concordance <- classconc::Concordance
    makeTransform <- dembase:::makeTransform
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(reg = c("a", "b", "c"),
                          sex = c("m", "f"))))
    y <- Counts(array(1:2,
                      dim = c(1, 2),
                      dimnames = list(reg = "A",
                          sex = c("f", "m"))))
    conc <- Concordance(data.frame(from = c("a", "b", "c"), to = c("A", "A", "B")))
    concordances = list(reg = conc)
    ans.obtained <- makeTransform(x, y, subset = TRUE, concordances = concordances)
    ans.expected <- new("CollapseTransform",
                        dims = 1:2,
                        indices = list(c(1L, 1L, 0L), 2:1),
                        dimBefore = c(3L, 2L),
                        dimAfter = c(1L, 2L))
    expect_identical(ans.obtained, ans.expected)
    x <- Counts(array(1:18,
                      dim = c(3, 2, 3),
                      dimnames = list(reg_orig= c("a", "b", "c"),
                          sex = c("m", "f"),
                          reg_dest = c("a", "b", "c"))))
    y <- Values(array(1:8,
                      dim = c(2, 2, 2),
                      dimnames = list(reg_orig= c("A", "B"),
                          sex = c("m", "f"),
                          reg_dest = c("A", "B"))))
    conc <- Concordance(data.frame(from = c("a", "b", "c"), to = c("A", "A", "B")))
    concordances = list(reg = conc)
    ans.obtained <- makeTransform(x, y, subset = FALSE, concordances = concordances)
    ans.expected <- new("CollapseTransform",
                        dims = 1:3,
                        indices = list(c(1L, 1L, 2L), 1:2, c(1L, 1L, 2L)),
                        dimBefore = c(3L, 2L, 3L),
                        dimAfter = c(2L, 2L, 2L))
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeTransform method for Counts works when y has class numeric", {
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

test_that("dplot works", {
    x <- Counts(array(rpois(n = 36, lambda = 20),
                      dim = c(3, 3, 4),
                      dimnames = list(reg_orig = c("a", "b", "c"),
                      reg_dest = c("a", "b", "c"),
                      age = c("0-4", "5-9", "10-14", "15+"))))
    p <- dplot(count ~ age | reg_orig + reg_dest, data = x)
    expect_is(p, "trellis")
    p <- dplot(count ~ age | reg_orig + reg_dest, data = x, midpoints = "age")
    expect_is(p, "trellis")
    p <- dplot(~ reg_dest, data = x)
    expect_is(p, "trellis")
    p <- dplot(percent ~ reg_orig, data = x, groups = reg_dest)
    expect_is(p, "trellis")
    p <- dplot(log(count) ~ reg_orig, data = x)
    expect_is(p, "trellis")
    p <- dplot(count + log(count) ~ reg_orig, data = x)
    expect_is(p, "trellis")
    f <- function(df) dplot(count ~ reg_orig, data = df)
    p <- f(x)
    expect_is(p, "trellis")
    p <- dplot(count ~ reg_dest, data = x, subarray = age > 5)
    expect_is(p, "trellis")
    p <- dplot(count ~ reg_dest, data = x, subarray = age == "0-4")
    expect_is(p, "trellis")
    p <- dplot(count ~ age | reg_dest + reg_orig, data = x, subarray = reg_dest == "a" & reg_orig == "b")
    expect_is(p, "trellis")
    p <- dplot(count ~ age, data = x, subarray = reg_dest == "a" & reg_orig == "b")
    expect_is(p, "trellis")
    p <- dplot(count ~ age,
               data = x,
               midpoints = TRUE,
               type = "o",
               subarray = ((reg_dest == "a") &
                           (reg_orig == "b") &
                           (age > 1) &
                           (age < 14)))
    expect_is(p, "trellis")
    lambda <- Counts(array(c(10, 15, 20, 5, 10, 15),
                    dim = c(3, 2),
                    dimnames = list(age = c("0-4", "5-9", "10+"), sex = c("f", "m"))))
    x <- Counts(array(replicate(n = 100, rpois(n = 6, lambda = lambda)),
                      dim = c(dim(lambda), 100),
                      dimnames = c(dimnames(lambda), list(iteration = 1:100))))
    p <- dplot( ~ age, data = x)
    expect_is(p, "trellis")
    p <- dplot( ~ age, data = x, overlay = list(values = lambda))
    expect_is(p, "trellis")
    p <- dplot( ~ age, data = x, midpoints = TRUE)
    expect_is(p, "trellis")
    p <- dplot( ~ age | sex, data = x)
    expect_is(p, "trellis")
    p <- dplot( ~ age | sex, data = x, probs = c(0.025, 0.2, 0.8, 0.975))
    expect_is(p, "trellis")
    p <- dplot( ~ age | sex, col = "red", alpha = 0.5, data = x, probs = c(0.025, 0.2, 0.8, 0.975),
               overlay = list(values = lambda, col = "blue", alpha = 0.5))
    expect_is(p, "trellis")
    p <- dplot( ~ age, groups = sex, data = x)
    p <- dplot( ~ age, groups = sex, col = c("light pink", "light blue"), data = x,
               overlay = list(values = lambda, pch = 3, col = c("red", "dark blue")),
               midpoints = "age")
    expect_is(p, "trellis")
    x.with.missing <- x
    x.with.missing[1] <- NA
    p <- dplot( ~ age, data = x.with.missing, na.rm = TRUE)
})


test_that("redistribute works when given valid inputs - counts is Counts", {
    ## no iterations, no 'n'
    counts <- Counts(array(rpois(n = 8, lambda = 10),
                           dim = c(4, 2),
                           dimnames = list(age = 0:3, sex = c("f", "m"))))
    weights <- Counts(array(rpois(n = 24, lambda = 10),
                            dim = 4:2,
                            dimnames = list(age = 0:3, reg = 1:3, sex = c("f", "m"))))
    ans <- redistribute(counts = counts, weights = weights)
    expect_identical(collapseDimension(ans, dimension = "reg"),
                     counts)
    ## no iterations, has 'n'
    counts <- Counts(array(rpois(n = 8, lambda = 10),
                           dim = c(4, 2),
                           dimnames = list(age = 0:3, sex = c("f", "m"))))
    weights <- Counts(array(rpois(n = 24, lambda = 10),
                            dim = 4:2,
                            dimnames = list(age = 0:3, reg = 1:3, sex = c("f", "m"))))
    ans <- redistribute(counts = counts, weights = weights, n = 5)
    expect_identical(dim(ans), c(4:2, 5L))
    expect_identical(collapseDimension(ans[,,,3L], dim = "reg"),
                     counts)
    ## counts has iterations; weights does not
    counts <- Counts(array(rpois(n = 24, lambda = 10),
                           dim = c(4, 2, 3),
                           dimnames = list(age = 0:3, sex = c("f", "m"),
                               iteration = 1:3)))
    weights <- Counts(array(rpois(n = 24, lambda = 10),
                            dim = 4:2,
                            dimnames = list(age = 0:3, reg = 1:3, sex = c("f", "m"))))
    ans <- redistribute(counts = counts, weights = weights)
    expect_identical(dim(ans), c(4:2, 3L))
    expect_identical(collapseDimension(ans[,,,3], dim = "reg"),
                     counts[,,3])
    ## counts does not have iterations; weights does
    counts <- Counts(array(rpois(n = 8, lambda = 10),
                           dim = c(4, 2),
                           dimnames = list(age = 0:3, sex = c("f", "m"))))
    weights <- Counts(array(rpois(n = 24, lambda = 10),
                            dim = c(4:2, 3),
                            dimnames = list(age = 0:3, reg = 1:3, sex = c("f", "m"),
                                iteration = 1:3)))
    ans <- redistribute(counts = counts, weights = weights)
    expect_identical(dim(ans), c(4:2, 3L))
    expect_identical(collapseDimension(ans[,,,3], dim = "reg"),
                     counts)
    ## counts and weights both have iterations
    counts <- Counts(array(rpois(n = 24, lambda = 10),
                           dim = c(4, 2, 3),
                           dimnames = list(age = 0:3, sex = c("f", "m"),
                               iteration = 1:3)))
    weights <- Counts(array(rpois(n = 24, lambda = 10),
                            dim = c(4:2, 3),
                            dimnames = list(age = 0:3, reg = 1:3, sex = c("f", "m"),
                                iteration = 1:3)))
    ans <- redistribute(counts = counts, weights = weights)
    expect_identical(dim(ans), c(4:2, 3L))
    expect_identical(collapseDimension(ans[,,,3], dim = "reg"),
                     counts[,,3])
})

test_that("redistribute works when given valid inputs - counts is numeric", {
    ## no iterations, no 'n'
    weights <- Counts(array(rpois(n = 24, lambda = 10),
                            dim = 4:2,
                            dimnames = list(age = 0:3, reg = 1:3, sex = c("f", "m"))))
    ans <- redistribute(counts = 24, weights = weights)
    expect_identical(collapseDimension(ans, margin = character()),
                     24L)
    ## no iterations, has 'n'
    counts <- 10
    weights <- Counts(array(rpois(n = 24, lambda = 10),
                            dim = 4:2,
                            dimnames = list(age = 0:3, reg = 1:3, sex = c("f", "m"))))
    ans <- redistribute(counts = counts, weights = weights, n = 5)
    expect_identical(dim(ans), c(4:2, 5L))
    expect_identical(collapseDimension(ans, margin = "iteration"),
                     CountsOne(values = 10L, labels = 1:5, name = "iteration"))
    ## counts has iterations; weights does not
    counts <- Counts(array(rpois(n = 24, lambda = 10),
                           dim = c(4, 2, 3),
                           dimnames = list(age = 0:3, sex = c("f", "m"),
                               iteration = 1:3)))
    weights <- Counts(array(rpois(n = 24, lambda = 10),
                            dim = 4:2,
                            dimnames = list(age = 0:3, reg = 1:3, sex = c("f", "m"))))
    ans <- redistribute(counts = counts, weights = weights)
    expect_identical(dim(ans), c(4:2, 3L))
    expect_identical(collapseDimension(ans[,,,3], dim = "reg"),
                     counts[,,3])
    ## counts does not have iterations; weights does
    counts <- Counts(array(rpois(n = 8, lambda = 10),
                           dim = c(4, 2),
                           dimnames = list(age = 0:3, sex = c("f", "m"))))
    weights <- Counts(array(rpois(n = 24, lambda = 10),
                            dim = c(4:2, 3),
                            dimnames = list(age = 0:3, reg = 1:3, sex = c("f", "m"),
                                iteration = 1:3)))
    ans <- redistribute(counts = counts, weights = weights)
    expect_identical(dim(ans), c(4:2, 3L))
    expect_identical(collapseDimension(ans[,,,3], dim = "reg"),
                     counts)
    ## counts and weights both have iterations
    counts <- Counts(array(rpois(n = 24, lambda = 10),
                           dim = c(4, 2, 3),
                           dimnames = list(age = 0:3, sex = c("f", "m"),
                               iteration = 1:3)))
    weights <- Counts(array(rpois(n = 24, lambda = 10),
                            dim = c(4:2, 3),
                            dimnames = list(age = 0:3, reg = 1:3, sex = c("f", "m"),
                                iteration = 1:3)))
    ans <- redistribute(counts = counts, weights = weights)
    expect_identical(dim(ans), c(4:2, 3L))
    expect_identical(collapseDimension(ans[,,,3], dim = "reg"),
                     counts[,,3])
})

test_that("redistribute throws appropriate errors", {
    ## no quantile dimtype
    counts <- Counts(array(rpois(n = 8, lambda = 10),
                           dim = c(4, 2),
                           dimnames = list(age = 0:3, quantile = c(0.1, 0.9))))
    weights <- Counts(array(rpois(n = 24, lambda = 10),
                            dim = 4:2,
                            dimnames = list(age = 0:3, reg = 1:3, sex = c("f", "m"))))
    expect_error(redistribute(counts, weights),
                 "'counts' has dimension with dimtype \"quantile\"")
    ## arguments have length > 0
    counts <- Counts(array(rpois(n = 8, lambda = 10),
                           dim = c(4, 2),
                           dimnames = list(age = 0:3, sex = c("f", "m"))))
    weights <- Counts(array(0L,
                            dim = 0,
                            dimnames = list(age = character())))
    expect_error(redistribute(counts, weights),
                 "'weights' has length 0")
    ## no missing values
    counts <- Counts(array(rpois(n = 24, lambda = 10),
                           dim = c(4, 2, 3),
                           dimnames = list(age = 0:3, sex = c("f", "m"),
                               iteration = 1:3)))
    weights <- Counts(array(rpois(n = 24, lambda = 10),
                            dim = c(4:2, 3),
                            dimnames = list(age = 0:3, reg = 1:3, sex = c("f", "m"),
                                iteration = 1:3)))
    counts[1] <- NA
    expect_error(redistribute(counts, weights),
                 "'counts' has missing values")
    ## no negative values
    counts <- Counts(array(rpois(n = 24, lambda = 10),
                           dim = c(4, 2, 3),
                           dimnames = list(age = 0:3, sex = c("f", "m"),
                               iteration = 1:3)))
    weights <- Counts(array(rpois(n = 24, lambda = 10),
                            dim = c(4:2, 3),
                            dimnames = list(age = 0:3, reg = 1:3, sex = c("f", "m"),
                                iteration = 1:3)))
    weights[1] <- -1
    expect_error(redistribute(counts, weights),
                 "'weights' has negative values")
    ## weights do not sum to 0
    counts <- Counts(array(rpois(n = 24, lambda = 10),
                           dim = c(4, 2, 3),
                           dimnames = list(age = 0:3, sex = c("f", "m"),
                               iteration = 1:3)))
    weights <- Counts(array(0,
                            dim = c(4:2, 3),
                            dimnames = list(age = 0:3, reg = 1:3, sex = c("f", "m"),
                                iteration = 1:3)))
    expect_error(redistribute(counts, weights),
                 "'weights' sum to 0")
    ## counts all integer
    counts <- Counts(array(rpois(n = 24, lambda = 10),
                           dim = c(4, 2, 3),
                           dimnames = list(age = 0:3, sex = c("f", "m"),
                               iteration = 1:3)))
    weights <- Counts(array(rpois(n = 24, lambda = 10),
                            dim = c(4:2, 3),
                            dimnames = list(age = 0:3, reg = 1:3, sex = c("f", "m"),
                                iteration = 1:3)))
    counts[2] <- 1.01
    expect_error(redistribute(counts, weights),
                 "'counts' has non-integer values")
    ## weights is not compatible with counts
    counts <- Counts(array(rpois(n = 24, lambda = 10),
                           dim = c(4, 2, 3),
                           dimnames = list(age = 0:3, sex = c("f", "m"),
                               iteration = 1:3)))
    weights <- Counts(array(rpois(n = 24, lambda = 10),
                            dim = c(4:2, 3),
                            dimnames = list(age = 0:3, reg = 1:3, sex = c("female", "male"),
                                iteration = 1:3)))
    expect_error(redistribute(counts, weights),
                 "'weights' not compatible with 'counts'")
})

test_that("redistributeCategory works", {
    for (seed in seq_len(n.test)) {
        ## one dimension, one category
        set.seed(seed)
        x <- Counts(array(1:5, dim = 5, dimnames = list(reg = letters[1:5])))
        ans.obtained <- redistributeCategory(x, dimension = "reg", category = "e")
        set.seed(seed)
        ans.expected <- x[1:4] + redistribute(5, weights = x[1:4])
        expect_identical(ans.obtained, ans.expected)
        ## one dimension, two categories, n = 3
        x <- Counts(array(1:5, dim = 5, dimnames = list(reg = letters[1:5])))
        set.seed(seed)
        ans.obtained <- redistributeCategory(x, dimension = "reg",
                                             category = c("a", "b"),
                                             epsilon = 0.1, n = 3)
        set.seed(seed)
        ans.expected <- x[3:5] + redistribute(3, weights = x[3:5] + 0.1, n = 3)
        expect_identical(ans.obtained, ans.expected)
        ## one dimension, only 1 category remaining
        x <- Counts(array(1:5, dim = 5, dimnames = list(reg = letters[1:5])))
        set.seed(seed)
        ans.obtained <- redistributeCategory(x, dimension = "reg",
                                             category = c("a", "b", "c", "d"))
        set.seed(seed)
        ans.expected <- Counts(array(15L, dim = 1, dimnames = list(reg = "e")))
        expect_identical(ans.obtained, ans.expected)
        ## two dimensions, 3 iterations, one category
        set.seed(seed)
        x <- Counts(array(rpois(60, lambda = 10),
                          dim = 5:3,
                          dimnames = list(reg = letters[1:5], age = 0:3, iter = 1:3)))
        set.seed(seed)
        ans.obtained <- redistributeCategory(x, dimension = "reg", category = "e")
        set.seed(seed)
        ans.expected <- x[-5,,] + redistribute(x[5, , ], weights = x[-5, , ])
        expect_identical(ans.obtained, ans.expected)
        
        ## orig-dest, 3 iterations, 1 category
        x <- Counts(array(rpois(48, lambda = 10),
                          dim = c(4, 4, 3),
                          dimnames = list(reg_orig = letters[1:4],
                              reg_dest = letters[1:4],
                              iter = 1:3)))
        set.seed(seed)
        ans.obtained <- redistributeCategory(x, dimension = "reg", category = "d")
        set.seed(seed)
        xx <- Counts(array(x@.Data,
                          dim = c(4, 4, 3),
                          dimnames = list(reg_1 = letters[1:4],
                              reg_2 = letters[1:4],
                              iter = 1:3)))
        xx <- xx[-4,,] + redistribute(xx[4, , ], weights = xx[-4, , ])
        xx <- xx[,-4,] + redistribute(xx[ , 4, ], weights = xx[ , -4, ])
        xx <- as(xx, "array")
        names(dimnames(xx))[1:2] <- c("reg_orig", "reg_dest")
        ans.expected <- Counts(xx)
        expect_identical(ans.obtained, ans.expected)
        ## orig-dest, 3 iterations, 2 categories
        x <- Counts(array(rpois(48, lambda = 10),
                          dim = c(4, 4, 3),
                          dimnames = list(reg_orig = letters[1:4],
                              reg_dest = letters[1:4],
                              iter = 1:3)))
        set.seed(seed)
        ans.obtained <- redistributeCategory(x, dimension = "reg",
                                             category = c("b", "c"))
        set.seed(seed)
        xx <- Counts(array(x@.Data,
                          dim = c(4, 4, 3),
                          dimnames = list(reg_1 = letters[1:4],
                              reg_2 = letters[1:4],
                              iter = 1:3)))
        xx <- xx[-(2:3),,] + redistribute(xx[2,,] + xx[3,,],
                                          weights = xx[-(2:3), , ])
        xx <- xx[,-(2:3),] + redistribute(xx[,2,] + xx[,3,], weights = xx[,-(2:3),])
        xx <- as(xx, "array")
        names(dimnames(xx))[1:2] <- c("reg_orig", "reg_dest")
        ans.expected <- Counts(xx)
        expect_identical(ans.obtained, ans.expected)
        ## check dimension
        x <- Counts(array(1:5, dim = 5, dimnames = list(reg = letters[1:5])))
        expect_error(redistributeCategory(x,
                                          dimension = c("reg", "wrong"),
                                          category = "e"),
                     "'dimension' does not have length 1")
        ## check category
        x <- Counts(array(1:5, dim = 5, dimnames = list(reg = letters[1:5])))
        expect_error(redistributeCategory(x,
                                          dimension = "reg",
                                          category = integer()),
                     "'category' has length 0")
        x <- Counts(array(1:5, dim = 5, dimnames = list(reg = letters[1:5])))
        expect_error(redistributeCategory(x,
                                          dimension = "reg",
                                          category = c("a", NA)),
                     "'category' has missing values")
        ## check DimScales
        x <- Counts(array(1:5, dim = 5, dimnames = list(age = 0:4)))
        expect_error(redistributeCategory(x,
                                          dimension = "age",
                                          category = "0"),
                     "dimension \"age\" does not have dimscale \"Categories\"")
        ## check has category
        x <- Counts(array(1:5, dim = 5, dimnames = list(reg = letters[1:5])))
        expect_error(redistributeCategory(x,
                                          dimension = "reg",
                                          category = "wrong"),
                     "dimension \"reg\" does not have category \"wrong\"")
        ## contains whole dimension
        x <- Counts(array(1:5, dim = 5, dimnames = list(reg = letters[1:5])))
        expect_error(redistributeCategory(x,
                                          dimension = "reg",
                                          category = letters[1:5]),
                     "'category' contains all of dimension \"reg\"")
    }
})

