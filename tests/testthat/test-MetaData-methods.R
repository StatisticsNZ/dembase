
context("MetaData-methods")

## Test many methods via tests of DemographicArray methods, to avoid duplication

test_that("Extract methods work", {
    metadata <- dembase:::metadata
    a <- array(1:6,
               dim = c(3, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
               sex = c("Male", "Female")))
    x <- Counts(a)
    m <- metadata(x)
    expect_identical(dimtypes(m[1]), c(age = "age"))
    expect_identical(dimscales(m[1]), c(age = "Intervals"))
    expect_identical(dimtypes(m[-2]), c(age = "age"))
    expect_identical(dimtypes(m[2:1]), dimtypes(m)[2:1])
    expect_identical(dimtypes(m[c(FALSE, TRUE)]), dimtypes(m)[2])
    expect_identical(m["age"], m[1])
    expect_error(m[3], "'i' outside valid range")
    expect_error(m[-4], "'i' outside valid range")
    expect_error(m[rep(1, 2)], "'names' has duplicates")
    expect_error(m["wrong"], "'i' outside valid range")
    expect_error(m[c(TRUE, FALSE, TRUE)], "'i' outside valid range")
    expect_error(m[NA], "'i' has missing values")
    a <- array(1:4,
               dim = c(2, 2),
               dimnames = list(reg_orig = c("a", "b"), reg_dest = c("a", "b")))
    x <- Counts(a)
    m <- metadata(x)
    expect_identical(dimtypes(m[1]), c(reg = "state"))
    expect_identical(dimtypes(m[2]), c(reg = "state"))
    expect_identical(dimtypes(m[2:1]), c(reg_dest = "destination", reg_orig = "origin"))
})

test_that("ageMax works", {
    x <- new("MetaData",
             nms = "age",
             dimtypes = "age",
             DimScales = list(new("Points", dimvalues = c(0, 1, 5))))
    expect_identical(ageMax(x), 5)
})

test_that("ageMax<- works", {
    x <- new("MetaData",
             nms = "age",
             dimtypes = "age",
             DimScales = list(new("Intervals", dimvalues = c(0, 1, 5))))
    x.new <- x
    ageMax(x.new) <- Inf
    expect_identical(x.new,
                     new("MetaData",
                         nms = "age",
                         dimtypes = "age",
                         DimScales = list(new("Intervals", dimvalues = c(0, 1, Inf)))))
})

test_that("ageMin works", {
    x <- new("MetaData",
             nms = "age",
             dimtypes = "age",
             DimScales = list(new("Points", dimvalues = c(0, 1, 5))))
    expect_identical(ageMin(x), 0)
})

test_that("ageMin<- works", {
    x <- new("MetaData",
             nms = "age",
             dimtypes = "age",
             DimScales = list(new("Intervals", dimvalues = c(0, 1, 5))))
    x.new <- x
    ageMin(x.new) <- -Inf
    expect_identical(x.new,
                     new("MetaData",
                         nms = "age",
                         dimtypes = "age",
                         DimScales = list(new("Intervals", dimvalues = c(-Inf, 1, 5)))))
})

test_that("dimtypes<- works", {
    metadata <- dembase:::metadata
    DimScales <- dembase:::DimScales
    a <- array(1:6,
               dim = c(3, 2),
               dimnames = list(age = 1:3,
               sex = c("Male", "Female")))
    x <- Counts(a)
    m <- metadata(x)
    dimtypes(m) <- rep("state", 2)
    expect_identical(dimtypes(m),
                     c(age = "state", sex = "state"))
    dimtypes(m) <- c("iteration", "state")
    expect_identical(dimtypes(m),
                     c(age = "iteration", sex = "state"))
    expect_identical(dimscales(m),
                     c(age = "Iterations", sex = "Categories"))
    expect_error(dimtypes(m) <- c("quantile", "state"),
                 "labels not valid for dimscale")
    a <- array(1:6,
               dim = c(3, 2),
               dimnames = list(quant = c(0.025, 0.5, 0.975),
               sex = c("Male", "Female")))
    x <- Counts(a, dimtypes = c(quant = "time"), dimscales = c(quant = "Points"))
    m <- metadata(x)
    dimtypes(m)["quant"] <- "quantile"
    expect_identical(dimtypes(m),
                     c(quant = "quantile", sex = "sex"))
    expect_identical(dimscales(m),
                     c(quant = "Quantiles", sex = "Sexes"))
    expect_error(dimtypes(m)["quant"] <- "triangle",
                 "labels not valid for dimscale")
    a <- array(0,
               dim = c(0, 2),
               dimnames = list(sex = NULL,
               cohort = 2000:2001))
    x <- Counts(a)
    m <- metadata(x)
    dimtypes(m)["sex"] <- "iteration"
    expect_identical(dimtypes(m)[[1]], "iteration")
    dimtypes(m) <- "state"
    expect_identical(dimtypes(m),
                     c(sex = "state", cohort = "state"))
    expect_error(dimtypes(m) <- rep("state", 3),
                 "number of items to replace is not a multiple of replacement length")
})

test_that("dimscales<- works", {
    metadata <- dembase:::metadata
    DimScales <- dembase:::DimScales
    a <- array(1:6,
               dim = c(3, 2),
               dimnames = list(age = 0:2,
               sex = c("Male", "Female")))
    x <- Counts(a, dimscales = c(age = "Points"))
    m <- metadata(x)
    dimscales(m) <- c("Intervals", "Sexes")
    expect_identical(dimscales(m),
                     c(age = "Intervals", sex = "Sexes"))
    expect_identical(DimScales(m)[[1]],
                     new("Intervals", dimvalues = c(0, 1, 2, 3), isAge = TRUE))
    m <- metadata(x)
    dimscales(m)["age"] <- "Intervals"
    expect_identical(DimScales(m)[[1]],
                     new("Intervals", dimvalues = c(0, 1, 2, 3), isAge = TRUE))
    m <- metadata(x)
    expect_error(dimscales(m) <- rep("Categories", 3),
                 paste("number of items to replace",
                       "is not a multiple of replacement length"))
    m <- metadata(x)
    expect_error(dimscales(m)[2] <- "Points",
                 "labels not valid for dimscale")
    m <- metadata(x)
    expect_error(dimscales(m)[1] <- "Categories",
                 "dimension \"age\" has dimtype \"age\" but dimscale \"Categories\"")
})

test_that("limits works", {
    metadata <- new("MetaData",
                    nms = c("age", "sex", "time", "region"),
                    dimtypes = c("age", "state", "time", "state"),
                    DimScales = list(new("Intervals", dimvalues = c(0, 1, 5, 10, Inf), isAge = TRUE),
                        new("Categories", dimvalues = c("f", "m")),
                        new("Points", dimvalues = 2000),
                        new("Categories", dimvalues = character())))
    ans.obtained <- limits(metadata)
    ans.expected <- data.frame(age = c("0", "10+"),
                               sex = c("f", "m"),
                               time = c("2000", "2000"),
                               region = as.character(c(NA, NA)),
                               row.names = c("first", "last"))
    expect_identical(ans.obtained, ans.expected)
    metadata <- new("MetaData",
                    nms = "age",
                    dimtypes = "age",
                    DimScales = list(new("Intervals", dimvalues = c(0, 1, 5, 10, Inf), isAge = TRUE)))
    ans.obtained <- limits(metadata)
    ans.expected <- data.frame(age = c("0", "10+"),
                               row.names = c("first", "last"))
})    

test_that("resetIterations works", {
    x <- new("MetaData",
             nms = c("age", "sex"),
             dimtypes = c("age", "state"),
             DimScales = list(new("Intervals", dimvalues = 0:5),
                 new("Categories", dimvalues = c("f", "m"))))
    expect_identical(resetIterations(x), x)
    x <- new("MetaData",
             nms = c("age", "sex", "iteration"),
             dimtypes = c("age", "state", "iteration"),
             DimScales = list(new("Intervals", dimvalues = 0:5),
                 new("Categories", dimvalues = c("f", "m")),
                 new("Iterations", dimvalues = 1:5)))
    expect_identical(resetIterations(x), x)
    x0 <- new("MetaData",
             nms = c("age", "sex", "iteration"),
             dimtypes = c("age", "state", "iteration"),
             DimScales = list(new("Intervals", dimvalues = 0:5),
                 new("Categories", dimvalues = c("f", "m")),
                 new("Iterations", dimvalues = 6:8)))
    x1 <- new("MetaData",
             nms = c("age", "sex", "iteration"),
             dimtypes = c("age", "state", "iteration"),
             DimScales = list(new("Intervals", dimvalues = 0:5),
                 new("Categories", dimvalues = c("f", "m")),
                 new("Iterations", dimvalues = 1:3)))
    expect_identical(resetIterations(x0), x1)
})

test_that("setAgeMax works", {
    x <- new("MetaData",
             nms = "age",
             dimtypes = "age",
             DimScales = list(new("Intervals", dimvalues = c(0, 1, 5))))
    expect_identical(setAgeMax(x, value = Inf),
                     new("MetaData",
                         nms = "age",
                         dimtypes = "age",
                         DimScales = list(new("Intervals", dimvalues = c(0, 1, Inf)))))
})

test_that("setAgeMin works", {
    x <- new("MetaData",
             nms = "age",
             dimtypes = "age",
             DimScales = list(new("Intervals", dimvalues = c(0, 1, 5))))
    expect_identical(setAgeMin(x, value = -5),
                     new("MetaData",
                         nms = "age",
                         dimtypes = "age",
                         DimScales = list(new("Intervals", dimvalues = c(-5, 1, 5)))))
})



    
