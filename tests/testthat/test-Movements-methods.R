
test.extended <- FALSE

test_that("accession works with Movements - no age", {
    population <- CountsOne(values = seq(100, 200, 10),
                            labels = seq(2000, 2100, 10),
                            name = "time")
    births <- CountsOne(values = 15,
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    deaths <- CountsOne(values = 5,
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    x <- Movements(population = population,
                   births = births,
                   exits = list(deaths = deaths))
    expect_null(accession(x))
})

test_that("accession works with Movements - with age, account contains births - openAge is FALSE", {
    population <- Counts(array(rpois(n = 90, lambda = 100),
                               dim = c(3, 2, 4, 3),
                               dimnames = list(age = c("0-4", "5-9", "10+"),
                                               sex = c("f", "m"),
                                               reg = 1:4,
                                               time = c(2000, 2005, 2010))))
    births <- Counts(array(rpois(n = 90, lambda = 5),
                           dim = c(1, 2, 5, 2),
                           dimnames = list(age = "5-9",
                                           sex = c("m", "f"),
                                           reg = 1:5,
                                           time = c("2001-2005", "2006-2010"))))
    internal <- Counts(array(rpois(n = 300, lambda = 10),
                             dim = c(3, 2, 5, 5, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                             sex = c("m", "f"),
                                             reg_orig = 1:5,
                                             reg_dest = 1:5,
                                             time = c("2001-2005", "2006-2010"))))
    deaths <- Counts(array(rpois(n = 72, lambda = 10),
                           dim = c(3, 2, 4, 3),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                           sex = c("m", "f"),
                                           reg = 4:1,
                                           time = c("2001-2005", "2006-2010", "2011-2015"))))
    immigration <- Counts(array(rpois(n = 72, lambda = 5),
                                dim = c(3, 2, 4, 2),
                                dimnames = list(age = c("0-4", "5-9", "10+"),
                                                sex = c("m", "f"),
                                                reg = 1:4,
                                                time = c("2001-2005", "2006-2010"))))
    emigration <- Counts(array(rpois(n = 72, lambda = 5),
                               dim = c(3, 2, 4, 2),
                               dimnames = list(age = c("0-4", "5-9", "10+"),
                                               sex = c("m", "f"),
                                               reg = 1:4,
                                               time = c("2001-2005", "2006-2010"))))
    reclassification <- Counts(array(c(1, -1),
                                     dim = c(3, 2, 2, 4),
                                     dimnames = list(age = c("0-4", "5-9", "10+"),
                                                     sex = c("m", "f"),
                                                     time = c("2001-2005", "2006-2010"),
                                                     reg = 1:4)))
    x <- Movements(population = population,
                   births = births,
                   internal = internal,
                   entries = list(immigration = immigration),
                   exits = list(deaths = deaths, emigration = emigration),
                   net = list(reclassification = reclassification))
    acc.births <- collapseDimension(x@components[[1]],
                                    dimension = c("age", "triangle"))
    acc.births <- addDimension(acc.births,
                               name = "age",
                               labels = "0",
                               dimscale = "Points")
    acc.internal <- slab(as(x@components[[2]], "Counts"),
                         dimension = "triangle",
                         elements = "Upper")
    acc.internal <- collapseOrigDest(acc.internal)
    acc.internal <- slab(acc.internal,
                         dimension = "age",
                         elements = 1:2)
    acc.immigration <- slab(as(x@components[[3]], "Counts"),
                            dimension = "triangle",
                            elements = "Upper")
    acc.immigration <- slab(acc.immigration,
                            dimension = "age",
                            elements = 1:2)
    acc.deaths <- slab(as(x@components[[4]], "Counts"),
                       dimension = "triangle",
                       elements = "Upper")
    acc.deaths <- slab(acc.deaths,
                       dimension = "age",
                       elements = 1:2)
    acc.deaths <- -1L * acc.deaths
    acc.emigration <- slab(as(x@components[[5]], "Counts"),
                           dimension = "triangle",
                           elements = "Upper")
    acc.emigration <- slab(acc.emigration,
                           dimension = "age",
                           elements = 1:2)
    acc.emigration <- -1L * acc.emigration
    acc.reclassification <- slab(as(x@components[[6]], "Counts"),
                                 dimension = "triangle",
                                 elements = "Upper")
    acc.reclassification <- slab(acc.reclassification,
                                 dimension = "age",
                                 elements = 1:2)
    popn.forward <- slab(x@population,
                         dimension = "time",
                         elements = 1:2)
    popn.forward <- slab(popn.forward,
                         dimension = "age",
                         elements = 1:2)
    popn.forward@metadata@DimScales[[4]] <- new("Intervals", dimvalues = c(2000, 2005, 2010))
    ## births = TRUE
    ans.obtained <- accession(x)
    ans.expected <- (popn.forward + acc.internal + acc.immigration + acc.deaths
        + acc.emigration + acc.reclassification)
    ans.expected@metadata@DimScales[[1]] <- new("Points", dimvalues = c(5, 10))
    dimnames(ans.expected@.Data)[[1]] <- c("5", "10")
    ans.expected <- dbind(ans.expected, acc.births, along = "age")
    expect_identical(ans.obtained, ans.expected)
    ## births = FALSE
    ans.obtained <- accession(x, births = FALSE)
    ans.expected <- (popn.forward + acc.internal + acc.immigration + acc.deaths
        + acc.emigration + acc.reclassification)
    ans.expected@metadata@DimScales[[1]] <- new("Points", dimvalues = c(5, 10))
    dimnames(ans.expected@.Data)[[1]] <- c("5", "10")
    expect_identical(ans.obtained, ans.expected)
})

test_that("accession works with Movements - with age, account contains births - openAge is TRUE", {
    population <- Counts(array(rpois(n = 90, lambda = 100),
                               dim = c(3, 2, 4, 3),
                               dimnames = list(age = c("0-4", "5-9", "10+"),
                                               sex = c("f", "m"),
                                               reg = 1:4,
                                               time = c(2000, 2005, 2010))))
    births <- Counts(array(rpois(n = 90, lambda = 5),
                           dim = c(1, 2, 5, 2),
                           dimnames = list(age = "5-9",
                                           sex = c("m", "f"),
                                           reg = 1:5,
                                           time = c("2001-2005", "2006-2010"))))
    internal <- Counts(array(rpois(n = 300, lambda = 10),
                             dim = c(3, 2, 5, 5, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                             sex = c("m", "f"),
                                             reg_orig = 1:5,
                                             reg_dest = 1:5,
                                             time = c("2001-2005", "2006-2010"))))
    deaths <- Counts(array(rpois(n = 72, lambda = 10),
                           dim = c(3, 2, 4, 3),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                           sex = c("m", "f"),
                                           reg = 4:1,
                                           time = c("2001-2005", "2006-2010", "2011-2015"))))
    immigration <- Counts(array(rpois(n = 72, lambda = 5),
                                dim = c(3, 2, 4, 2),
                                dimnames = list(age = c("0-4", "5-9", "10+"),
                                                sex = c("m", "f"),
                                                reg = 1:4,
                                                time = c("2001-2005", "2006-2010"))))
    emigration <- Counts(array(rpois(n = 72, lambda = 5),
                               dim = c(3, 2, 4, 2),
                               dimnames = list(age = c("0-4", "5-9", "10+"),
                                               sex = c("m", "f"),
                                               reg = 1:4,
                                               time = c("2001-2005", "2006-2010"))))
    reclassification <- Counts(array(c(1, -1),
                                     dim = c(3, 2, 2, 4),
                                     dimnames = list(age = c("0-4", "5-9", "10+"),
                                                     sex = c("m", "f"),
                                                     time = c("2001-2005", "2006-2010"),
                                                     reg = 1:4)))
    x <- Movements(population = population,
                   births = births,
                   internal = internal,
                   entries = list(immigration = immigration),
                   exits = list(deaths = deaths, emigration = emigration),
                   net = list(reclassification = reclassification))
    acc.births <- collapseDimension(x@components[[1]],
                                    dimension = c("age", "triangle"))
    acc.births <- addDimension(acc.births,
                               name = "age",
                               labels = "0",
                               dimscale = "Points")
    acc.internal <- slab(as(x@components[[2]], "Counts"),
                         dimension = "triangle",
                         elements = "Upper")
    acc.internal <- collapseOrigDest(acc.internal)
    acc.immigration <- slab(as(x@components[[3]], "Counts"),
                            dimension = "triangle",
                            elements = "Upper")
    acc.deaths <- slab(as(x@components[[4]], "Counts"),
                       dimension = "triangle",
                       elements = "Upper")
    acc.deaths <- -1L * acc.deaths
    acc.emigration <- slab(as(x@components[[5]], "Counts"),
                           dimension = "triangle",
                           elements = "Upper")
    acc.emigration <- -1L * acc.emigration
    acc.reclassification <- slab(as(x@components[[6]], "Counts"),
                                 dimension = "triangle",
                                 elements = "Upper")
    popn.forward <- slab(x@population,
                         dimension = "time",
                         elements = 1:2)
    popn.forward@metadata@DimScales[[4]] <- new("Intervals", dimvalues = c(2000, 2005, 2010))
    ## births = TRUE
    ans.obtained <- accession(x, openAge = TRUE)
    ans.expected <- (popn.forward + acc.internal + acc.immigration + acc.deaths
        + acc.emigration + acc.reclassification)
    ans.expected@metadata@DimScales[[1]] <- new("Points", dimvalues = c(5, 10, 15))
    dimnames(ans.expected@.Data)[[1]] <- c("5", "10", "15")
    ans.expected <- dbind(ans.expected, acc.births, along = "age")
    expect_identical(ans.obtained, ans.expected)
    ## births = FALSE
    ans.obtained <- accession(x, births = FALSE, openAge = TRUE)
    ans.expected <- (popn.forward + acc.internal + acc.immigration + acc.deaths
        + acc.emigration + acc.reclassification)
    ans.expected@metadata@DimScales[[1]] <- new("Points", dimvalues = c(5, 10, 15))
    dimnames(ans.expected@.Data)[[1]] <- c("5", "10", "15")
    expect_identical(ans.obtained, ans.expected)
})

test_that("accession works with Movements - with age, account does not contain births", {
    population <- Counts(array(rpois(n = 90, lambda = 100),
                               dim = c(3, 2, 4, 3),
                               dimnames = list(age = c("0-4", "5-9", "10+"),
                                               sex = c("f", "m"),
                                               reg = 1:4,
                                               time = c(2000, 2005, 2010))))
    internal <- Counts(array(rpois(n = 300, lambda = 10),
                             dim = c(3, 2, 5, 5, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                             sex = c("m", "f"),
                                             reg_orig = 1:5,
                                             reg_dest = 1:5,
                                             time = c("2001-2005", "2006-2010"))))
    deaths <- Counts(array(rpois(n = 72, lambda = 10),
                           dim = c(3, 2, 4, 3),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                           sex = c("m", "f"),
                                           reg = 4:1,
                                           time = c("2001-2005", "2006-2010", "2011-2015"))))
    immigration <- Counts(array(rpois(n = 72, lambda = 5),
                                dim = c(3, 2, 4, 2),
                                dimnames = list(age = c("0-4", "5-9", "10+"),
                                                sex = c("m", "f"),
                                                reg = 1:4,
                                                time = c("2001-2005", "2006-2010"))))
    emigration <- Counts(array(rpois(n = 72, lambda = 5),
                               dim = c(3, 2, 4, 2),
                               dimnames = list(age = c("0-4", "5-9", "10+"),
                                               sex = c("m", "f"),
                                               reg = 1:4,
                                               time = c("2001-2005", "2006-2010"))))
    reclassification <- Counts(array(c(1, -1),
                                     dim = c(3, 2, 2, 4),
                                     dimnames = list(age = c("0-4", "5-9", "10+"),
                                                     sex = c("m", "f"),
                                                     time = c("2001-2005", "2006-2010"),
                                                     reg = 1:4)))
    x <- Movements(population = population,
                   internal = internal,
                   entries = list(immigration = immigration),
                   exits = list(deaths = deaths, emigration = emigration),
                   net = list(reclassification = reclassification))
    acc.internal <- slab(as(x@components[[1]], "Counts"),
                         dimension = "triangle",
                         elements = "Upper")
    acc.internal <- collapseOrigDest(acc.internal)
    acc.internal <- slab(acc.internal,
                         dimension = "age",
                         elements = 1:2)
    acc.immigration <- slab(as(x@components[[2]], "Counts"),
                            dimension = "triangle",
                            elements = "Upper")
    acc.immigration <- slab(acc.immigration,
                            dimension = "age",
                            elements = 1:2)
    acc.deaths <- slab(as(x@components[[3]], "Counts"),
                       dimension = "triangle",
                       elements = "Upper")
    acc.deaths <- slab(acc.deaths,
                       dimension = "age",
                       elements = 1:2)
    acc.deaths <- -1L * acc.deaths
    acc.emigration <- slab(as(x@components[[4]], "Counts"),
                           dimension = "triangle",
                           elements = "Upper")
    acc.emigration <- slab(acc.emigration,
                           dimension = "age",
                           elements = 1:2)
    acc.emigration <- -1L * acc.emigration
    acc.reclassification <- slab(as(x@components[[5]], "Counts"),
                                 dimension = "triangle",
                                 elements = "Upper")
    acc.reclassification <- slab(acc.reclassification,
                                 dimension = "age",
                                 elements = 1:2)
    popn.forward <- slab(x@population,
                         dimension = "time",
                         elements = 1:2)
    popn.forward <- slab(popn.forward,
                         dimension = "age",
                         elements = 1:2)
    popn.forward@metadata@DimScales[[4]] <- new("Intervals", dimvalues = c(2000, 2005, 2010))
    ## births = TRUE
    ans.obtained <- accession(x)
    ans.expected <- (popn.forward + acc.internal + acc.immigration + acc.deaths
        + acc.emigration + acc.reclassification)
    ans.expected@metadata@DimScales[[1]] <- new("Points", dimvalues = c(5, 10))
    dimnames(ans.expected@.Data)[[1]] <- c("5", "10")
    acc.births <- Counts(array(0L,
                               dim = c(1, 2, 4, 2),
                               dimnames = list(age = "0",
                                               sex = c("f", "m"),
                                               reg = as.character(1:4),
                                               time = c("2001-2005", "2006-2010"))),
                         dimscales = c(age = "Points"))
    ans.expected <- dbind(ans.expected, acc.births, along = "age")
    expect_identical(ans.obtained, ans.expected)
    ## births = FALSE
    ans.obtained <- accession(x, births = FALSE)
    ans.expected <- (popn.forward + acc.internal + acc.immigration + acc.deaths
        + acc.emigration + acc.reclassification)
    ans.expected@metadata@DimScales[[1]] <- new("Points", dimvalues = c(5, 10))
    dimnames(ans.expected@.Data)[[1]] <- c("5", "10")
    expect_identical(ans.obtained, ans.expected)
})

test_that("accession works with Movements - with age", {
    population <- Counts(array(rpois(n = 90, lambda = 100),
                               dim = c(3, 2, 4, 3),
                               dimnames = list(age = c("0-4", "5-9", "10+"),
                                               sex = c("f", "m"),
                                               reg = 1:4,
                                               time = c(2000, 2005, 2010))))
    births <- Counts(array(rpois(n = 90, lambda = 5),
                           dim = c(1, 2, 5, 2),
                           dimnames = list(age = "5-9",
                                           sex = c("m", "f"),
                                           reg = 1:5,
                                           time = c("2001-2005", "2006-2010"))))
    internal <- Counts(array(rpois(n = 300, lambda = 10),
                             dim = c(3, 2, 5, 5, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                             sex = c("m", "f"),
                                             reg_orig = 1:5,
                                             reg_dest = 1:5,
                                             time = c("2001-2005", "2006-2010"))))
    deaths <- Counts(array(rpois(n = 72, lambda = 10),
                           dim = c(3, 2, 4, 3),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                           sex = c("m", "f"),
                                           reg = 4:1,
                                           time = c("2001-2005", "2006-2010", "2011-2015"))))
    immigration <- Counts(array(rpois(n = 72, lambda = 5),
                                dim = c(3, 2, 4, 2),
                                dimnames = list(age = c("0-4", "5-9", "10+"),
                                                sex = c("m", "f"),
                                                reg = 1:4,
                                                time = c("2001-2005", "2006-2010"))))
    emigration <- Counts(array(rpois(n = 72, lambda = 5),
                               dim = c(3, 2, 4, 2),
                               dimnames = list(age = c("0-4", "5-9", "10+"),
                                               sex = c("m", "f"),
                                               reg = 1:4,
                                               time = c("2001-2005", "2006-2010"))))
    reclassification <- Counts(array(c(1, -1),
                                     dim = c(3, 2, 2, 4),
                                     dimnames = list(age = c("0-4", "5-9", "10+"),
                                                     sex = c("m", "f"),
                                                     time = c("2001-2005", "2006-2010"),
                                                     reg = 1:4)))
    x <- Movements(population = population,
                   births = births,
                   internal = internal,
                   entries = list(immigration = immigration),
                   exits = list(deaths = deaths, emigration = emigration),
                   net = list(reclassification = reclassification))
    expect_false(all(isConsistent(x)))
})

test_that("isConsistent works with Movements - no age", {
    population <- CountsOne(values = seq(100, 200, 10),
                            labels = seq(2000, 2100, 10),
                            name = "time")
    births <- CountsOne(values = 15,
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    deaths <- CountsOne(values = 5,
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    x <- Movements(population = population,
                   births = births,
                   exits = list(deaths = deaths))
    expect_true(all(isConsistent(x)))
})

test_that("makeConsistent works with Movements - no age", {
    population <- CountsOne(values = seq(100, 200, 10),
                            labels = seq(2000, 2100, 10),
                            name = "time")
    births <- CountsOne(values = 15,
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    deaths <- CountsOne(values = 10,
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    deaths[1] <- NA
    x <- Movements(population = population,
                   births = births,
                   exits = list(deaths = deaths))
    x <- makeConsistent(x)
    expect_true(all(isConsistent(x)))
})

test_that("makeConsistent works with Movements - no age, births fixed", {
    population <- CountsOne(values = seq(100, 200, 10),
                            labels = seq(2000, 2100, 10),
                            name = "time")
    births <- CountsOne(values = 15,
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    deaths <- CountsOne(values = 10,
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    deaths[1] <- NA
    x0 <- Movements(population = population,
                   births = births,
                   exits = list(deaths = deaths))
    expect_false(all(isConsistent(x0)))
    x1 <- makeConsistent(x0, fixed = "births")
    expect_true(all(isConsistent(x1)))
    expect_identical(x0@components[[1]], x1@components[[1]])
})

test_that("isConsistent works with Movements - with age", {
    set.seed(0)
    population <- Counts(array(rpois(n = 90, lambda = 100),
                               dim = c(3, 2, 4, 3),
                               dimnames = list(age = c("0-4", "5-9", "10+"),
                                               sex = c("f", "m"),
                                               reg = 1:4,
                                               time = c(2000, 2005, 2010))))
    births <- Counts(array(rpois(n = 90, lambda = 5),
                           dim = c(1, 2, 5, 2),
                           dimnames = list(age = "5-9",
                                           sex = c("m", "f"),
                                           reg = 1:5,
                                           time = c("2001-2005", "2006-2010"))))
    internal <- Counts(array(rpois(n = 300, lambda = 10),
                             dim = c(3, 2, 5, 5, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                             sex = c("m", "f"),
                                             reg_orig = 1:5,
                                             reg_dest = 1:5,
                                             time = c("2001-2005", "2006-2010"))))
    deaths <- Counts(array(rpois(n = 72, lambda = 10),
                           dim = c(3, 2, 4, 3),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                           sex = c("m", "f"),
                                           reg = 4:1,
                                           time = c("2001-2005", "2006-2010", "2011-2015"))))
    immigration <- Counts(array(rpois(n = 72, lambda = 5),
                                dim = c(3, 2, 4, 2),
                                dimnames = list(age = c("0-4", "5-9", "10+"),
                                                sex = c("m", "f"),
                                                reg = 1:4,
                                                time = c("2001-2005", "2006-2010"))))
    emigration <- Counts(array(rpois(n = 72, lambda = 5),
                               dim = c(3, 2, 4, 2),
                               dimnames = list(age = c("0-4", "5-9", "10+"),
                                               sex = c("m", "f"),
                                               reg = 1:4,
                                               time = c("2001-2005", "2006-2010"))))
    reclassification <- Counts(array(c(1, -1),
                                     dim = c(3, 2, 2, 4),
                                     dimnames = list(age = c("0-4", "5-9", "10+"),
                                                     sex = c("m", "f"),
                                                     time = c("2001-2005", "2006-2010"),
                                                     reg = 1:4)))
    x0 <- Movements(population = population,
                    births = births,
                    internal = internal,
                    entries = list(immigration = immigration),
                    exits = list(deaths = deaths, emigration = emigration),
                    net = list(reclassification = reclassification))
    expect_false(all(isConsistent(x0)))
    x1 <- makeConsistent(x0)
    expect_true(all(isConsistent(x1)))
    x2 <- x0
    x2@population[1] <- NA
    x3 <- makeConsistent(x2)
    expect_true(all(isConsistent(x3)))
    ## accession in last age group must be positive
    x4 <- x3
    x4@components[[5]]["10+", "f", "1", "2001-2005", "Upper"] <-
        x4@components[[5]]["10+", "f", "1", "2001-2005", "Upper"] + 10000L # emigration
    x4@components[[3]]["10+", "f", "1", "2001-2005", "Lower"] <-
        x4@components[[3]]["10+", "f", "1", "2001-2005", "Lower"] + 10000L # immigration
    expect_false(isConsistent(x4)["10+", "f", "1", "2001-2005"])
    expect_identical(sum(!isConsistent(x4)), 1L)
    ## accession in last age group must be positive
    x5 <- x3
    x5@components[[3]]["10+", "f", "1", "2001-2005", "Upper"] <-
        x5@components[[3]]["10+", "f", "1", "2001-2005", "Upper"] + 10000L # immigration
    x5@components[[5]]["10+", "f", "1", "2001-2005", "Lower"] <-
        x5@components[[5]]["10+", "f", "1", "2001-2005", "Lower"] + 10000L # emigration
    expect_false(isConsistent(x5)["10+", "f", "1", "2001-2005"])
    expect_identical(sum(!isConsistent(x5)), 1L)
    if (test.extended) {
        ## emigration fixed
        x6 <- makeConsistent(x0, fixed = "emig")
        expect_true(all(isConsistent(x6)))
        expect_identical(x0@components[[5]], x6@components[[5]])
    }
})

test_that("makeConsistent throws expected errors", {
    population <- CountsOne(values = seq(100, 200, 10),
                            labels = seq(2000, 2100, 10),
                            name = "time")
    births <- CountsOne(values = 15,
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    deaths <- CountsOne(values = 10,
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    deaths[1] <- NA
    x <- Movements(population = population,
                   births = births,
                   exits = list(deaths = deaths))
    expect_error(makeConsistent(x, fixed = "wrong"),
                 "'fixed' has element \\[\"wrong\"\\] that is not the name of a component")
    expect_error(makeConsistent(x, fixed = c("births", "deaths")),
                 "'fixed' includes every component")
})






