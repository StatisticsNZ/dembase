
context("Movements-generator")

test_that("Movements creates valid object from valid arguments", {
    ## no age, single dimension
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
    expect_true(validObject(x))
    ## with age and internal
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
    expect_true(validObject(x))
    expect_error(Movements(population = population,
                           entries = list(immigration = immigration, immigration = immigration)),
                 "names for 'entries' have duplicates")
    expect_error(Movements(population = population,
                           entries = list(immigration = immigration),
                           exits = list(immigration = immigration)),
                 "names for components have duplicates")
})

    
    
        






