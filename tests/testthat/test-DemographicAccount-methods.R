
context("DemographicAccount-methods")

test_that("components works", {
    population <- CountsOne(values = seq(100, 200, 10),
                            labels = seq(2000, 2100, 10),
                            name = "time")
    births <- CountsOne(values = 15L,
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    deaths <- CountsOne(values = 5L,
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    x <- Movements(population = population,
                   births = births,
                   exits = list(deaths = deaths))
    expect_identical(components(x),
                     list(births = births, deaths = deaths))
    expect_identical(components(x, names = "deaths"),
                     deaths)
    expect_identical(components(x, names = "deaths", simplify = FALSE),
                     list(deaths = deaths))
    expect_error(components(x, names = character()),
                 "'names' has length 0")
    expect_error(components(x, names = c("deaths", NA)),
                 "'names' has missing values")
    expect_error(components(x, names = c("deaths", "wrong")),
                 "account does not contain component called \"wrong\"")
})

test_that("componentNames works", {
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
    expect_identical(componentNames(x),
                     c("births", "deaths"))
    componentNames(x) <- c("B", "D")
    expect_identical(componentNames(x),
                     c("B", "D"))
    componentNames(x)[1] <- "births"
    expect_identical(componentNames(x),
                     c("births", "D"))
    expect_error(componentNames(x) <- "wrong",
                 "length of replacement value \\[1\\] does not equal number of components \\[2\\]")
    expect_error(componentNames(x) <- c("births", "births"),
                 "'namesComponents' has duplicates")
})

test_that("setComponentNames works", {
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
    x1 <- setComponentNames(x, value = c("Births", "Deaths"))
    expect_identical(componentNames(x1),
                     c("Births", "Deaths"))
})

test_that("population works", {
    population <- CountsOne(values = seq(100L, 200L, 10L),
                            labels = seq(2000, 2100, 10),
                            name = "time")
    births <- CountsOne(values = 15L,
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    deaths <- CountsOne(values = 5L,
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    x <- Movements(population = population,
                   births = births,
                   exits = list(deaths = deaths))
    expect_identical(population(x),
                     population)
})





