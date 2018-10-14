

context("Component-methods")

## aperm ################################################################

test_that("aperm works with Net", {
    component <- Counts(array(1:96,
                              dim = c(2, 3, 2, 2, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                  age = c("0-4", "5-9", "10+"),
                                  time = c("2001-2005", "2006-2010"),
                                  reg_orig = c("a", "b"),
                                  reg_dest = c("a", "b"))))
    net <- collapseOrigDest(component, to = "net")
    ans.obtained <- aperm(net, perm = c("reg", "age", "triangle", "time"))
    expect_is(ans.obtained, "Net")
    expect_identical(names(ans.obtained), c("reg", "age", "triangle", "time"))
})


test_that("aperm works with Pool", {
    component <- Counts(array(1:96,
                              dim = c(2, 3, 2, 2, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                  age = c("0-4", "5-9", "10+"),
                                  time = c("2001-2005", "2006-2010"),
                                  reg_orig = c("a", "b"),
                                  reg_dest = c("a", "b"))))
    pool <- collapseOrigDest(component, to = "pool")
    ans.obtained <- aperm(pool, perm = c("reg", "age", "triangle", "direction", "time"))
    expect_is(ans.obtained, "Pool")
    expect_identical(names(ans.obtained), c("reg", "age", "triangle", "direction", "time"))
})



## incrementLowerTri ################################################################

test_that("default method of incrementLowerTri works", {
    Population <- dembase:::Population
    incrementLowerTri <- dembase:::incrementLowerTri
    component <- Counts(array(1:12,
                              dim = c(2, 3, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                  age = c("0-4", "5-9", "10+"),
                                  time = c("2001-2005", "2006-2010"))))
    component <- new("EntriesMovements",
                     .Data = component@.Data,
                     metadata = component@metadata)
    population <- Counts(array(1:9,
                               dim = c(3, 3),
                               dimnames = list(time = c(2000, 2005, 2010),
                                   age = c("0-4", "5-9", "10+"))))
    population <- Population(population)
    ans.obtained <- incrementLowerTri(component = component,
                                       population = population)
    ans.expected <- Counts(array(c(1L, 3L, 5L, 7L, 9L, 11L),
                                 dim = c(3, 2),
                                 dimnames = list(age = c("0-4", "5-9", "10+"),
                                     time = c("2005", "2010"))))
    ans.expected <- t(ans.expected)
    expect_identical(ans.obtained, ans.expected)
})

test_that("BirthsMovements method of incrementLowerTri works", {
    incrementLowerTri <- dembase:::incrementLowerTri
    BirthsMovements <- dembase:::BirthsMovements
    Population <- dembase:::Population
    component <- Counts(array(1:12,
                              dim = c(2, 3, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                              age = c("10-14", "15-19", "20-24"),
                                              time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(1:20,
                             dim = c(2, 5, 2),
                             dimnames = list(triangle = c("Lower", "Upper"),
                                             age = c("0-4", "5-9", "10-14", "15-19", "20-24"),
                                             time = c("2001-2005", "2006-2010"))))
    component <- BirthsMovements(component,
                                 template = template)
    population <- Counts(array(1:18,
                               dim = c(3, 6),
                               dimnames = list(time = c(2000, 2005, 2010),
                                               age = c("0-4", "5-9", "10-14",
                                                       "15-19", "20-24", "25+"))))
    population <- Population(population)
    ans.obtained <- incrementLowerTri(component = component,
                                      population = population)
    ans.expected <- 0L
    expect_identical(ans.obtained, ans.expected)
})

test_that("Pool method of incrementLowerTri works", {
    incrementLowerTri <- dembase:::incrementLowerTri
    Population <- dembase:::Population
    InternalMovements <- dembase:::InternalMovements
    component <- Counts(array(1:96,
                              dim = c(2, 3, 2, 2, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                  age = c("0-4", "5-9", "10+"),
                                  time = c("2001-2005", "2006-2010"),
                                  reg_orig = c("a", "b"),
                                  reg_dest = c("a", "b"))))
    net <- collapseOrigDest(component, to = "net")
    component <- collapseOrigDest(component, base = "reg", to = "pool")
    template <- Counts(array(0L,
                             dim = c(2, 3, 2, 2),
                             dimnames = list(triangle = c("Lower", "Upper"),
                                  age = c("0-4", "5-9", "10+"),
                                  time = c("2001-2005", "2006-2010"),
                                  reg = c("a", "b"))))
    component <- InternalMovements(internal = component,
                                   template = template)
    population <- Counts(array(1L,
                               dim = c(3, 2, 3),
                               dimnames = list(time = c(2000, 2005, 2010),
                                   reg = c("a", "b"),
                                   age = c("0-4", "5-9", "10+"))))
    population <- Population(population)
    ans.obtained <- incrementLowerTri(component = component,
                                      population = population)
    net <- slab(net, dimension = "triangle", elements = "Lower")
    net@metadata@DimScales[[2]] <- new("Points", dimvalues = c(2005, 2010))
    dimnames(net@.Data)[[2]] <- c("2005", "2010")
    ans.expected <- aperm(net, perm = names(population))
    expect_identical(ans.obtained, ans.expected)
})

test_that("Orig-Dest method of incrementLowerTri works", {
    incrementLowerTri <- dembase:::incrementLowerTri
    Population <- dembase:::Population
    InternalMovements <- dembase:::InternalMovements
    component <- Counts(array(1:96,
                              dim = c(2, 3, 2, 2, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                  age = c("0-4", "5-9", "10+"),
                                  time = c("2001-2005", "2006-2010"),
                                  reg_orig = c("a", "b"),
                                  reg_dest = c("a", "b"))))
    net <- collapseOrigDest(component, to = "net")
    template <- Counts(array(0L,
                             dim = c(2, 3, 2, 2),
                             dimnames = list(triangle = c("Lower", "Upper"),
                                  age = c("0-4", "5-9", "10+"),
                                  time = c("2001-2005", "2006-2010"),
                                  reg = c("a", "b"))))
    component <- InternalMovements(internal = component,
                                   template = template)
    population <- Counts(array(1L,
                               dim = c(3, 2, 3),
                               dimnames = list(time = c(2000, 2005, 2010),
                                   reg = c("a", "b"),
                                   age = c("0-4", "5-9", "10+"))))
    population <- Population(population)
    ans.obtained <- incrementLowerTri(component = component,
                                      population = population)
    net <- slab(net, dimension = "triangle", elements = "Lower")
    net@metadata@DimScales[[2]] <- new("Points", dimvalues = c(2005, 2010))
    dimnames(net@.Data)[[2]] <- c("2005", "2010")
    ans.expected <- aperm(net, perm = names(population))
    expect_identical(ans.obtained, ans.expected)
})

test_that("ExitsMovements method of incrementLowerTri works", {
    incrementLowerTri <- dembase:::incrementLowerTri
    Population <- dembase:::Population
    ExitsMovements <- dembase:::ExitsMovements
    component <- Counts(array(1:12,
                              dim = c(2, 3, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                  age = c("0-4", "5-9", "10+"),
                                  time = c("2001-2005", "2006-2010"))))
    component <- ExitsMovements(component,
                                template = component,
                                name = "emigration")
    population <- Counts(array(1:9,
                               dim = c(3, 3),
                               dimnames = list(time = c(2000, 2005, 2010),
                                   age = c("0-4", "5-9", "10+"))))
    population <- Population(population)
    ans.obtained <- incrementLowerTri(component = component,
                                       population = population)
    ans.expected <- Counts(array(-1L * c(1L, 3L, 5L, 7L, 9L, 11L),
                                 dim = c(3, 2),
                                 dimnames = list(age = c("0-4", "5-9", "10+"),
                                     time = c("2005", "2010"))))
    ans.expected <- t(ans.expected)
    expect_identical(ans.obtained, ans.expected)
})


## incrementInteger ################################################################

test_that("default method of incrementInteger works", {
    EntriesMovements <- dembase:::EntriesMovements
    incrementInteger <- dembase:::incrementInteger
    component <- Counts(array(1:12,
                              dim = c(2, 3, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                  age = c("0-4", "5-9", "10+"),
                                  time = c("2001-2005", "2006-2010"))))
    component <- EntriesMovements(entries = component,
                                  template = component)
    ans.obtained <- incrementInteger(component)
    ans.expected <- as.integer(component@.Data)
    expect_identical(ans.obtained, ans.expected)
})

test_that("BirthsMovements method of incrementInteger works", {
    incrementInteger <- dembase:::incrementInteger
    BirthsMovements <- dembase:::BirthsMovements
    component <- Counts(array(1:16,
                              dim = c(2, 2, 2, 2, 3, 2),
                              dimnames = list(sex = c("m", "f"),
                                              time = c("2001-2005", "2006-2010"),
                                              eth_parent = c("a", "b"),
                                              eth_child = c("a", "b"),
                                              age = c("10-14", "15-19", "20-24"),
                                              triangle = c("Lower", "Upper"))))
    template <- Counts(array(1:6,
                             dim = c(2, 2, 2, 6, 2),
                             dimnames = list(sex = c("m", "f"),
                                             time = c("2001-2005", "2006-2010"),
                                             eth = c("a", "b"),
                                             age = c("0-4", "5-9", "10-14", "15-19", "20-24", "25+"),
                                             triangle = c("Lower", "Upper"))))
    component <- BirthsMovements(component,
                                 template = template)
    ans.obtained <- incrementInteger(component)
    ans.expected <- collapseDimension(component,
                                      dimension = c("eth_parent", "age", "triangle"))
    ans.expected <- as.integer(ans.expected@.Data)
    expect_identical(ans.obtained, ans.expected)    
})

test_that("Orig-Dest method of incrementInteger works", {
    incrementInteger <- dembase:::incrementInteger
    InternalMovements <- dembase:::InternalMovements
    component <- Counts(array(1:96,
                              dim = c(2, 3, 2, 2, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                              age = c("0-4", "5-9", "10+"),
                                              time = c("2001-2005", "2006-2010"),
                                              reg_orig = c("a", "b"),
                                              reg_dest = c("a", "b"))))
    net <- collapseOrigDest(component, to = "net")
    template <- Counts(array(0L,
                             dim = c(2, 3, 2, 2),
                             dimnames = list(triangle = c("Lower", "Upper"),
                                             age = c("0-4", "5-9", "10+"),
                                             time = c("2001-2005", "2006-2010"),
                                             reg = c("a", "b"))))
    component <- InternalMovements(internal = component,
                                   template = template)
    ans.obtained <- incrementInteger(component)
    ans.expected <- as.integer(net@.Data)
    expect_identical(ans.obtained, ans.expected)
})

test_that("Pool method of incrementInteger works", {
    incrementInteger <- dembase:::incrementInteger
    InternalMovements <- dembase:::InternalMovements
    component <- Counts(array(1:96,
                              dim = c(2, 3, 2, 2, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                              age = c("0-4", "5-9", "10+"),
                                              time = c("2001-2005", "2006-2010"),
                                              reg_orig = c("a", "b"),
                                              reg_dest = c("a", "b"))))
    net <- collapseOrigDest(component, to = "net")
    component <- collapseOrigDest(component, to = "pool")
    template <- Counts(array(0L,
                             dim = c(2, 3, 2, 2),
                             dimnames = list(triangle = c("Lower", "Upper"),
                                             age = c("0-4", "5-9", "10+"),
                                             time = c("2001-2005", "2006-2010"),
                                             reg = c("a", "b"))))
    component <- InternalMovements(internal = component,
                                   template = template)
    ans.obtained <- incrementInteger(component)
    ans.expected <- as.integer(net@.Data)
    expect_identical(ans.obtained, ans.expected)
})



## incrementOpen ################################################################

test_that("default method of incrementOpen works", {
    Population <- dembase:::Population
    incrementOpen <- dembase:::incrementOpen
    component <- Counts(array(1:12,
                              dim = c(2, 3, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                  age = c("0-4", "5-9", "10+"),
                                  time = c("2001-2005", "2006-2010"))))
    component <- new("EntriesMovements",
                     .Data = component@.Data,
                     metadata = component@metadata)
    population <- Counts(array(1:9,
                               dim = c(3, 3),
                               dimnames = list(time = c(2000, 2005, 2010),
                                   age = c("0-4", "5-9", "10+"))))
    population <- Population(population)
    ans.obtained <- incrementOpen(component = component,
                                       population = population)
    ans.expected <- Counts(array(c(0L, 0L, 6L, 0L, 0L, 12L),
                                 dim = c(3, 2),
                                 dimnames = list(age = c("0-4", "5-9", "10+"),
                                     time = c("2005", "2010"))))
    ans.expected <- t(ans.expected)
    expect_identical(ans.obtained, ans.expected)
})


test_that("BirthsMovements method of incrementOpen works", {
    incrementOpen <- dembase:::incrementOpen
    BirthsMovements <- dembase:::BirthsMovements
    Population <- dembase:::Population
    component <- Counts(array(1:12,
                              dim = c(2, 3, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                              age = c("10-14", "15-19", "20-24"),
                                              time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(1:20,
                             dim = c(2, 5, 2),
                             dimnames = list(triangle = c("Lower", "Upper"),
                                             age = c("0-4", "5-9", "10-14", "15-19", "20-24"),
                                             time = c("2001-2005", "2006-2010"))))
    component <- BirthsMovements(component,
                                 template = template)
    population <- Counts(array(1:18,
                               dim = c(3, 6),
                               dimnames = list(time = c(2000, 2005, 2010),
                                               age = c("0-4", "5-9", "10-14",
                                                       "15-19", "20-24", "25+"))))
    population <- Population(population)
    ans.obtained <- incrementOpen(component = component,
                                  population = population)
    ans.expected <- 0L
    expect_identical(ans.obtained, ans.expected)
})

test_that("Pool method of incrementOpen works", {
    incrementOpen <- dembase:::incrementOpen
    InternalMovements <- dembase:::InternalMovements
    Population <- dembase:::Population
    component <- Counts(array(1:96,
                              dim = c(2, 3, 2, 2, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                              age = c("0-4", "5-9", "10+"),
                                              time = c("2001-2005", "2006-2010"),
                                              reg_orig = c("a", "b"),
                                              reg_dest = c("a", "b"))))
    net <- collapseOrigDest(component, to = "net")
    component <- collapseOrigDest(component, base = "reg", to = "pool")
    template <- Counts(array(0L,
                             dim = c(2, 3, 2, 2),
                             dimnames = list(triangle = c("Lower", "Upper"),
                                             age = c("0-4", "5-9", "10+"),
                                             time = c("2001-2005", "2006-2010"),
                                             reg = c("a", "b"))))
    component <- InternalMovements(internal = component,
                                   template = template)
    population <- Counts(array(1L,
                               dim = c(3, 2, 3),
                               dimnames = list(time = c(2000, 2005, 2010),
                                               reg = c("a", "b"),
                                               age = c("0-4", "5-9", "10+"))))
    population <- Population(population)
    ans.obtained <- incrementOpen(component = component,
                                  population = population)
    net <- slab(net, dimension = "triangle", elements = "Upper")
    net@metadata@DimScales[[2]] <- new("Points", dimvalues = c(2005, 2010))
    dimnames(net@.Data)[[2]] <- c("2005", "2010")
    net[1:2,,] <- 0L
    ans.expected <- aperm(net, perm = names(population))
    expect_identical(ans.obtained, ans.expected)
})

test_that("Orig-Dest method of incrementOpen works", {
    incrementOpen <- dembase:::incrementOpen
    InternalMovements <- dembase:::InternalMovements
    Population <- dembase:::Population
    component <- Counts(array(1:96,
                              dim = c(2, 3, 2, 2, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                  age = c("0-4", "5-9", "10+"),
                                  time = c("2001-2005", "2006-2010"),
                                  reg_orig = c("a", "b"),
                                  reg_dest = c("a", "b"))))
    net <- collapseOrigDest(component, to = "net")
    template <- Counts(array(0L,
                             dim = c(2, 3, 2, 2),
                             dimnames = list(triangle = c("Lower", "Upper"),
                                  age = c("0-4", "5-9", "10+"),
                                  time = c("2001-2005", "2006-2010"),
                                  reg = c("a", "b"))))
    component <- InternalMovements(internal = component,
                                   template = template)
    population <- Counts(array(1L,
                               dim = c(3, 2, 3),
                               dimnames = list(time = c(2000, 2005, 2010),
                                   reg = c("a", "b"),
                                   age = c("0-4", "5-9", "10+"))))
    population <- Population(population)
    ans.obtained <- incrementOpen(component = component,
                                      population = population)
    net <- slab(net, dimension = "triangle", elements = "Lower")
    net@metadata@DimScales[[2]] <- new("Points", dimvalues = c(2005, 2010))
    dimnames(net@.Data)[[2]] <- c("2005", "2010")
    net[1:2,,] <- 0L
    ans.expected <- aperm(net, perm = names(population))
    expect_identical(ans.obtained, ans.expected)
})

test_that("ExitsMovements method of incrementOpen works", {
    incrementOpen <- dembase:::incrementOpen
    ExitsMovements <- dembase:::ExitsMovements
    Population <- dembase:::Population
    component <- Counts(array(1:12,
                              dim = c(2, 3, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                  age = c("0-4", "5-9", "10+"),
                                  time = c("2001-2005", "2006-2010"))))
    component <- ExitsMovements(component,
                                template = component,
                                name = "emigration")
    population <- Counts(array(1:9,
                               dim = c(3, 3),
                               dimnames = list(time = c(2000, 2005, 2010),
                                   age = c("0-4", "5-9", "10+"))))
    population <- Population(population)
    ans.obtained <- incrementOpen(component = component,
                                       population = population)
    ans.expected <- Counts(array(-1L * c(0L, 0L, 6L, 0L, 0L, 12L),
                                 dim = c(3, 2),
                                 dimnames = list(age = c("0-4", "5-9", "10+"),
                                     time = c("2005", "2010"))))
    ans.expected <- t(ans.expected)
    expect_identical(ans.obtained, ans.expected)
})



## incrementSquare ################################################################

test_that("default method of incrementSquare works", {
    incrementSquare <- dembase:::incrementSquare
    EntriesMovements <- dembase:::EntriesMovements
    Population <- dembase:::Population
    component <- Counts(array(1:2,
                              dim = 2,
                              dimnames = list(time = c("2001-2005", "2006-2010"))))
    component <- EntriesMovements(component,
                                  template = component,
                                  name = "entries")
    population <- Counts(array(3,
                               dim = 3,
                               dimnames = list(time = c(2000, 2005, 2010))))
    population <- Population(population)
    ans.obtained <- incrementSquare(component = component,
                                    population = population)
    ans.expected <- Counts(array(1:2,
                                 dim = 2,
                                 dimnames = list(time = c("2005", "2010"))))
    expect_identical(ans.obtained, ans.expected)
})

test_that("BirthsMovements method of incrementSquare works", {
    incrementSquare <- dembase:::incrementSquare
    BirthsMovements <- dembase:::BirthsMovements
    Population <- dembase:::Population
    ## has parent
    component <- Counts(array(1:16,
                              dim = c(2, 2, 2, 2),
                              dimnames = list(sex = c("m", "f"),
                                              time = c("2001-2005", "2006-2010"),
                                              eth_parent = c("a", "b"),
                                              eth_child = c("a", "b"))))
    template <- Counts(array(1:6,
                             dim = c(2, 2, 2),
                             dimnames = list(sex = c("m", "f"),
                                             time = c("2001-2005", "2006-2010"),
                                             eth = c("a", "b"))))
    component <- BirthsMovements(component,
                                 template = template)
    population <- Counts(array(1L,
                               dim = c(2, 3, 2),
                               dimnames = list(sex = c("m", "f"),
                                               time = c(2000, 2005, 2010),
                                               eth = c("a", "b"))))
    population <- Population(population)
    ans.obtained <- incrementSquare(component = component,
                                    population = population)
    ans.expected <- collapseDimension(component, dimension = "eth_parent")
    ans.expected <- aperm(ans.expected, perm = names(population))
    ans.expected@metadata@DimScales[[2]] <- new("Points", dimvalues = c(2005, 2010))
    dimnames(ans.expected@.Data)[[2]] <- c("2005", "2010")
    expect_identical(ans.obtained, ans.expected)    
})

test_that("Pool method of incrementSquare works", {
    incrementSquare <- dembase:::incrementSquare
    InternalMovements <- dembase:::InternalMovements
    Population <- dembase:::Population
    component <- Counts(array(1:96,
                              dim = c(2, 2, 2),
                              dimnames = list(time = c("2001-2005", "2006-2010"),
                                  reg_orig = c("a", "b"),
                                  reg_dest = c("a", "b"))))
    net <- collapseOrigDest(component, to = "net")
    component <- collapseOrigDest(component, base = "reg", to = "pool")
    template <- Counts(array(0L,
                             dim = c(2, 2),
                             dimnames = list(time = c("2001-2005", "2006-2010"),
                                             reg = c("a", "b"))))
    component <- InternalMovements(internal = component,
                                   template = template)
    population <- Counts(array(1L,
                               dim = c(3, 2),
                               dimnames = list(time = c(2000, 2005, 2010),
                                   reg = c("a", "b"))))
    population <- Population(population)
    ans.obtained <- incrementSquare(component = component,
                                    population = population)
    ans.expected <- net
    ans.expected@metadata@DimScales[[1]] <- new("Points", dimvalues = c(2005, 2010))
    dimnames(ans.expected@.Data)[[1]] <- c("2005", "2010")
    ans.expected <- Counts(ans.expected)
    expect_identical(ans.obtained, ans.expected)
})

test_that("Orig-Dest method of incrementSquare works", {
    incrementSquare <- dembase:::incrementSquare
    InternalMovements <- dembase:::InternalMovements
    Population <- dembase:::Population
    component <- Counts(array(1:96,
                              dim = c(2, 2, 2),
                              dimnames = list(time = c("2001-2005", "2006-2010"),
                                  reg_orig = c("a", "b"),
                                  reg_dest = c("a", "b"))))
    net <- collapseOrigDest(component, to = "net")
    template <- Counts(array(0L,
                             dim = c(2, 2),
                             dimnames = list(time = c("2001-2005", "2006-2010"),
                                             reg = c("a", "b"))))
    component <- InternalMovements(internal = component,
                                   template = template)
    population <- Counts(array(1L,
                               dim = c(3, 2),
                               dimnames = list(time = c(2000, 2005, 2010),
                                   reg = c("a", "b"))))
    population <- Population(population)
    ans.obtained <- incrementSquare(component = component,
                                    population = population)
    ans.expected <- net
    ans.expected@metadata@DimScales[[1]] <- new("Points", dimvalues = c(2005, 2010))
    dimnames(ans.expected@.Data)[[1]] <- c("2005", "2010")
    ans.expected <- Counts(ans.expected)
    expect_identical(ans.obtained, ans.expected)
})

test_that("ExitsMovements method of incrementSquare works", {
    incrementSquare <- dembase:::incrementSquare
    ExitsMovements <- dembase:::ExitsMovements
    Population <- dembase:::Population
    component <- Counts(array(1:2,
                              dim = 2,
                              dimnames = list(time = c("2001-2005", "2006-2010"))))
    component <- ExitsMovements(component,
                                template = component,
                                name = "entries")
    population <- Counts(array(3,
                               dim = 3,
                               dimnames = list(time = c(2000, 2005, 2010))))
    population <- Population(population)
    ans.obtained <- incrementSquare(component = component,
                                    population = population)
    ans.expected <- Counts(-1L * array(1:2,
                                      dim = 2,
                                      dimnames = list(time = c("2005", "2010"))))
    expect_identical(ans.obtained, ans.expected)
})


## incrementUpperTri ################################################################

test_that("default method of incrementUpperTri works", {
    incrementUpperTri <- dembase:::incrementUpperTri
    Population <- dembase:::Population
    component <- Counts(array(1:12,
                              dim = c(2, 3, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                  age = c("0-4", "5-9", "10+"),
                                  time = c("2001-2005", "2006-2010"))))
    component <- new("EntriesMovements",
                     .Data = component@.Data,
                     metadata = component@metadata)
    population <- Counts(array(1:9,
                               dim = c(3, 3),
                               dimnames = list(time = c(2000, 2005, 2010),
                                   age = c("0-4", "5-9", "10+"))))
    population <- Population(population)
    ans.obtained <- incrementUpperTri(component = component,
                                       population = population)
    ans.expected <- Counts(array(c(2L, 4L, 8L, 10L),
                                 dim = c(2, 2),
                                 dimnames = list(age = c("5", "10"),
                                     time = c("2001-2005", "2006-2010"))))
    ans.expected <- t(ans.expected)
    expect_identical(ans.obtained, ans.expected)
})

test_that("BirthsMovements method of incrementUpperTri works", {
    incrementUpperTri <- dembase:::incrementUpperTri
    BirthsMovements <- dembase:::BirthsMovements
    Population <- dembase:::Population
    ## no parent
    component <- Counts(array(1:12,
                              dim = c(2, 3, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                              age = c("10-14", "15-19", "20-24"),
                                              time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(1:20,
                             dim = c(2, 5, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                              age = c("0-4", "5-9", "10-14", "15-19", "20-24"),
                                              time = c("2001-2005", "2006-2010"))))
    component <- BirthsMovements(component,
                                 template = template)
    population <- Counts(array(1:18,
                               dim = c(3, 6),
                               dimnames = list(time = c(2000, 2005, 2010),
                                               age = c("0-4", "5-9", "10-14",
                                                       "15-19", "20-24", "25+"))))
    population <- Population(population)
    ans.obtained <- incrementUpperTri(component = component,
                                      population = population)
    ans.expected <- collapseDimension(component, margin = "time")
    ans.expected <- addDimension(ans.expected, name = "age", labels = "0", dimscale = "Points")
    expect_identical(ans.obtained, ans.expected)
    ## has parent
    component <- Counts(array(1:16,
                              dim = c(2, 2, 2, 2, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                  age = c("10-14", "15-19"),
                                  time = c("2001-2005", "2006-2010"),
                                  eth_parent = c("a", "b"),
                                  eth_child = c("a", "b"))))
    template <- Counts(array(1:40,
                             dim = c(2, 5, 2, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                              age = c("0-4", "5-9", "10-14", "15-19", "20-24"),
                                              time = c("2001-2005", "2006-2010"),
                                              eth = c("a", "b"))))
    component <- BirthsMovements(component,
                                 template = template)
    population <- Counts(array(1:36,
                               dim = c(3, 6, 2),
                               dimnames = list(time = c(2000, 2005, 2010),
                                               age = c("0-4", "5-9", "10-14",
                                                       "15-19", "20-24", "25+"),
                                               eth = c("a", "b"))))
    population <- Population(population)
    ans.obtained <- incrementUpperTri(component = component,
                                      population = population)
    ans.expected <- collapseDimension(component, dimension = c("age", "triangle", "eth_parent"))
    ans.expected <- addDimension(ans.expected, name = "age", labels = "0", dimscale = "Points")
    ans.expected <- aperm(ans.expected, perm = names(population))
    expect_identical(ans.obtained, ans.expected)    
})

test_that("Pool method of incrementUpperTri works", {
    incrementUpperTri <- dembase:::incrementUpperTri
    Population <- dembase:::Population
    InternalMovements <- dembase:::InternalMovements
    component <- Counts(array(1:96,
                              dim = c(2, 3, 2, 2, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                  age = c("0-4", "5-9", "10+"),
                                  time = c("2001-2005", "2006-2010"),
                                  reg_orig = c("a", "b"),
                                  reg_dest = c("a", "b"))))
    net <- collapseOrigDest(component, to = "net")
    component <- collapseOrigDest(component, base = "reg", to = "pool")
    template <- Counts(array(0L,
                             dim = c(2, 3, 2, 2),
                             dimnames = list(triangle = c("Lower", "Upper"),
                                  age = c("0-4", "5-9", "10+"),
                                  time = c("2001-2005", "2006-2010"),
                                  reg = c("a", "b"))))
    component <- InternalMovements(internal = component,
                                   template = template)
    population <- Counts(array(1L,
                               dim = c(3, 2, 3),
                               dimnames = list(time = c(2000, 2005, 2010),
                                   reg = c("a", "b"),
                                   age = c("0-4", "5-9", "10+"))))
    population <- Population(population)
    ans.obtained <- incrementUpperTri(component = component,
                                      population = population)
    net <- slab(net, dimension = "triangle", elements = "Upper")
    net <- net[1:2,,]
    net@metadata@DimScales[[1]] <- new("Points", dimvalues = c(5, 10))
    dimnames(net@.Data)[[1]] <- c("5", "10")
    ans.expected <- aperm(net, perm = names(population))
    expect_identical(ans.obtained, ans.expected)
})

test_that("Orig-Dest method of incrementUpperTri works", {
    incrementUpperTri <- dembase:::incrementUpperTri
    Population <- dembase:::Population
    InternalMovements <- dembase:::InternalMovements
    component <- Counts(array(1:96,
                              dim = c(2, 3, 2, 2, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                  age = c("0-4", "5-9", "10+"),
                                  time = c("2001-2005", "2006-2010"),
                                  reg_orig = c("a", "b"),
                                  reg_dest = c("a", "b"))))
    net <- collapseOrigDest(component, to = "net")
    template <- Counts(array(0L,
                             dim = c(2, 3, 2, 2),
                             dimnames = list(triangle = c("Lower", "Upper"),
                                  age = c("0-4", "5-9", "10+"),
                                  time = c("2001-2005", "2006-2010"),
                                  reg = c("a", "b"))))
    component <- InternalMovements(internal = component,
                                   template = template)
    population <- Counts(array(1L,
                               dim = c(3, 2, 3),
                               dimnames = list(time = c(2000, 2005, 2010),
                                   reg = c("a", "b"),
                                   age = c("0-4", "5-9", "10+"))))
    population <- Population(population)
    ans.obtained <- incrementUpperTri(component = component,
                                      population = population)
    net <- slab(net, dimension = "triangle", elements = "Upper")
    net <- net[1:2,,]
    net@metadata@DimScales[[1]] <- new("Points", dimvalues = c(5, 10))
    dimnames(net@.Data)[[1]] <- c("5", "10")
    ans.expected <- aperm(net, perm = names(population))
    expect_identical(ans.obtained, ans.expected)
})

test_that("ExitsMovements method of incrementUpperTri works", {
    incrementUpperTri <- dembase:::incrementUpperTri
    Population <- dembase:::Population
    ExitsMovements <- dembase:::ExitsMovements
    component <- Counts(array(1:12,
                              dim = c(2, 3, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                  age = c("0-4", "5-9", "10+"),
                                  time = c("2001-2005", "2006-2010"))))
    component <- ExitsMovements(component,
                                template = component,
                                name = "emigration")
    population <- Counts(array(1:9,
                               dim = c(3, 3),
                               dimnames = list(time = c(2000, 2005, 2010),
                                   age = c("0-4", "5-9", "10+"))))
    population <- Population(population)
    ans.obtained <- incrementUpperTri(component = component,
                                       population = population)
    ans.expected <- Counts(array(-1L * c(2L, 4L, 8L, 10L),
                                 dim = c(2, 2),
                                 dimnames = list(age = c("5", "10"),
                                     time = c("2001-2005", "2006-2010"))))
    ans.expected <- t(ans.expected)
    expect_identical(ans.obtained, ans.expected)
})
    

## isCompatibleWithPopn ################################################################

test_that("MovementsComponent method of isCompatibleWithPopn works", {
    EntriesMovements <- dembase:::EntriesMovements
    isCompatibleWithPopn <- dembase:::isCompatibleWithPopn
    component <- Counts(array(1:12,
                              dim = c(2, 3, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                  age = c("0-4", "5-9", "10+"),
                                  time = c("2001-2005", "2006-2010"))))
    component <- EntriesMovements(entries = component,
                                  template = component)
    expect_true(isCompatibleWithPopn(component,
                                     metadata = component@metadata,
                                     name = "immigration"))
    expect_identical(isCompatibleWithPopn(component,
                                          metadata = rev(component@metadata),
                                          name = "immigration"),
                     "'immigration' not compatible with 'population'")
})

test_that("TransitionsComponent method of isCompatibleWithPopn works", {
    EntriesTransitions <- dembase:::EntriesTransitions
    isCompatibleWithPopn <- dembase:::isCompatibleWithPopn
    component <- Counts(array(1:24,
                              dim = c(3, 2, 2, 2),
                              dimnames = list(age = c("0-4", "5-9", "10+"),
                                              reg_orig = c("a", "b"),
                                              reg_dest = c("a", "b"),
                                              time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(1:12,
                              dim = c(3, 2, 2),
                              dimnames = list(age = c("0-4", "5-9", "10+"),
                                              reg = c("a", "b"),
                                              time = c("2001-2005", "2006-2010"))))
    component <- EntriesTransitions(entries = component,
                                    template = template,
                                    name = "entries")
    expect_true(isCompatibleWithPopn(component,
                                     metadata = template@metadata,
                                     name = "immigration"))
    expect_identical(isCompatibleWithPopn(component,
                                          metadata = rev(template@metadata),
                                          name = "immigration"),
                     "'immigration' not compatible with 'population'")
})

test_that("isCompatibleWithPopn works with BirthsMovements", {
    isCompatibleWithPopn <- dembase:::isCompatibleWithPopn
    BirthsMovements <- dembase:::BirthsMovements
    ## both have age
    component <- Counts(array(1:12,
                              dim = c(2, 3, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                              age = c("10-14", "15-19", "20-24"),
                                              time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(1:20,
                             dim = c(2, 5, 2),
                             dimnames = list(triangle = c("Lower", "Upper"),
                                             age = c("0-4", "5-9", "10-14", "15-19", "20-24"),
                                             time = c("2001-2005", "2006-2010"))))
    component <- BirthsMovements(component,
                                 template = template)
    template <- Counts(array(1:24,
                             dim = c(2, 6, 2),
                             dimnames = list(time = c("2001-2005", "2006-2010"),
                                             age = c("0-4", "5-9", "10-14",
                                                     "15-19", "20-24", "25+"),
                                             triangle = c("Lower", "Upper"))))
    metadata <- template@metadata
    ans.obtained <- isCompatibleWithPopn(component = component,
                                         metadata = metadata,
                                         name = "births")
    ans.expected <- TRUE
    expect_identical(ans.obtained, ans.expected)
    template.wrong <- Counts(array(1:36,
                                   dim = c(3, 6, 2),
                                   dimnames = list(time = c("2001-2005", "2006-2010", "2011-2015"),
                                                   age = c("0-4", "5-9", "10-14",
                                                           "15-19", "20-24", "25+"),
                                                   triangle = c("Lower", "Upper"))))
    metadata.wrong <- template.wrong@metadata
    ans.obtained <- isCompatibleWithPopn(component = component,
                                         metadata = metadata.wrong,
                                         name = "births")
    ans.expected <- "'births' not compatible with 'population'"
    expect_identical(ans.obtained, ans.expected)
    ## neither has age
    component <- Counts(array(1:4,
                              dim = c(2, 2),
                              dimnames = list(sex = c("f", "m"),
                                              time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(1:4,
                             dim = c(2, 2),
                             dimnames = list(sex = c("f", "m"),
                                             time = c("2001-2005", "2006-2010"))))
    component <- BirthsMovements(component,
                                 template = template)
    template <- Counts(array(1:24,
                             dim = c(2, 2),
                             dimnames = list(sex = c("f", "m"),
                                             time = c("2001-2005", "2006-2010"))))
    metadata <- template@metadata
    ans.obtained <- isCompatibleWithPopn(component = component,
                                         metadata = metadata,
                                         name = "births")
    ans.expected <- TRUE
    expect_identical(ans.obtained, ans.expected)
    ## has parent
    component <- Counts(array(1:4,
                              dim = c(2, 2, 2, 2),
                              dimnames = list(sex = c("f", "m"),
                                              time = c("2001-2005", "2006-2010"),
                                              reg_parent = c("a", "b"),
                                              reg_child = c("a", "b"))))
    template <- Counts(array(1:8,
                             dim = c(2, 2, 2),
                             dimnames = list(sex = c("f", "m"),
                                             time = c("2001-2005", "2006-2010"),
                                             reg = c("a", "b"))))
    component <- BirthsMovements(component,
                                 template = template)
    template <- Counts(array(1:8,
                             dim = c(2, 2, 2),
                             dimnames = list(sex = c("f", "m"),
                                             time = c("2001-2005", "2006-2010"),
                                             reg = c("a", "b"))))
    metadata <- template@metadata
    ans.obtained <- isCompatibleWithPopn(component = component,
                                         metadata = metadata,
                                         name = "births")
    ans.expected <- TRUE
    expect_identical(ans.obtained, ans.expected)
})


test_that("isCompatibleWithPopn works with BirthsTransitions", {
    isCompatibleWithPopn <- dembase:::isCompatibleWithPopn
    BirthsTransitions <- dembase:::BirthsTransitions
    ## both have age
    component <- Counts(array(1:24,
                              dim = c(2, 2, 3, 2),
                              dimnames = list(reg_orig = c("a", "b"),
                                              reg_dest = c("a", "b"),
                                              age = c("10-14", "15-19", "20-24"),
                                              time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(1:20,
                             dim = c(2, 5, 2),
                             dimnames = list(reg = c("a", "b"),
                                             age = c("0-4", "5-9", "10-14", "15-19", "20-24"),
                                             time = c("2001-2005", "2006-2010"))))
    component <- BirthsTransitions(component,
                                   template = template)
    template <- Counts(array(1:24,
                             dim = c(2, 2, 6),
                             dimnames = list(reg = c("a", "b"),
                                             time = c("2001-2005", "2006-2010"),
                                             age = c("0-4", "5-9", "10-14",
                                                     "15-19", "20-24", "25+"))))
    metadata <- template@metadata
    ans.obtained <- isCompatibleWithPopn(component = component,
                                         metadata = metadata,
                                         name = "births")
    ans.expected <- TRUE
    expect_identical(ans.obtained, ans.expected)
    template.wrong <- Counts(array(1:36,
                                   dim = c(3, 6, 2),
                                   dimnames = list(time = c("2001-2005", "2006-2010", "2011-2015"),
                                                   age = c("0-4", "5-9", "10-14",
                                                           "15-19", "20-24", "25+"),
                                                   reg = c("a", "b"))))
    metadata.wrong <- template.wrong@metadata
    ans.obtained <- isCompatibleWithPopn(component = component,
                                         metadata = metadata.wrong,
                                         name = "births")
    ans.expected <- "'births' not compatible with 'population'"
    expect_identical(ans.obtained, ans.expected)
    ## neither has age
    component <- Counts(array(1:4,
                              dim = c(2, 2),
                              dimnames = list(sex = c("f", "m"),
                                              time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(1:4,
                             dim = c(2, 2),
                             dimnames = list(sex = c("f", "m"),
                                             time = c("2001-2005", "2006-2010"))))
    component <- BirthsTransitions(component,
                                   template = template)
    template <- Counts(array(1:24,
                             dim = c(2, 2),
                             dimnames = list(sex = c("f", "m"),
                                             time = c("2001-2005", "2006-2010"))))
    metadata <- template@metadata
    ans.obtained <- isCompatibleWithPopn(component = component,
                                         metadata = metadata,
                                         name = "births")
    ans.expected <- TRUE
    expect_identical(ans.obtained, ans.expected)
    ## has parent
    component <- Counts(array(1:4,
                              dim = c(2, 2, 2, 2),
                              dimnames = list(sex = c("f", "m"),
                                              time = c("2001-2005", "2006-2010"),
                                              reg_parent = c("a", "b"),
                                              reg_child = c("a", "b"))))
    template <- Counts(array(1:8,
                             dim = c(2, 2, 2),
                             dimnames = list(sex = c("f", "m"),
                                             time = c("2001-2005", "2006-2010"),
                                             reg = c("a", "b"))))
    component <- BirthsTransitions(component,
                                   template = template)
    template <- Counts(array(1:8,
                             dim = c(2, 2, 2),
                             dimnames = list(sex = c("f", "m"),
                                             time = c("2001-2005", "2006-2010"),
                                             reg = c("a", "b"))))
    metadata <- template@metadata
    ans.obtained <- isCompatibleWithPopn(component = component,
                                         metadata = metadata,
                                         name = "births")
    ans.expected <- TRUE
    expect_identical(ans.obtained, ans.expected)
})


test_that("Orig-Dest method of isCompatibleWithPopn works", {
    isCompatibleWithPopn <- dembase:::isCompatibleWithPopn
    InternalMovements <- dembase:::InternalMovements
    component <- Counts(array(1:96,
                              dim = c(2, 3, 2, 2, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                              age = c("0-4", "5-9", "10+"),
                                              time = c("2001-2005", "2006-2010"),
                                              reg_orig = c("a", "b"),
                                              reg_dest = c("a", "b"))))
    net <- collapseOrigDest(component, to = "net")
    template <- Counts(array(0L,
                             dim = c(2, 3, 2, 2),
                             dimnames = list(triangle = c("Lower", "Upper"),
                                             age = c("0-4", "5-9", "10+"),
                                             time = c("2001-2005", "2006-2010"),
                                             reg = c("a", "b"))))
    component <- InternalMovements(internal = component,
                                   template = template)
    ans.obtained <- isCompatibleWithPopn(component,
                                         metadata = template@metadata,
                                         name = "internal")
    ans.expected <- TRUE
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- isCompatibleWithPopn(component = component,
                                         metadata = rev(template@metadata),
                                         name = "internal")
    ans.expected <- "'internal' not compatible with 'population'"
    expect_identical(ans.obtained, ans.expected)                                         
})

test_that("Pool method of isCompatibleWithPopn works", {
    isCompatibleWithPopn <- dembase:::isCompatibleWithPopn
    InternalMovements <- dembase:::InternalMovements
    component <- Counts(array(1:96,
                              dim = c(2, 3, 2, 2, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                              age = c("0-4", "5-9", "10+"),
                                              time = c("2001-2005", "2006-2010"),
                                              reg_orig = c("a", "b"),
                                              reg_dest = c("a", "b"))))
    net <- collapseOrigDest(component, to = "net")
    component <- collapseOrigDest(component, to = "pool")
    template <- Counts(array(0L,
                             dim = c(2, 3, 2, 2),
                             dimnames = list(triangle = c("Lower", "Upper"),
                                             age = c("0-4", "5-9", "10+"),
                                             time = c("2001-2005", "2006-2010"),
                                             reg = c("a", "b"))))
    component <- InternalMovements(internal = component,
                                   template = template)
    ans.obtained <- isCompatibleWithPopn(component = component,
                                         metadata = template@metadata,
                                         name = "internal")
    ans.expected <- TRUE
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- isCompatibleWithPopn(component = component,
                                         metadata = rev(template@metadata),
                                         name = "internal")
    ans.expected <- "'internal' not compatible with 'population'"
    expect_identical(ans.obtained, ans.expected)
})





## isPositiveIncrement #############################################################

test_that("isPositiveIncrement worths with Births", {
    isPositiveIncrement <- dembase:::isPositiveIncrement
    x <- new("BirthsMovementsHasParentChild")
    expect_true(isPositiveIncrement(x))
    x <- new("BirthsMovementsNoParentChild")
    expect_true(isPositiveIncrement(x))
})

test_that("isPositiveIncrement worths with Internal", {
    isPositiveIncrement <- dembase:::isPositiveIncrement
    x <- new("InternalMovementsNet")
    expect_true(isPositiveIncrement(x))
    x <- new("InternalMovementsOrigDest")
    expect_true(isPositiveIncrement(x))
    x <- new("InternalMovementsPool")
    expect_true(isPositiveIncrement(x))
})

test_that("isPositiveIncrement worths with Entries", {
    isPositiveIncrement <- dembase:::isPositiveIncrement
    x <- new("EntriesMovements")
    expect_true(isPositiveIncrement(x))
})

test_that("isPositiveIncrement worths with Exits", {
    isPositiveIncrement <- dembase:::isPositiveIncrement
    x <- new("ExitsMovements")
    expect_false(isPositiveIncrement(x))
})

test_that("isPositiveIncrement worths with NetMovements", {
    isPositiveIncrement <- dembase:::isPositiveIncrement
    x <- new("NetMovements")
    expect_true(isPositiveIncrement(x))
})


## midpoints #############################################################


test_that("midpoints works with Component", {
    EntriesMovements <- dembase:::EntriesMovements
    component <- Counts(array(1:12,
                              dim = c(2, 3, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                  age = c("0-4", "5-9", "10+"),
                                  time = c("2001-2005", "2006-2010"))))
    component <- EntriesMovements(entries = component,
                                  template = component)
    ans.obtained <- midpoints(component)
    ans.expected <- midpoints(as(component, "Counts"))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- midpoints(component, dimension = "time")
    ans.expected <- midpoints(as(component, "Counts"), dimension = "time")
    expect_identical(ans.obtained, ans.expected)
})



## slab #############################################################

test_that("slab works with MovementsComponent", {
    EntriesMovements <- dembase:::EntriesMovements
    component <- Counts(array(1:12,
                              dim = c(2, 3, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                  age = c("0-4", "5-9", "10+"),
                                  time = c("2001-2005", "2006-2010"))))
    component <- EntriesMovements(entries = component,
                                  template = component)
    ans.obtained <- slab(component,
                         dimension = "time",
                         elements = "2006-2010",
                         drop = FALSE)
    ans.expected <- Counts(array(7:12,
                              dim = c(2, 3, 1),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                  age = c("0-4", "5-9", "10+"),
                                  time = "2006-2010")))
    ans.expected <- EntriesMovements(ans.expected,
                                     template = ans.expected)
    expect_identical(ans.obtained, ans.expected)
})


test_that("slab works with BirthsMovements", {
    BirthsMovements <- dembase:::BirthsMovements
    component <- Counts(array(1:4,
                              dim = c(2, 1, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                              age = "5-9", 
                                              time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(1:12,
                             dim = c(2, 3, 2),
                             dimnames = list(triangle = c("Lower", "Upper"),
                                             age = c("0-4", "5-9", "10+"),
                                             time = c("2001-2005", "2006-2010"))))
    component <- BirthsMovements(births = component,
                                 template = template)
    ans.obtained <- slab(component,
                         dimension = "time",
                         elements = "2006-2010",
                         drop = FALSE)
    ans.expected <- Counts(array(3:4,
                                 dim = c(2, 1, 1),
                                 dimnames = list(triangle = c("Lower", "Upper"),
                                                 age = "5-9",
                                                 time = "2006-2010")))
    template <- Counts(array(7:12,
                             dim = c(2, 3, 1),
                             dimnames = list(triangle = c("Lower", "Upper"),
                                             age = c("0-4", "5-9", "10+"),
                                             time = "2006-2010")))
    ans.expected <- BirthsMovements(ans.expected,
                                    template = template)
    expect_identical(ans.obtained, ans.expected)
})

test_that("slab works with InternalMovementsPool", {
    InternalMovements <- dembase:::InternalMovements
    component <- Counts(array(1:96,
                              dim = c(2, 3, 2, 2, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                              age = c("0-4", "5-9", "10+"),
                                              time = c("2001-2005", "2006-2010"),
                                              reg_orig = c("a", "b"),
                                              reg_dest = c("a", "b"))))
    template <- Counts(array(0L,
                             dim = c(2, 3, 2, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                              age = c("0-4", "5-9", "10+"),
                                              time = c("2001-2005", "2006-2010"),
                                              reg = c("a", "b"))))
    component <- collapseOrigDest(component, to = "pool")
    component <- InternalMovements(internal = component,
                                   template = template)
    ans.obtained <- slab(component,
                         dimension = "age",
                         elements = "0-4",
                         drop = FALSE)
    ans.expected <- as(component, "Counts")
    ans.expected <- subarray(ans.expected, age == "0-4", drop = FALSE)
    ans.expected <- new("InternalMovementsPool",
                        .Data = ans.expected@.Data,
                        metadata = ans.expected@metadata,
                        iDirection = 5L,
                        iBetween = 4L)
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- slab(component,
                         dimension = "direction",
                         elements = "Out",
                         drop = TRUE)
    ans.expected <- Counts(component@.Data[,,,,"Out"])
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- slab(component,
                         dimension = "reg",
                         elements = "a",
                         drop = TRUE)
    ans.expected <- Counts(component@.Data[,,,1,])
    expect_identical(ans.obtained, ans.expected)
})

test_that("slab works with InternalMovementsNet", {
    InternalMovements <- dembase:::InternalMovements
    component <- Counts(array(1:96,
                              dim = c(2, 3, 2, 2, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                              age = c("0-4", "5-9", "10+"),
                                              time = c("2001-2005", "2006-2010"),
                                              reg_orig = c("a", "b"),
                                              reg_dest = c("a", "b"))))
    template <- Counts(array(0L,
                             dim = c(2, 3, 2, 2),
                              dimnames = list(triangle = c("Lower", "Upper"),
                                              age = c("0-4", "5-9", "10+"),
                                              time = c("2001-2005", "2006-2010"),
                                              reg = c("a", "b"))))
    component <- collapseOrigDest(component, to = "net")
    component <- InternalMovements(internal = component,
                                   template = template)
    ans.obtained <- slab(component,
                         dimension = "age",
                         elements = "0-4",
                         drop = FALSE)
    ans.expected <- as(component, "Counts")
    ans.expected <- subarray(ans.expected, age == "0-4", drop = FALSE)
    ans.expected <- new("InternalMovementsNet",
                        .Data = ans.expected@.Data,
                        metadata = ans.expected@metadata,
                        iBetween = 4L)
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- slab(component,
                         dimension = "reg",
                         elements = "a",
                         drop = TRUE)
    ans.expected <- Counts(component@.Data[,,,1])
    expect_identical(ans.obtained, ans.expected)
})

