
context("Component-methods")

## incrementLowerTri ################################################################

test_that("default method of incrementLowerTri works", {
    Population <- dembase:::Population
    incrementLowerTri <- dembase:::incrementLowerTri
    component <- Counts(array(1:12,
                              dim = c(2, 3, 2),
                              dimnames = list(triangle = c("TL", "TU"),
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
                              dimnames = list(triangle = c("TL", "TU"),
                                              age = c("10-14", "15-19", "20-24"),
                                              time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(1:20,
                             dim = c(2, 5, 2),
                             dimnames = list(triangle = c("TL", "TU"),
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
                              dimnames = list(triangle = c("TL", "TU"),
                                  age = c("0-4", "5-9", "10+"),
                                  time = c("2001-2005", "2006-2010"),
                                  reg_orig = c("a", "b"),
                                  reg_dest = c("a", "b"))))
    net <- collapseOrigDest(component, to = "net")
    component <- collapseOrigDest(component, base = "reg", to = "pool")
    template <- Counts(array(0L,
                             dim = c(2, 3, 2, 2),
                             dimnames = list(triangle = c("TL", "TU"),
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
    net <- slab(net, dimension = "triangle", elements = "TL")
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
                              dimnames = list(triangle = c("TL", "TU"),
                                  age = c("0-4", "5-9", "10+"),
                                  time = c("2001-2005", "2006-2010"),
                                  reg_orig = c("a", "b"),
                                  reg_dest = c("a", "b"))))
    net <- collapseOrigDest(component, to = "net")
    template <- Counts(array(0L,
                             dim = c(2, 3, 2, 2),
                             dimnames = list(triangle = c("TL", "TU"),
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
    net <- slab(net, dimension = "triangle", elements = "TL")
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
                              dimnames = list(triangle = c("TL", "TU"),
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




## incrementOpen ################################################################

test_that("default method of incrementOpen works", {
    Population <- dembase:::Population
    incrementOpen <- dembase:::incrementOpen
    component <- Counts(array(1:12,
                              dim = c(2, 3, 2),
                              dimnames = list(triangle = c("TL", "TU"),
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
                              dimnames = list(triangle = c("TL", "TU"),
                                              age = c("10-14", "15-19", "20-24"),
                                              time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(1:20,
                             dim = c(2, 5, 2),
                             dimnames = list(triangle = c("TL", "TU"),
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
                              dimnames = list(triangle = c("TL", "TU"),
                                              age = c("0-4", "5-9", "10+"),
                                              time = c("2001-2005", "2006-2010"),
                                              reg_orig = c("a", "b"),
                                              reg_dest = c("a", "b"))))
    net <- collapseOrigDest(component, to = "net")
    component <- collapseOrigDest(component, base = "reg", to = "pool")
    template <- Counts(array(0L,
                             dim = c(2, 3, 2, 2),
                             dimnames = list(triangle = c("TL", "TU"),
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
    net <- slab(net, dimension = "triangle", elements = "TU")
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
                              dimnames = list(triangle = c("TL", "TU"),
                                  age = c("0-4", "5-9", "10+"),
                                  time = c("2001-2005", "2006-2010"),
                                  reg_orig = c("a", "b"),
                                  reg_dest = c("a", "b"))))
    net <- collapseOrigDest(component, to = "net")
    template <- Counts(array(0L,
                             dim = c(2, 3, 2, 2),
                             dimnames = list(triangle = c("TL", "TU"),
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
    net <- slab(net, dimension = "triangle", elements = "TL")
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
                              dimnames = list(triangle = c("TL", "TU"),
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
                              dimnames = list(triangle = c("TL", "TU"),
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
                              dimnames = list(triangle = c("TL", "TU"),
                                              age = c("10-14", "15-19", "20-24"),
                                              time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(1:20,
                             dim = c(2, 5, 2),
                              dimnames = list(triangle = c("TL", "TU"),
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
                              dimnames = list(triangle = c("TL", "TU"),
                                  age = c("10-14", "15-19"),
                                  time = c("2001-2005", "2006-2010"),
                                  eth_parent = c("a", "b"),
                                  eth_child = c("a", "b"))))
    template <- Counts(array(1:40,
                             dim = c(2, 5, 2, 2),
                              dimnames = list(triangle = c("TL", "TU"),
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
                              dimnames = list(triangle = c("TL", "TU"),
                                  age = c("0-4", "5-9", "10+"),
                                  time = c("2001-2005", "2006-2010"),
                                  reg_orig = c("a", "b"),
                                  reg_dest = c("a", "b"))))
    net <- collapseOrigDest(component, to = "net")
    component <- collapseOrigDest(component, base = "reg", to = "pool")
    template <- Counts(array(0L,
                             dim = c(2, 3, 2, 2),
                             dimnames = list(triangle = c("TL", "TU"),
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
    net <- slab(net, dimension = "triangle", elements = "TU")
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
                              dimnames = list(triangle = c("TL", "TU"),
                                  age = c("0-4", "5-9", "10+"),
                                  time = c("2001-2005", "2006-2010"),
                                  reg_orig = c("a", "b"),
                                  reg_dest = c("a", "b"))))
    net <- collapseOrigDest(component, to = "net")
    template <- Counts(array(0L,
                             dim = c(2, 3, 2, 2),
                             dimnames = list(triangle = c("TL", "TU"),
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
    net <- slab(net, dimension = "triangle", elements = "TU")
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
                              dimnames = list(triangle = c("TL", "TU"),
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
    





