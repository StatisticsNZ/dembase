
## context("Component-methods")

## test_that("default method of accessionComponent works", {
##     accessionComponent <- dembase:::accessionComponent
##     ## has age
##     component <- Counts(array(1:12,
##                               dim = c(2, 3, 2),
##                               dimnames = list(triangle = c("TL", "TU"),
##                                   age = c("0-4", "5-9", "10+"),
##                                   time = c("2001-2005", "2006-2010"))))
##     component <- new("EntriesMovements",
##                      .Data = component@.Data,
##                      metadata = component@metadata)
##     population <- Counts(array(1:9,
##                                dim = c(3, 3),
##                                dimnames = list(time = c(2000, 2005, 2010),
##                                    age = c("0-4", "5-9", "10+"))))
##     population <- Population(population)
##     ans.obtained <- accessionComponent(component = component,
##                                        population = population)
##     ans.expected <- Counts(array(c(0L, 2L, 4L, 0L, 8L, 10L),
##                                  dim = c(3, 2),
##                                  dimnames = list(age = c("0-4", "5-9", "10+"),
##                                      time = c("2001-2005", "2006-2010"))))
##     ans.expected <- t(ans.expected)
##     expect_identical(ans.obtained, ans.expected)
##     ## no age
##     component <- Counts(array(1:2,
##                               dim = 2,
##                               dimnames = list(time = c("2001-2005", "2006-2010"))))
##     component <- new("EntriesMovements",
##                      .Data = component@.Data,
##                      metadata = component@metadata)
##     population <- Counts(array(3,
##                                dim = 3,
##                                dimnames = list(time = c(2000, 2005, 2010))))
##     population <- Population(population)
##     ans.obtained <- accessionComponent(component = component,
##                                        population = population)
##     ans.expected <- NULL
##     expect_identical(ans.obtained, ans.expected)
## })

## test_that("accessionComponent works with BirthsMovements", {
##     accessionComponent <- dembase:::accessionComponent
##     ## has age, no parent
##     component <- Counts(array(1:12,
##                               dim = c(2, 3, 2),
##                              dimnames = list(triangle = c("TL", "TU"),
##                                   age = c("10-14", "15-19", "20-24"),
##                                   time = c("2001-2005", "2006-2010"))))
##     component <- new("BirthsMovements",
##                      .Data = component@.Data,
##                      metadata = component@metadata)
##     population <- Counts(array(1:9,
##                                dim = c(3, 6),
##                                dimnames = list(time = c(2000, 2005, 2010),
##                                    age = c("0-4", "5-9", "10-14",
##                                        "15-19", "20-24", "25+"))))
##     population <- new("Population",
##                       .Data = population@.Data,
##                       metadata = population@metadata)
##     ans.obtained <- accessionComponent(component = component,
##                                        population = population)
##     ans.expected <- Counts(array(c(21L, 57L, 0L, 0L, 0L, 0L,
##                                    0L, 0L, 0L, 0L, 0L, 0L),
##                                  dim = c(2, 6),
##                                  dimnames = list(time = c("2001-2005", "2006-2010"),
##                                      age = c("0-4", "5-9", "10-14",
##                                          "15-19", "20-24", "25+"))))
##     expect_identical(ans.obtained, ans.expected)
##     ## has age, has parent
##     component <- Counts(array(1:16,
##                               dim = c(2, 2, 2, 2, 2),
##                               dimnames = list(triangle = c("TL", "TU"),
##                                   age = c("10-14", "15-19"),
##                                   time = c("2001-2005", "2006-2010"),
##                                   eth_parent = c("a", "b"),
##                                   eth_child = c("a", "b"))))
##     component <- new("BirthsMovements",
##                      .Data = component@.Data,
##                      metadata = component@metadata)
##     population <- Counts(array(1L,
##                                dim = c(3, 2, 5),
##                                dimnames = list(time = c(2000, 2005, 2010),
##                                    eth = c("a", "b"),
##                                    age = c("0-4", "5-9", "10-14",
##                                        "15-19", "20+"))))
##     population <- new("Population",
##                       .Data = population@.Data,
##                       metadata = population@metadata)
##     ans.obtained <- accessionComponent(component = component,
##                                        population = population)
##     ans.expected <- Counts(array(c(52L, 84L, 52L, 84L, rep(0L, 16)),
##                                  dim = c(2, 2, 5),
##                                  dimnames = list(time = c("2001-2005", "2006-2010"),
##                                      eth = c("a", "b"),
##                                      age = c("0-4", "5-9", "10-14",
##                                          "15-19", "20+"))))
##     expect_identical(ans.obtained, ans.expected)    
##     ## no age
##     component <- Counts(array(1:16,
##                               dim = c(2, 2, 2, 2),
##                               dimnames = list(sex = c("m", "f"),
##                                   time = c("2001-2005", "2006-2010"),
##                                   eth_parent = c("a", "b"),
##                                   eth_child = c("a", "b"))))
##     component <- new("BirthsMovements",
##                      .Data = component@.Data,
##                      metadata = component@metadata)
##     population <- Counts(array(1L,
##                                dim = c(3, 2),
##                                dimnames = list(time = c(2000, 2005, 2010),
##                                    eth = c("a", "b"))))
##     population <- new("Population",
##                       .Data = population@.Data,
##                       metadata = population@metadata)
##     ans.obtained <- accessionComponent(component = component,
##                                        population = population)
##     ans.expected <- NULL
##     expect_identical(ans.obtained, ans.expected)    
## })

## test_that("accessionComponent works with InternalMovementsPool", {
##     accessionComponent <- dembase:::accessionComponent
##     ## has age
##     component <- Counts(array(1:96,
##                               dim = c(2, 3, 2, 2, 2),
##                               dimnames = list(triangle = c("TL", "TU"),
##                                   age = c("0-4", "5-9", "10+"),
##                                   time = c("2001-2005", "2006-2010"),
##                                   reg_orig = c("a", "b"),
##                                   reg_dest = c("a", "b"))))
##     net <- collapseOrigDest(component, to = "net")
##     component <- collapseOrigDest(component, base = "reg", to = c("in", "out"))
##     component <- new("InternalMovementsPool",
##                      .Data = component@.Data,
##                      metadata = component@metadata,
##                      iBetween = 4L,
##                      iDirection = 5L)
##     population <- Counts(array(1L,
##                                dim = c(3, 2, 3),
##                                dimnames = list(time = c(2000, 2005, 2010),
##                                    reg = c("a", "b"),
##                                    age = c("0-4", "5-9", "10+"))))
##     population <- new("Population",
##                       .Data = population@.Data,
##                       metadata = population@metadata)
##     ans.obtained <- accessionComponent(component = component,
##                                        population = population)
##     net <- slab(net, dimension = "triangle", elements = "TU")
##     ans.expected <- net
##     ans.expected[] <- 0L
##     ans.expected[-1,,] <- net[-3,,]
##     ans.expected <- aperm(ans.expected, perm = names(population))
##     expect_identical(ans.obtained, ans.expected)
##     ## no age
##     component <- Counts(array(1:96,
##                               dim = c(2, 2, 2),
##                               dimnames = list(time = c("2001-2005", "2006-2010"),
##                                   reg_orig = c("a", "b"),
##                                   reg_dest = c("a", "b"))))
##     component <- collapseOrigDest(component, base = "reg", to = c("in", "out"))
##     component <- new("InternalMovementsPool",
##                      .Data = component@.Data,
##                      metadata = component@metadata,
##                      iBetween = 2L,
##                      iDirection = 3L)
##     population <- Counts(array(1L,
##                                dim = c(3, 2),
##                                dimnames = list(time = c(2000, 2005, 2010),
##                                    reg = c("a", "b"))))
##     population <- new("Population",
##                       .Data = population@.Data,
##                       metadata = population@metadata)
##     ans.obtained <- accessionComponent(component = component,
##                                        population = population)
##     ans.expected <- NULL
##     expect_identical(ans.obtained, ans.expected)    
## })

## test_that("accessionComponent works with InternalMovementsOrigDest", {
##     accessionComponent <- dembase:::accessionComponent
##     ## has age
##     component <- Counts(array(1:96,
##                               dim = c(2, 3, 2, 2, 2),
##                               dimnames = list(triangle = c("TL", "TU"),
##                                   age = c("0-4", "5-9", "10+"),
##                                   time = c("2001-2005", "2006-2010"),
##                                   reg_orig = c("a", "b"),
##                                   reg_dest = c("a", "b"))))
##     net <- collapseOrigDest(component, to = "net")
##     component <- new("InternalMovementsOrigDest",
##                      .Data = component@.Data,
##                      metadata = component@metadata)
##     population <- Counts(array(1L,
##                                dim = c(3, 2, 3),
##                                dimnames = list(time = c(2000, 2005, 2010),
##                                    reg = c("a", "b"),
##                                    age = c("0-4", "5-9", "10+"))))
##     population <- new("Population",
##                       .Data = population@.Data,
##                       metadata = population@metadata)
##     ans.obtained <- accessionComponent(component = component,
##                                        population = population)
##     net <- slab(net, dimension = "triangle", elements = "TU")
##     ans.expected <- net
##     ans.expected[] <- 0L
##     ans.expected[-1,,] <- net[-3,,]
##     ans.expected <- aperm(ans.expected, perm = names(population))
##     expect_identical(ans.obtained, ans.expected)
##     ## no age
##     component <- Counts(array(1:96,
##                               dim = c(2, 2, 2),
##                               dimnames = list(time = c("2001-2005", "2006-2010"),
##                                   reg_orig = c("a", "b"),
##                                   reg_dest = c("a", "b"))))
##     component <- new("InternalMovementsOrigDest",
##                      .Data = component@.Data,
##                      metadata = component@metadata)
##     population <- Counts(array(1L,
##                                dim = c(3, 2),
##                                dimnames = list(time = c(2000, 2005, 2010),
##                                    reg = c("a", "b"))))
##     population <- new("Population",
##                       .Data = population@.Data,
##                       metadata = population@metadata)
##     ans.obtained <- accessionComponent(component = component,
##                                        population = population)
##     ans.expected <- NULL
##     expect_identical(ans.obtained, ans.expected)    
## })

## test_that("Exists method of accessionComponent works", {
##     accessionComponent <- dembase:::accessionComponent
##     ## has age
##     component <- Counts(array(1:12,
##                               dim = c(2, 3, 2),
##                               dimnames = list(triangle = c("TL", "TU"),
##                                   age = c("0-4", "5-9", "10+"),
##                                   time = c("2001-2005", "2006-2010"))))
##     component <- new("ExitsMovements",
##                      .Data = component@.Data,
##                      metadata = component@metadata)
##     population <- Counts(array(1:9,
##                                dim = c(3, 3),
##                                dimnames = list(time = c(2000, 2005, 2010),
##                                    age = c("0-4", "5-9", "10+"))))
##     population <- Population(population)
##     ans.obtained <- accessionComponent(component = component,
##                                        population = population)
##     ans.expected <- Counts(array(c(0L, -2L, -4L, 0L, -8L, -10L),
##                                  dim = c(3, 2),
##                                  dimnames = list(age = c("0-4", "5-9", "10+"),
##                                      time = c("2001-2005", "2006-2010"))))
##     ans.expected <- t(ans.expected)
##     expect_identical(ans.obtained, ans.expected)
##     ## no age
##     component <- Counts(array(1:2,
##                               dim = 2,
##                               dimnames = list(time = c("2001-2005", "2006-2010"))))
##     component <- new("ExitsMovements",
##                      .Data = component@.Data,
##                      metadata = component@metadata)
##     population <- Counts(array(3,
##                                dim = 3,
##                                dimnames = list(time = c(2000, 2005, 2010))))
##     population <- Population(population)
##     ans.obtained <- accessionComponent(component = component,
##                                        population = population)
##     ans.expected <- NULL
##     expect_identical(ans.obtained, ans.expected)
## })

    





