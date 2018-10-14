
context("Component-generator")

test_that("Accession works with valid inputs", {
    Accession <- dembase:::Accession
    accession <- Counts(array(c(1L, NA),
                            dim = c(3, 3, 2),
                            dimnames = list(age = c("5", "10", "15"),
                                region = 1:3,
                                time = c("2001-2005", "2006-2010"))))
    set.seed(1)
    ans.obtained <- Accession(accession = accession)
    set.seed(1)
    ans.expected <- new("Accession",
                        .Data = accession@.Data,
                        metadata = accession@metadata)
    expect_identical(ans.obtained, ans.expected)
    accession <- Counts(array(c(1L, NA),
                            dim = c(3, 2),
                            dimnames = list(region = 1:3,
                                time = c("2001-2005", "2006-2010"))))
    set.seed(1)
    ans.obtained <- Accession(accession = accession)
    set.seed(1)
    ans.expected <- new("Accession",
                        .Data = accession@.Data,
                        metadata = accession@metadata)
    expect_identical(ans.obtained, ans.expected)
})


test_that("BirthsMovements works with valid inputs", {
    BirthsMovements <- dembase:::BirthsMovements
    splitTriangles <- dembase:::splitTriangles
    ## no parent-child dimensions; no permuting, collapsing or subsetting
    births <- Counts(array(10L,
                           dim = c(3, 3, 2),
                           dimnames = list(age = c("15-19", "20-24", "25-29"),
                               region = 1:3,
                               time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(0L,
                             dim = c(20, 3, 2, 2),
                             dimnames = list(age = c(paste(seq(0, 90, 5), seq(4, 94, 5), sep = "-"), "95+"),
                                 region = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    set.seed(1)
    ans.obtained <- BirthsMovements(births = births,
                                    template = template)
    set.seed(1)
    lower <- rbinom(n = 18, size = 10, prob = 0.5)
    upper <- births@.Data - lower
    ans.expected <- Counts(array(c(lower, upper),
                                 dim = c(3, 3, 2, 2),
                                 dimnames = list(age = c("15-19", "20-24", "25-29"),
                                     region = 1:3,
                                     time = c("2001-2005", "2006-2010"),
                                     triangle = c("Lower", "Upper"))))
    ans.expected <- new("BirthsMovementsNoParentChild",
                        .Data = ans.expected@.Data,
                        metadata = ans.expected@metadata,
                        iMinAge = 4L)
    expect_identical(ans.obtained, ans.expected)
    ## has parent-child dimensions; has collapsing and subsetting
    set.seed(1)
    births <- Counts(array(rpois(n = 108, lambda = 20),
                           dim = c(3, 3, 2, 2, 3),
                           dimnames = list(age = c("15-19", "20-24", "25-29"),
                               region = 1:3,
                               eth_child = 1:2,
                               eth_parent = 1:2,
                               time = c("2001-2005", "2006-2010", "2011-2015"))))
    template <- Counts(array(0L,
                             dim = c(20, 3, 2, 2, 2),
                             dimnames = list(age = c(paste(seq(0, 90, 5), seq(4, 94, 5), sep = "-"), "95+"),
                                 region = 1:3,
                                 eth = 2:1,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    set.seed(1)
    ans.obtained <- BirthsMovements(births = births,
                                    template = template)
    set.seed(1)
    births <- splitTriangles(births)
    births <- aperm(births, perm = c("age", "region", "eth_parent", "eth_child", "time", "triangle"))
    births <- subarray(births, time < 2010)
    births <- births[,,2:1,2:1,,]
    ans.expected <- new("BirthsMovementsHasParentChild",
                        .Data = births@.Data,
                        metadata = births@metadata,
                        iMinAge = 4L)
    expect_identical(ans.obtained, ans.expected)
    ## births does not have age dimension; template does
    births <- Counts(array(10L,
                           dim = c(3, 2),
                           dimnames = list(region = 1:3,
                                           time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(0L,
                             dim = c(20, 3, 2, 2),
                             dimnames = list(age = c(paste(seq(0, 90, 5), seq(4, 94, 5), sep = "-"), "95+"),
                                 region = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    ans.obtained <- BirthsMovements(births = births,
                                    template = template)
    expect_identical(names(ans.obtained), c("age", "region", "time", "triangle"))
    expect_identical(collapseDimension(as(ans.obtained, "Counts"), margin = c("region", "time")),
                     births)
})

test_that("BirthsMovements raises appropriate errors", {
    BirthsMovements <- dembase:::BirthsMovements
    births <- Counts(array(10L,
                           dim = c(3, 3, 4),
                           dimnames = list(age = c("15-19", "20-24", "25-29"),
                               region = 1:3,
                               time = c("2001-2005", "2006-2010", "2011-2015", "2016-2020"))))
    template <- Counts(array(0L,
                             dim = c(11, 3, 2, 2),
                             dimnames = list(age = c(paste(seq(0, 90, 10), seq(9, 99, 10), sep = "-"), "100+"),
                                 region = 1:3,
                                 time = c("2001-2010", "2011-2020"),
                                 triangle = c("Lower", "Upper"))))
    expect_error(BirthsMovements(births = births, template = template),
                 "'births' is incompatible with 'population")
    births <- Counts(array(10L,
                           dim = c(3, 3, 2),
                           dimnames = list(age = c("15-19", "20-24", "25-29"),
                               region = 1:3,
                               time = c("2006-2010", "2011-2015"))))
    template <- Counts(array(0L,
                             dim = c(20, 3, 2, 2),
                             dimnames = list(age = c(paste(seq(0, 90, 5), seq(4, 94, 5), sep = "-"), "95+"),
                                 region = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    expect_error(BirthsMovements(births = births, template = template),
                 "'births' is incompatible with 'population")
})


test_that("BirthsTransitions works with valid inputs", {
    BirthsTransitions <- dembase:::BirthsTransitions
    ## no parent-child dimensions; no permuting, collapsing or subsetting
    births <- Counts(array(10L,
                           dim = c(3, 3, 3, 2),
                           dimnames = list(age = c("15-19", "20-24", "25-29"),
                                           region_orig = 1:3,
                                           region_dest = 1:3,
                                           time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(0L,
                             dim = c(20, 3, 2),
                             dimnames = list(age = c(paste(seq(0, 90, 5), seq(4, 94, 5), sep = "-"), "95+"),
                                             region = 1:3,
                                             time = c("2001-2005", "2006-2010"))))
    ans.obtained <- BirthsTransitions(births = births,
                                      template = template)
    ans.expected <- Counts(array(10L,
                                 dim = c(3, 3, 3, 2),
                                 dimnames = list(age = c("15-19", "20-24", "25-29"),
                                                 region_orig = 1:3,
                                                 region_dest = 1:3,
                                                 time = c("2001-2005", "2006-2010"))))
    ans.expected <- new("BirthsTransitionsNoParentChild",
                        .Data = ans.expected@.Data,
                        metadata = ans.expected@metadata,
                        iMinAge = 4L)
    expect_identical(ans.obtained, ans.expected)
    ## has parent-child dimensions; has collapsing and subsetting
    births <- Counts(array(rpois(n = 108, lambda = 20),
                           dim = c(3, 3, 2, 2, 3),
                           dimnames = list(age = c("15-19", "20-24", "25-29"),
                                           region = 1:3,
                                           eth_child = 1:2,
                                           eth_parent = 1:2,
                                           time = c("2001-2005", "2006-2010", "2011-2015"))))
    template <- Counts(array(0L,
                             dim = c(20, 3, 2, 2),
                             dimnames = list(age = c(paste(seq(0, 90, 5), seq(4, 94, 5), sep = "-"), "95+"),
                                             region = 1:3,
                                             eth = 2:1,
                                             time = c("2001-2005", "2006-2010"))))
    ans.obtained <- BirthsTransitions(births = births,
                                      template = template)
    births <- aperm(births, perm = c("age", "region", "eth_parent", "eth_child", "time"))
    births <- subarray(births, time < 2010)
    births <- births[,,2:1,2:1,]
    ans.expected <- new("BirthsTransitionsHasParentChild",
                        .Data = births@.Data,
                        metadata = births@metadata,
                        iMinAge = 4L)
    expect_identical(ans.obtained, ans.expected)
})


test_that("EntriesMovements works with valid inputs", {
    EntriesMovements <- dembase:::EntriesMovements
    entries <- Counts(array(c(1L, NA),
                            dim = c(3, 3, 2),
                            dimnames = list(age = c("0-4", "5-9", "10+"),
                                region = 1:3,
                                time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(0L,
                            dim = c(3, 3, 2, 2),
                            dimnames = list(age = c("0-4", "5-9", "10+"),
                                region = 1:3,
                                time = c("2001-2005", "2006-2010"),
                                triangle = c("Lower", "Upper"))))
    set.seed(1)
    ans.obtained <- EntriesMovements(entries = entries,
                                     template = template,
                                     name = "immigration")
    set.seed(1)
    lower <- suppressWarnings(rbinom(n = 18, size = c(1, NA), prob = 0.5))
    upper <- entries@.Data - lower
    ans.expected <- Counts(array(c(lower, upper),
                                 dim = c(3, 3, 2, 2),
                                 dimnames = list(age = c("0-4", "5-9", "10+"),
                                     region = 1:3,
                                     time = c("2001-2005", "2006-2010"),
                                     triangle = c("Lower", "Upper"))))
    ans.expected <- new("EntriesMovements",
                        .Data = ans.expected@.Data,
                        metadata = ans.expected@metadata)
    expect_identical(ans.obtained, ans.expected)
})

test_that("EntriesMovements throws appropriate errors", {
    EntriesMovements <- dembase:::EntriesMovements
    entries <- Counts(array(c(1, NA),
                            dim = c(3, 3, 2),
                            dimnames = list(age = c("0-4", "5-9", "10+"),
                                region = 2:4,
                                time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(0L,
                            dim = c(3, 3, 2, 2),
                            dimnames = list(age = c("0-4", "5-9", "10+"),
                                region = 1:3,
                                time = c("2001-2005", "2006-2010"),
                                triangle = c("Lower", "Upper"))))
    set.seed(1)
    expect_error(EntriesMovements(entries = entries,
                                     template = template,
                                  name = "immigration"),
                 "'immigration' is incompatible with 'population' :")
})


test_that("EntriesTransitions works with valid inputs", {
    EntriesTransitions <- dembase:::EntriesTransitions
    entries <- Counts(array(1L,
                            dim = c(3, 3, 3, 2),
                            dimnames = list(age = c("0-4", "5-9", "10+"),
                                            region_orig = 1:3,
                                            region_dest = 1:3,
                                            time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(0L,
                             dim = c(3, 3, 2),
                             dimnames = list(region = 1:3,
                                             age = c("0-4", "5-9", "10+"),
                                             time = c("2001-2005", "2006-2010"))))
    ans.obtained <- EntriesTransitions(entries = entries,
                                       template = template,
                                       name = "immigration")
    ans.expected <- Counts(array(1L,
                                 dim = c(3, 3, 3, 2),
                                 dimnames = list(region_orig = 1:3,
                                                 region_dest = 1:3,
                                                 age = c("0-4", "5-9", "10+"),
                                                 time = c("2001-2005", "2006-2010"))))
    ans.expected <- new("EntriesTransitions",
                        .Data = ans.expected@.Data,
                        metadata = ans.expected@metadata)
    expect_identical(ans.obtained, ans.expected)
})


test_that("ExitsMovements works with valid inputs", {
    ExitsMovements <- dembase:::ExitsMovements
    exits <- Counts(array(c(1L, NA),
                          dim = c(3, 3, 2),
                          dimnames = list(age = c("0-4", "5-9", "10+"),
                              region = 1:3,
                              time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(0L,
                             dim = c(3, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 region = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    set.seed(1)
    ans.obtained <- ExitsMovements(exits = exits,
                                   template = template,
                                   name = "emigration")
    set.seed(1)
    lower <- suppressWarnings(rbinom(n = 18, size = c(1, NA), prob = 0.5))
    upper <- exits@.Data - lower
    ans.expected <- Counts(array(c(lower, upper),
                                 dim = c(3, 3, 2, 2),
                                 dimnames = list(age = c("0-4", "5-9", "10+"),
                                     region = 1:3,
                                     time = c("2001-2005", "2006-2010"),
                                     triangle = c("Lower", "Upper"))))
    ans.expected <- new("ExitsMovements",
                        .Data = ans.expected@.Data,
                        metadata = ans.expected@metadata)
    expect_identical(ans.obtained, ans.expected)
})

test_that("ExitsMovements throws appropriate errors", {
    ExitsMovements <- dembase:::ExitsMovements
    exits <- Counts(array(c(1, NA),
                          dim = c(3, 3, 2),
                          dimnames = list(age = c("0-4", "5-9", "10+"),
                              region = 2:4,
                              time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(0L,
                             dim = c(3, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 region = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    set.seed(1)
    expect_error(ExitsMovements(exits = exits,
                                template = template,
                                name = "emigration"),
                 "'emigration' is incompatible with 'population' :")
})

test_that("Exposure works with valid inputs", {
    Exposure <- dembase:::Exposure
    exposure <- Counts(array(c(1, NA),
                            dim = c(3, 3, 2, 2),
                            dimnames = list(age = c("0-4", "5-9", "10+"),
                                region = 1:3,
                                time = c("2001-2005", "2006-2010"),
                                triangle = c("Lower", "Upper"))))
    set.seed(1)
    ans.obtained <- Exposure(exposure = exposure)
    set.seed(1)
    ans.expected <- new("Exposure",
                        .Data = exposure@.Data,
                        metadata = exposure@metadata)
    expect_identical(ans.obtained, ans.expected)
    exposure <- Counts(array(c(1, NA),
                            dim = c(3, 2),
                            dimnames = list(region = 1:3,
                                time = c("2001-2005", "2006-2010"))))
    set.seed(1)
    ans.obtained <- Exposure(exposure = exposure)
    set.seed(1)
    ans.expected <- new("Exposure",
                        .Data = exposure@.Data,
                        metadata = exposure@metadata)
    expect_identical(ans.obtained, ans.expected)
})


test_that("InternalMovements works with valid inputs - orig-dest", {
    InternalMovements <- dembase:::InternalMovements
    ## single between dimension retained
    internal <- Counts(array(rpois(n = 108, lambda = 20),
                             dim = c(3, 2, 3, 3, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 triangle = c("Lower", "Upper"),
                                 region_orig = 1:3,
                                 region_dest = 1:3,
                                 time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(0L,
                             dim = c(3, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 region = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    set.seed(1)
    ans.obtained <- InternalMovements(internal = internal, template = template)
    set.seed(1)
    ans.expected <- aperm(internal@.Data, perm = c("age", "region_orig", "region_dest", "time", "triangle"))
    ans.expected <- Counts(ans.expected)
    ans.expected <- new("InternalMovementsOrigDest",
                        .Data = ans.expected@.Data,
                        metadata = ans.expected@metadata)
    expect_identical(ans.obtained, ans.expected)
    ## one of two between dimensions lost
    internal <- Counts(array(rpois(n = 432, lambda = 20),
                             dim = c(3, 2, 3, 3, 2, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 triangle = c("Lower", "Upper"),
                                 region_orig = 1:3,
                                 region_dest = 1:3,
                                 status_orig = 1:2,
                                 status_dest = 1:2,
                                 time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(0L,
                             dim = c(3, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 region = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    set.seed(1)
    ans.obtained <- InternalMovements(internal = internal, template = template)
    set.seed(1)
    ans.expected <- collapseDimension(internal,
                                      margin = c("age", "region_orig", "region_dest", "time", "triangle"))
    ans.expected <- Counts(ans.expected)
    ans.expected <- new("InternalMovementsOrigDest",
                        .Data = ans.expected@.Data,
                        metadata = ans.expected@metadata)
    expect_identical(ans.obtained, ans.expected)
})

test_that("InternalMovements throws appropriate errors - orig-dest", {
    InternalMovements <- dembase:::InternalMovements
    ## not compatible
    internal <- Counts(array(rpois(n = 108, lambda = 20),
                             dim = c(2, 2, 3, 3, 2),
                             dimnames = list(age = c("0-4", "5-9"),
                                 triangle = c("Lower", "Upper"),
                                 region_orig = 1:3,
                                 region_dest = 1:3,
                                 time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(0L,
                             dim = c(3, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 region = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    expect_error(InternalMovements(internal = internal, template = template),
                 "'internal' is incompatible with 'population' :")
    ## no orig dest dimensions
    internal <- Counts(array(rpois(n = 36, lambda = 20),
                             dim = c(3, 2, 3, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 triangle = c("Lower", "Upper"),
                                 region = 1:3,
                                 time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(0L,
                             dim = c(3, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 region = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    expect_error(InternalMovements(internal = internal, template = template),
                 "'internal' does not have class \"Net\" or \"Pool\" and does not have dimensions with dimtype \"origin\" or \"destination\"")
})



test_that("InternalMovements works with valid inputs - Net", {
    InternalMovements <- dembase:::InternalMovements
    ## single between dimension retained
    internal <- Counts(array(rpois(n = 108, lambda = 20),
                             dim = c(3, 2, 3, 3, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 triangle = c("Lower", "Upper"),
                                 region_orig = 1:3,
                                 region_dest = 1:3,
                                 time = c("2001-2005", "2006-2010"))))
    internal <- collapseOrigDest(internal, to = "net")
    template <- Counts(array(0L,
                             dim = c(3, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 region = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    set.seed(1)
    ans.obtained <- InternalMovements(internal = internal, template = template)
    set.seed(1)
    ans.expected <- aperm(internal@.Data, perm = c("age", "region", "time", "triangle"))
    ans.expected <- Counts(ans.expected)
    ans.expected <- new("InternalMovementsNet",
                        .Data = ans.expected@.Data,
                        metadata = ans.expected@metadata,
                        iBetween = 2L)
    expect_identical(ans.obtained, ans.expected)
    ## one of two between dimensions lost
    internal <- Counts(array(rpois(n = 432, lambda = 20),
                             dim = c(3, 2, 3, 3, 2, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 triangle = c("Lower", "Upper"),
                                 region_orig = 1:3,
                                 region_dest = 1:3,
                                 status_orig = 1:2,
                                 status_dest = 1:2,
                                 time = c("2001-2005", "2006-2010"))))
    internal <- collapseOrigDest(internal, to = "net")
    template <- Counts(array(0L,
                             dim = c(3, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 region = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    set.seed(1)
    ans.obtained <- InternalMovements(internal = internal, template = template)
    set.seed(1)
    ans.expected <- Counts(internal)
    ans.expected <- collapseDimension(internal, margin = c("age", "region", "time", "triangle"))
    ans.expected <- Counts(ans.expected)
    ans.expected <- new("InternalMovementsNet",
                        .Data = ans.expected@.Data,
                        metadata = ans.expected@metadata,
                        iBetween = 2L)
    expect_identical(ans.obtained, ans.expected)
})

test_that("InternalMovements throws appropriate errors - Net", {
    InternalMovements <- dembase:::InternalMovements
    ## not compatible
    internal <- Counts(array(rpois(n = 108, lambda = 20),
                             dim = c(2, 2, 3, 3, 2),
                             dimnames = list(age = c("0-4", "5-9"),
                                 triangle = c("Lower", "Upper"),
                                 region_orig = 1:3,
                                 region_dest = 1:3,
                                 time = c("2001-2005", "2006-2010"))))
    internal <- collapseOrigDest(internal, to = "net")
    template <- Counts(array(0L,
                             dim = c(3, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 region = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    expect_error(InternalMovements(internal = internal, template = template),
                 "'internal' is incompatible with 'population' :")
    ## between dimension collapsed away
    internal <- Counts(array(rpois(n = 108, lambda = 20),
                             dim = c(3, 2, 3, 3, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 triangle = c("Lower", "Upper"),
                                 region_orig = 1:3,
                                 region_dest = 1:3,
                                 time = c("2001-2005", "2006-2010"))))
    internal <- collapseOrigDest(internal, to = "net")
    template <- Counts(array(0L,
                             dim = c(3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    expect_error(InternalMovements(internal = internal, template = template),
                 "no \"between\" dimensions from 'internal' found in 'population'")
})

test_that("InternalMovements works with valid inputs - Pool", {
    InternalMovements <- dembase:::InternalMovements
    ## single between dimension retained
    internal <- Counts(array(rpois(n = 108, lambda = 20),
                             dim = c(3, 2, 3, 3, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 triangle = c("Lower", "Upper"),
                                 region_orig = 1:3,
                                 region_dest = 1:3,
                                 time = c("2001-2005", "2006-2010"))))
    internal <- collapseOrigDest(internal, to = "pool")
    template <- Counts(array(0L,
                             dim = c(3, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 region = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    set.seed(1)
    ans.obtained <- InternalMovements(internal = internal, template = template)
    set.seed(1)
    ans.expected <- aperm(internal@.Data, perm = c("age", "region", "time", "triangle", "direction"))
    ans.expected <- Counts(ans.expected)
    ans.expected <- new("InternalMovementsPool",
                        .Data = ans.expected@.Data,
                        metadata = ans.expected@metadata,
                        iBetween = 2L,
                        iDirection = 5L)
    expect_identical(ans.obtained, ans.expected)
    ## one of two between dimensions lost
    internal <- Counts(array(rpois(n = 432, lambda = 20),
                             dim = c(3, 2, 3, 3, 2, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 triangle = c("Lower", "Upper"),
                                 region_orig = 1:3,
                                 region_dest = 1:3,
                                 status_orig = 1:2,
                                 status_dest = 1:2,
                                 time = c("2001-2005", "2006-2010"))))
    internal <- collapseOrigDest(internal, to = "Pool")
    template <- Counts(array(0L,
                             dim = c(2, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5+"),
                                 region = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    set.seed(1)
    ans.obtained <- InternalMovements(internal = internal, template = template)
    set.seed(1)
    ans.expected <- Counts(internal)
    ans.expected <- collapseIntervals(ans.expected, dimension = "age", breaks = 5)
    ans.expected <- collapseDimension(ans.expected, margin = c("age", "region", "time", "triangle", "direction"))
    ans.expected <- Counts(ans.expected)
    ans.expected <- new("InternalMovementsPool",
                        .Data = ans.expected@.Data,
                        metadata = ans.expected@metadata,
                        iBetween = 2L,
                        iDirection = 5L)
    expect_identical(ans.obtained, ans.expected)
})

test_that("InternalMovements throws appropriate errors - Pool", {
    InternalMovements <- dembase:::InternalMovements
    ## not compatible
    internal <- Counts(array(rpois(n = 108, lambda = 20),
                             dim = c(2, 2, 3, 3, 2),
                             dimnames = list(age = c("0-4", "5-9"),
                                 triangle = c("Lower", "Upper"),
                                 region_orig = 1:3,
                                 region_dest = 1:3,
                                 time = c("2001-2005", "2006-2010"))))
    internal <- collapseOrigDest(internal, to = "Pool")
    template <- Counts(array(0L,
                             dim = c(3, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 region = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    expect_error(InternalMovements(internal = internal, template = template),
                 "'internal' is incompatible with 'population' :")
    ## between dimension collapsed away
    internal <- Counts(array(rpois(n = 108, lambda = 20),
                             dim = c(3, 2, 3, 3, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 triangle = c("Lower", "Upper"),
                                 region_orig = 1:3,
                                 region_dest = 1:3,
                                 time = c("2001-2005", "2006-2010"))))
    internal <- collapseOrigDest(internal, to = "pool")
    template <- Counts(array(0L,
                             dim = c(3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    expect_error(InternalMovements(internal = internal, template = template),
                 "no \"between\" dimensions from 'internal' found in 'population'")
})

test_that("NetMovements works with valid inputs", {
    NetMovements <- dembase:::NetMovements
    net <- Counts(array(c(1:9, -(1:9)),
                        dim = c(3, 3, 2),
                        dimnames = list(age = c("0-4", "5-9", "10+"),
                            region = 1:3,
                            time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(0L,
                             dim = c(3, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 region = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    set.seed(1)
    ans.obtained <- NetMovements(net = net,
                                 template = template,
                                 name = "immigration")
    set.seed(1)
    lower <- c(rbinom(n = 9, size = 1:9, prob = 0.5),
               -1L * rbinom(n = 9, size = 1:9, prob = 0.5))
    upper <- net@.Data - lower
    ans.expected <- Counts(array(c(lower, upper),
                                 dim = c(3, 3, 2, 2),
                                 dimnames = list(age = c("0-4", "5-9", "10+"),
                                     region = 1:3,
                                     time = c("2001-2005", "2006-2010"),
                                     triangle = c("Lower", "Upper"))))
    ans.expected <- new("NetMovements",
                        .Data = ans.expected@.Data,
                        metadata = ans.expected@metadata)
    expect_identical(ans.obtained, ans.expected)
})

test_that("NetMovements throws appropriate errors", {
    NetMovements <- dembase:::NetMovements
    net <- Counts(array(c(1, -1),
                        dim = c(3, 3, 2),
                        dimnames = list(age = c("0-4", "5-9", "10+"),
                            region = 2:4,
                            time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(0L,
                             dim = c(3, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 region = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    set.seed(1)
    expect_error(NetMovements(net = net,
                              template = template,
                              name = "immigration"),
                 "'immigration' is incompatible with 'population' :")
})

test_that("Net works", {
    object <- Counts(array(c(1, NA),
                           dim = c(3, 3, 2),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                               region = 1:3,
                               time = c("2001-2005", "2006-2010"))))
    ans.obtained <- Net(object, between = "region")
    ans.expected <- new("Net",
                        .Data = object@.Data,
                        metadata = object@metadata,
                        iBetween = 2L)
    expect_identical(ans.obtained, ans.expected)
    object <- Counts(array(as.integer(NA),
                           dim = c(3, 3, 2),
                           dimnames = list(eth = 1:3,
                               region = 1:3,
                               time = c("2001-2005", "2006-2010"))))
    ans.obtained <- Net(object,  between = 1:2)
    ans.expected <- Counts(array(as.integer(NA),
                                 dim = c(3, 3, 2),
                                 dimnames = list(eth = 1:3,
                                     region = 1:3,
                                     time = c("2001-2005", "2006-2010"))))
    ans.expected <- new("Net",
                        .Data = ans.expected@.Data,
                        metadata = ans.expected@metadata,
                        iBetween = 1:2)
    expect_identical(ans.obtained, ans.expected)
    object <- Counts(array(0,
                           dim = c(3, 3, 0),
                           dimnames = list(eth = 1:3,
                               region = 1:3,
                               time = character())),
                     dimscales = c(time = "Intervals"))
    ans.obtained <- Net(object, between = 1:2)
    ans.expected <- Counts(array(0,
                                 dim = c(3, 3, 0),
                                 dimnames = list(eth = 1:3,
                                     region = 1:3,
                                     time = character())),
                           dimscales = c(time = "Intervals"))
    ans.expected <- new("Net",
                        .Data = ans.expected@.Data,
                        metadata = ans.expected@metadata,
                        iBetween = 1:2)
    expect_identical(ans.obtained, ans.expected)
})

test_that("Net throws appropriate errors", {
    object <- Counts(array(c(1, NA),
                           dim = c(3, 3, 2),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                               region = 1:3,
                               time = c("2001-2005", "2006-2010"))))
    expect_error(Net(object, between = integer()),
                 "'between' has length 0")
    expect_error(Net(Counts(array(c(1, NA),
                                   dim = c(3, 1, 2),
                                   dimnames = list(age = c("0-4", "5-9", "10+"),
                                       region = 1,
                                       time = c("2001-2005", "2006-2010")))),
                      between = "region"),
                 "\"between\" dimension \"region\" has length 1")
    expect_error(Net(Counts(array(c(1, NA),
                                   dim = c(3, 3, 2),
                                   dimnames = list(age = c("0-4", "5-9", "10+"),
                                       region = 1:3,
                                       time = c("2001-2005", "2006-2010")))),
                      between = "time"),
                 "\"between\" dimension \"time\" has dimtype \"time\"")
})

test_that("Pool works", {
    object <- Counts(array(c(1, NA),
                           dim = c(2, 3, 3, 2),
                           dimnames = list(direction = c("Out", "In"),
                               age = c("0-4", "5-9", "10+"),
                               region = 1:3,
                               time = c("2001-2005", "2006-2010"))))
    ans.obtained <- Pool(object, direction = "direction", between = "region")
    ans.expected <- new("Pool",
                        .Data = object@.Data,
                        metadata = object@metadata,
                        iBetween = 3L,
                        iDirection = 1L)
    expect_identical(ans.obtained, ans.expected)
    object <- Counts(array(1,
                           dim = c(3, 3, 2, 2),
                           dimnames = list(eth = 1:3,
                               region = 1:3,
                               time = c("2001-2005", "2006-2010"),
                               type = c("outs", "ins"))))
    ans.obtained <- Pool(object, direction = "type", between = 1:2)
    ans.expected <- Counts(array(1,
                                 dim = c(3, 3, 2, 2),
                                 dimnames = list(eth = 1:3,
                                     region = 1:3,
                                     time = c("2001-2005", "2006-2010"),
                                     type = c("Out", "In"))))
    ans.expected <- new("Pool",
                        .Data = ans.expected@.Data,
                        metadata = ans.expected@metadata,
                        iBetween = 1:2,
                        iDirection = 4L)
    expect_identical(ans.obtained, ans.expected)
    object <- Counts(array(1,
                           dim = c(3, 3, 0, 2),
                           dimnames = list(eth = 1:3,
                               region = 1:3,
                               time = character(),
                               type = c("outs", "ins"))),
                     dimscales = c(time = "Intervals"))
    ans.obtained <- Pool(object, direction = "type", between = 1:2)
    ans.expected <- Counts(array(1,
                                 dim = c(3, 3, 0, 2),
                                 dimnames = list(eth = 1:3,
                                     region = 1:3,
                                     time = character(),
                                     type = c("Out", "In"))),
                           dimscales = c(time = "Intervals"))
    ans.expected <- new("Pool",
                        .Data = ans.expected@.Data,
                        metadata = ans.expected@metadata,
                        iBetween = 1:2,
                        iDirection = 4L)
    expect_identical(ans.obtained, ans.expected)
})

test_that("Pool throws appropriate errors", {
    object <- Counts(array(c(1, NA),
                           dim = c(2, 3, 3, 2),
                           dimnames = list(direction = c("Out", "In"),
                               age = c("0-4", "5-9", "10+"),
                               region = 1:3,
                               time = c("2001-2005", "2006-2010"))))
    expect_error(Pool(object, direction = c("direction", "age"), between = "region"),
                 "'direction' has length 2")
    expect_error(Pool(object, direction = "direction", between = integer()),
                 "'between' has length 0")
    expect_error(Pool(Counts(array(c(1, NA),
                                   dim = c(3, 3, 3, 2),
                                   dimnames = list(direction = c("Out", "In", "Wrong"),
                                       age = c("0-4", "5-9", "10+"),
                                       region = 1:3,
                                       time = c("2001-2005", "2006-2010")))),
                      direction = 1, between = 3),
                 "\"direction\" dimension has length 3")
    expect_error(Pool(Counts(array(c(1, NA),
                                   dim = c(2, 3, 3, 2),
                                   dimnames = list(direction = c("In", "Wrong"),
                                       age = c("0-4", "5-9", "10+"),
                                       region = 1:3,
                                       time = c("2001-2005", "2006-2010")))),
                      direction = 1, between = 3),
                 "\"direction\" dimension has invalid categories")
    expect_error(Pool(Counts(array(c(1, NA),
                                   dim = c(2, 3, 0, 2),
                                   dimnames = list(direction = c("Out", "In"),
                                       age = c("0-4", "5-9", "10+"),
                                       region = character(),
                                       time = c("2001-2005", "2006-2010")))),
                      direction = 1, between = 3),
                 "\"between\" dimension \"region\" has length 0")
    expect_error(Pool(Counts(array(c(1, NA),
                                   dim = c(2, 3, 3, 2),
                                   dimnames = list(direction = c("Out", "In"),
                                       age = c("0-4", "5-9", "10+"),
                                       region = 1:3,
                                       time = c("2001-2005", "2006-2010")))),
                      direction = "direction", between = "time"),
                 "\"between\" dimension \"time\" has dimtype \"time\"")
})


    





