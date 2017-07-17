

context("miscellaneous-functions")
n.test <- 5
test.identity <- FALSE

## FUNCTIONS TO OBTAIN CONSTANTS ###################################################

test_that("getDimtypesPairs works", {
  getDimtypesPairs <- dembase:::getDimtypesPairs
  expect_that(getDimtypesPairs(dimtypes = c("origin", "parent")),
              is_identical_to(c("destination", "child")))
  expect_that(getDimtypesPairs(dimtypes = c("child", "destination")),
              is_identical_to(c("parent", "origin")))
  expect_that(getDimtypesPairs(dimtypes = character()),
              is_identical_to(character()))
  expect_that(getDimtypesPairs(dimtypes = "wrong"),
              throws_error("invalid value for \"dimtype\""))
})

test_that("getDimtypesWithPairs works", {
  getDimtypesWithPairs <- dembase:::getDimtypesWithPairs
  expect_that(getDimtypesWithPairs(),
              is_identical_to(c("origin", "destination",
                                "parent", "child")))
  expect_that(getDimtypesWithPairs(firstElementOnly = TRUE),
              is_identical_to(c("origin", "parent")))
})

test_that("getIntervalSeparator works", {
  getIntervalSeparator <- dembase:::getIntervalSeparator
  expect_that(getIntervalSeparator(),
              is_identical_to("-"))
})

test_that("getLimitPrintLower works", {
  getLimitPrintLower <- dembase:::getLimitPrintLower
  expect_identical(getLimitPrintLower(), 1000L)
})

test_that("getNamesPairs works", {
  getNamesPairs <- dembase:::getNamesPairs
  expect_that(getNamesPairs(names = c("reg_dest", "eth_child")),
              is_identical_to(c("reg_orig", "eth_parent")))
  expect_that(getNamesPairs(names = c("ethnicity_parent", "ethnicity_wrong")),
              is_identical_to(c("ethnicity_child", "ethnicity_wrong")))
  expect_that(getNamesPairs(names = character()),
              is_identical_to(character()))
  expect_that(getNamesPairs(names = c("region_orig", "age")),
              is_identical_to(c("region_dest", "age")))
})

test_that("getOpenIntervalSymbol works", {
  getOpenIntervalSymbol <- dembase:::getOpenIntervalSymbol
  expect_that(getOpenIntervalSymbol(),
              is_identical_to("+"))
  expect_that(getOpenIntervalSymbol(which = "final"),
              is_identical_to("+"))
  expect_that(getOpenIntervalSymbol(which = "fir"),
              is_identical_to("<"))
  expect_that(getOpenIntervalSymbol(which = "wrong"),
              throws_error(sprintf("'arg' should be one of %s, %s",
                                   dQuote("final"), dQuote("first"))))
})

test_that("getPossibleDimscales works", {
  getPossibleDimscales <- dembase:::getPossibleDimscales
  expect_that(getPossibleDimscales("age"),
              is_identical_to(c("Intervals", "Points")))
  expect_that(getPossibleDimscales("wrong"),
              throws_error("\"wrong\" is not a valid dimtype"))
})

test_that("getSuffixes works", {
  getSuffixes <- dembase:::getSuffixes
  expect_that(getSuffixes(dimtype = "origin"), is_identical_to("_orig"))
  expect_that(getSuffixes(dimtype = "child"), is_identical_to("_child"))
  expect_that(getSuffixes(dimtype = "wrong"), throws_error("invalid dimtype"))
  expect_that(getSuffixes(character()), is_identical_to(character()))
})

test_that("getSuffixPattern works", {
  getSuffixPattern <- dembase:::getSuffixPattern
  expect_that(getSuffixPattern(),
              is_identical_to(c("_orig$|_dest$|_parent$|_child$")))
  expect_that(getSuffixPattern(firstElementOnly = TRUE),
              is_identical_to(c("_orig$|_parent$")))
})

test_that("getSynonymsForIntervalSeparator works", {
  getSynonymsForIntervalSeparator <- dembase:::getSynonymsForIntervalSeparator
  expect_that(getSynonymsForIntervalSeparator(),
              is_identical_to(c("-", "to")))
})

test_that("getSynonymsForOpenIntervalSymbol works", {
  getSynonymsForOpenIntervalSymbol <- dembase:::getSynonymsForOpenIntervalSymbol
  expect_true("+" %in% getSynonymsForOpenIntervalSymbol())
  expect_true("andover" %in% getSynonymsForOpenIntervalSymbol(which = "final"))
  expect_true("<" %in% getSynonymsForOpenIntervalSymbol(which = "firstLeft"))
  expect_true("orless" %in% getSynonymsForOpenIntervalSymbol("firstRight"))
  expect_error(getSynonymsForOpenIntervalSymbol(which = "wrong"),
               sprintf("'arg' should be one of %s, %s, %s",
                       dQuote("final"), dQuote("firstLeft"), dQuote("firstRight")))
})

test_that("getUniqueDimtypes works", {
  getUniqueDimtypes <- dembase:::getUniqueDimtypes
  expect_false("sex" %in% getUniqueDimtypes())
  expect_true("time" %in% getUniqueDimtypes())
  expect_false("origin" %in% getUniqueDimtypes())
})

test_that("getValidDimtypes works", {
  getValidDimtypes <- dembase:::getValidDimtypes
  expect_true("state" %in% getValidDimtypes())
  expect_true("sex" %in% getValidDimtypes())
  expect_false("class" %in% getValidDimtypes())
  expect_false("pool" %in% getValidDimtypes())
})


## FUNCTIONS TO PREPARE DATA ########################################################

test_that("checkAndTidyYearStart works", {
    checkAndTidyYearStart <- dembase:::checkAndTidyYearStart
    expect_identical(checkAndTidyYearStart(2000),
                     2000L)
    expect_error(checkAndTidyYearStart("5"),
                 "'yearStart' is non-numeric")
    expect_error(checkAndTidyYearStart(5:6),
                 "'yearStart' does not have length 1")
    expect_error(checkAndTidyYearStart(as.numeric(NA)),
                 "'yearStart' is missing")
    expect_error(checkAndTidyYearStart(5.5),
                 "'yearStart' is not an integer")
})

test_that("checkAndTidyDateAndDOB", {
    checkAndTidyDateAndDOB <- dembase:::checkAndTidyDateAndDOB
    dob <- as.Date("2000-06-30")
    date <- as.Date(c("2000-06-30", "2000-07-01", "2001-06-29", "2001-06-30",
                      "2001-07-01", "2005-01-01", "2005-12-01"))
    ans.obtained <- checkAndTidyDateAndDOB(date = date, dob = dob)
    ans.expected <- list(date = as.Date(c("2000-06-30", "2000-07-01", "2001-06-29", "2001-06-30",
                                          "2001-07-01", "2005-01-01", "2005-12-01")),
                         dob = as.Date(rep("2000-06-30", 7)))
    expect_identical(ans.obtained, ans.expected)
    dob <- as.Date(c("2000-06-30", "2000-01-02"))
    date <- as.Date("2000-06-30")
    ans.obtained <- checkAndTidyDateAndDOB(date = date, dob = dob)
    ans.expected <- list(date = rep(date, 2),
                         dob = dob)
    expect_identical(ans.obtained, ans.expected)
    expect_error(checkAndTidyDateAndDOB(date = "wrong", dob = dob),
                 "'date' does not have class \"Date\"")
    expect_error(checkAndTidyDateAndDOB(date = as.Date(NA), dob = dob),
                 "all elements of 'date' are missing")
    date <- as.Date(rep("2001-06-30", 7))
    dob <- as.Date(rep("2000-06-30", 4))
    expect_warning(checkAndTidyDateAndDOB(date = date, dob = dob),
                   "length of 'date' \\[7\\] not a multiple of length of 'dob' \\[4\\]")
    date <- as.Date(rep("2001-06-30", 4))
    dob <- as.Date(rep("2000-06-30", 7))
    expect_warning(checkAndTidyDateAndDOB(date = date, dob = dob),
                   "length of 'dob' \\[7\\] not a multiple of length of 'date' \\[4\\]")
    expect_error(checkAndTidyDateAndDOB(date = as.Date("2000-01-01"), dob = as.Date("2000-01-02")),
                 "some elements of 'date' are less than the corresponding elements of 'dob'")
})

test_that("checkDate", {
    checkDate <- dembase:::checkDate
    date <- as.Date(c("2000-06-30", "2000-07-01", "2001-06-29", "2001-06-30",
                      "2001-07-01", "2005-01-01", "2005-12-01"))
    expect_null(checkDate(date = date))
    date <- as.Date("2000-06-30")
    expect_null(checkDate(date))
    expect_error(checkDate(date = "wrong"),
                 "'date' does not have class \"Date\"")
    expect_error(checkDate(date = as.Date(NA)),
                 "all elements of 'date' are missing")
})

test_that("checkLastOpen works", {
    checkLastOpen <- dembase:::checkLastOpen
    expect_null(checkLastOpen(TRUE))
    expect_null(checkLastOpen(FALSE))
    expect_error(checkLastOpen("wrong"),
                 "'lastOpen' does not have type \"logical\"")
    expect_error(checkLastOpen(c(TRUE, FALSE),
                             "'lastOpen' does not have length 1"))
    expect_error(checkLastOpen(NA),
                 "'lastOpen' is missing")
})

test_that("completedYears works", {
    completedYears <- dembase:::completedYears
    ans.obtained <- completedYears(date = as.Date("2001-04-03"),
                                         dob = as.Date("2001-01-01"))
    ans.expected <- 0L
    expect_identical(ans.obtained, ans.expected)
    date <- as.Date(c("2000-01-31", "2000-02-01", "2000-12-31", "2001-01-01"))
    dob <- as.Date(c("2000-01-01", "2000-01-01", "2000-01-01", "2000-01-01"))
    ans.obtained <- completedYears(date = date, dob = dob)
    ans.expected <- c(0L, 0L, 0L, 1L)
    expect_identical(ans.expected, ans.obtained)
    ans.obtained <- completedYears(date = as.Date(c("2001-02-28", "2001-02-27", "2001-03-01")),
                                   dob = rep(as.Date("2000-02-29"), 3))
    ans.expected <- c(1L, 0L, 1L)
    expect_identical(ans.obtained, ans.expected)
})

test_that("completedMonths works", {
    completedMonths <- dembase:::completedMonths
    ans.obtained <- completedMonths(date = as.Date("2001-04-03"),
                                         dob = as.Date("2001-01-01"))
    ans.expected <- 3L
    expect_identical(ans.obtained, ans.expected)
    date <- as.Date(c("2000-01-31", "2000-02-01", "2000-12-31", "2001-01-01"))
    dob <- as.Date(c("2000-01-01", "2000-01-01", "2000-01-01", "2000-01-01"))
    ans.obtained <- completedMonths(date = date, dob = dob)
    ans.expected <- c(0L, 1L, 11L, 12L)
    expect_identical(ans.expected, ans.obtained)
    ans.obtained <- completedMonths(date = as.Date(c("2001-02-28", "2001-02-27", "2001-03-01")),
                                         dob = rep(as.Date("2000-02-29"), 3))
    ans.expected <- c(12L, 11L, 12L)
    expect_identical(ans.obtained, ans.expected)
})

test_that("datesToAgeGroups works", {
    dob <- as.Date("2000-06-30")
    date <- as.Date(c("2000-06-30", "2000-07-01", "2001-06-29", "2001-06-30",
                      "2001-07-01", "2005-01-01", "2005-12-01"))
    ans.obtained <- datesToAgeGroups(date = date, dob = dob, lastOpen = FALSE)
    ans.expected <- factor(c("0", 0, 0, 1, 1, 4, 5),
                           levels = as.character(0:5))
    expect_identical(ans.obtained, ans.expected)
    dob <- as.Date(c("2000-06-30", "2000-08-30", "2001-01-01", "2001-12-31"))
    date <- as.Date(c("2002-06-30", "2003-07-01"))
    ans.obtained <- datesToAgeGroups(date = date, dob = dob, lastOpen = FALSE)
    ans.expected <- factor(c("2", "2", "1", "1"),
                           levels = as.character(0:2))
    expect_identical(ans.obtained, ans.expected)
    dob <- as.Date(c(NA, "2000-08-30", "2001-01-01", "2001-12-31"))
    date <- as.Date(c("2002-06-30", "2003-07-01"))
    ans.obtained <- datesToAgeGroups(date = date, dob = dob, lastOpen = FALSE)
    ans.expected <- factor(c(NA, "2", "1", "1"),
                           levels = as.character(0:2))
    expect_identical(ans.obtained, ans.expected)
    ## 5-year intervals
    dob <- as.Date("2000-06-30")
    date <- as.Date(c("2000-06-30", "2000-07-01", "2001-06-29", "2001-06-30",
                      "2001-07-01", "2005-01-01", "2005-12-01"))
    ans.obtained <- datesToAgeGroups(date = date, dob = dob, step = "5 years", lastOpen = TRUE)
    ans.expected <- factor(c("0-4", "0-4", "0-4", "0-4", "0-4", "0-4", "5+"),
                           levels = c("0-4", "5+"))
    expect_identical(ans.obtained, ans.expected)
    ## quarter intervals
    dob <- as.Date("2000-01-01")
    date <- as.Date(c("2000-06-30", "2000-07-01", "2001-06-29", "2001-07-01"))
    ans.obtained <- datesToAgeGroups(date = date, dob = dob, step = "quarter", lastOpen = TRUE)
    ans.expected <- factor(c("0.25-0.5", "0.5-0.75", "1.25-1.5", "1.5+"),
                           levels = c("0-0.25", "0.25-0.5", "0.5-0.75", "0.75-1", "1-1.25", "1.25-1.5", "1.5+"))
    expect_identical(ans.obtained, ans.expected)
})

test_that("datesToCohorts works", {
    dob <- as.Date(c("2000-06-30", "2000-07-01", "2001-06-29", "2001-06-30",
                      "2001-07-01", "2005-01-01", "2005-12-01"))
    ans.obtained <- datesToCohorts(dob = dob)
    ans.expected <- factor(c("2000", 2000, 2001, 2001, 2001, 2005, 2005),
                           levels = as.character(2000:2005))
    expect_identical(ans.obtained, ans.expected)
    dob <- as.Date(c("2000-06-30", "2000-07-01", "2001-07-01"))
    ans.obtained <- datesToCohorts(dob = dob, step = "6 months")
    ans.expected <- factor(c("2000-2000.5", "2000.5-2001", "2001.5-2002"),
                           levels = c("2000-2000.5", "2000.5-2001", "2001-2001.5", "2001.5-2002"))
    expect_identical(ans.obtained, ans.expected)    
    dob <- as.Date(c("2000-06-30", "2000-01-03","2000-07-01"))
    ans.obtained <- datesToCohorts(dob = dob, step = "quarter")
    ans.expected <- factor(c("2000.25-2000.5", "2000-2000.25", "2000.5-2000.75"),
                           levels = c("2000-2000.25", "2000.25-2000.5", "2000.5-2000.75"))
    expect_identical(ans.obtained, ans.expected)    
    dob <- as.Date(c("2000-06-30", "2000-01-03","2000-07-01"))
    ans.obtained <- datesToCohorts(dob = dob, step = "quarter", monthStart = "April")
    ans.expected <- factor(c("2000.25-2000.5", "2000-2000.25", "2000.5-2000.75"),
                           levels = c("2000-2000.25", "2000.25-2000.5", "2000.5-2000.75"))
    expect_identical(ans.obtained, ans.expected)    
})

test_that("datesToPeriods works", {
    date <- as.Date(c("2000-06-30", "2000-07-01", "2001-06-29", "2001-06-30",
                      "2001-07-01", "2005-01-01", "2005-12-01"))
    ans.obtained <- datesToPeriods(date = date)
    ans.expected <- factor(c("2000", 2000, 2001, 2001, 2001, 2005, 2005),
                           levels = as.character(2000:2005))
    expect_identical(ans.obtained, ans.expected)
    date <- as.Date(c("2000-06-30", "2000-07-01", "2001-07-01"))
    ans.obtained <- datesToPeriods(date = date, step = "6 months")
    ans.expected <- factor(c("2000-2000.5", "2000.5-2001", "2001.5-2002"),
                           levels = c("2000-2000.5", "2000.5-2001", "2001-2001.5", "2001.5-2002"))
    expect_identical(ans.obtained, ans.expected)    
    date <- as.Date(c("2000-06-30", "2000-01-03","2000-07-01"))
    ans.obtained <- datesToPeriods(date = date, step = "quarter")
    ans.expected <- factor(c("2000.25-2000.5", "2000-2000.25", "2000.5-2000.75"),
                           levels = c("2000-2000.25", "2000.25-2000.5", "2000.5-2000.75"))
    expect_identical(ans.obtained, ans.expected)    
    date <- as.Date(c("2000-06-30", "2000-01-03","2000-07-01"))
    ans.obtained <- datesToPeriods(date = date, step = "quarter", monthStart = "April")
    ans.expected <- factor(c("2000.25-2000.5", "2000-2000.25", "2000.5-2000.75"),
                           levels = c("2000-2000.25", "2000.25-2000.5", "2000.5-2000.75"))
    expect_identical(ans.obtained, ans.expected)    
})

test_that("datesToTriangles works", {
    dob <- as.Date("2000-06-30")
    date <- as.Date(c("2000-06-30", "2000-07-01", "2001-06-29", "2001-06-30",
                      "2001-07-01", "2005-01-01", "2005-12-01"))
    ans.obtained <- datesToTriangles(date = date, dob = dob)
    ans.expected <- factor(c("TL", "TL", "TU", "TL", "TL", "TU", "TL"),
                           levels = c("TL", "TU"))
    expect_identical(ans.obtained, ans.expected)
    dob <- as.Date(c("2000-06-30", "2000-08-30", "2001-01-01", "2001-12-31"))
    date <- as.Date(c("2002-06-30", "2003-07-01"))
    ans.obtained <- datesToTriangles(date = date, dob = dob)
    ans.expected <- factor(c("TL", "TU", "TL", "TU"),
                           levels = c("TL", "TU"))
    expect_identical(ans.obtained, ans.expected)
    dob <- as.Date(c(NA, "2000-08-30", "2001-01-01", "2001-12-31"))
    date <- as.Date(c("2002-06-30", "2003-07-01"))
    ans.obtained <- datesToTriangles(date = date, dob = dob)
    ans.expected <- factor(c(NA, "TU", "TL", "TU"),
                           levels = c("TL", "TU"))
    expect_identical(ans.obtained, ans.expected)
    ## 5-year intervals
    dob <- as.Date("2000-06-30")
    date <- as.Date(c("2000-06-30", "2000-07-01", "2001-06-29", "2001-06-30",
                      "2001-07-01", "2005-01-01", "2005-12-01"))
    ans.obtained <- datesToTriangles(date = date, dob = dob, step = "5 years")
    ans.expected <- factor(c("TL", "TL", "TL", "TL", "TL", "TU", "TL"),
                           levels = c("TL", "TU"))
    expect_identical(ans.obtained, ans.expected)
})

test_that("iIntervalSinceBirthYears works", {
    iIntervalSinceBirthYears <- dembase:::iIntervalSinceBirthYears
    date <- as.Date(c("2001-10-03", "2006-10-01", "2000-12-13", "2005-01-01"))
    dob <- as.Date(rep("2000-01-01", 4))
    ans.obtained <- iIntervalSinceBirthYears(date = date,
                                            dob = dob,
                                            stepNum = 1L,
                                            monthStartNum = 1L,
                                            yearStart = 2000L)
    ans.expected <- c(2L, 7L, 1L, 6L)
    expect_identical(ans.obtained, ans.expected)
    date <- as.Date(c("2001-10-03", "2006-10-01", "2000-12-13", "2005-01-01"))
    dob <- as.Date(rep("2000-01-01", 4))
    ans.obtained <- iIntervalSinceBirthYears(date = date,
                                            dob = dob,
                                            stepNum = 5L,
                                            monthStartNum = 1L,
                                            yearStart = 2000L)
    ans.expected <- c(1L, 2L, 1L, 2L)
    expect_identical(ans.obtained, ans.expected)
    date <- as.Date(c("2001-10-03", "2006-10-01", "2000-12-13", "2005-01-01"))
    dob <- as.Date(rep("2000-01-01", 4))
    ans.obtained <- iIntervalSinceBirthYears(date = date,
                                            dob = dob,
                                            stepNum = 1L,
                                            monthStartNum = 7L,
                                            yearStart = 2000L)
    ans.expected <- c(3L, 8L, 2L, 6L)
    expect_identical(ans.obtained, ans.expected)
    dob <- as.Date(c(NA, "2000-08-30"))
    date <- as.Date(c("2002-06-30", "2003-07-01"))
    ans.obtained <- iIntervalSinceBirthYears(date = date,
                                            dob = dob,
                                            stepNum = 1L,
                                            monthStartNum = 1L,
                                            yearStart = 2000L)
    ans.expected <- c(NA, 4L)
    expect_identical(ans.obtained, ans.expected)
})

test_that("iIntervalSinceBirthMonths works", {
    iIntervalSinceBirthMonths <- dembase:::iIntervalSinceBirthMonths
    date <- as.Date(c("2001-10-03", "2000-10-01", "2000-12-13", "2005-01-01"))
    dob <- as.Date(rep("2000-01-01", 4))
    ans.obtained <- iIntervalSinceBirthMonths(date = date,
                                              dob = dob,
                                              stepNum = 1L)
    ans.expected <- c(22L, 10L, 12L, 61L)
    expect_identical(ans.obtained, ans.expected)
    date <- as.Date(c("2001-10-03", "2000-10-01", "2000-12-13", "2005-01-01"))
    dob <- as.Date(rep("2000-01-01", 4))
    ans.obtained <- iIntervalSinceBirthMonths(date = date,
                                              dob = dob,
                                              stepNum = 6L)
    ans.expected <- c(4L, 2L, 2L, 11L)
    expect_identical(ans.obtained, ans.expected)
    date <- as.Date(c("2001-10-03", "2000-10-01", "2000-12-13", "2005-01-01"))
    dob <- as.Date(rep("2000-01-01", 4))
    ans.obtained <- iIntervalSinceBirthMonths(date = date,
                                              dob = dob,
                                              stepNum = 3L)
    ans.expected <- c(8L, 4L, 4L, 21L)
    expect_identical(ans.obtained, ans.expected)
    date <- as.Date(c("2001-10-03", "2000-10-01", "2000-12-13", "2005-01-01"))
    dob <- as.Date(rep("2000-01-01", 4))
    ans.obtained <- iIntervalSinceBirthMonths(date = date,
                                              dob = dob,
                                              stepNum = 3L)
    ans.expected <- c(8L, 4L, 4L, 21L)
    expect_identical(ans.obtained, ans.expected)
    dob <- as.Date(c(NA, "2000-08-30"))
    date <- as.Date(c("2002-06-30", "2003-07-01"))
    ans.obtained <- iIntervalSinceBirthMonths(date = date,
                                              dob = dob,
                                              stepNum = 3L)
    ans.expected <- c(NA, 13L)
    expect_identical(ans.obtained, ans.expected)
})

test_that("isLeapYear works", {
    isLeapYear <- dembase:::isLeapYear
    ans.obtained <- isLeapYear(c(1999, 2000, 2100, 2004, 2003))
    ans.expected <- c(FALSE, TRUE, FALSE, TRUE, FALSE)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeDateVecYears works", {
    makeDateVecYears <- dembase:::makeDateVecYears
    ## one year, starting 1 Jan
    ans.obtained <- makeDateVecYears(dates = c(as.Date("2017-05-04"), as.Date("2001-04-03")),
                                     stepNum = 1L,
                                     monthStartNum = 1L,
                                     yearStart = 2000L)
    ans.expected <- seq(from = as.Date("2001-01-01"),
                        to = as.Date("2018-01-01"),
                        by = "1 year")
    expect_identical(ans.obtained, ans.expected)
    ## change start date
    ans.obtained <- makeDateVecYears(dates = c(as.Date("2017-05-04"), as.Date("2001-04-03")),
                                     stepNum = 1L,
                                     monthStartNum = 1L,
                                     yearStart = 2010L)
    ans.expected <- seq(from = as.Date("2001-01-01"),
                        to = as.Date("2018-01-01"),
                        by = "1 year")
    expect_identical(ans.obtained, ans.expected)
    ## 5 years
    ans.obtained <- makeDateVecYears(date = c(as.Date("2017-05-04"), as.Date("2001-04-03")),
                                     stepNum = 5L,
                                     monthStartNum = 1L,
                                     yearStart = 2000L)
    ans.expected <- seq(from = as.Date("2000-01-01"),
                        to = as.Date("2020-01-01"),
                        by = "5 years")
    expect_identical(ans.obtained, ans.expected)
    ## 5 years, start in 2001
    ans.obtained <- makeDateVecYears(date = c(as.Date("2017-05-04"), as.Date("2001-04-03")),
                                     stepNum = 5L,
                                     monthStartNum = 1L,
                                     yearStart = 2001L)
    ans.expected <- seq(from = as.Date("2001-01-01"),
                        to = as.Date("2021-01-01"),
                        by = "5 years")
    expect_identical(ans.obtained, ans.expected)
    ## 5 years, start in July 2001
    ans.obtained <- makeDateVecYears(date = c(as.Date("2017-05-04"), as.Date("2001-04-03")),
                                     stepNum = 5L,
                                     monthStartNum = 7L,
                                     yearStart = 2001L)
    ans.expected <- seq(from = as.Date("1996-07-01"),
                        to = as.Date("2021-07-01"),
                        by = "5 years")
    expect_identical(ans.obtained, ans.expected)
    ## 5 years, start in 1 July 2001 - single observation
    ans.obtained <- makeDateVecYears(date = as.Date("2016-07-01"),
                                     stepNum = 5L,
                                     monthStartNum = 7L,
                                     yearStart = 2001L)
    ans.expected <- seq(from = as.Date("2016-07-01"),
                        to = as.Date("2021-07-01"),
                        by = "5 years")
    expect_identical(ans.obtained, ans.expected)
    ## 5 years, start in 1 July 2001 - single observation
    ans.obtained <- makeDateVecYears(date = as.Date("2016-06-30"),
                                     stepNum = 5L,
                                     monthStartNum = 7L,
                                     yearStart = 2001L)
    ans.expected <- seq(from = as.Date("2011-07-01"),
                        to = as.Date("2016-07-01"),
                        by = "5 years")
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeDateVecMonths works", {
    makeDateVecMonths <- dembase:::makeDateVecMonths
    ## 1 month
    ans.obtained <- makeDateVecMonths(dates = c(as.Date("2017-05-04"), as.Date("2001-04-03")),
                                      stepNum = 1L)
    ans.expected <- seq(from = as.Date("2001-04-01"),
                        to = as.Date("2017-06-01"),
                        by = "month")
    expect_identical(ans.obtained, ans.expected)
    ## 1 quarter
    ans.obtained <- makeDateVecMonths(date = c(as.Date("2017-05-04"), as.Date("2001-04-03")),
                                      stepNum = 3L)
    ans.expected <- seq(from = as.Date("2001-04-01"),
                        to = as.Date("2017-07-01"),
                        by = "quarter")
    expect_identical(ans.obtained, ans.expected)
    ## 1 quarter
    ans.obtained <- makeDateVecMonths(date = c(as.Date("2017-06-30"), as.Date("2001-04-01"),
                                               as.Date("2018-11-24")),
                                      stepNum = 3L)
    ans.expected <- seq(from = as.Date("2001-04-01"),
                        to = as.Date("2019-01-01"),
                        by = "quarter")
    expect_identical(ans.obtained, ans.expected)
    ## 20 February 2000
    ans.obtained <- makeDateVecMonths(date = as.Date("2000-02-29"),
                                      step = 1L)
    ans.expected <- seq(from = as.Date("2000-02-01"),
                        to = as.Date("2000-03-01"),
                        by = "1 month")
    expect_identical(ans.obtained, ans.expected)
    ## date 31 December
    ans.obtained <- makeDateVecMonths(date = c(as.Date("2017-12-31"), as.Date("2001-04-03")),
                                      step = 2L)
    ans.expected <- seq(from = as.Date("2001-03-01"),
                        to = as.Date("2018-01-01"),
                        by = "2 months")
    expect_identical(ans.obtained, ans.expected)
    ## 6 months
    ans.obtained <- makeDateVecMonths(date = c(as.Date("2017-12-31"), as.Date("2001-04-03")),
                                      step = 6L)
    ans.expected <- seq(from = as.Date("2001-01-01"),
                        to = as.Date("2018-01-01"),
                        by = "6 months")
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeAgeLabels works", {
    makeAgeLabels <- dembase:::makeAgeLabels
    ans.obtained <- makeAgeLabels(stepNum = 1L,
                                  stepUnit = "years",
                                  nAgeInterval = 3,
                                  lastOpen = TRUE)
    ans.expected <- c("0", "1", "2+")
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- makeAgeLabels(stepNum = 1L,
                                  stepUnit = "years",
                                  nAgeInterval = 3,
                                  lastOpen = FALSE)
    ans.expected <- c("0", "1", "2")
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- makeAgeLabels(stepNum = 5L,
                                  stepUnit = "years",
                                  nAgeInterval = 3,
                                  lastOpen = TRUE)
    ans.expected <- c("0-4", "5-9", "10+")
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- makeAgeLabels(stepNum = 1L,
                                  stepUnit = "years",
                                  nAgeInterval = 4,
                                  lastOpen = FALSE)
    ans.expected <- c("0", "1", "2", "3")
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- makeAgeLabels(stepNum = 3L,
                                  stepUnit = "months",
                                  nAgeInterval = 4,
                                  lastOpen = FALSE)
    ans.expected <- c("0-0.25", "0.25-0.5", "0.5-0.75", "0.75-1")
    expect_identical(ans.obtained, ans.expected)    
    ans.obtained <- makeAgeLabels(stepNum = 6L,
                                  stepUnits = "months",
                                  nAgeInterval = 4,
                                  lastOpen = TRUE)
    ans.expected <- c("0-0.5", "0.5-1", "1-1.5", "1.5+")
    expect_identical(ans.obtained, ans.expected)    
})

test_that("makePeriodLabelsYears works", {
    makePeriodLabelsYears <- dembase:::makePeriodLabelsYears
    vec <- as.Date(c("2000-01-01", "2001-01-01", "2002-01-01"))
    stepNum <- 1L
    ans.obtained <- makePeriodLabelsYears(dateVec = vec,
                                          stepNum = stepNum)
    ans.expected <- c("2000", "2001")
    expect_identical(ans.obtained, ans.expected)
    vec <- as.Date(c("2000-07-01", "2001-07-01", "2002-07-01"))
    step <- 1L
    ans.obtained <- makePeriodLabelsYears(dateVec = vec,
                                          stepNum = step)
    ans.expected <- c("2001", "2002")
    expect_identical(ans.obtained, ans.expected)
    dateVec <- as.Date(c("2000-07-01", "2005-07-01", "2010-07-01"))
    stepNum <- 5L
    ans.obtained <- makePeriodLabelsYears(dateVec = dateVec,
                                          stepNum = stepNum)
    ans.expected <- c("2001-2005", "2006-2010")
    expect_identical(ans.obtained, ans.expected)
})

test_that("makePeriodLabelsFromVec works with step lengths < 1 year", {
    makePeriodLabelsMonths <- dembase:::makePeriodLabelsMonths
    ## 6 month start on 1 Jan
    dateVec <- as.Date(c("2000-01-01", "2000-07-01", "2001-01-01", "2001-07-01", "2002-01-01"))
    stepNum <- 6L
    ans.obtained <- makePeriodLabelsMonths(dateVec = dateVec,
                                            stepNum = stepNum)
    ans.expected <- c("2000-2000.5", "2000.5-2001", "2001-2001.5", "2001.5-2002")
    expect_identical(ans.obtained, ans.expected)
    ## 6 month start on 1 July
    dateVec <- as.Date(c("2000-07-01", "2001-01-01", "2001-07-01", "2002-01-01"))
    stepNum <- 6L
    ans.obtained <- makePeriodLabelsMonths(dateVec = dateVec,
                                            stepNum = stepNum)
    ans.expected <- c("2000.5-2001", "2001-2001.5", "2001.5-2002")
    expect_identical(ans.obtained, ans.expected)
    ## quarter start on 1 Jan
    dateVec <- as.Date(c("2000-01-01", "2000-04-01", "2000-07-01", "2000-10-01", "2001-01-01"))
    stepNum <- 3L
    ans.obtained <- makePeriodLabelsMonths(dateVec = dateVec,
                                            stepNum = stepNum)
    ans.expected <- c("2000-2000.25", "2000.25-2000.5", "2000.5-2000.75", "2000.75-2001")
    expect_identical(ans.obtained, ans.expected)
    ## month start on 1 Feb
    dateVec <- as.Date(c("2000-02-01", "2000-03-01", "2000-04-01", "2000-05-01"))
    stepNum <- 1L
    ans.obtained <- makePeriodLabelsMonths(dateVec = dateVec,
                                            stepNum = stepNum)
    ans.expected <- c("2000.0833-2000.1667", "2000.1667-2000.25", "2000.25-2000.3333")
    expect_identical(ans.obtained, ans.expected)
})


test_that("makeStepUnitsAndStepNum works", {
    makeStepUnitsAndStepNum <- dembase:::makeStepUnitsAndStepNum
    expect_identical(makeStepUnitsAndStepNum("year"),
                     list(stepUnits = "years", stepNum = 1L))
    expect_identical(makeStepUnitsAndStepNum("5 years"),
                     list(stepUnits = "years", stepNum = 5L))
    expect_identical(makeStepUnitsAndStepNum("2 mon"),
                     list(stepUnits = "months", stepNum = 2L))
    expect_identical(makeStepUnitsAndStepNum("1 quarter"),
                     list(stepUnits = "months", stepNum = 3L))
    expect_identical(makeStepUnitsAndStepNum("2 q"),
                     list(stepUnits = "months", stepNum = 6L))
    expect_identical(makeStepUnitsAndStepNum("4 qu"),
                     list(stepUnits = "months", stepNum = 12L))
    expect_error(makeStepUnitsAndStepNum(5),
                 "'step' does not have type \"character\"")
    expect_error(makeStepUnitsAndStepNum(c("year", "month")),
                 "'step' does not have length 1")
    expect_error(makeStepUnitsAndStepNum(as.character(NA)),
                 "'step' is missing")
    expect_error(makeStepUnitsAndStepNum("1 years months"),
                 "invalid value for 'step'")
    expect_error(makeStepUnitsAndStepNum("1 week"),
                 "invalid value for 'step' : invalid units")
    expect_error(makeStepUnitsAndStepNum("2.5 years"),
                 "invalid value for 'step' : non-integer number of units")
    expect_error(makeStepUnitsAndStepNum("0 years"),
                 "invalid value for 'step' : non-positive number of units")
    expect_error(makeStepUnitsAndStepNum("5 months"),
                 "invalid value for 'step' : one year cannot be divided into intervals of length \"5 months\"")
    expect_error(makeStepUnitsAndStepNum("3 quarters"),
                 "invalid value for 'step' : one year cannot be divided into intervals of length \"3 quarters\"")
})

test_that("monthStartNum works", {
    monthStartNum <- dembase:::monthStartNum
    expect_identical(monthStartNum("January"),
                     1L)
    expect_identical(monthStartNum("Mar"),
                     3L)
    expect_error(monthStartNum(5),
                 "'monthStart' does not have type \"character\"")
    expect_error(monthStartNum(c("Jan", "Feb")),
                 "'monthStart' does not have length 1")
    expect_error(monthStartNum(as.character(NA)),
                 "'monthStart' is missing")
    expect_error(monthStartNum("wrong"),
                 "invalid value for 'monthStart' : \"wrong\" is not a valid month")
})




## FUNCTION FOR PROCESSING DIMENSION NAMES AND INDICES ###############################

test_that("checkAndTidyAlong works", {
    checkAndTidyAlong <- dembase:::checkAndTidyAlong
    metadata <- new("MetaData",
                    nms = c("age", "sex"),
                    dimtypes = c("age", "state"),
                    DimScales = list(new("Points", dimvalues = 0:4),
                    new("Categories", dimvalues = c("a", "b"))))
    expect_identical(checkAndTidyAlong(along = 1,
                                       metadata = metadata,
                                       numericDimScales = TRUE),
                     1L)
    metadata <- new("MetaData",
                    nms = c("age", "sex"),
                    dimtypes = c("age", "state"),
                    DimScales = list(new("Points", dimvalues = 0:4),
                    new("Categories", dimvalues = c("a", "b"))))
    expect_identical(checkAndTidyAlong("age",
                                       metadata = metadata,
                                       numericDimScales = TRUE),
                     1L)
    metadata <- new("MetaData",
                    nms = c("age", "sex"),
                    dimtypes = c("age", "state"),
                    DimScales = list(new("Points", dimvalues = 0:4),
                    new("Categories", dimvalues = c("a", "b"))))
    expect_identical(checkAndTidyAlong(NULL,
                                       metadata = metadata,
                                       numericDimScales = TRUE),
                     1L)
    metadata <- new("MetaData",
                    nms = c("age", "time"),
                    dimtypes = c("age", "time"),
                    DimScales = list(new("Points", dimvalues = 0:4),
                    new("Points", dimvalues = 1:10)))
    expect_identical(checkAndTidyAlong(NULL,
                                       metadata = metadata,
                                       numericDimScales = TRUE),
                     2L)
    metadata <- new("MetaData",
                    nms = c("time", "sex"),
                    dimtypes = c("time", "state"),
                    DimScales = list(new("Points", dimvalues = 0:4),
                    new("Categories", dimvalues = c("a", "b"))))
    expect_identical(checkAndTidyAlong(NULL,
                                       metadata = metadata,
                                       numericDimScales = TRUE),
                     1L)
    metadata <- new("MetaData",
                    nms = c("cohort", "sex"),
                    dimtypes = c("cohort", "state"),
                    DimScales = list(new("Intervals", dimvalues = 0:4),
                    new("Categories", dimvalues = c("a", "b"))))
    expect_identical(checkAndTidyAlong(NULL,
                                       metadata = metadata,
                                       numericDimScales = TRUE),
                     1L)
    metadata <- new("MetaData",
                    nms = "age",
                    dimtypes = "age",
                    DimScales = list(new("Intervals", dimvalues = 0:4)))
    expect_identical(checkAndTidyAlong(NULL,
                                       metadata = metadata,
                                       numericDimScales = TRUE),
                     1L)
    expect_identical(checkAndTidyAlong(NULL,
                                       metadata = metadata,
                                       numericDimScales = TRUE),
                     1L)
    metadata <- new("MetaData",
                    nms = c("iteration", "sex"),
                    dimtypes = c("iteration", "state"),
                    DimScales = list(new("Iterations", dimvalues = 1:4),
                    new("Categories", dimvalues = c("a", "b"))))
    expect_error(checkAndTidyAlong(NULL, metadata = metadata, numericDimScales = TRUE),
                 "no 'along' argument supplied but no dimension with dimtype \"time\", \"age\", or \"cohort\"")
    metadata <- new("MetaData",
                    nms = c("age", "sex"),
                    dimtypes = c("age", "state"),
                    DimScales = list(new("Points", dimvalues = 0:4),
                    new("Categories", dimvalues = c("a", "b"))))
    expect_error(checkAndTidyAlong(1:2, metadata = metadata, numericDimScales = TRUE),
                 "'along' does not have length 1")
    metadata <- new("MetaData",
                    nms = c("age", "sex"),
                    dimtypes = c("age", "state"),
                    DimScales = list(new("Points", dimvalues = 0:4),
                    new("Categories", dimvalues = c("a", "b"))))
    expect_error(checkAndTidyAlong(NA, metadata = metadata, numericDimScales = TRUE),
                 "'along' is missing")
    metadata <- new("MetaData",
                    nms = c("age", "sex"),
                    dimtypes = c("age", "state"),
                    DimScales = list(new("Points", dimvalues = 0:4),
                    new("Categories", dimvalues = c("a", "b"))))
    expect_error(checkAndTidyAlong("wrong", metadata = metadata, numericDimScales = TRUE),
                 "'along' outside valid range")
    metadata <- new("MetaData",
                    nms = c("age", "iteration"),
                    dimtypes = c("age", "iteration"),
                    DimScales = list(new("Points", dimvalues = 0:4),
                    new("Iterations", dimvalues = 1:10)))
    expect_error(checkAndTidyAlong(2, metadata = metadata, numericDimScales = TRUE),
                 "'along' dimension \\[\"iteration\"\\] has dimtype \"iteration\"")
    metadata <- new("MetaData",
                    nms = c("age", "sex"),
                    dimtypes = c("age", "state"),
                    DimScales = list(new("Points", dimvalues = 0:4),
                    new("Categories", dimvalues = c("a", "b"))))
    expect_error(checkAndTidyAlong("sex", metadata = metadata, numericDimScales = TRUE),
                 "'along' dimension \\[\"sex\"\\] has dimscale \"Categories\"")
    expect_identical(checkAndTidyAlong("sex", metadata = metadata, numericDimScales = FALSE),
                     2L)
})

test_that("checkAndTidyDimColExtCat works", {
    checkAndTidyDimColExtCat <- dembase:::checkAndTidyDimColExtCat
    dimension <- "region"
    names <- c("region", "age")
    DimScales <- list(new("Categories", dimvalues = c("a", "b")),
                      new("Intervals", dimvalues = 0:2))
    expect_identical(checkAndTidyDimColExtCat(dimension = dimension,
                                              names = names,
                                              DimScales = DimScales),
                     1L)
    dimension <- NULL
    names <- c("age", "region")
    DimScales <- list(new("Intervals", dimvalues = 0:2),
                      new("Categories", dimvalues = c("a", "b")))
    expect_identical(checkAndTidyDimColExtCat(dimension = dimension,
                                              names = names,
                                              DimScales = DimScales),
                     2L)
    expect_error(checkAndTidyDimColExtCat(dimension = 1,
                                          names = names,
                                          DimScales = DimScales),
                 "dimension \"age\" has dimscale \"Intervals\"")
})

test_that("checkAndTidyOldNew works", {
    checkAndTidyOldNew <- dembase:::checkAndTidyOldNew
    expect_identical(checkAndTidyOldNew("a",
                                        name = "old",
                                        lengthOne = TRUE),
                     "a")
    expect_identical(checkAndTidyOldNew(c("a", "b"),
                                        name = "old",
                                        lengthOne = FALSE),
                     c("a", "b"))
    expect_identical(checkAndTidyOldNew(1:2,
                                        name = "old",
                                        lengthOne = FALSE),
                     c("1", "2"))
    expect_error(checkAndTidyOldNew(as.character(NA),
                                    name = "old",
                                    lengthOne = TRUE),
                 "'old' has missing values")
    expect_error(checkAndTidyOldNew("",
                                    name = "old",
                                    lengthOne = TRUE),
                 "'old' has blanks")
    expect_error(checkAndTidyOldNew(c("a", "b"),
                                    name = "old",
                                    lengthOne = TRUE),
                 "'old' does not have length 1")
    expect_error(checkAndTidyOldNew(integer(),
                                    name = "new",
                                    lengthOne = FALSE),
                 "'new' has length 0")
    expect_error(checkAndTidyOldNew(c("a", "a"),
                                    name = "new",
                                    lengthOne = FALSE),
                 "'new' has duplicates")
})

test_that("expandNamesSupplied works", {
    expandNamesSupplied <- dembase:::expandNamesSupplied
    expect_identical(expandNamesSupplied(namesSupplied = c("age", "reg", "status_dest"),
                                         namesAll = c("status_orig", "status_dest",
                                         "reg_orig", "reg_dest", "age")),
                     c("age", "reg_orig", "reg_dest", "status_dest"))
    expect_identical(expandNamesSupplied(namesSupplied = c("age", "eth_child", "reg"),
                                         namesAll = c("sex", "eth_parent", "eth_child",
                                         "age", "reg_dest", "reg_orig")),
                     c("age", "eth_child", "reg_orig", "reg_dest"))
    expect_identical(expandNamesSupplied(namesSupplied = character(),
                                         namesAll = character()),
                     character())
    expect_identical(expandNamesSupplied(namesSupplied = "eth",
                                         namesAll = c("eth_child", "eth_parent")),
                     c("eth_parent", "eth_child"))
})

test_that("iFemale Works", {
    DimScale <- new("Sexes", dimvalues = c("f", "m"))
    expect_identical(iFemale(DimScale), 1L)
    DimScale <- new("Sexes", dimvalues = c("male", "female"))
    expect_identical(iFemale(DimScale), 2L)
    DimScale <- new("Sexes", dimvalues = "females")
    expect_identical(iFemale(DimScale), 1L)
    DimScale <- new("Sexes", dimvalues = "males")
    expect_identical(iFemale(DimScale), 0L)
    expect_error(iFemale("wrong"),
                 "'DimScale' has class \"character\"")
})

test_that("iMale Works", {
    DimScale <- new("Sexes", dimvalues = c("f", "m"))
    expect_identical(iMale(DimScale), 2L)
    DimScale <- new("Sexes", dimvalues = c("male", "female"))
    expect_identical(iMale(DimScale), 1L)
    DimScale <- new("Sexes", dimvalues = "females")
    expect_identical(iMale(DimScale), 0L)
    DimScale <- new("Sexes", dimvalues = "males")
    expect_identical(iMale(DimScale), 1L)
    expect_error(iMale("wrong"),
                 "'DimScale' has class \"character\"")
})

test_that("invertSubscript works", {
  invertSubscript <- dembase:::invertSubscript
  expect_that(invertSubscript(subscript = 1:2, nDim = 4L),
              is_identical_to(3:4))
  expect_that(invertSubscript(subscript = integer(), nDim = 4L),
              is_identical_to(1:4))
  expect_that(invertSubscript(subscript = 1:4, nDim = 4L),
              is_identical_to(integer()))
  expect_that(invertSubscript(subscript = "age", nDim = 4L),
              throws_error("'subscript' does not have type \"integer\""))
  expect_that(invertSubscript(subscript = 5L, nDim = 4L),
              throws_error("'subscript' outside valid range"))
})

test_that("orderLabelsNumerically works", {
    orderLabelsNumerically <- dembase:::orderLabelsNumerically
    expect_identical(orderLabelsNumerically(c("0-4", "10+", "5-9")),
                     c("0-4", "5-9", "10+"))
    expect_identical(orderLabelsNumerically(c("0 - 4", "10+", "5 - 9")),
                     c("0 - 4", "5 - 9", "10+"))
    expect_identical(orderLabelsNumerically(c("0+", "<0")),
                     c("<0", "0+"))
    expect_identical(orderLabelsNumerically(c("0-1.3", "-5.2-0", "11.2+", "1.3-11.2")),
                     c("-5.2-0", "0-1.3", "1.3-11.2", "11.2+"))
    expect_identical(orderLabelsNumerically(c("0-4", "10+", "5-9", "a")),
                     c("0-4", "10+", "5-9", "a"))
})

test_that("permuteToMatchIntervalOrPointMetadata works", {
    permuteToMatchIntervalOrPointMetadata <- dembase:::permuteToMatchIntervalOrPointMetadata
    a <- array(1:6,
               dim = c(3, 2),
               dimnames = list(age = c("0-4", "10+", "5-9"), sex = c("m", "f")))
    b <- array(1:6,
               dim = c(3, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"), sex = c("m", "f")))
    m <- new("MetaData",
             nms = c("age", "sex"),
             dimtypes = c("age", "state"),
             DimScales = list(new("Intervals", dimvalues = c(0, 5, 10, Inf)),
             new("Categories", dimvalues = c("m", "f"))))
    expect_identical(permuteToMatchIntervalOrPointMetadata(a, metadata = m),
                     a[c(1,3,2), ])
    expect_identical(permuteToMatchIntervalOrPointMetadata(b, metadata = m),
                     b)
    a <- array(1:6,
               dim = c(3, 2),
               dimnames = list(age = c("0 - 4", "10+", "5 - 9"), sex = c("m", "f")))
    b <- array(1:6,
               dim = c(3, 2),
               dimnames = list(age = c("0 - 4", "5 - 9", "10+"), sex = c("m", "f")))
    m <- new("MetaData",
             nms = c("age", "sex"),
             dimtypes = c("age", "state"),
             DimScales = list(new("Intervals", dimvalues = c(0, 5, 10, Inf)),
             new("Categories", dimvalues = c("m", "f"))))
    expect_identical(permuteToMatchIntervalOrPointMetadata(a, metadata = m),
                     a[c(1,3,2), ])
    expect_identical(permuteToMatchIntervalOrPointMetadata(b, metadata = m),
                     b)
    a <- array(1:6,
               dim = c(3, 2),
               dimnames = list(age = c("0-4", "10+", "5-9"), time = c(2005, 2000)))
    m <- new("MetaData",
             nms = c("age", "time"),
             dimtypes = c("age", "time"),
             DimScales = list(new("Intervals", dimvalues = c(0, 5, 10, Inf)),
             new("Points", dimvalues = c(2000, 2005))))
    expect_identical(permuteToMatchIntervalOrPointMetadata(a, metadata = m),
                     a[c(1,3,2), 2:1])
})

test_that("makeDimensionSubscript works", {
    makeDimensionSubscript <- dembase:::makeDimensionSubscript
    expect_identical(makeDimensionSubscript(dimension = "age",
                                            margin = NULL,
                                            nDim = 2,
                                            names = c("sex", "age")),
                     2L)
    expect_identical(makeDimensionSubscript(dimension = 2,
                                            margin = NULL,
                                            nDim = 2,
                                            names = c("sex", "age")),
                     2L)
    expect_identical(makeDimensionSubscript(dimension = NULL,
                                            margin = "sex",
                                            nDim = 2,
                                            names = c("sex", "age")),
                     2L)
    expect_identical(makeDimensionSubscript(dimension = NULL,
                                            margin = 1,
                                            nDim = 2,
                                            names = c("sex", "age")),
                     2L)
    expect_error(makeDimensionSubscript(dimension = "age",
                                        margin = 1,
                                        nDim = 2,
                                        names = c("sex", "age")),
                 "has 'dimension' and 'margin' arguments")
    expect_error(makeDimensionSubscript(dimension = NULL,
                                        margin = NULL,
                                        nDim = 2,
                                        names = c("sex", "age")),
                 "no 'dimension' or 'margin' arguments")
    expect_error(makeDimensionSubscript(dimension = "wrong",
                                        margin = NULL,
                                        nDim = 2,
                                        names = c("sex", "age")),
                 sprintf("subscript %s outside valid range",
                         dQuote("wrong")))
    expect_error(makeDimensionSubscript(dimension = NULL,
                                        margin = 100,
                                        nDim = 2,
                                        names = c("sex", "age")),
                 sprintf("subscript %s outside valid range",
                         sQuote('100')))
})

test_that("removeSuffixes works", {
  removeSuffixes <- dembase:::removeSuffixes
  expect_that(removeSuffixes(names = c("reg_dest", "status_orig")),
              is_identical_to(c("reg", "status")))
  expect_that(removeSuffixes(names = c("eth_child", "reg_parent")),
              is_identical_to(c("eth", "reg")))
  expect_that(removeSuffixes(names = character()),
              is_identical_to(character()))
  expect_that(removeSuffixes("age"),
              is_identical_to("age"))
})

test_that("tidySubscript works", {
  tidySubscript <- dembase:::tidySubscript
  expect_that(tidySubscript(subscript = c("sex", "reg"),
                            nDim = 4L,
                            names = c("age", "sex", "reg_orig", "reg_dest")),
              is_identical_to(2:4))
  expect_that(tidySubscript(subscript = c("sex", "reg"),
                            nDim = 4L,
                            names = c("age", "sex", "reg_dest", "reg_orig")),
              is_identical_to(c(2L, 4L, 3L)))
  expect_that(tidySubscript(subscript = c("sex", "age"),
                            nDim = 4L,
                            names = c("age", "sex", "reg_dest", "reg_orig")),
              is_identical_to(2:1))
  expect_that(tidySubscript(subscript = character(),
                            nDim = 4L,
                            names = c("age", "sex", "reg_dest", "reg_orig")),
              is_identical_to(integer()))
  expect_that(tidySubscript(subscript = 2:1,
                            nDim = 4L),
              is_identical_to(2:1))
  expect_that(tidySubscript(subscript = "age",
                            nDim = 4L),
              throws_error("'X' must have named dimnames"))
  expect_that(tidySubscript(subscript = c(1, NA),
                            nDim = 4L),
              throws_error("'subscript' has missing values"))
  expect_error(tidySubscript(subscript = -1,
                            nDim = 4L),
              sprintf("subscript %s outside valid range", sQuote('-1')))
  expect_error(tidySubscript(subscript = "wrong",
                            nDim = 4L,
                            names = c("age", "sex", "reg_dest", "reg_orig")),
              sprintf("subscript %s outside valid range",
                      dQuote("wrong")))
  expect_error(tidySubscript(subscript = c("wrong1", "wrong2"),
                            nDim = 4L,
                            names = c("age", "sex", "reg_dest", "reg_orig")),
              sprintf("subscripts %s, %s outside valid range",
                      dQuote("wrong1"), dQuote("wrong2")))
  expect_error(tidySubscript(subscript = rep("age", 2),
                            nDim = 4,
                            names = c("age", "sex", "reg_dest", "reg_orig")),
              "'subscript' contains duplicates")
  expect_error(tidySubscript(subscript = 5L,
                            nDim = 4L,
                            names = c("age", "sex", "reg_dest", "reg_orig")),
              sprintf("subscript %s outside valid range",
                      sQuote('5')))
  expect_error(tidySubscript(subscript = 5:6,
                            nDim = 4L,
                            names = c("age", "sex", "reg_dest", "reg_orig")),
              sprintf("subscripts %s, %s outside valid range",
                      sQuote('5'), sQuote('6')))
})

## TRANSFORMS ########################################################################


## Functions for making CollapseTransformExtra

test_that("makeInvIndices works", {
    makeInvIndices <- dembase:::makeInvIndices
    ## 3x2 matrix, identical
    indices <- list(1:3, 1:2)
    ans.obtained <- makeInvIndices(indices)
    ans.expected <- list(as.list(1:3), as.list(1:2))
    expect_identical(ans.obtained, ans.expected)
    ## 3x2 matrix, first dimension collapsed
    indices <- list(rep(1L, 3), 1:2)
    ans.obtained <- makeInvIndices(indices)
    ans.expected <- list(list(1:3), as.list(1:2))
    expect_identical(ans.obtained, ans.expected)
    ## 3x2 matrix, first row of first dim dropped, second dimension collapsed
    indices <- list(0:2, c(1L, 1L))
    ans.obtained <- makeInvIndices(indices)
    ans.expected <- list(list(2L, 3L), list(1:2))
    expect_identical(ans.obtained, ans.expected)
    ## 4x3x2 array, second dimension collapsed, fourth row of first dim dropped,
    ## rows of last dimension reversed
    indices <- list(c(1:3, 0L), rep(1L, 3), 2:1)
    ans.obtained <- makeInvIndices(indices)
    ans.expected <- list(as.list(1:3), list(1:3), as.list(2:1))
    expect_identical(ans.obtained, ans.expected)
    ## 4-element vector, first element dropped, second and fourth elements combined
    indices <- list(c(0L, 1L, 2L, 1L))
    ans.obtained <- makeInvIndices(indices)
    ans.expected <- list(list(c(2L, 4L), 3L))
    expect_identical(ans.obtained, ans.expected)
    ## 3x3x3 array, first and last dimensions collapsed
    indices <- list(rep(1L, 3), 1:3, rep(1L, 3))
    ans.obtained <- makeInvIndices(indices)
    ans.expected <- list(list(1:3), list(1L, 2L, 3L), list(1:3))
    expect_identical(ans.obtained, ans.expected)
})

## 'makeCollapseTransformExtra' tested more effectively by tests of
## 'getIAfter' and 'getIShared'
test_that("makeCollapseTransformExtra works", {
    makeCollapseTransformExtra <- dembase:::makeCollapseTransformExtra
    ## 3x2 matrix, first dimension collapsed
    transform <- new("CollapseTransform",
                     indices = list(c(1L, 1L, 1L), 1:2),
                     dims = c(0L, 1L),
                     dimBefore = 3:2,
                     dimAfter = 2L)
    ans.obtained <- makeCollapseTransformExtra(transform)
    ans.expected <- new("CollapseTransformExtra",
                        indices = list(c(1L, 1L, 1L), 1:2),
                        dims = c(0L, 1L),
                        dimBefore = 3:2,
                        dimAfter = 2L,
                        multiplierBefore = c(1L, 3L),
                        multiplierAfter = 1L,
                        invIndices = list(list(1:3), list(1L, 2L)))
    expect_identical(ans.obtained, ans.expected)
    ## 3x2 matrix, first row dropped, then result transposed
    transform <- new("CollapseTransform",
                     indices = list(c(0L, 1L, 2L), 1:2),
                     dims = c(2L, 1L),
                     dimBefore = 3:2,
                     dimAfter = c(2L, 2L))
    ans.obtained <- makeCollapseTransformExtra(transform)
    ans.expected <- new("CollapseTransformExtra",
                        indices = list(c(0L, 1L, 2L), 1:2),
                        dims = c(2L, 1L),
                        dimBefore = 3:2,
                        dimAfter = c(2L, 2L),
                        multiplierBefore = c(1L, 3L),
                        multiplierAfter = c(1L, 2L),
                        invIndices = list(list(2L, 3L), list(1L, 2L)))
    expect_identical(ans.obtained, ans.expected)
})


## Helper functions used by 'getIAfter', 'getIBefore' and 'getIShared'

test_that("R version of posToMar works", {
    posToMar <- dembase:::posToMar
    ans.obtained <- lapply(1:6, posToMar, dim = 3:2)
    ans.expected <- list(c(1L, 1L), c(2L, 1L), c(3L, 1L),
                         c(1L, 2L), c(2L, 2L), c(3L, 2L))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- sapply(1:6, posToMar, dim = 6L)
    ans.expected <- 1:6
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- lapply(1:6, posToMar, dim = c(2L, 1L, 3L))
    ans.expected <- list(c(1L, 1L, 1L), c(2L, 1L, 1L),
                         c(1L, 1L, 2L), c(2L, 1L, 2L),
                         c(1L, 1L, 3L), c(2L, 1L, 3L))
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of posToMar give same answer", {
    posToMar <- dembase:::posToMar
    dim <- c(3L, 1L, 4L, 2L)
    ans.R <- lapply(seq_len(prod(dim)), posToMar, dim = dim, useC = FALSE)
    ans.C <- lapply(seq_len(prod(dim)), posToMar, dim = dim, useC = TRUE)
    expect_identical(ans.R, ans.C)
    dim <- 3L
    ans.R <- lapply(seq_len(prod(dim)), posToMar, dim = dim, useC = FALSE)
    ans.C <- lapply(seq_len(prod(dim)), posToMar, dim = dim, useC = TRUE)
    expect_identical(ans.R, ans.C)
})

test_that("R version of marToPos works", {
    marToPos <- dembase:::marToPos
    dim <-  c(3L, 2L, 5L, 1L)
    margins <- list(c(2L, 1L, 5L, 1L),
                    c(1L, 2L, 3L, 1L),
                    c(3L, 1L, 1L, 1L),
                    c(3L, 2L, 5L, 1L))
    ans.obtained <- sapply(margins, marToPos, multiplier = c(1L, 3L, 6L, 30L))
    ans.expected <- c(26L, 16L, 3L, 30L)
    expect_identical(ans.obtained, ans.expected)
    dim <- 5L
    margins <- c(3L, 1L)
    ans.obtained <- sapply(margins, marToPos, multiplier = 1L)
    ans.expected <- margins
    expect_identical(ans.obtained, ans.expected)
    dim <- c(3L, 1L, 2L)
    a <- array(seq_len(prod(dim)), dim = dim)
    margins <- lapply(seq_along(a),
                      function(i) c(slice.index(a, 1)[i],
                                    slice.index(a, 2)[i],
                                    slice.index(a, 3)[i]))
    ans.obtained <- sapply(margins, marToPos, multiplier = c(1L, 3L, 3L))
    ans.expected <- seq_along(a)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of marToPos give same answer", {
    marToPos <- dembase:::marToPos
    dim <- c(4L, 5L, 3L, 2L)
    multiplier <- c(1L, 4L, 20L, 60L)
    a <- array(seq_len(prod(dim)), dim = dim)
    margins <- lapply(seq_along(a),
                      function(i) c(slice.index(a, 1)[i],
                                    slice.index(a, 2)[i],
                                    slice.index(a, 3)[i],
                                    slice.index(a, 4)[i]))
    ans.R <- sapply(margins, marToPos, multiplier = multiplier, useC = FALSE)
    ans.C <- sapply(margins, marToPos, multiplier = multiplier, useC = TRUE)
    expect_identical(ans.R, ans.C)
    dim <- 6L
    ans.R <- sapply(seq_len(dim), marToPos, multiplier = 1L, useC = FALSE)
    ans.C <- sapply(seq_len(dim), marToPos, multiplier = 1L, useC = TRUE)
    expect_identical(ans.R, ans.C)
})

test_that("R version of marBeforeToMarAfter works", {
    marBeforeToMarAfter <- dembase:::marBeforeToMarAfter
    ## 3x2 matrix, second dim collapsed
    indices <- list(1:3, c(1L, 1L))
    dims <- c(1L, 0L)
    dimAfter <- 3L
    margins <- list(c(1L, 1L), c(2L, 1L), c(3L, 1L),
                    c(1L, 2L), c(2L, 2L), c(3L, 2L))
    ans.obtained <- lapply(margins, marBeforeToMarAfter,
                           indices = indices, dims = dims, dimAfter = dimAfter)
    ans.expected <- c(as.list(1:3), as.list(1:3))
    expect_identical(ans.obtained, ans.expected)
    ## 3x2 matrix, first row dropped, remainder transposed
    indices <- list(0:2, 1:2)
    dims <- c(2L, 1L)
    dimAfter <- c(2L, 2L)
    margins <- list(c(1L, 1L), c(2L, 1L), c(3L, 1L),
                    c(1L, 2L), c(2L, 2L), c(3L, 2L))
    ans.obtained <- lapply(margins, marBeforeToMarAfter,
                           indices = indices, dims = dims, dimAfter = dimAfter)
    ans.expected <- list(c(0L, 0L), c(1L, 1L), c(1L, 2L),
                         c(0L, 0L), c(2L, 1L), c(2L, 2L))
    expect_identical(ans.obtained, ans.expected)
    ## 4x3x2 array, first two rows combined, first and last dimensions permuted
    indices <- list(c(1L, 1L, 2L, 3L), 1:3, 1:2)
    dims <- c(3L, 2L, 1L)
    dimAfter <- c(2L, 3L, 3L)
    ans.obtained <- marBeforeToMarAfter(c(1L, 1L, 1L), indices = indices,
                                        dims = dims, dimAfter = dimAfter)
    ans.expected <- c(1L, 1L, 1L)
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- marBeforeToMarAfter(c(3L, 2L, 1L), indices = indices,
                                        dims = dims, dimAfter = dimAfter)
    ans.expected <- c(1L, 2L, 2L)
    expect_identical(ans.obtained, ans.expected)
    ## length-4 vector, elements 2 and 3 combined, last element dropped
    indices <- list(c(1L, 2L, 2L, 0L))
    dims <- 1L
    dimAfter <- 2L
    ans.obtained <- sapply(1:4, marBeforeToMarAfter, indices = indices,
                           dims = dims, dimAfter = dimAfter)
    ans.expected <- c(1L, 2L, 2L, 0L)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of marBeforeToMarAfter give same answer", {
    marBeforeToMarAfter <- dembase:::marBeforeToMarAfter
    ## 4x5x1x2 array, first 3 rows and last 2 rows of dim 2 collapsed; dim 3 dropped
    indices <- list(1:4, c(1L, 1L, 1L, 2L, 2L), 1L, 1:2)
    dims <- c(1L, 2L, 0L, 3L)
    dimBefore <- c(4L, 5L, 1L, 2L)
    dimAfter <- c(4L, 2L, 2L)
    a <- array(1:40, dim = dimBefore)
    margins <- lapply(seq_along(a),
                      function(i) c(slice.index(a, 1)[i],
                                    slice.index(a, 2)[i],
                                    1L,
                                    slice.index(a, 4)[i]))
    ans.R <- lapply(margins, marBeforeToMarAfter, indices = indices,
                    dims = dims, dimAfter = dimAfter, useC = FALSE)
    ans.C <- lapply(margins, marBeforeToMarAfter, indices = indices,
                    dims = dims, dimAfter = dimAfter, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## 3x3x3 array, second dimension collapsed, first row of third dimension dropped
    indices <- list(1:3, c(1L, 1L, 1L), c(0L, 1L, 2L))
    dims <- c(1L, 0L, 2L)
    dimBefore <- c(3L, 3L, 3L)
    dimAfter <- c(3L, 2L)
    a <- array(1:27, dim = dimBefore)
    margins <- lapply(seq_along(a),
                      function(i) c(slice.index(a, 1)[i],
                                    slice.index(a, 2)[i],
                                    slice.index(a, 3)[i]))
    ans.R <- lapply(margins, marBeforeToMarAfter, indices = indices,
                    dims = dims, dimAfter = dimAfter, useC = FALSE)
    ans.C <- lapply(margins, marBeforeToMarAfter, indices = indices,
                    dims = dims, dimAfter = dimAfter, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## length-4 vector, elements 2 and 3 combined, last element dropped
    indices <- list(c(1L, 2L, 2L, 0L))
    dims <- 1L
    dimAfter <- 2L
    ans.R <- sapply(1:4, marBeforeToMarAfter, indices = indices,
                    dims = dims, dimAfter = dimAfter, useC = FALSE)
    ans.C <- sapply(1:4, marBeforeToMarAfter, indices = indices,
                    dims = dims, dimAfter = dimAfter, useC = TRUE)
    expect_identical(ans.R, ans.C)
})

test_that("R version of marAfterToPosBefore works", {
    marAfterToPosBefore <- dembase:::marAfterToPosBefore
    marBeforeToMarAfter <- dembase:::marBeforeToMarAfter
    ## 3x2 matrix, nothing changed
    dims <- 1:2
    multiplierBefore <- c(1L, 3L)
    invIndices <- list(list(1L, 2L, 3L), list(1L, 2L))
    margins <- list(c(1L, 1L), c(3L, 1L), c(1L, 2L), c(2L, 2L))
    ans.obtained <- sapply(margins, marAfterToPosBefore,
                           dims = dims, multiplierBefore = multiplierBefore,
                           invIndices = invIndices)
    ans.expected <- c(1L, 3L, 4L, 5L)
    expect_identical(ans.obtained, ans.expected)
    ## 3x2 matrix, first dimension collapsed
    dims <- c(0L, 1L)
    multiplierBefore <- c(1L, 3L)
    invIndices <- list(list(1:3), list(1L, 2L))
    margins <- 1:2
    ans.obtained <- lapply(1:2, marAfterToPosBefore,
                           dims = dims, multiplierBefore = multiplierBefore,
                           invIndices = invIndices)
    ans.expected <- list(1:3, 4:6)
    expect_identical(ans.obtained, ans.expected)
    ## 3x2 matrix transposed
    dims <- 2:1
    multiplierBefore <- c(1L, 3L)
    invIndices <- list(list(1L, 2L, 3L), list(1L, 2L))
    margins <- list(c(1L, 1L), c(2L, 3L), c(2L, 1L))
    ans.obtained <- sapply(margins, marAfterToPosBefore,
                           dims = dims, multiplierBefore = multiplierBefore,
                           invIndices = invIndices)
    ans.expected <- c(1L, 6L, 4L)
    expect_identical(ans.obtained, ans.expected)
    ## 4x3x1 array, second dimension collapsed
    dims <- c(1L, 0L, 2L)
    multiplierBefore <- c(1L, 4L, 12L)
    invIndices <- list(list(1L, 2L, 3L, 4L), list(1:3), list(1L, 2L))
    margins <- list(c(1L, 1L), c(2L, 1L), c(3L, 1L), c(4L, 1L))
    ans.obtained <- lapply(margins, marAfterToPosBefore,
                           dims = dims, multiplierBefore = multiplierBefore,
                           invIndices = invIndices)
    ans.expected <- list(c(1L, 5L, 9L), c(2L, 6L, 10L), c(3L, 7L, 11L), c(4L, 8L, 12L))
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of marAfterToPosBefore give same answer", {
    marAfterToPosBefore <- dembase:::marAfterToPosBefore
    marBeforeToMarAfter <- dembase:::marBeforeToMarAfter
    ## 4x5x1x2 array, first 3 rows and last 2 rows of dim 2 collapsed; dim 3 dropped
    indices <- list(1:4, c(1L, 1L, 1L, 2L, 2L), 1L, 1:2)
    dims <- c(1L, 2L, 0L, 3L)
    dimBefore <- c(4L, 5L, 1L, 2L)
    dimAfter <- c(4L, 2L, 2L)
    multiplierBefore <- c(1L, 4L, 20L, 20L)
    invIndices <- list(list(1L, 2L, 3L, 4L), list(1:3, 4:5), list(1L), list(1L, 2L))
    a <- array(1:40, dim = dimBefore)
    margins <- lapply(seq_along(a),
                      function(i) c(slice.index(a, 1)[i],
                                    slice.index(a, 2)[i],
                                    1L,
                                    slice.index(a, 4)[i]))
    margins <- lapply(margins, marBeforeToMarAfter, indices = indices,
                      dims = dims, dimAfter = dimAfter)
    ans.R <- lapply(margins, marAfterToPosBefore, dims = dims,
                    multiplierBefore = multiplierBefore,
                    invIndices = invIndices, useC = FALSE)
    ans.C <- lapply(margins, marAfterToPosBefore, dims = dims,
                    multiplierBefore = multiplierBefore,
                    invIndices = invIndices, useC = TRUE)
    expect_identical(ans.R, ans.C)

    if (0) { ## test does not work - margins returned by marBeforeToMarAfter are all 0's
    ## 3x3x3 array, second dimension collapsed, first row of third dimension dropped
    indices <- list(1:3, c(1L, 1L, 1L), c(0L, 1L, 2L))
    dims <- c(1L, 0L, 2L)
    dimBefore <- c(3L, 3L, 3L)
    dimAfter <- c(3L, 2L)
    multiplierBefore <- c(1L, 3L, 9L)
    invIndices <- list(list(1L, 2L, 3L), list(1:3), list(2:3))
    a <- array(1:27, dim = dimBefore)
    margins <- lapply(seq_along(a),
                      function(i) c(slice.index(a, 1)[i],
                                    slice.index(a, 2)[i],
                                    slice.index(a, 3)[i]))
    margins <- lapply(margins, marBeforeToMarAfter, indices = indices,
                      dims = dims, dimAfter = dimAfter)
    ans.R <- lapply(margins, marAfterToPosBefore, dims = dims,
                    multiplierBefore = multiplierBefore,
                    invIndices = invIndices, useC = FALSE)
    ans.C <- lapply(margins, marAfterToPosBefore, dims = dims,
                    multiplierBefore = multiplierBefore,
                    invIndices = invIndices, useC = TRUE)
    expect_identical(ans.R, ans.C)
    }

    ## 3x2 matrix, first dimension collapsed
    dims <- c(0L, 1L)
    multiplierBefore <- c(1L, 3L)
    invIndices <- list(list(1:3), list(1L, 2L))
    margins <- 1:2
    ans.R <- lapply(margins, marAfterToPosBefore,
                           dims = dims, multiplierBefore = multiplierBefore,
                           invIndices = invIndices, useC = FALSE)
    ans.C <- lapply(margins, marAfterToPosBefore, dims = dims,
                    multiplierBefore = multiplierBefore,
                    invIndices = invIndices, useC = TRUE)
    expect_identical(ans.R, ans.C)

    ## 4x3x1 array, second dimension collapsed
    dims <- c(1L, 0L, 2L)
    multiplierBefore <- c(1L, 4L, 12L)
    invIndices <- list(list(1L, 2L, 3L, 4L), list(1:3), list(1L, 2L))
    margins <- list(c(1L, 1L), c(2L, 1L), c(3L, 1L), c(4L, 1L))
    ans.R <- lapply(margins, marAfterToPosBefore,
                           dims = dims, multiplierBefore = multiplierBefore,
                           invIndices = invIndices, useC = FALSE)
    ans.C <- lapply(margins, marAfterToPosBefore, dims = dims,
                    multiplierBefore = multiplierBefore,
                    invIndices = invIndices, useC = TRUE)
    expect_identical(ans.R, ans.C)

    ## length-4 vector, elements 2 and 3 combined, last element dropped
    indices <- list(c(1L, 2L, 2L, 0L))
    dims <- 1L
    dimAfter <- 2L
    multiplierBefore <- 1L
    invIndices <- list(list(1L, 2:3))
    margins <- 1:2
    ans.R <- lapply(margins, marAfterToPosBefore, dims = dims,
                    multiplierBefore = multiplierBefore,
                    invIndices = invIndices, useC = FALSE)
    ans.C <- lapply(margins, marAfterToPosBefore, dims = dims,
                    multiplierBefore = multiplierBefore,
                    invIndices = invIndices, useC = TRUE)
    expect_identical(ans.R, ans.C)
})

## 'getIAfter', 'getIBefore', 'getIShared', (the functions
##  that other parts of DemographicEstimation use)

test_that("R version of getIAfter works", {
    getIAfter <- dembase:::getIAfter
    collapse <- dembase:::collapse
    makeCollapseTransformExtra <- dembase:::makeCollapseTransformExtra
    ## 3x2 matrix, first dimension collapsed
    transform <- new("CollapseTransform",
                     indices = list(c(1L, 1L, 1L), 1:2),
                     dims = c(0L, 1L),
                     dimBefore = 3:2,
                     dimAfter = 2L)
    transform <- makeCollapseTransformExtra(transform)
    ans.obtained <- sapply(1:6, getIAfter, transform = transform)
    ans.expected <- c(rep(1L, times = 3), rep(2L, times = 3))
    expect_identical(ans.obtained, ans.expected)
    ## 3x2 matrix, first row dropped, then result transposed
    transform <- new("CollapseTransform",
                     indices = list(c(0L, 1L, 2L), 1:2),
                     dims = c(2L, 1L),
                     dimBefore = 3:2,
                     dimAfter = c(2L, 2L))
    transform <- makeCollapseTransformExtra(transform)
    ans.obtained <- sapply(1:6, getIAfter, transform = transform)
    ans.expected <- c(0L, 1L, 3L, 0L, 2L, 4L)
    expect_identical(ans.obtained, ans.expected)
    ## array permuted and subsetted but not collapsed
    A <- array(1:60, dim = c(4,3,5))
    transform <- new("CollapseTransform",
                     indices = list(c(3L, 2L, 0L, 1L), c(1L, 0L, 0L), c(1:4, 0L)),
                     dims = c(2L, 0L, 1L),
                     dimBefore = c(4L, 3L, 5L),
                     dimAfter = c(4L, 3L))
    transform <- makeCollapseTransformExtra(transform)
    B <- collapse(A, transform = transform)
    stopifnot(all(B %in% A))
    stopifnot(!any(duplicated(B)))
    x <- makeCollapseTransformExtra(transform)
    ans.obtained <- sapply(1:60, getIAfter, transform = transform)
    ans.expected <- match(1:60, B, nomatch = 0L)
    expect_identical(ans.obtained, ans.expected)
    ## 4x3x3 array, second dimension collapsed
    transform <- new("CollapseTransform",
                     indices = list(1:4, rep(1L, 3), 1:3),
                     dims = c(1L, 0L, 2L),
                     dimBefore = c(4L, 3L, 3L),
                     dimAfter = c(4L, 3L))
    transform <- makeCollapseTransformExtra(transform)
    ans.obtained <- sapply(1:36, getIAfter, transform = transform)
    ans.expected <- c(rep(1:4, times = 3), rep(5:8, times = 3), rep(9:12, times = 3))
    expect_identical(ans.obtained, ans.expected)
    ## 3x1 matrix, transposed
    transform <- new("CollapseTransform",
                     indices = list(1:3, 1L),
                     dims = 2:1,
                     dimBefore = c(3L, 1L),
                     dimAfter = c(1L, 3L))
    transform <- makeCollapseTransformExtra(transform)
    ans.obtained <- sapply(1:3, getIAfter, transform = transform)
    ans.expected <- 1:3
    expect_identical(ans.obtained, ans.expected)
    ## 4x3x2 array, first two rows combined, then all permuted
    transform <- new("CollapseTransform",
                     indices = list(c(1L, 1L, 2L, 3L), 1:3, 1:2),
                     dims = c(3L, 2L, 1L),
                     dimBefore = 4:2,
                     dimAfter = c(2L, 3L, 3L))
    transform <- makeCollapseTransformExtra(transform)
    ans.obtained <- sapply(1:24, getIAfter, transform = transform)
    ans.expected <- c(1L, 1L, 7L, 13L,
                      3L, 3L, 9L, 15L,
                      5L, 5L, 11L, 17L,
                      2L, 2L, 8L, 14L,
                      4L, 4L, 10L, 16L,
                      6L, 6L, 12L, 18L)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of getIAfter give same answer", {
    getIAfter <- dembase:::getIAfter
    makeCollapseTransformExtra <- dembase:::makeCollapseTransformExtra
    ## 3x2 matrix, first dimension collapsed
    transform <- new("CollapseTransform",
                     indices = list(c(1L, 1L, 1L), 1:2),
                     dims = c(0L, 1L),
                     dimBefore = 3:2,
                     dimAfter = 2L)
    transform <- makeCollapseTransformExtra(transform)
    ans.R <- sapply(1:6, getIAfter, transform = transform, useC = FALSE)
    ans.C <- sapply(1:6, getIAfter, transform = transform, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## 3x2 matrix, first row dropped, then result transposed
    transform <- new("CollapseTransform",
                     indices = list(c(0L, 1L, 2L), 1:2),
                     dims = c(2L, 1L),
                     dimBefore = 3:2,
                     dimAfter = c(2L, 2L))
    transform <- makeCollapseTransformExtra(transform)
    ans.R <- sapply(1:6, getIAfter, transform = transform, useC = FALSE)
    ans.C <- sapply(1:6, getIAfter, transform = transform, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## array permuted and subsetted but not collapsed
    A <- array(1:60, dim = c(4,3,5))
    transform <- new("CollapseTransform",
                     indices = list(c(3L, 2L, 0L, 1L), c(1L, 0L, 0L), c(1:4, 0L)),
                     dims = c(2L, 0L, 1L),
                     dimBefore = c(4L, 3L, 5L),
                     dimAfter = c(4L, 3L))
    transform <- makeCollapseTransformExtra(transform)
    ans.R <- sapply(1:60, getIAfter, transform = transform, useC = FALSE)
    ans.C <- sapply(1:60, getIAfter, transform = transform, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## 4x3x3 array, second dimension collapsed
    transform <- new("CollapseTransform",
                     indices = list(1:4, rep(1L, 3), 1:3),
                     dims = c(1L, 0L, 2L),
                     dimBefore = c(4L, 3L, 3L),
                     dimAfter = c(4L, 3L))
    transform <- makeCollapseTransformExtra(transform)
    ans.R <- sapply(1:36, getIAfter, transform = transform, useC = FALSE)
    ans.C <- sapply(1:36, getIAfter, transform = transform, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## 3x1 matrix, transposed
    transform <- new("CollapseTransform",
                     indices = list(1:3, 1L),
                     dims = 2:1,
                     dimBefore = c(3L, 1L),
                     dimAfter = c(1L, 3L))
    transform <- makeCollapseTransformExtra(transform)
    ans.R <- sapply(1:3, getIAfter, transform = transform, useC = FALSE)
    ans.C <- sapply(1:3, getIAfter, transform = transform, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## 4x3x2 array, first two rows combined, then all permuted
    transform <- new("CollapseTransform",
                     indices = list(c(1L, 1L, 2L, 3L), 1:3, 1:2),
                     dims = c(3L, 2L, 1L),
                     dimBefore = 4:2,
                     dimAfter = c(2L, 3L, 3L))
    transform <- makeCollapseTransformExtra(transform)
    ans.R <- sapply(1:24, getIAfter, transform = transform, useC = FALSE)
    ans.C <- sapply(1:24, getIAfter, transform = transform, useC = TRUE)
    expect_identical(ans.R, ans.C)
})

test_that("R version of getIBefore works", {
    getIBefore <- dembase:::getIBefore
    makeCollapseTransformExtra <- dembase:::makeCollapseTransformExtra
    ## 4x3 matrix, first 3 rows of matrix combined
    transform <- new("CollapseTransform",
                     indices = list(c(1L, 1L, 1L, 2L), 1:3),
                     dims = 1:2,
                     dimBefore = 4:3,
                     dimAfter = 2:3)
    transform <- makeCollapseTransformExtra(transform)
    ans.obtained <- lapply(1:6, getIBefore, transform = transform)
    ans.expected <- list(1:3, 4L, 5:7, 8L, 9:11, 12L)
    expect_identical(ans.obtained, ans.expected)
    ## length 5 vector, 3rd element dropped
    transform <- new("CollapseTransform",
                     indices = list(c(1:2, 0L, 3:4)),
                     dims = 1L,
                     dimBefore = 5L,
                     dimAfter = 4L)
    transform <- makeCollapseTransformExtra(transform)
    ans.obtained <- sapply(1:4, getIBefore, transform = transform)
    ans.expected <- c(1:2, 4:5)
    expect_identical(ans.obtained, ans.expected)
    ## 3x4x5 array, second dimension collapsed
    transform <- new("CollapseTransform",
                     indices = list(1:3, rep(1L, 4), 1:5),
                     dims = c(1L, 0L, 2L),
                     dimBefore = 3:5,
                     dimAfter = c(3L, 5L))
    transform <- makeCollapseTransformExtra(transform)
    ans.obtained <- lapply(1:4, getIBefore, transform = transform)
    ans.expected <- list(c(1L, 4L, 7L, 10L),
                         c(2L, 5L, 8L, 11L),
                         c(3L, 6L, 9L, 12L),
                         c(13L, 16L, 19L, 22L))
    expect_identical(ans.obtained, ans.expected)
    ## collapsed to single cell
    transform <- new("CollapseTransform",
                     indices = list(rep(1L, 3), rep(1L, 4)),
                     dims = c(1L, 0L),
                     dimBefore = c(3L, 4L),
                     dimAfter = 1L)
    transform <- makeCollapseTransformExtra(transform)
    ans.obtained <- getIBefore(1L, transform = transform)
    ans.expected <- 1:12
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of getIBefore give same answer", {
    getIBefore <- dembase:::getIBefore
    makeCollapseTransformExtra <- dembase:::makeCollapseTransformExtra
    ## 1x5x2 array, first and second dimensions permuted, last dimension collapsed
    transform <- new("CollapseTransform",
                     indices = list(1L, 1:5, c(1L, 1L)),
                     dims = c(2L, 1L, 0L),
                     dimBefore = c(1L, 5L, 2L),
                     dimAfter = c(5L, 1L))
    transform <- makeCollapseTransformExtra(transform)
    ans.R <- lapply(1:5, getIBefore, transform = transform, useC = FALSE)
    ans.C <- lapply(1:5, getIBefore, transform = transform, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## 4x2 matrix, first row of matrix dropped, second and fourth rows combined
    transform <- new("CollapseTransform",
                     ##indices = list(1L, 1:5, c(1L, 1L)), ## JAH change
                     indices = list(c(0L, 1L, 2L, 1L), 1:2), ## think this is right!
                     dims = 1:2,
                     dimBefore = c(4L, 2L),
                     dimAfter = c(2L, 2L))
    transform <- makeCollapseTransformExtra(transform)
    ans.R <- lapply(1:4, getIBefore, transform = transform, useC = FALSE)
    ans.C <- lapply(1:4, getIBefore, transform = transform, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## 4x5x3 array, middle dimension collapsed, first and last dimensions permuted
    transform <- new("CollapseTransform",
                     indices = list(1:4, rep(1L, 5), 1:3),
                     dims = c(2L, 0L, 1L),
                     dimBefore = c(4L, 5L, 3L),
                     dimAfter = c(3L, 4L))
    transform <- makeCollapseTransformExtra(transform)
    ans.R <- lapply(1:12, getIBefore, transform = transform, useC = FALSE)
    ans.C <- lapply(1:12, getIBefore, transform = transform, useC = TRUE)
    expect_identical(ans.R, ans.C)
})

test_that("R version of getIShared works", {
    getIShared <- dembase:::getIShared
    makeCollapseTransformExtra <- dembase:::makeCollapseTransformExtra
    collapse <- dembase:::collapse
    ## 3x2 matrix, first dimension collapsed
    transform <- new("CollapseTransform",
                     indices = list(c(1L, 1L, 1L), 1:2),
                     dims = c(0L, 1L),
                     dimBefore = 3:2,
                     dimAfter = 2L)
    transform <- makeCollapseTransformExtra(transform)
    ans.obtained <- lapply(1:6, getIShared, transform = transform)
    ans.expected <- c(rep(list(1:3), times = 3), rep(list(4:6), times = 3))
    expect_identical(ans.obtained, ans.expected)
    ## 3x2 matrix, first row dropped, then result transposed
    transform <- new("CollapseTransform",
                     indices = list(c(0L, 1L, 2L), 1:2),
                     dims = c(2L, 1L),
                     dimBefore = 3:2,
                     dimAfter = c(2L, 2L))
    transform <- makeCollapseTransformExtra(transform)
    ans.obtained <- lapply(1:6, getIShared, transform = transform)
    ans.expected <- list(integer(), 2L, 3L,
                         integer(), 5L, 6L)
    expect_identical(ans.obtained, ans.expected)
    ## array permuted and subsetted but not collapsed
    A <- array(1:60, dim = c(4,3,5))
    transform <- new("CollapseTransform",
                     indices = list(c(3L, 2L, 0L, 1L), c(1L, 0L, 0L), c(1:4, 0L)),
                     dims = c(2L, 0L, 1L),
                     dimBefore = c(4L, 3L, 5L),
                     dimAfter = c(4L, 3L))
    transform <- makeCollapseTransformExtra(transform)
    B <- collapse(A, transform = transform)
    stopifnot(all(B %in% A))
    stopifnot(!any(duplicated(B)))
    x <- makeCollapseTransformExtra(transform)
    ans.obtained <- lapply(1:60, getIShared, transform = transform)
    ans.expected <-c(list(1L), list(2L), list(integer()), list(4L), rep(list(integer()), times = 8),
                     list(13L), list(14L), list(integer()), list(16L), rep(list(integer()), times = 8),
                     list(25L), list(26L), list(integer()), list(28L), rep(list(integer()), times = 8),
                     list(37L), list(38L), list(integer()), list(40L), rep(list(integer()), times = 8),
                     rep(list(integer()), times = 12))
    expect_identical(ans.obtained, ans.expected)
    ## 4x3x3 array, second dimension collapsed
    transform <- new("CollapseTransform",
                     indices = list(1:4, rep(1L, 3), 1:3),
                     dims = c(1L, 0L, 2L),
                     dimBefore = c(4L, 3L, 3L),
                     dimAfter = c(4L, 3L))
    transform <- makeCollapseTransformExtra(transform)
    ans.obtained <- lapply(1:36, getIShared, transform = transform)
    ans.expected <- lapply(c(rep(1:4, 3), rep(13:16, 3), rep(25:28, 3)),
                           function(i) i + c(0L, 4L, 8L))
    expect_identical(ans.obtained, ans.expected)
    ## 3x1 matrix, transposed
    transform <- new("CollapseTransform",
                     indices = list(1:3, 1L),
                     dims = 2:1,
                     dimBefore = c(3L, 1L),
                     dimAfter = c(1L, 3L))
    transform <- makeCollapseTransformExtra(transform)
    ans.obtained <- sapply(1:3, getIShared, transform = transform)
    ans.expected <- 1:3
    expect_identical(ans.obtained, ans.expected)
    ## 4x3x2 array, first two rows combined, then all permuted
    transform <- new("CollapseTransform",
                     indices = list(c(1L, 1L, 2L, 3L), 1:3, 1:2),
                     dims = c(3L, 2L, 1L),
                     dimBefore = 4:2,
                     dimAfter = c(2L, 3L, 3L))
    transform <- makeCollapseTransformExtra(transform)
    ans.obtained <- lapply(1:24, getIShared, transform = transform)
    ans.expected <- list(1:2, 1:2, 3L, 4L, 5:6L, 5:6L, 7L, 8L, 9:10, 9:10, 11L, 12L)
    ans.expected <- c(ans.expected, lapply(ans.expected, function(x) x + 12L))
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of getIShared give same answer", {
    getIShared <- dembase:::getIShared
    makeCollapseTransformExtra <- dembase:::makeCollapseTransformExtra
    ## 3x2 matrix, first dimension collapsed
    transform <- new("CollapseTransform",
                     indices = list(c(1L, 1L, 1L), 1:2),
                     dims = c(0L, 1L),
                     dimBefore = 3:2,
                     dimAfter = 2L)
    transform <- makeCollapseTransformExtra(transform)
    ans.R <- lapply(1:6, getIShared, transform = transform, useC = FALSE)
    ans.C <- lapply(1:6, getIShared, transform = transform, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## 3x2 matrix, first row dropped, then result transposed
    transform <- new("CollapseTransform",
                     indices = list(c(0L, 1L, 2L), 1:2),
                     dims = c(2L, 1L),
                     dimBefore = 3:2,
                     dimAfter = c(2L, 2L))
    transform <- makeCollapseTransformExtra(transform)
    ans.R <- lapply(1:6, getIShared, transform = transform, useC = FALSE)
    ans.C <- lapply(1:6, getIShared, transform = transform, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## array permuted and subsetted but not collapsed
    A <- array(1:60, dim = c(4,3,5))
    transform <- new("CollapseTransform",
                     indices = list(c(3L, 2L, 0L, 1L), c(1L, 0L, 0L), c(1:4, 0L)),
                     dims = c(2L, 0L, 1L),
                     dimBefore = c(4L, 3L, 5L),
                     dimAfter = c(4L, 3L))
    transform <- makeCollapseTransformExtra(transform)
    ans.R <- lapply(1:60, getIShared, transform = transform, useC = FALSE)
    ans.C <- lapply(1:60, getIShared, transform = transform, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## 4x3x3 array, second dimension collapsed
    transform <- new("CollapseTransform",
                     indices = list(1:4, rep(1L, 3), 1:3),
                     dims = c(1L, 0L, 2L),
                     dimBefore = c(4L, 3L, 3L),
                     dimAfter = c(4L, 3L))
    transform <- makeCollapseTransformExtra(transform)
    ans.R <- lapply(1:36, getIShared, transform = transform, useC = FALSE)
    ans.C <- lapply(1:36, getIShared, transform = transform, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## 3x1 matrix, transposed
    transform <- new("CollapseTransform",
                     indices = list(1:3, 1L),
                     dims = 2:1,
                     dimBefore = c(3L, 1L),
                     dimAfter = c(1L, 3L))
    transform <- makeCollapseTransformExtra(transform)
    ans.R <- lapply(1:3, getIShared, transform = transform, useC = FALSE)
    ans.C <- lapply(1:3, getIShared, transform = transform, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## 4x3x2 array, first two rows combined, then all permuted
    transform <- new("CollapseTransform",
                     indices = list(c(1L, 1L, 2L, 3L), 1:3, 1:2),
                     dims = c(3L, 2L, 1L),
                     dimBefore = 4:2,
                     dimAfter = c(2L, 3L, 3L))
    transform <- makeCollapseTransformExtra(transform)
    ans.R <- lapply(1:24, getIShared, transform = transform, useC = FALSE)
    ans.C <- lapply(1:24, getIShared, transform = transform, useC = TRUE)
    expect_identical(ans.R, ans.C)
})

## population <- Counts(array(1:6,
##                            dim = c(3, 2),
##                            dimnames = list(reg = 1:3,
##                                time = 0:1)))
## deaths <- Counts(array(1:12,
##                        dim = c(3, 1),
##                        dimnames = list(reg = 1:3, time = 1)))
## exposure <- Counts(array(1:12,
##                          dim = c(3, 1),
##                          dimnames = list(reg = 1:3, time = 1)))
           

## transform <- new("ExposeTransform",
##                  indices = list(1:3, 1L, 2L),
##                  dims = c(1L, 2L, 2L),
##                  dimBefore = c(3L, 2L),
##                  dimAfter = c(3L, 1L, 2L))


## population <- Counts(array(1:6,
##                            dim = c(3, 2),
##                            dimnames = list(age = c(0, 1, "2+"),
##                                time = 0:1)))
## deaths <- Counts(array(1:12,
##                        dim = c(3, 1, 2),
##                        dimnames = list(age = c(0, 1, "2+"),
##                            time = 1,
##                            triangle = c("TL", "TU"))))
## exposure <- Counts(array(1:12,
##                          dim = c(3, 1, 2),
##                          dimnames = list(age = c(0, 1, "2+"),
##                              time = 1,
##                              triangle = c("TL", "TU"))))

                 
## population <- Counts(array(1:6,
##                            dim = c(3, 2),
##                            dimnames = list(age = c(0, 1, "2+"),
##                                time = 0:1)))
## accession <- Counts(array(0:2,
##                            dim = c(3),
##                            dimnames = list(age = c(0, 1, "2+"),
##                                time = 1)))
## mapping <- new("MappingPopnToAcc",
##                indices = list(c(1L, 2L, 0L), c(1L, 0L)),
##                dims = 1:2,
##                dimBefore = c(3L, 2L),
##                dimAfter = 3L)

test_that("makeMetaDataSubarraysBefore works", {
    makeMetaDataSubarraysBefore <- dembase:::makeMetaDataSubarraysBefore
    x <- Counts(array(1,
                      dim = c(5, 2, 3),
                      dimnames = list(age = c("0-4", "5-9", "10-14", "15-19", "20+"),
                          sex = c("f", "m"),
                          region = c("a", "b", "c"))))
    y <- Counts(array(1,
                      dim = c(2, 3),
                      dimnames = list(age = c("0-9", "10+"),
                          region = c("c", "b", "a"))))
    transform <- makeTransform(x = x, y = y)
    transform <- makeCollapseTransformExtra(transform)
    metadata <- x@metadata
    ans.obtained <- makeMetaDataSubarraysBefore(metadata = metadata,
                                                transform = transform)
    expect_identical(ans.obtained[[1]],
                     new("MetaData",
                         nms = c("age", "sex", "region"),
                         dimtypes = c("age", "sex", "state"),
                         DimScales = list(new("Intervals", dimvalues = c(0, 5, 10)),
                             new("Sexes", dimvalues = c("f", "m")),
                             new("Categories", dimvalues = "c"))))
    expect_identical(ans.obtained[[4]],
                     new("MetaData",
                         nms = c("age", "sex", "region"),
                         dimtypes = c("age", "sex", "state"),
                         DimScales = list(new("Intervals", dimvalues = c(10, 15, 20, Inf)),
                             new("Sexes", dimvalues = c("f", "m")),
                             new("Categories", dimvalues = "b"))))
    x <- Counts(array(1,
                      dim = c(5, 2, 3),
                      dimnames = list(age = c("0-4", "5-9", "10-14", "15-19", "20+"),
                          sex = c("f", "m"),
                          region = c("a", "b", "c"))))
    y <- Counts(array(1,
                      dim = c(1, 1, 2),
                      dimnames = list(age = "0-9",
                          sex = "f",
                          region = c("a", "b"))))
    transform <- makeTransform(x = x, y = y, subset = TRUE)
    transform <- makeCollapseTransformExtra(transform)
    metadata <- x@metadata
    ans.obtained <- makeMetaDataSubarraysBefore(metadata = metadata,
                                                transform = transform)
    expect_identical(ans.obtained[[1]],
                     new("MetaData",
                         nms = c("age", "sex", "region"),
                         dimtypes = c("age", "sex", "state"),
                         DimScales = list(new("Intervals", dimvalues = c(0, 5, 10)),
                             new("Sexes", dimvalues = "f"),
                             new("Categories", dimvalues = "a"))))
    expect_identical(ans.obtained[[2]],
                     new("MetaData",
                         nms = c("age", "sex", "region"),
                         dimtypes = c("age", "sex", "state"),
                         DimScales = list(new("Intervals", dimvalues = c(0, 5, 10)),
                             new("Sexes", dimvalues = "f"),
                             new("Categories", dimvalues = "b"))))
    x <- Counts(array(1,
                      dim = c(5, 2, 3),
                      dimnames = list(age = c("0-4", "5-9", "10-14", "15-19", "20+"),
                          sex = c("f", "m"),
                          region = c("a", "b", "c"))))
    y <- 1
    transform <- makeTransform(x = x, y = y, subset = TRUE)
    transform <- makeCollapseTransformExtra(transform)
    metadata <- x@metadata
    ans.obtained <- makeMetaDataSubarraysBefore(metadata = metadata,
                                                transform = transform)
    expect_identical(ans.obtained,
                     list(metadata))
})

                     


## FUNCTIONS FOR VALIDITY CHECKING #################################################

test_that("checkAge works", {
    checkAge <- dembase:::checkAge
    x <- new("MetaData",
             nms = "age",
             dimtypes = "age",
             DimScales = list(new("Intervals", dimvalues = c(0, 1, 5, 10, Inf))))
    expect_identical(checkAge(x, openRightOK = TRUE),
                     NULL)
    x <- new("MetaData",
             nms = "age",
             dimtypes = "age",
             DimScales = list(new("Points", dimvalues = c(0, 1, 5, 10))))
    expect_identical(checkAge(x, expectedDimscale = "Points"),
                     NULL)
    x <- new("MetaData",
             nms = "age",
             dimtypes = "age",
             DimScales = list(new("Points", dimvalues = numeric())))
    expect_identical(checkAge(x, minAges = 0L, expectedDimscale = "Points"),
                     NULL)
    ## has age dimension
    x <- new("MetaData",
             nms = "region",
             dimtypes = "state",
             DimScales = list(new("Categories", dimvalues = c("a", "b"))))
    expect_error(checkAge(x),
                 "no dimension with dimtype \"age\"")
    ## age dimentions has length 2 or more
    x <- new("MetaData",
             nms = "age",
             dimtypes = "age",
             DimScales = list(new("Points", dimvalues = numeric())))
    expect_error(checkAge(x, expectedDimscale = "Points"),
                 "dimension with dimtype \"age\" has length 0")
    ## regular
    x <- new("MetaData",
             nms = "age",
             dimtypes = "age",
             DimScales = list(new("Intervals", dimvalues = c(0, 1, 5, 10, Inf))))
    expect_error(checkAge(x, regular = TRUE),
                 "dimension with dimtype \"age\" is not regular")
    ## age dimension has expected dimscale
    x <- new("MetaData",
             nms = "age",
             dimtypes = "age",
             DimScales = list(new("Intervals", dimvalues = c(0, 1, 5, 10, Inf))))
    expect_error(checkAge(x, expectedDimscale = "Points"),
                 "dimension with dimtype \"age\" has dimscale \"Intervals\"")
    ## age dimension only open on left or right if permitted
    x <- new("MetaData",
             nms = "age",
             dimtypes = "age",
             DimScales = list(new("Intervals", dimvalues = c(0, 1, 5, 10, Inf))))
    expect_error(checkAge(x),
                 "last age group is open")
    x <- new("MetaData",
             nms = "age",
             dimtypes = "age",
             DimScales = list(new("Intervals", dimvalues = c(-Inf, 0, 1, 5, 10, Inf))))
    expect_error(checkAge(x, openRightOK = TRUE),
                 "first age group is open")
    expect_identical(checkAge(x, openLeftOK = TRUE, openRight = TRUE),
                     NULL)
})

test_that("checkAndTidyNIncrement works", {
    checkAndTidyNIncrement <- dembase:::checkAndTidyNIncrement
    expect_identical(checkAndTidyNIncrement(3),
                     3L)
    expect_identical(checkAndTidyNIncrement(-5),
                     -5L)
    expect_error(checkAndTidyNIncrement("5"),
                 "'n' is non-numeric")
    expect_error(checkAndTidyNIncrement(5:6),
                 "'n' does not have length 1")
    expect_error(checkAndTidyNIncrement(as.numeric(NA)),
                 "'n' is missing")
    expect_error(checkAndTidyNIncrement(5.5),
                 "'n' is not an integer")
    expect_error(checkAndTidyNIncrement(0),
                 "'n' equals 0")
})

test_that("checkDimnames works", {
  checkDimnames <- dembase:::checkDimnames
  expect_that(checkDimnames(list(age = 1, age = 2)),
              throws_error("'names' has duplicates"))
})

test_that("checkDimtypesOrDimscalesArg works", {
    checkDimtypesOrDimscalesArg <- dembase:::checkDimtypesOrDimscalesArg
    expect_identical(checkDimtypesOrDimscalesArg(arg = c(age = "age", sim = "iterations"),
                                                 nameArg = "dimtypes",
                                                 names = c("age", "sex", "sim")),
                     NULL)
    expect_identical(checkDimtypesOrDimscalesArg(arg = c(age = "Intervals", sim = "Iterations"),
                                                 nameArg = "dimscales",
                                                 names = c("age", "sex", "sim")),
                     NULL)
    expect_identical(checkDimtypesOrDimscalesArg(arg = NULL,
                                                 nameArg = "dimscales",
                                                 names = c("age", "sex", "iterations")),
                     NULL)
    ## is character vector
    expect_error(checkDimtypesOrDimscalesArg(arg = c(age = 1),
                                             nameArg = "dimscales",
                                             names = c("age", "sex", "iterations")),
                 "'dimscales' has class \"numeric\"")
    ## length > 0
    expect_error(checkDimtypesOrDimscalesArg(arg = character(),
                                             nameArg = "dimscales",
                                             names = c("age", "sex", "iterations")),
                 "'dimscales' has length 0")
    ## has names
    expect_error(checkDimtypesOrDimscalesArg(arg = c("Intervals", "Iterations"),
                                             nameArg = "dimscales",
                                             names = c("age", "sex", "iterations")),
                 "'dimscales' does not have names")
    ## no blank names
    expect_error(checkDimtypesOrDimscalesArg(arg = c(age = "Intervals", "Iterations"),
                                             nameArg = "dimscales",
                                             names = c("age", "sex", "iterations")),
                 "names for 'dimscales' have blanks")
    ## no duplicate names
    expect_error(checkDimtypesOrDimscalesArg(arg = c(age = "age", age = "state"),
                                             nameArg = "dimtypes",
                                             names = c("age", "sex", "iterations")),
                 "names for 'dimtypes' have duplicates")
    ## names refer to dimensions
    expect_error(checkDimtypesOrDimscalesArg(arg = c(age = "age", wrong = "state"),
                                             nameArg = "dimtypes",
                                             names = c("age", "sex", "iterations")),
                 "'dimtypes' argument invalid : \"wrong\" is not a dimension name")
})



test_that("checkNames works", {
    checkNames <- dembase:::checkNames
    expect_that(checkNames(c("sex", NA)),
                throws_error("'names' has missing values"))
})

test_that("checkAndTidyObjForExpCatCounts works", {
    checkAndTidyObjExpCatCounts <- dembase:::checkAndTidyObjExpCatCounts
    ## object does not have iteration; weights does not have iterations
    object <- Counts(array(1L,
                           dim = c(3, 2),
                           dimnames = list(age = 0:2,
                               sex = c("f", "m"))))
    weights <- Counts(array(1L,
                            dim = 3,
                            dimnames = list(age = 0:2)))
    expect_identical(checkAndTidyObjExpCatCounts(object = object,
                                                 weights = weights,
                                                 n = NULL),
                     object)
    ## object has iterations; weights does not have iterations
    object <- Counts(array(1L,
                           dim = c(3, 2, 5),
                           dimnames = list(age = 0:2,
                               sex = c("f", "m"),
                               iteration = 1:5)))
    weights <- Counts(array(1L,
                            dim = 3,
                            dimnames = list(age = 0:2)))
    expect_identical(checkAndTidyObjExpCatCounts(object = object,
                                                 weights = weights,
                                                 n = NULL),
                     object)
    ## object does not have iterations; weights has iterations
    object <- Counts(array(1L,
                           dim = c(3, 2),
                           dimnames = list(age = 0:2,
                               sex = c("f", "m"))))
    weights <- Counts(array(1L,
                            dim = c(3, 5),
                            dimnames = list(age = 0:2,
                                iteration = 1:5)))
    object.iter <- Counts(array(1L,
                                dim = c(3, 2, 5),
                                dimnames = list(age = 0:2,
                                    sex = c("f", "m"),
                                    iteration = 1:5)))
    expect_identical(checkAndTidyObjExpCatCounts(object = object,
                                                 weights = weights,
                                                 n = NULL),
                     object.iter)
    ## object does not have iterations; weights does not have iterations; n = 5
    object <- Counts(array(1L,
                           dim = c(3, 2),
                           dimnames = list(age = 0:2,
                               sex = c("f", "m"))))
    weights <- Counts(array(1L,
                            dim = 3,
                            dimnames = list(age = 0:2)))
    object.iter <- Counts(array(1L,
                                dim = c(3, 2, 5),
                                dimnames = list(age = 0:2,
                                    sex = c("f", "m"),
                                    iteration = 1:5)))
    expect_identical(checkAndTidyObjExpCatCounts(object = object,
                                                 weights = weights,
                                                 n = 5),
                     object.iter)
    ## object has iterations; weights has iterations
    object <- Counts(array(1L,
                           dim = c(3, 2, 5),
                           dimnames = list(age = 0:2,
                               sex = c("f", "m"),
                               iteration = 1:5)))
    weights <- Counts(array(1L,
                            dim = c(3, 5),
                            dimnames = list(age = 0:2,
                                iteration = 1:5)))
    expect_identical(checkAndTidyObjExpCatCounts(object = object,
                                                 weights = weights,
                                                 n = 222),
                     object)
    ## object and weights have different numbers of iterations
    object <- Counts(array(1L,
                           dim = c(3, 2, 5),
                           dimnames = list(age = 0:2,
                               sex = c("f", "m"),
                               iteration = 1:5)))
    weights <- Counts(array(1L,
                            dim = c(3, 3),
                            dimnames = list(age = 0:2,
                                iteration = 1:3)))
    expect_identical(checkAndTidyObjExpCatCounts(object = object,
                                                 weights = weights,
                                                 n = 222),
                     object)
    ## Data integers
    object <- Counts(array(c(1.1, NA),
                           dim = c(3, 2, 5),
                           dimnames = list(age = 0:2,
                               sex = c("f", "m"),
                               iteration = 1:5)))
    weights <- Counts(array(1L,
                            dim = c(3, 3),
                            dimnames = list(age = 0:2,
                                iteration = 1:3)))
    expect_error(checkAndTidyObjExpCatCounts(object = object,
                                                 weights = weights,
                                                 n = 222),
                 "'object' has non-integer values")
    ## Data non.negative
    object <- Counts(array(c(-1, 0),
                           dim = c(3, 2, 5),
                           dimnames = list(age = 0:2,
                               sex = c("f", "m"),
                               iteration = 1:5)))
    weights <- Counts(array(1L,
                            dim = c(3, 3),
                            dimnames = list(age = 0:2,
                                iteration = 1:3)))
    expect_error(checkAndTidyObjExpCatCounts(object = object,
                                                 weights = weights,
                                                 n = 222),
                 "'object' has negative values")
})

test_that("checkIterationDimvalues works", {
    checkIterationDimvalues <- dembase:::checkIterationDimvalues
    object <- Counts(array(1:30,
                           dim = c(3, 2, 5),
                           dimnames = list(age = 0:2,
                               sex = c("f", "m"),
                               iteration = 1:5)),
                     dimscales = c(age = "Intervals"))
    x <- object
    dots <- list(object, object-1, t(object[1:2, , 5]), 4)
    expect_identical(checkIterationDimvalues(x, dots), NULL)
    dots <- c(1,3, 5)
    expect_identical(checkIterationDimvalues(x, dots), NULL)
    wrong <- Counts(array(1:36,
                           dim = c(3, 2, 6),
                           dimnames = list(age = 0:2,
                               sex = c("f", "m"),
                               iteration = 1:6)),
                    dimscales = c(age = "Intervals"))
    expect_error(checkIterationDimvalues(x = x, dots = list(wrong)),
                 "dimensions with dimtype \"iteration\" inconsistent")
})

test_that("getIterationDimvalues works", {
    getIterationDimvalues <- dembase:::getIterationDimvalues
    object <- Counts(array(1:30,
                           dim = c(3, 2, 5),
                           dimnames = list(age = 0:2,
                               sex = c("f", "m"),
                               iteration = 1:5)),
                     dimscales = c(age = "Intervals"))
    expect_identical(getIterationDimvalues(object),
                     1:5)
    object <- Counts(array(1:30,
                           dim = c(3, 2, 5),
                           dimnames = list(age = 0:2,
                               sex = c("f", "m"),
                               region = 1:5)),
                     dimscales = c(age = "Intervals"))
    expect_identical(getIterationDimvalues(object),
                     NULL)
    expect_identical(getIterationDimvalues(1),
                     NULL)
})


test_that("validDimnames works", {
  validDimnames <- dembase:::validDimnames
  expect_that(validDimnames(list(age = "0-4", sex = "Male")),
              is_true())
  expect_that(validDimnames(NULL),
              is_identical_to("'dimnames' is NULL"))
  expect_that(validDimnames(list(age = "0-4", sex = c("Male", NA))),
              is_identical_to("element 2 of 'dimnames' has missing values"))
  expect_that(validDimnames(list(age = "0-4", sex = c("Male", ""))),
              is_identical_to("element 2 of 'dimnames' has elements with length 0"))
  expect_that(validDimnames(list(age = "0-4", sex = c("Male", "Male"))),
              is_identical_to("element 2 of 'dimnames' has duplicates"))
  expect_that(validDimnames(list(age = "0-4", age = c("Female", "Male"))),
              is_identical_to("'names' has duplicates"))
  expect_that(validDimnames(list(age = "0-4", age = c("Female", "Male")),
                            includeNames = FALSE),
              is_true())
})

test_that("validNames works", {
  validNames <- dembase:::validNames
  expect_that(validNames(c("sex", "age")),
              is_true())
  expect_that(validNames(NULL),
              is_identical_to("'names' is NULL"))
  expect_that(validNames(c("age", NA)),
              is_identical_to("'names' has missing values"))
  expect_that(validNames(c("age", "")),
              is_identical_to("'names' has elements with length 0"))
  expect_that(validNames(c("age", "age")),
              is_identical_to("'names' has duplicates"))
  expect_that(validNames(character()),
              is_true())
})


## FUNCTIONS FOR MAKING INTERVAL LABELS ###############################################

test_that("ageMinMax works", {
    ageMinMax <- dembase:::ageMinMax
    ## Intervals
    x <- Counts(array(1,
                      dim = 2:3,
                      dimnames = list(sex = c("f", "m"),
                                      age = c("0-4", "5-9", "10+"))))
    expect_identical(ageMinMax(x, min = TRUE), 0)
    expect_identical(ageMinMax(x, min = FALSE), Inf)
    ## Points
    x <- Counts(array(1,
                      dim = 2:3,
                      dimnames = list(sex = c("f", "m"),
                                      age = c("0", "5", "10"))))
    expect_identical(ageMinMax(x, min = TRUE), 0)
    expect_identical(ageMinMax(x, min = FALSE), 10)
    ## 0-length
    x <- Counts(array(1,
                      dim = c(2, 0),
                      dimnames = list(sex = c("f", "m"),
                                      age = character())))
    expect_identical(ageMinMax(x, min = TRUE), integer())
    expect_identical(ageMinMax(x, min = FALSE), integer())
    ## no age dimension
    x <- Counts(array(1,
                      dim = c(2, 2),
                      dimnames = list(sex = c("f", "m"),
                                      region = c("a", "b"))))
    expect_null(ageMinMax(x, min = TRUE),
                NULL)
    expect_null(ageMinMax(x, min = FALSE),
                NULL)
})

test_that("ageMinMaxReplace works", {
    ageMinMaxReplace <- dembase:::ageMinMaxReplace
    ageMinMax <- dembase:::ageMinMax
    x <- Counts(array(1,
                      dim = 2:3,
                      dimnames = list(sex = c("f", "m"),
                                      age = c("0-4", "5-9", "10-14"))))
    x.new <- ageMinMaxReplace(x, value = -5, min = TRUE)
    expect_identical(ageMinMax(x.new, min = TRUE), -5)
    x.new <- ageMinMaxReplace(x, value = Inf, min = FALSE)
    expect_identical(ageMinMax(x.new, min = FALSE), Inf)
})

test_that("ageMinMaxRemplace throws appropriate errors", {
    ageMinMaxReplace <- dembase:::ageMinMaxReplace
    x <- Counts(array(1,
                      dim = 2:3,
                      dimnames = list(sex = c("f", "m"),
                                      age = c("0-4", "5-9", "10-14"))))
    expect_error(ageMinMaxReplace(x,
                                  value = "1",
                                  min = TRUE),
                 "replacement value is non-numeric")
    expect_error(ageMinMaxReplace(x,
                                  value = 1:2,
                                  min = TRUE),
                 "replacement value does not have length 1")
    expect_error(ageMinMaxReplace(x,
                                  value = as.integer(NA),
                                  min = TRUE),
                 "replacement value is missing")
    x.wrong <- Counts(array(1,
                            dim = 2:3,
                            dimnames = list(sex = c("f", "m"),
                                            region = c("a", "b", "c"))))
    expect_error(ageMinMaxReplace(x.wrong,
                                  value = -Inf,
                                  min = TRUE),
                 "no dimension with dimtype \"age\"")
    x.wrong <- Counts(array(1,
                            dim = 2:3,
                            dimnames = list(sex = c("f", "m"),
                                            age = c("0", "5", "10"))))
    expect_error(ageMinMaxReplace(x.wrong,
                                  value = -Inf,
                                  min = TRUE),
                 "dimension with dimtype \"age\" has dimscale \"Points\"")
    x.wrong <- Counts(array(1,
                            dim = c(2, 0),
                            dimnames = list(sex = c("f", "m"),
                                            age = character())))
    expect_error(ageMinMaxReplace(x.wrong,
                                  value = -Inf,
                                  min = TRUE),
                 "dimension with dimtype \"age\" has length 0")
    expect_error(ageMinMaxReplace(x,
                                  value = 5,
                                  min = TRUE),
                 "replacement value greater than or equal to upper limit of first age group")
    expect_error(ageMinMaxReplace(x,
                                  value = 9,
                                  min = FALSE),
                 "replacement value less than or equal to lower limit of last age group")
})




test_that("makeLabelsForClosedIntervals works", {
    makeLabelsForClosedIntervals <- dembase:::makeLabelsForClosedIntervals
    expect_identical(makeLabelsForClosedIntervals(c(0, 5, 10)),
                     c("0-4", "5-9"))
    expect_identical(makeLabelsForClosedIntervals(c(2000, 2005, 2010)),
                     c("2001-2005", "2006-2010"))
    expect_identical(makeLabelsForClosedIntervals(1:4),
                     c("1", "2", "3"))
    expect_identical(makeLabelsForClosedIntervals(c(1:4, 5.1)),
                     c("1-2", "2-3", "3-4", "4-5.1"))
    expect_identical(makeLabelsForClosedIntervals(c(0.01, 1.01, 2.01)),
                     c("0.01-1.01", "1.01-2.01"))
    expect_identical(makeLabelsForClosedIntervals(c(0.1, 5, 10)),
                     c("0.1-5", "5-10"))
    expect_identical(makeLabelsForClosedIntervals(c(-10, -5, 0)),
                     c("-10--6", "-5--1"))
    expect_identical(makeLabelsForClosedIntervals(c(-10, -5, 0),
                                                  intervalSeparator = " to "),
                     c("-10 to -6", "-5 to -1"))
    expect_identical(makeLabelsForClosedIntervals(c(0, 5, 10),
                                                  limitPrintLower = -Inf),
                     c("1-5", "6-10"))
    expect_identical(makeLabelsForClosedIntervals(numeric()),
                     character())
    expect_identical(makeLabelsForClosedIntervals(2000:2004),
                     as.character(2001:2004))
    expect_identical(makeLabelsForClosedIntervals(c(2000:2004, 2005.1)),
                     c("2000-2001", "2001-2002", "2002-2003", "2003-2004",
                       "2004-2005.1"))
})

test_that("makeLabelsForIntervals works", {
  makeLabelsForIntervals <- dembase:::makeLabelsForIntervals
  expect_identical(makeLabelsForIntervals(c(0, 5, Inf)),
              c("0-4", "5+"))
  expect_identical(makeLabelsForIntervals(c(0, 5, 10)),
              c("0-4", "5-9"))
  expect_identical(makeLabelsForIntervals(c(-Inf, 0, 5, 10)),
              c("<0", "0-4", "5-9"))
  expect_identical(makeLabelsForIntervals(c(-Inf, 0, 5, 10, Inf)),
              c("<0", "0-4", "5-9", "10+"))
  expect_identical(makeLabelsForIntervals(c(0, 5, Inf), intervalSeparator = " to "),
              c("0 to 4", "5+"))
  expect_identical(makeLabelsForIntervals(c(0, 5, 10), limitPrintLower = -Inf),
              c("1-5", "6-10"))
  expect_identical(makeLabelsForIntervals(numeric()),
              character())
  expect_identical(makeLabelsForIntervals(c(2000, Inf)), "2001+")
  expect_identical(makeLabelsForIntervals(c(2000, Inf), limitPrintLower = Inf),
                   "2000+")
  expect_identical(makeLabelsForIntervals(c(1995, 2000, Inf)), c("1996-2000", "2001+"))
  expect_identical(makeLabelsForIntervals(c(-Inf, 0)), "<0")
  expect_identical(makeLabelsForIntervals(0:5), as.character(0:4))
  expect_identical(makeLabelsForIntervals(2000:2005), as.character(2001:2005))
  expect_identical(makeLabelsForIntervals(c(2000:2002, 2004.5)),
                   c("2000-2001", "2001-2002", "2002-2004.5"))
})


## FUNCTIONS FOR INFERRING DIMVALUES FOR INTERVALS ###################################

test_that("extractNumberFromOpenInterval works", {
  extractNumberFromOpenInterval <- dembase:::extractNumberFromOpenInterval
  expect_that(extractNumberFromOpenInterval("100+"),
              is_identical_to(100))
  expect_that(extractNumberFromOpenInterval("100.5 and over", which = "final"),
              is_identical_to(100.5))
  expect_that(extractNumberFromOpenInterval("-20+"),
              is_identical_to(-20))
  expect_that(extractNumberFromOpenInterval("<-20", which = "firstLeft"),
              is_identical_to(-20))
  expect_that(extractNumberFromOpenInterval("less than 0", which = "firstLeft"),
              is_identical_to(0))
  expect_that(extractNumberFromOpenInterval("0 or less", which = "firstRight"),
              is_identical_to(0))
  expect_that(extractNumberFromOpenInterval("0 OR LESS", which = "firstRight"),
              is_identical_to(0))
  expect_that(extractNumberFromOpenInterval("0ORLESS", which = "firstRight"),
              is_identical_to(0))
  expect_error(extractNumberFromOpenInterval("0 or less", which = "wrong"),
              sprintf("'arg' should be one of %s, %s, %s",
                      dQuote("final"), dQuote("firstLeft"), dQuote("firstRight")))
  expect_error(extractNumberFromOpenInterval(c("0 or less", "0 or less"), which = "firstRight"),
              "'name' does not have length 1")
})

test_that("extractNumbersFromEndOfStrings works", {
  extractNumbersFromEndOfStrings <- dembase:::extractNumbersFromEndOfStrings
  expect_that(extractNumbersFromEndOfStrings("0-4"),
              is_identical_to(4))
  expect_that(extractNumbersFromEndOfStrings(c("0-4", "5-9", "10+")),
              is_identical_to(c(4, 9, NA)))
  expect_that(extractNumbersFromEndOfStrings("0 to 4", " to "),
              is_identical_to(4))
  expect_that(extractNumbersFromEndOfStrings("-5--1"),
              is_identical_to(-1))
  expect_that(extractNumbersFromEndOfStrings("0.5-1.5"),
              is_identical_to(1.5))
  expect_that(extractNumbersFromEndOfStrings("-5.5--1.5"),
              is_identical_to(-1.5))
  expect_that(extractNumbersFromEndOfStrings("0..5"),
              is_identical_to(as.numeric(NA)))
  expect_that(extractNumbersFromEndOfStrings("-0..5"),
              is_identical_to(as.numeric(NA)))
  expect_that(extractNumbersFromEndOfStrings("a"),
              is_identical_to(as.numeric(NA)))
  expect_that(extractNumbersFromEndOfStrings("-.1-0"),
              is_identical_to(0))
  expect_that(extractNumbersFromEndOfStrings("a0-a1"),
              is_identical_to(as.numeric(NA)))
  expect_that(extractNumbersFromEndOfStrings("..1"),
              is_identical_to(as.numeric(NA)))
  expect_that(extractNumbersFromEndOfStrings("1.1.1"),
              is_identical_to(as.numeric(NA)))
})

test_that("extractNumbersFromStartOfStrings works", {
  extractNumbersFromStartOfStrings <- dembase:::extractNumbersFromStartOfStrings
  expect_that(extractNumbersFromStartOfStrings("0-4"),
              is_identical_to(0))
  expect_that(extractNumbersFromStartOfStrings(c("0-4", "5-9", "10+")),
              is_identical_to(c(0, 5, 10)))
  expect_that(extractNumbersFromStartOfStrings("0 to 4"),
              is_identical_to(0))
  expect_that(extractNumbersFromStartOfStrings("-5--1"),
              is_identical_to(-5))
  expect_that(extractNumbersFromStartOfStrings("0.5-1.5"),
              is_identical_to(0.5))
  expect_that(extractNumbersFromStartOfStrings("-0.5--1.5"),
              is_identical_to(-0.5))
  expect_that(extractNumbersFromStartOfStrings("0..5"),
              is_identical_to(0))
  expect_that(extractNumbersFromStartOfStrings("-0..5"),
              is_identical_to(0))
  expect_that(extractNumbersFromStartOfStrings("a"),
              is_identical_to(as.numeric(NA)))
  expect_that(extractNumbersFromStartOfStrings("-.1 - 0"),
              is_identical_to(-0.1))
  expect_that(extractNumbersFromStartOfStrings("a0-a1"),
              is_identical_to(as.numeric(NA)))
  expect_that(extractNumbersFromStartOfStrings("..1"),
              is_identical_to(as.numeric(NA)))
  expect_that(extractNumbersFromStartOfStrings("1.1.1"),
              is_identical_to(1.1))
})

test_that("stringsAreIntegers works", {
  stringsAreIntegers <- dembase:::stringsAreIntegers
  expect_that(stringsAreIntegers("1"), is_true())
  expect_that(stringsAreIntegers("1a"), is_false())
  expect_that(stringsAreIntegers(c("1", "5")), is_identical_to(c(TRUE, TRUE)))
  expect_that(stringsAreIntegers(c("1", "a")), is_identical_to(c(TRUE, FALSE)))
  expect_that(stringsAreIntegers("1.0"), is_true())
  expect_that(stringsAreIntegers("-1"), is_true())
  expect_that(stringsAreIntegers("Inf"), is_false())
  expect_that(stringsAreIntegers(character()), is_identical_to(logical()))
})

test_that("stringsAreNumbers works", {
  stringsAreNumbers <- dembase:::stringsAreNumbers
  expect_that(stringsAreNumbers(c("-1.003", ".3", "Inf", "1..1", "")),
              is_identical_to(c(TRUE, TRUE, TRUE, FALSE, FALSE)))
  expect_that(stringsAreNumbers(character()),
              is_identical_to(logical()))
})



## FUNCTIONS FOR INFERRING DIMTYPES AND DIMSCALES #####################################

test_that("inferDimScale works", {
    inferDimScale <- dembase:::inferDimScale
    expect_identical(inferDimScale(dimtype = "age",
                                   dimscale = "Intervals",
                                   labels = c("0-4", "5+"),
                                   name = "age"),
                     new("Intervals", dimvalues = c(0, 5, Inf)))
    expect_identical(inferDimScale(dimtype = "age",
                                   dimscale = NULL,
                                   labels = c("0-4", "5+")),
                     new("Intervals", dimvalues = c(0, 5, Inf)))
    expect_message(inferDimScale(dimtype = "age",
                                 dimscale = NULL,
                                 labels = c("0", "1"),
                                 name = "Age"),
                   "assuming dimension \"Age\" with dimtype \"age\" has dimscale \"Intervals\"")
    expect_identical(inferDimScale(dimtype = "time",
                                   dimscale = "Points",
                                   labels = c("0", "1"),
                                   name = "year"),
                     new("Points", dimvalues = c(0, 1)))
    expect_identical(inferDimScale(dimtype = "time",
                                   dimscale = "Intervals",
                                   labels = c("0", "1"),
                                   name = "year"),
                     new("Intervals", dimvalues = c(0, 1, 2)))
    expect_error(inferDimScale(dimtype = "time",
                               dimscale = NULL,
                               labels = c("0", "1"),
                               name = "year"),
                 "dimension \"year\" with dimtype \"time\" could have dimscale \"Intervals\" or dimscale \"Points\" : please supply a 'dimscale' or 'dimscales' argument")
    expect_identical(inferDimScale(dimtype = "state",
                                   dimscale = NULL,
                                   labels = c("a", "b"),
                                   name = "region"),
                     new("Categories", dimvalues = c("a", "b")))
    expect_identical(inferDimScale(dimtype = "origin",
                                   dimscale = NULL,
                                   labels = c("a", "b"),
                                   name = "region"),
                     new("Categories", dimvalues = c("a", "b")))
    expect_identical(inferDimScale(dimtype = "state",
                                   dimscale = NULL,
                                   labels = c("0", "1", "2+"),
                                   name = "parity"),
                     new("Categories", dimvalues = c("0", "1", "2+")))
    expect_error(inferDimScale(dimtype = "time",
                               dimscale = NULL,
                               labels = c("a", "b"),
                               name = "year"),
                 "unable to infer dimscale for dimension \"year\" with dimtype \"time\"")
    expect_identical(inferDimScale(dimtype = "sex",
                                   dimscale = NULL,
                                   labels = c("female", "male"),
                                   name = "gender"),
                     new("Sexes", dimvalues = c("female", "male")))
    expect_identical(inferDimScale(dimtype = "sex",
                                   dimscale = NULL,
                                   labels = c("Males", "Females"),
                                   name = "Sex"),
                     new("Sexes", dimvalues = c("Males", "Females")))
    expect_error(inferDimScale(dimtype = "sex",
                               dimscale = NULL,
                               labels = c("Male", "Females"),
                               name = "Sex"),
                 "unable to infer dimscale for dimension \"Sex\" with dimtype \"sex\"")
})

test_that("inferDimtypes works as expected", {
    inferDimtypes <- dembase:::inferDimtypes
    expect_identical(inferDimtypes(c("age", "age5", "age10yr", "sex", "year")),
                     c("age", "age", "age", "sex", "time"))
    expect_identical(inferDimtypes(c("duration", "unknown")),
                     c("age", "state"))
    expect_identical(inferDimtypes(c("start", "end")),
                     c("state", "state"))
    expect_identical(inferDimtypes(c("stage", "parity")),
                     c("state", "state"))
    expect_identical(inferDimtypes(c("reg_orig", "reg_dest", "birth cohort")),
                     c("origin", "destination", "cohort"))
    expect_identical(inferDimtypes(c("reg_dest", "birth cohort")),
                     c("destination", "cohort"))
    expect_identical(inferDimtypes(c("ethnicity_parent", "ethnicity_child")),
                     c("parent", "child"))
    expect_identical(inferDimtypes(c("Lexis triangle", "Lexis Triangles", "triangle", "Triangles")),
                     rep("triangle", 4))
    expect_identical(inferDimtypes(c("Sex", "sexes", "gender", "Genders")),
                     rep("sex", 4))
})



## FUNCTIONS FOR COERCION ###########################################################

test_that("asDataFrame works", {
    asDataFrame <- dembase:::asDataFrame
    x <- Counts(array(1:18,
                      dim = c(2, 3, 3),
                      dimnames = list(sex = c("f", "m"),
                      age = c(0, 5, 10),
                      quantile = c("5%", "50%", "95%"))))
    y <- as.data.frame.table(x@.Data, stringsAsFactors = TRUE, responseName = "count")
    y$age <- as.numeric(levels(y$age))[y$age]
    expect_identical(asDataFrame(x, responseName = "count"), y)
    expect_identical(sapply(asDataFrame(x, responseName = "count"), class),
                     c(sex = "factor", age = "numeric",
                       quantile = "factor", count = "integer"))
    x <- Counts(array(1:18,
                      dim = c(2, 3, 3),
                      dimnames = list(sex = c("f", "m"),
                      age = c(0, 5, 10),
                      quantile = c("5%", "20%", "95%"))))
    y <- as.data.frame.table(x@.Data,
                             stringsAsFactors = FALSE,
                             responseName = "count")
    y$age <- as.numeric(y$age)
    expect_identical(asDataFrame(x, responseName = "count",
                                 stringsAsFactors = FALSE),
                     y)
    expect_identical(sapply(asDataFrame(x,
                                        responseName = "count",
                                        stringsAsFactors = FALSE), class),
                     c(sex = "character",
                       age = "numeric", quantile = "character",
                       count = "integer"))
    x <- Counts(array(1:18,
                      dim = c(2, 3, 3),
                      dimnames = list(sex = c("f", "m"),
                      age = c(0, 5, 10),
                      quantile = c("5%", "20%", "95%"))))
    y <- as.data.frame.table(x@.Data, responseName = "count")
    y$age <- as.numeric(levels(y$age))[y$age]
    levels(y$quantile) <- c("5%", "20%", "95%")
    expect_identical(asDataFrame(x, responseName = "count"), y)
    x <- Counts(array(1:18,
                      dim = c(2, 3, 3),
                      dimnames = list(sex = c("f", "m"),
                      age = c(0, 5, 10),
                      quantile = c("5%", "20%", "95%"))))
    y <- as.data.frame.table(x@.Data, responseName = "number")
    y$age <- as.numeric(levels(y$age))[y$age]
    levels(y$quantile) <- c("5%", "20%", "95%")
    expect_identical(asDataFrame(x, responseName = "number"), y)
    x <- Counts(array(1:18,
                      dim = c(2, 3, 3),
                      dimnames = list(sex = c("f", "m"),
                      age = c(0, 5, 10),
                      region = c("a", "b", "c"))))
    ans.obtained <- asDataFrame(x, responseName = "count",
                                stringsAsFactors = FALSE)
    ans.expected <- as.data.frame.table(x@.Data, responseName = "count",
                                        stringsAsFactors = FALSE)
    ans.expected$age <- as.numeric(ans.expected$age)
    expect_identical(ans.obtained, ans.expected)
})

test_that("intervalsToPoints works", {
  intervalsToPoints <- dembase:::intervalsToPoints
  expect_identical(intervalsToPoints(new("Intervals", dimvalues = c(0, 1, 2, 3))),
                    new("Points", dimvalues = c(0.5, 1.5, 2.5)))
  expect_identical(intervalsToPoints(new("Intervals", dimvalues = c(0, 1, 5, 10, 15))),
                   new("Points", dimvalues = c(0.5, 3, 7.5, 12.5)))
  expect_identical(intervalsToPoints(new("Intervals")),
                   new("Points"))
  expect_identical(intervalsToPoints(new("Intervals", dimvalues = c(0, 5))),
              new("Points", dimvalues = 2.5))
  expect_identical(intervalsToPoints(new("Intervals", dimvalues = c(-2, 0, 2, 4))),
                   new("Points", dimvalues = c(-1, 1, 3)))
  expect_identical(intervalsToPoints(new("Intervals", dimvalues = c(0, 5, Inf))),
                   new("Points", dimvalues = c(2.5, 7.5)))
})

test_that("pointsToIntervals works", {
  pointsToIntervals <- dembase:::pointsToIntervals
  expect_that(pointsToIntervals(new("Points", dimvalues = c(0.5, 1.5, 2.5))),
              is_identical_to(new("Intervals", dimvalues = c(0, 1, 2, 3))))
  expect_that(pointsToIntervals(new("Points", dimvalues = c(0.5, 3, 7.5, 12.5))),
              is_identical_to(new("Intervals", dimvalues = c(0, 1, 5, 10, 15))))
  expect_that(pointsToIntervals(new("Points")),
              is_identical_to(new("Intervals")))
  expect_that(pointsToIntervals(new("Points", dimvalues = 2.5)),
              is_identical_to(new("Intervals", dimvalues = c(0, 5))))
  expect_that(pointsToIntervals(new("Points", dimvalues = c(-1, 1, 3))),
              is_identical_to(new("Intervals", dimvalues = c(-2, 0, 2, 4))))
})

test_that("intervalsBetweenPoints works", {
    intervalsBetweenPoints <- dembase:::intervalsBetweenPoints
    expect_identical(intervalsBetweenPoints(new("Points", dimvalues = c(0, 1, 5, 10))),
                     new("Intervals", dimvalues = c(0, 1, 5, 10)))
    expect_error(intervalsBetweenPoints(new("Intervals")),
                 "'object' has class \"Intervals\"")
    expect_error(intervalsBetweenPoints(new("Points")),
                 "'object' has 0 points")
    expect_error(intervalsBetweenPoints(new("Points", dimvalues = 0)),
                 "'object' has 1 point")
})


## FUNCTIONS FOR TESTING COMPATABILITY ###############################################

test_that("addMissingIter works", {
    addMissingIter <- dembase:::addMissingIter
    a <- Counts(array(1:6, dim = c(2, 3), dimnames = list(age = c("0-4", "5+"), sim = 1:3)))
    b <- array(1:2, dim = 2)
    c <- array(1:2, dim = c(2, 3))
    expect_identical(addMissingIter(x = b, y = a), c)
    expect_identical(addMissingIter(x = b, y = t(a)), t(c))
    a <- Counts(array(0, dim = c(2, 0), dimnames = list(age = c("0-4", "5+"), sim = NULL)))
    b <- array(1:2, dim = 2)
    c <- array(1:2, dim = c(2, 0))
    expect_identical(addMissingIter(x = b, y = a), c)
})


test_that("alsoHasIterations works", {
    alsoHasIterations <- dembase:::alsoHasIterations
    e1 <- Counts(array(0, dim = c(2, 2), dimnames = list(sex = c("m", "f"), iteration = 1:2)))
    e2 <- Counts(array(0, dim = c(1, 1), dimnames = list(sex = "m", iteration = 1)))
    expect_true(alsoHasIterations(e1, e2))
    e1 <- Counts(array(0, dim = c(2, 2), dimnames = list(sex = c("m", "f"), iteration = 1:2)))
    e2 <- Counts(array(0, dim = c(1, 1), dimnames = list(sex = "m", year = 1)),
                 dimscales = c(year = "Intervals"))
    expect_error(alsoHasIterations(e1, e2),
                 "dimension \"iteration\" has dimtype \"iteration\" and cannot be collapsed")
})

test_that("alsoHasZeroLengthDim works", {
    alsoHasZeroLengthDim <- dembase:::alsoHasZeroLengthDim
    e1 <- Counts(array(0, dim = c(0, 2), dimnames = list(a = NULL, b = 1:2)))
    e2 <- Counts(array(0, dim = c(1, 0), dimnames = list(b = 1, a = NULL)))
    expect_true(alsoHasZeroLengthDim(e1, e2))
    e1 <- Counts(array(0, dim = 0, dimnames = list(a = NULL)))
    e2 <- Counts(array(0, dim = 0, dimnames = list(a = NULL)))
    expect_true(alsoHasZeroLengthDim(e1, e2))
    e1 <- Counts(array(0, dim = c(0, 2), dimnames = list(a = NULL, b = 1:2)))
    e2 <- Counts(array(0, dim = c(1, 1), dimnames = list(b = 1, c = 1)))
    expect_error(alsoHasZeroLengthDim(e1, e2),
                 sprintf("one object has dimension \\[%s\\] with length 0 that other does not",
                         dQuote("a")))
    e1 <- Counts(array(0, dim = c(0, 2, 0), dimnames = list(a = NULL, b = 1:2, c = NULL)))
    e2 <- Counts(array(0, dim = c(1, 1), dimnames = list(b = 1, d = 1)))
    expect_error(alsoHasZeroLengthDim(e1, e2),
                sprintf("one object has dimensions \\[%s, %s\\] with length 0 that other does not",
                        dQuote("a"), dQuote("c")))
})

test_that("bothHaveIter works", {
    bothHaveIter <- dembase:::bothHaveIter
    e1 <- Counts(array(0, dim = 2, dimnames = list(sex = c("m", "f"))))
    e2 <- Counts(array(0, dim = 2:3, dimnames = list(sex = c("m", "f"), sim = 1:3)))
    expect_true(bothHaveIter(x = e1, y = e2))
    expect_error(bothHaveIter(x = e2, y = e1),
                 "one object has dimension with dimtype \"iteration\" but other does not")
})

test_that("canMakeDemographicAndArrayCompatible", {
    canMakeDemographicAndArrayCompatible <- dembase:::canMakeDemographicAndArrayCompatible
    a <- array(1:4, dim = c(2, 2), dimnames = list(sex = c("m", "f"), age = c("0-4", "5+")))
    x <- Counts(a)
    expect_true(canMakeDemographicAndArrayCompatible(x, a))
    expect_error(canMakeDemographicAndArrayCompatible(x, t(a)),
                 "names of dimensions do not match \\[\"sex\" versus \"age\"\\]")
    x <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(sex = c("f", "m"), sim = 1:2)))
    a <- array(1:2, dim = 2)
    expect_true(canMakeDemographicAndArrayCompatible(x, a))
})


test_that("canMakeSharedDimScalePairsCompatible works", {
    canMakeSharedDimScalePairsCompatible <- dembase:::canMakeSharedDimScalePairsCompatible
    e1 <- Counts(array(0,
                       dim = c(3, 2),
                       dimnames = list(age = c("0-4", "5-9", "10+"),
                                       sex = c("m", "f"))))
    e2 <- Values(array(0,
                       dim = c(2, 2),
                       dimnames = list(age = c("0-9", "10+"),
                                       sex = c("f", "m"))))
    expect_true(canMakeSharedDimScalePairsCompatible(e1, e2))
    expect_true(canMakeSharedDimScalePairsCompatible(e2, e1))
    e1 <- Counts(array(0,
                       dim = c(2, 2),
                       dimnames = list(age = c("-5-4", "5-9"),
                                       sex = c("m", "f"))))
    e2 <- Counts(array(0,
                       dim = c(1, 2),
                       dimnames = list(age = "0-9",
                                       sex = c("f", "m"))))
    expect_error(canMakeSharedDimScalePairsCompatible(e1, e2),
                 paste("\"age\" dimensions have incompatible dimscales :",
                       "intervals do not align"))
    e1 <- Counts(array(0,
                       dim = c(3, 2),
                       dimnames = list(age = c("0-4", "5-9", "10+"),
                                       sex = c("m", "f"))),
                 dimtypes = c(sex = "state"))
    e2 <- Values(array(0,
                       dim = c(2, 3),
                       dimnames = list(age = c("0-9", "10+"),
                                       sex = c("f", "m", "wrong"))),
                 dimtypes = c(sex = "state"))
    expect_true(canMakeSharedDimScalePairsCompatible(e1, e2))
    e1 <- Values(array(0,
                       dim = c(3, 2),
                       dimnames = list(age = c("0-4", "5-9", "10+"),
                                       sex = c("m", "f"))),
                 dimtypes = c(sex = "state"))
    e2 <- Counts(array(0,
                       dim = c(2, 3),
                       dimnames = list(age = c("0-9", "10+"),
                                       sex = c("f", "m", "wrong"))),
                 dimtypes = c(sex = "state"))
    expect_error(canMakeSharedDimScalePairsCompatible(e1, e2),
                 "\"age\" dimensions have incompatible dimscales : intervals do not align")
    e1 <- Counts(array(0,
                       dim = c(3, 2),
                       dimnames = list(age = c("0-4", "5-9", "10+"),
                                       sex = c("m", "f"))))
    e2 <- Counts(array(0,
                       dim = c(2, 2),
                       dimnames = list(age = c("0-9", "10+"),
                                       sex = c("F", "M"))))
    expect_error(canMakeSharedDimScalePairsCompatible(e1, e2),
                 "\"sex\" dimensions have incompatible dimscales : no values in common")
})

test_that("checkQuantilesDemographicArray works", {
    checkQuantilesDemographicArray <- dembase:::checkQuantilesDemographicArray
    x <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(sex = c("f", "m"), quantile = c(0.1, 0.9))))
    expect_error(checkQuantilesDemographicArray(x = x, .Generic = "*"),
                 "dimension \"quantile\" has dimtype \"quantile\"")
    expect_true(checkQuantilesDemographicArray(x = x, .Generic = "|"))
    expect_true(checkQuantilesDemographicArray(x = x, .Generic = ">"))
})

test_that("checkQuantilesDemographicNumeric works", {
    checkQuantilesDemographicNumeric <- dembase:::checkQuantilesDemographicNumeric
    e1 <- Counts(array(1:3, dim = 3, dimnames = list(quantile = c(0.1, 0.5, 0.9))))
    expect_true(checkQuantilesDemographicNumeric(e1, c(0, 0, 0), "+"))
    expect_true(checkQuantilesDemographicNumeric(e1, 0, "-"))
    expect_true(checkQuantilesDemographicNumeric(e1, 1.3, "*"))
    expect_true(checkQuantilesDemographicNumeric(e1, 1.3, "^"))
    expect_true(checkQuantilesDemographicNumeric(e1, Inf, "/"))
    expect_error(checkQuantilesDemographicNumeric(e1, 1, "+"),
                 "dimension \"quantile\" has dimtype \"quantile\"")
    expect_error(checkQuantilesDemographicNumeric(e1, -0.1, "-"),
                 "dimension \"quantile\" has dimtype \"quantile\"")
    expect_error(checkQuantilesDemographicNumeric(e1, 1:2, "*"),
                 "dimension \"quantile\" has dimtype \"quantile\"")
    expect_error(checkQuantilesDemographicNumeric(e1, 1:2, "/"),
                 "dimension \"quantile\" has dimtype \"quantile\"")
})

test_that("checkQuantilesNumericDemographic works", {
    checkQuantilesNumericDemographic <- dembase:::checkQuantilesNumericDemographic
    e2 <- Counts(array(1:3, dim = 3, dimnames = list(quantile = c(0.1, 0.5, 0.9))))
    expect_true(checkQuantilesNumericDemographic(0, e2, "+"))
    expect_true(checkQuantilesNumericDemographic(1.3, e2, "*"))
    expect_error(checkQuantilesNumericDemographic(1, e2, "+"),
                 "dimension \"quantile\" has dimtype \"quantile\"")
    expect_error(checkQuantilesNumericDemographic(1:2, e2, "*"),
                 "dimension \"quantile\" has dimtype \"quantile\"")
    expect_error(checkQuantilesNumericDemographic(1, e2, "^"),
                 "dimension \"quantile\" has dimtype \"quantile\"")
    expect_error(checkQuantilesNumericDemographic(1, e2, "/"),
                 "dimension \"quantile\" has dimtype \"quantile\"")
})

test_that("compatibleDimAndDimnames works", {
    compatibleDimAndDimnames <- dembase:::compatibleDimAndDimnames
    dim.x <- c(3, 2)
    dn.x <- list(age = c("0-4", "5-9", "10+"), sex = c("f", "m"))
    expect_true(compatibleDimAndDimnames(dim.x = dim.x, dn.x = dn.x,
                                         dim.y = dim.x, dn.y = dn.x))
    expect_true(compatibleDimAndDimnames(dim.x = dim.x, dn.x = dn.x,
                                         dim.y = dim.x, dn.y = unname(dn.x)))
    expect_true(compatibleDimAndDimnames(dim.x = dim.x, dn.x = dn.x,
                                         dim.y = dim.x, dn.y = NULL))
    expect_true(compatibleDimAndDimnames(dim.x = dim.x, dn.x = dn.x,
                                         dim.y = dim.x, dn.y = c(dn.x[1], list(sex = NULL))))
    expect_error(compatibleDimAndDimnames(dim.x = dim.x, dn.x = dn.x,
                                          dim.y = c(2, 2), dn.y = NULL),
                 "non-conformable arrays")
    expect_error(compatibleDimAndDimnames(dim.x = dim.x,
                                          dn.x = dn.x,
                                          dim.y = dim.x,
                                          dn.y = list(wrong = NULL, sex = c("f", "m"))),
                 "names of dimensions do not match \\[\"age\" versus \"wrong\"\\]")
    expect_error(compatibleDimAndDimnames(dim.x = dim.x,
                                          dn.x = dn.x,
                                          dim.y = dim.x,
                                          dn.y = list(age = rep("wrong", 3), sex = c("f", "m"))),
                 "dimnames for dimension \"age\" do not match")
})

test_that("consistentDimtypes works", {
    consistentDimtypes <- dembase:::consistentDimtypes
    e1 <- Counts(array(0, dim = c(2, 2),
                       dimnames = list(age = c("0-4", "5+"), sex = c("M", "F"))))
    e2 <- Counts(array(0, dim = c(2, 2),
                       dimnames = list(sex = c("M", "F"), age = c("0-9", "10+"))))
    expect_true(consistentDimtypes(e1, e2))
    e1 <- Counts(array(0,
                       dim = c(2, 1, 2),
                       dimnames = list(age = c("0-4", "5+"),
                           time = "2001-2005",
                           triangle = c("Lower", "Upper"))))
    e2 <- Counts(array(0,
                       dim = c(2, 1, 2),
                       dimnames = list(age = c("0-4", "5+"),
                           time = "2001-2005",
                           triangle = c("Lower", "Upper"))),
                 dimtypes = c(triangle = "state"))
    expect_error(consistentDimtypes(e1, e2),
                 sprintf("%s dimensions have different dimtypes : %s versus %s",
                         dQuote("triangle"), dQuote("triangle"), dQuote("state")))
    e1 <- Counts(array(0, dim = c(2, 2), dimnames = list(age = c("0-4", "5+"), iteration = 1:2)))
    e2 <- Counts(array(0,
                       dim = c(2, 2),
                       dimnames = list(age = c("0-4", "5+"), iteration = 1:2)),
                 dimtypes = c(age = "state", iteration = "state"))
    expect_error(consistentDimtypes(e1, e2),
                 sprintf("%s, %s dimensions have different dimtypes : %s, %s versus %s, %s",
                         dQuote("age"), dQuote("iteration"), dQuote("age"),
                         dQuote("iteration"), dQuote("state"), dQuote("state")))
})

test_that("containsNames works", {
    containsNames <- dembase:::containsNames
    e1 <- Counts(array(0, dim = c(1, 1), dimnames = list(age = "0-4", sex = "f")))
    e2 <- Counts(array(0, dim = 1, dimnames = list(age = "0-4")))
    expect_true(containsNames(e1, e2))
    expect_error(containsNames(e2, e1),
                 sprintf("one object has dimension \\[%s\\] that other does not",
                         dQuote("sex")))
    e1 <- Counts(array(0, dim = c(1, 1, 1), dimnames = list(age = "0-4", sex = "f", region = "a")))
    e2 <- Counts(array(0, dim = 1, dimnames = list(age = "0-4")))
    expect_true(containsNames(e1, e2))
    expect_error(containsNames(e2, e1),
                 sprintf("one object has dimensions \\[%s, %s\\] that other does not",
                         dQuote("sex"), dQuote("region")))
    e1 <- Counts(array(0, dim = 1, dimnames = list(age = "0-4")))
    e2 <- Counts(array(0, dim = c(1, 1), dimnames = list(age = "0-4", iteration = 1)))
    expect_true(containsNames(e1, e2))
    expect_error(containsNames(e1, e2, ignoreIterations = FALSE),
                 sprintf("one object has dimension \\[%s\\] that other does not",
                         dQuote("iteration")))
})

test_that("copyIterDim works", {
    copyIterDim <- dembase:::copyIterDim
    e1 <- Counts(array(1:2, dim = 2, dimnames = list(sex = c("f", "m"))))
    e2 <- Counts(array(1:2, dim = c(2, 3), dimnames = list(sex = c("f", "m"), sim = 1:3)))
    expect_identical(copyIterDim(x = e1, y = e2), e2)
    expect_identical(copyIterDim(x = e1, y = e1), e1)
    e1 <- Counts(array(1:2, dim = 2, dimnames = list(sex = c("f", "m"))))
    e2 <- Counts(array(1:2, dim = c(2, 0), dimnames = list(sex = c("f", "m"), sim = NULL)))
    expect_identical(copyIterDim(x = e1, y = e2), e2)
})

test_that("copyZeroDim works", {
    copyZeroDim <- dembase:::copyZeroDim
    x <- Counts(array(1:2, dim = 2, dimnames = list(sex = c("f", "m"))))
    y <- Counts(array(0, dim = c(2, 0), dimnames = list(sex = c("f", "m"), age = NULL)))
    expect_identical(copyZeroDim(x = x, y = y), toInteger(y))
    expect_identical(copyZeroDim(x = y, y = y), y)
    expect_identical(copyZeroDim(x = x, y = x), x)
})

test_that("doesNotHaveQuantiles works", {
    doesNotHaveQuantiles <- dembase:::doesNotHaveQuantiles
    object <- Counts(array(0, dim = 3, dimnames = list(quantile = c(0.025, 0.5, 0.975))))
    expect_error(doesNotHaveQuantiles(object),
                 "dimension \"quantile\" has dimtype \"quantile\"")
    object <- Counts(array(0, dim = 2, dimnames = list(sex = c("male", "female"))))
    expect_true(doesNotHaveQuantiles(object))
})

test_that("haveNamesInCommon works", {
    haveNamesInCommon <- dembase:::haveNamesInCommon
    e1 <- Counts(array(0, dim = c(1, 1, 1), dimnames = list(age = "0-4", sex = "f", region = "a")))
    e2 <- Counts(array(0, dim = 1, dimnames = list(age = "0-4")))
    expect_true(haveNamesInCommon(e1, e2))
    e1 <- Counts(array(0, dim = c(1, 1, 1), dimnames = list(age = "0-4", sex = "f", region = "a")))
    e2 <- Counts(array(0, dim = 1, dimnames = list(time = 2000)), dimscales = c(time = "Intervals"))
    expect_error(haveNamesInCommon(e1, e2),
                 "no dimensions in common")
    e1 <- Counts(array(0, dim = c(1, 1, 1), dimnames = list(age = "0-4", sex = "f", sim = 1)))
    e2 <- Counts(array(0, dim = c(1, 1), dimnames = list(time = 2000, sim = 1)),
                 dimscales = c(time = "Intervals"))
    expect_error(haveNamesInCommon(e1, e2),
                 "no dimensions in common \\(apart from dimension with dimtype \"iteration\"\\)")
    expect_true(haveNamesInCommon(e1, e2, ignoreIterations = FALSE))
})

test_that("internalDetailGreaterOrEqual works", {
    internalDetailGreaterOrEqual <- dembase:::internalDetailGreaterOrEqual
    e1 <- new("Intervals", dimvalues = c(0, 1, 2, 5))
    e2 <- new("Intervals", dimvalues = c(0, 1, 5))
    expect_true(internalDetailGreaterOrEqual(e1, e2))
    e1 <- new("Intervals", dimvalues = c(0, 1, 2, 5))
    e2 <- new("Intervals", dimvalues = c(0L, 1L, 5L))
    expect_true(internalDetailGreaterOrEqual(e1, e2))
    e1 <- new("Intervals", dimvalues = c(-Inf, 0, 1, 2, Inf))
    e2 <- new("Intervals", dimvalues = c(0, 1, 2))
    expect_true(internalDetailGreaterOrEqual(e1, e2))
    e1 <- new("Intervals", dimvalues = c(0, 0.5, 1, 1.5, 2))
    e2 <- new("Intervals", dimvalues = c(-Inf, 0, 1, 2, Inf))
    expect_true(internalDetailGreaterOrEqual(e1, e2))
    e1 <- new("Intervals", dimvalues = c(0, 1, 2, 5))
    e2 <- new("Intervals", dimvalues = c(-1, 0, 2, 5, 10, Inf))
    expect_true(internalDetailGreaterOrEqual(e1, e2))
    e1 <- new("Intervals", dimvalues = c(-Inf, 0))
    e2 <- new("Intervals", dimvalues = c(-Inf, 0, Inf))
    expect_true(internalDetailGreaterOrEqual(e1, e2))
    e1 <- new("Intervals", dimvalues = c(-Inf, 0, Inf))
    e2 <- new("Intervals")
    expect_true(internalDetailGreaterOrEqual(e1, e2))
    e1 <- new("Intervals")
    e2 <- new("Intervals", dimvalues = c(-Inf, 0, Inf))
    expect_error(internalDetailGreaterOrEqual(e1, e2),
                "one dimension has 2 intervals but other has none")
    e1 <- new("Intervals")
    e2 <- new("Intervals")
    expect_true(internalDetailGreaterOrEqual(e1, e2))
    e1 <- new("Intervals", dimvalues = c(0, 5))
    e2 <- new("Intervals", dimvalues = c(0, 1, 5))
    expect_error(internalDetailGreaterOrEqual(e1, e2),
                 "one dimension has break \\[1\\] that other does not")
    e1 <- new("Intervals", dimvalues = c(0, 5))
    e2 <- new("Intervals", dimvalues = 0:5)
    expect_error(internalDetailGreaterOrEqual(e1, e2),
                 "one dimension has breaks \\[1, 2, 3, 4\\] that other does not")
})

test_that("isPositiveScalar works", {
    isPositiveScalar <- dembase:::isPositiveScalar
    expect_true(isPositiveScalar(1))
    expect_false(isPositiveScalar(0))
    expect_false(isPositiveScalar(1:2))
    expect_false(isPositiveScalar(array(1, dim = 1)))
    expect_false(isPositiveScalar(as.numeric(NA)))
    expect_false(isPositiveScalar("a"))
})

test_that("limitsEqual works", {
    limitsEqual <- dembase:::limitsEqual
    e1 <- new("Intervals", dimvalues = c(0, 1, 2, 5))
    e2 <- new("Intervals", dimvalues = c(0, 1, 5))
    expect_true(limitsEqual(e1, e2))
    e1 <- new("Intervals", dimvalues = c(0, 1, 2, 5))
    e2 <- new("Intervals", dimvalues = c(0L, 1L, 5L))
    expect_true(limitsEqual(e1, e2))
    e1 <- new("Intervals", dimvalues = c(-Inf, 0, 1, 2))
    e2 <- new("Intervals", dimvalues = c(0, 1, 2))
    expect_error(limitsEqual(e1, e2),
                "one dimension starts at -Inf and other starts at 0")
    e1 <- new("Intervals", dimvalues = c(0, 1, 2))
    e2 <- new("Intervals", dimvalues = c(0, 1, 2, Inf))
    expect_error(limitsEqual(e1, e2),
                "one dimension ends at 2 and other ends at Inf")
    e1 <- new("Intervals")
    e2 <- new("Intervals", dimvalues = c(-Inf, 0, Inf))
    expect_error(limitsEqual(e1, e2),
                 "one dimension has 2 intervals but other has none")
    e1 <- new("Intervals", dimvalues = c(-Inf, 0, Inf))
    e2 <- new("Intervals")
    expect_error(limitsEqual(e1, e2),
                 "one dimension has 2 intervals but other has none")
    expect_that(limitsEqual(new("Intervals"),
                            new("Intervals")),
                is_true())
})

test_that("limitsGreaterOrEqual works", {
  limitsGreaterOrEqual <- dembase:::limitsGreaterOrEqual
  e1 <- new("Intervals", dimvalues = c(0, 1, 2, 5))
  e2 <- new("Intervals", dimvalues = c(0, 1, 5))
  expect_true(limitsGreaterOrEqual(e1, e2))
  e1 <- new("Intervals", dimvalues = c(-Inf, 0, 1, 2, 5, Inf))
  e2 <- new("Intervals", dimvalues = c(0, 1, 5))
  expect_true(limitsGreaterOrEqual(e1, e2))
  e1 <- new("Intervals", dimvalues = c(0, 1, 2))
  e2 <- new("Intervals", dimvalues = c(0, 1, 2, 5))
  expect_error(limitsGreaterOrEqual(e1, e2),
               "one dimension ends at 2 and other ends at 5")
  e1 <- new("Intervals", dimvalues = c(0, 1, 2))
  e2 <- new("Intervals", dimvalues = c(-Inf, 0, 1, 2, 5))
  expect_error(limitsGreaterOrEqual(e1, e2),
               "one dimension starts at 0 and other starts at -Inf")
  e1 <- new("Intervals")
  e2 <- new("Intervals")
  expect_true(limitsGreaterOrEqual(e1, e2))
  e1 <- new("Intervals", dimvalues = c(0, Inf))
  e2 <- new("Intervals")
  expect_true(limitsGreaterOrEqual(e1, e2))
  e1 <- new("Intervals")
  e2 <- new("Intervals", dimvalues = c(-Inf, 0))
  expect_error(limitsGreaterOrEqual(e1, e2),
               "one dimension has 1 interval but other has none")
})





## HELPER FUNCTIONS FOR DBIND ##################################################

test_that("checkCanCombineAlong", {
    checkCanCombineAlong <- dembase:::checkCanCombineAlong
    x <- Counts(array(1:2,
                       dim = 2:1,
                       dimnames = list(sex = c("m", "f"), iteration = 1)))
    y <- Counts(array(3:10,
                       dim = c(2, 4),
                       dimnames = list(sex = c("m", "f"), iteration = 1:4)))
    expect_identical(checkCanCombineAlong(e1 = x, e2 = y, along = "iteration"),
                     NULL)
    expect_identical(checkCanCombineAlong(e1 = y, e2 = x, along = "iteration"),
                     NULL)
    expect_error(checkCanCombineAlong(e1 = x, e2 = y, along = "sex"),
                 sprintf("\"sex\" dimensions both have values %s, %s",
                         dQuote("m"), dQuote("f")))
})

test_that("combineDbindData works", {
    combineDbindData <- dembase:::combineDbindData
    ## both integer
    e1 <- Counts(array(1:2,
                       dim = 2:1,
                       dimnames = list(sex = c("m", "f"), iteration = 1)))
    e2 <- Counts(array(3:10,
                       dim = c(2, 4),
                       dimnames = list(sex = c("m", "f"), iteration = 2:5)))
    metadata <- new("MetaData",
                    nms = c("sex", "iteration"),
                    dimtypes = c("state", "iteration"),
                    DimScales = list(new("Categories", dimvalues = c("m", "f")),
                        new("Iterations", dimvalues = 1:5)))
    ans.obtained <- combineDbindData(e1 = e1, e2 = e2, metadata = metadata)
    ans.expected <- array(1:10,
                          dim = c(2, 5),
                          dimnames = list(sex = c("m", "f"), iteration = 1:5))
    expect_identical(ans.obtained, ans.expected)
    ## one numeric
    e1 <- Counts(array(1:2,
                       dim = 2:1,
                       dimnames = list(sex = c("m", "f"), iteration = 1)))
    e2 <- Counts(array(as.numeric(3:10),
                       dim = c(2, 4),
                       dimnames = list(sex = c("m", "f"), iteration = 2:5)))
    metadata <- new("MetaData",
                    nms = c("sex", "iteration"),
                    dimtypes = c("state", "iteration"),
                    DimScales = list(new("Categories", dimvalues = c("m", "f")),
                        new("Iterations", dimvalues = 1:5)))
    ans.obtained <- combineDbindData(e1 = e1, e2 = e2, metadata = metadata)
    ans.expected <- array(as.numeric(1:10),
                          dim = c(2, 5),
                          dimnames = list(sex = c("m", "f"), iteration = 1:5))
    expect_identical(ans.obtained, ans.expected)
})

test_that("combineDbindMetadataCounts works", {
    combineDbindMetadataCounts <- dembase:::combineDbindMetadataCounts
    e1 <- Counts(array(1:2,
                       dim = 2:1,
                       dimnames = list(sex = c("m", "f"), iteration = 1)))
    e2 <- Counts(array(2:10,
                       dim = c(2, 4),
                       dimnames = list(sex = c("m", "f"), iteration = 2:5)))
    ans.obtained <- combineDbindMetadataCounts(e1 = e1, e2 = e2, along = "iteration")
    ans.expected <- new("MetaData",
                        nms = c("sex", "iteration"),
                        dimtypes = c("sex", "iteration"),
                        DimScales = list(new("Sexes", dimvalues = c("m", "f")),
                            new("Iterations", dimvalues = 1:5)))
    expect_identical(ans.obtained, ans.expected)
})

test_that("combineDbindMetadataValues works", {
    combineDbindMetadataValues <- dembase:::combineDbindMetadataValues
    metadata <- dembase:::metadata
    makePairTransformsDbind <- dembase:::makePairTransformsDbind
    e1 <- Values(array(1:2,
                       dim = 2:1,
                       dimnames = list(sex = c("m", "f"), iteration = 1)))
    e2 <- Values(array(3:10,
                       dim = c(2, 4),
                       dimnames = list(sex = c("m", "f"), iteration = 2:5)))
    pair <- makePairTransformsDbind(e1 = e1, e2 = e2, along = "iteration")
    ans.obtained <- combineDbindMetadataValues(metadata1 = metadata(e1),
                                               metadata2 = metadata(e2),
                                               transform1 = pair[[1]],
                                               transform2 = pair[[2]],
                                               along = "iteration")
    ans.expected <- new("MetaData",
                        nms = c("sex", "iteration"),
                        dimtypes = c("sex", "iteration"),
                        DimScales = list(new("Sexes", dimvalues = c("m", "f")),
                            new("Iterations", dimvalues = 1:5)))
    expect_identical(ans.obtained, ans.expected)
    e1 <- Values(array(1:4,
                       dim = c(2, 2),
                       dimnames = list(sex = c("m", "f"), region = c("a", "b"))))
    e2 <- Values(array(3:10,
                       dim = c(2, 4, 1),
                       dimnames = list(sex = c("m", "f"), age = 0:3, region = "c")))
    pair <- makePairTransformsDbind(e1 = e1, e2 = e2, along = "region")
    ans.obtained <- combineDbindMetadataValues(metadata1 = metadata(e1),
                                               metadata2 = metadata(e2),
                                               transform1 = pair[[1]],
                                               transform2 = pair[[2]],
                                               along = "region")
    ans.expected <- new("MetaData",
                        nms = c("sex", "age", "region"),
                        dimtypes = c("sex", "age", "state"),
                        DimScales = list(new("Sexes", dimvalues = c("m", "f")),
                            new("Intervals", dimvalues = as.numeric(0:4)),
                            new("Categories", dimvalues = c("a", "b", "c"))))
    expect_identical(ans.obtained, ans.expected)
})

test_that("combineDimvaluesForIntervals works", {
    combineDimvaluesForIntervals <- dembase:::combineDimvaluesForIntervals
    expect_identical(combineDimvaluesForIntervals(new("Intervals", dimvalues = c(0, 1, 5)),
                                                  new("Intervals", dimvalues = c(5, 10)),
                                               along = "age"),
                     c(0, 1, 5, 10))
    expect_identical(combineDimvaluesForIntervals(new("Intervals", dimvalues = c(2000, 2005)),
                                               new("Intervals", dimvalues = c(1995, 2000)),
                                               along = "year"),
                     c(1995, 2000, 2005))
    expect_identical(combineDimvaluesForIntervals(new("Intervals"),
                                               new("Intervals", dimvalues = c(1995, 2000)),
                                               along = "year"),
                     c(1995, 2000))
    expect_identical(combineDimvaluesForIntervals(new("Intervals"),
                                               new("Intervals"),
                                               along = "year"),
                     numeric())
    expect_error(combineDimvaluesForIntervals(new("Intervals", dimvalues = 0:3),
                                           new("Intervals", dimvalues = 2:4),
                                           along = "age"),
                 "\"age\" dimensions overlap")
    expect_error(combineDimvaluesForIntervals(new("Intervals", dimvalues = c(2005, 2010)),
                                           new("Intervals", dimvalues = c(2007, 2020)),
                                           along = "time"),
                 "\"time\" dimensions overlap")
    expect_error(combineDimvaluesForIntervals(new("Intervals", dimvalues = c(2005, 2006)),
                                           new("Intervals", dimvalues = c(2007, 2020)),
                                           along = "time"),
                 "gap between \"time\" dimensions")
})

test_that("combineDimvaluesForPoints works", {
    combineDimvaluesForPoints <- dembase:::combineDimvaluesForPoints
    expect_identical(combineDimvaluesForPoints(new("Points", dimvalues = c(0, 1, 5)),
                                               new("Points", dimvalues = c(10, 15)),
                                               along = "age"),
                     c(0, 1, 5, 10, 15))
    expect_identical(combineDimvaluesForPoints(new("Points", dimvalues = c(2000, 2005)),
                                               new("Points", dimvalues = 1995),
                                               along = "year"),
                     c(1995, 2000, 2005))
    expect_identical(combineDimvaluesForPoints(new("Points"),
                                               new("Points", dimvalues = 1995),
                                               along = "year"),
                     1995)
    expect_identical(combineDimvaluesForPoints(new("Points"),
                                               new("Points"),
                                               along = "year"),
                     numeric())
    expect_error(combineDimvaluesForPoints(new("Points", dimvalues = 0:3),
                                           new("Points", dimvalues = 3:4),
                                           along = "age"),
                 "\"age\" dimensions overlap")
    expect_error(combineDimvaluesForPoints(new("Points", dimvalues = c(2005, 2010)),
                                           new("Points", dimvalues = c(2007, 2020)),
                                           along = "time"),
                 "\"time\" dimensions overlap")
})

test_that("dbind works", {
    x <- Counts(array(1:2,
                      dim = c(1, 2),
                      dimnames = list(iteration = 1, sex = c("m", "f"))))
    y <- Counts(array(1:2,
                      dim = c(1, 2),
                      dimnames = list(iteration = 2, sex = c("m", "f"))))
    z <- Counts(array(1:2,
                      dim = c(1, 2),
                      dimnames = list(iteration = 3, sex = c("m", "f"))))
    ans <- Counts(array(rep(1:2, each = 3),
                        dim = c(3, 2),
                        dimnames = list(iteration = 1:3, sex = c("m", "f"))))
    expect_identical(dbind(x, y, z, along = "iteration"), ans)
    expect_identical(dbind(args = list(x, y, z), along = "iteration"), ans)
    expect_identical(dbind(x, y, args = list(z), along = "iteration"), ans)
    expect_identical(dbind(x = x, y = y, z = z, along = "iteration"), ans)
    expect_identical(dbind(args = list(x = x, y = y, z = z), along = "iteration"), ans)
    x <- CountsOne(values = 1:2, labels = c("f", "m"), name = "sex")
    y <- CountsOne(values = 3:4, labels = c("f", "m"), name = "sex")
    z <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(sex = c("f", "m"), reg = c("x", "y"))))
    expect_identical(dbind(x = x, args = list(y = y), along = "reg"),
                     z)
    x <- Values(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(age = c("0-4", "5+"), sex = c("f", "m"))))
    y <- Values(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(reg = c("a", "b"), sex = c("f", "m"))))
    z <- Values(array(1:3,
                      dim = c(3, 1),
                      dimnames = list(age = c("0-4", "5-9", "10+"), reg = "c")))
    ans <- Values(array(c(1L, 2L, 2L, 3L, 4L, 4L,
                          1L, 1L, 1L, 3L, 3L, 3L,
                          2L, 2L, 2L, 4L, 4L, 4L,
                          1L, 2L, 3L, 1L, 2L, 3L),
                        dim = c(3, 2, 4),
                        dimnames = list(age = c("0-4", "5-9", "10+"),
                            sex = c("f", "m"), reg = c("x", "a", "b", "c"))))
    expect_identical(dbind(x, y, z, along = "reg"), ans)
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(period = c("2001-2005", "2006-2010",
                                          "2011-2015"),
                          reg = c("a", "b"))))
    y <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(sex = c("m", "f"), period = c("2001-2010",
                                                             "2011-2015"))))
    ans <- Counts(array(c(3L, 3L, 9L, 6L, 3L, 7L),
                        dim = c(2, 3),
                        dimnames = list(period = c("2001-2010", "2011-2015"),
                            reg = c("a", "b", "y"))))
    expect_identical(dbind(x, y, along = "reg"), ans)
    expect_identical(dbind(args = list(x, y), along = "reg"), ans)
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(period = c("2001-2005", "2006-2010",
                                          "2011-2015"),
                          reg = c("a", "b"))))
    y <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(sex = c("m", "f"),
                          period = c("2001-2010", "2011-2015"))))
    ans <- Counts(array(c(3L, 3L, 9L, 6L, 3L, 7L),
                        dim = c(2, 3),
                        dimnames = list(period = c("2001-2010", "2011-2015"),
                            reg = c("a", "b", "my.name"))))
    expect_identical(dbind(x, my.name = y, along = "reg"), ans)
    x <- Values(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(eth_parent = c("a", "b"),
                          eth_child = c("a", "b"))))
    y <- Values(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(eth_parent = c("c", "d"),
                          eth_child = c("a", "b"))))
    ans <- Values(array(c(1:2, 1:2, 3:4, 3:4),
                        dim = c(4, 2),
                        dimnames = list(eth_parent = c("a", "b", "c", "d"),
                            eth_child = c("a", "b"))))
    expect_identical(dbind(x, y, along = "eth_parent"), ans)
    expect_error(dbind(1, 2, along = "age"),
                 "object with class \"numeric\"")
    x <- Values(array(1:2, dim = 2, dimnames = list(sex = c("f", "m"))))
    expect_identical(dbind(x, along = "sex"), x)
    expect_error(dbind(x, along = c("sex", "sex")),
                 "'along' does not have length 1")
    expect_error(dbind(x, along = NA),
                 "'along' is missing")
    expect_error(dbind(x, along = ""),
                 "'along' is blank")
    x <- Values(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(eth_parent = c("a", "b"),
                          eth_child = c("a", "b"))))
    y <- Values(array(0L,
                      dim = c(2, 0),
                      dimnames = list(eth_parent = c("a", "b"), eth_child = NULL)))
    expect_identical(dbind(x, y, along = "eth_child"), x)
    ## objects in wrong order
    x <- Values(array(3:6,
                      dim = c(2, 2),
                      dimnames = list(region = c("a", "b"),
                          age = c("5-9", "10+"))))
    y <- Values(array(1:2,
                      dim = c(2, 1),
                      dimnames = list(region = c("a", "b"), age = "0-4")))
    ans.obtained <- dbind(x, y, along = "age")
    ans.expected <- Values(array(1:6,
                                 dim = c(2, 3),
                                 dimnames = list(region = c("a", "b"), age = c("0-4", "5-9", "10+"))))
    expect_identical(ans.obtained, ans.expected)
    ## objects in wrong order, and inferring age group
    x <- Values(array(3:6,
                      dim = c(2, 2),
                      dimnames = list(region = c("a", "b"),
                          age = c("5-9", "10+"))))
    y <- Values(array(1:2,
                      dim = 2,
                      dimnames = list(region = c("a", "b"))))
    ans.obtained <- dbind(x, "0-4" = y, along = "age")
    ans.expected <- Values(array(1:6,
                                 dim = c(2, 3),
                                 dimnames = list(region = c("a", "b"), age = c("0-4", "5-9", "10+"))))
    expect_identical(ans.obtained, ans.expected)
})


test_that("dimtypeAlongDbind works", {
    dimtypeAlongDbind <- dembase:::dimtypeAlongDbind
    ## all objects have dimtype
    x <- Values(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(eth_parent = c("a", "b"),
                          eth_child = c("a", "b"))))
    y <- Values(array(0L,
                      dim = c(2, 0),
                      dimnames = list(eth_parent = c("a", "b"), eth_child = NULL)))
    ans.obtained <- dimtypeAlongDbind(list(x, y), along = "eth_child")
    ans.expected <- "child"
    expect_identical(ans.obtained, ans.expected)
    ## some objects have dimtype
    x <- Values(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(region = c("a", "b"),
                          age = c("0-4", "5+"))))
    y <- Values(array(5:6,
                      dim = 2,
                      dimnames = list(region = c("a", "b"))))
    ans.obtained <- dimtypeAlongDbind(list(x, y), along = "age")
    ans.expected <- "age"
    expect_identical(ans.obtained, ans.expected)
    ## objects have dimtype
    x <- Values(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(eth_parent = c("a", "b"),
                          eth_child = c("a", "b"))))
    y <- Values(array(0L,
                      dim = c(2, 0),
                      dimnames = list(eth_parent = c("a", "b"), eth_child = NULL)))
    ans.obtained <- dimtypeAlongDbind(list(x, y), along = "eth_child")
    ans.expected <- "child"
    expect_identical(ans.obtained, ans.expected)
    ## no objects have dimtype
    x <- Values(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(eth_parent = c("a", "b"),
                          eth_child = c("a", "b"))))
    y <- Values(array(0L,
                      dim = c(2, 0),
                      dimnames = list(eth_parent = c("a", "b"), eth_child = NULL)))
    ans.obtained <- dimtypeAlongDbind(list(x, y), along = "region")
    ans.expected <- "state"
    expect_identical(ans.obtained, ans.expected)
    ## different dimtypes 
    x <- Values(array(1:2,
                      dim = c(1, 2),
                      dimnames = list(age = "old",
                          region = c("a", "b"))),
                dimtypes = c(age = "state"))
    y <- Values(array(1:2,
                      dim = c(2, 1),
                      dimnames = list(region = c("a", "b"), age = "5-9")))
    expect_error(dimtypeAlongDbind(list(x, y), along = "age"),
                 "\"age\" dimensions have different dimtypes : \"state\" versus \"age\"")
})

test_that("e1IsFirst works", {
    e1IsFirst <- dembase:::e1IsFirst
    x <- Counts(array(1L,
                      dim = c(2, 2),
                      dimnames = list(sex = c("m", "f"), age = c("0", "1-4"))))
    y <- Counts(array(1L,
                      dim = c(2, 2),
                      dimnames = list(age = c("5-9", "10+"), sex = c("m", "f"))))
    expect_true(e1IsFirst(e1 = x, e2 = y, along = "age"))
    expect_true(e1IsFirst(e1 = x, e2 = y, along = "sex"))
    expect_false(e1IsFirst(e1 = y, e2 = x, along = "age"))
    expect_true(e1IsFirst(e1 = y, e2 = x, along = "sex"))
})

test_that("fixAlongForDbind works", {
    fixAlongForDbind <- dembase:::fixAlongForDbind
    object <- Counts(array(1L,
                           dim = c(2, 2),
                           dimnames = list(sex = c("m", "f"), age = c("0", "1-4"))))
    ## 'object' has along
    object <- Counts(array(1L,
                           dim = c(2, 2),
                           dimnames = list(sex = c("m", "f"), age = c("0", "1-4"))))
    ans.obtained <- fixAlongForDbind(object,
                                     name = "x",
                                     along = "age",
                                     dimtypeAlong = "age")
    ans.expected <- object
    expect_identical(ans.obtained, ans.expected)
    ## 'object' does not have along - along not iterations
    object <- Counts(array(1L,
                           dim = c(2, 2),
                           dimnames = list(sex = c("m", "f"), age = c("0", "1-4"))))
    ans.obtained <- fixAlongForDbind(object,
                                     name = "x",
                                     along = "region",
                                     dimtypeAlong = "state")
    ans.expected <- Counts(array(1L,
                                 dim = c(2, 2, 1),
                                 dimnames = list(sex = c("m", "f"),
                                     age = c("0", "1-4"),
                                     region = "x")))
    expect_identical(ans.obtained, ans.expected)
    ## 'object' does not have along - along is iterations
    object <- Counts(array(1L,
                           dim = c(2, 2),
                           dimnames = list(sex = c("m", "f"), age = c("0", "1-4"))))
    ans.obtained <- fixAlongForDbind(object,
                                     name = "x",
                                     along = "iter",
                                     dimtypeAlong = "iteration")
    ans.expected <- Counts(array(1L,
                                 dim = c(2, 2, 1),
                                 dimnames = list(sex = c("m", "f"),
                                     age = c("0", "1-4"),
                                     iter = 1)))
    expect_identical(ans.obtained, ans.expected)
    ## length-0 dimension
    object <- Counts(array(0L,
                           dim = c(0, 2),
                           dimnames = list(sex = NULL, age = c("0", "1-4"))))
    ans.obtained <- fixAlongForDbind(object,
                                     name = "x",
                                     along = "iter",
                                     dimtypeAlong = "iteration")
    ans.expected <- Counts(array(0L,
                                 dim = c(0, 2, 1),
                                 dimnames = list(sex = NULL,
                                     age = c("0", "1-4"),
                                     iter = 1)))
    expect_identical(ans.obtained, ans.expected)
})


## FUNCTIONS FOR MANIPULATING METADATA #########################################

test_that("addIterationsToMetadata works", {
    addIterationsToMetadata <- dembase:::addIterationsToMetadata
    x <- new("MetaData",
             nms = c("age", "sex"),
             dimtypes = c("age", "state"),
             DimScales = list(new("Intervals", dimvalues = c(0, 5, Inf)),
             new("Categories", dimvalues = c("a", "b"))))
    y <- new("MetaData",
             nms = c("age", "sex", "iteration"),
             dimtypes = c("age", "state", "iteration"),
             DimScales = list(new("Intervals", dimvalues = c(0, 5, Inf)),
             new("Categories", dimvalues = c("a", "b")),
             new("Iterations", dimvalues = c(1L, 3L, 5L))))
    expect_identical(addIterationsToMetadata(x, iteration = c(1L, 3L, 5L)),
                     y)
    x <- new("MetaData",
             nms = c("age", "iteration"),
             dimtypes = c("age", "state"),
             DimScales = list(new("Intervals", dimvalues = c(0, 5, Inf)),
             new("Categories", dimvalues = c("a", "b"))))
    y <- new("MetaData",
             nms = c("age", "iteration", "iteration.1"),
             dimtypes = c("age", "state", "iteration"),
             DimScales = list(new("Intervals", dimvalues = c(0, 5, Inf)),
             new("Categories", dimvalues = c("a", "b")),
             new("Iterations", dimvalues = c(1L, 3L, 5L))))
    expect_identical(addIterationsToMetadata(x, iteration = c(1L, 3L, 5L)),
                     y)
    expect_error(addIterationsToMetadata(NULL, iteration = 1:3),
                 "'object' has class \"NULL\"")
    expect_error(addIterationsToMetadata(x, iteration = c(1:3, NA)),
                 "'iterations' has missing values")
    expect_error(addIterationsToMetadata(x, iteration = c(1:3, 1.3)),
                 "'iterations' has non-integer values")
    expect_error(addIterationsToMetadata(x, iteration = c(1:3, -1)),
                 "'iterations' has negative values")
        expect_error(addIterationsToMetadata(x, iteration = c(1:3, 1)),
                 "'iterations' has duplicates")
    expect_error(addIterationsToMetadata(y, iteration = 1:3),
                 "'object' already has dimension with dimtype \"iteration\"")
    z <- new("MetaData",
             nms = c("age", "quantile"),
             dimtypes = c("age", "quantile"),
             DimScales = list(new("Intervals", dimvalues = c(0, 5, Inf)),
             new("Quantiles", dimvalues = c(0.1, 0.5, 0.9))))
    expect_error(addIterationsToMetadata(z, iteration = 1:3),
                 "'object' has dimension with dimtype \"quantile\"")
})

test_that("mergeMetadata works", {
    mergeMetadata <- dembase:::mergeMetadata
    makePairTransforms <- dembase:::makePairTransforms
    e1 <- Values(array(0,
                       dim = c(2, 2, 2, 2),
                       dimnames = list(reg_orig = c("a", "b"),
                       reg_dest = c("a", "b"),
                       age = c("0-9", "10+"),
                       sex = c("m", "f"))))
    e2 <- Values(array(0,
                       dim = c(3, 2, 2),
                       dimnames = list(age = c("0-4", "5-9", "10+"),
                       reg_orig = c("a", "b"),
                       reg_dest = c("a", "b"))))
    pair <- makePairTransforms(e1, e2)
    expect_identical(mergeMetadata(metadata1 = e1@metadata,
                                   metadata2 = e2@metadata,
                                   transform1 = pair[[1]],
                                   transform2 = pair[[2]]),
                     new("MetaData",
                         nms = c("reg_orig", "reg_dest", "age", "sex"),
                         dimtypes = c("origin", "destination", "age", "sex"),
                         DimScales = list(new("Categories", dimvalues = c("a", "b")),
                         new("Categories", dimvalues = c("a", "b")),
                         new("Intervals", dimvalues = c(0, 5, 10, Inf)),
                         new("Sexes", dimvalues = c("m", "f")))))
})


## HELPER FUNCTIONS FOR DEMOGRAPHIC ACCOUNTS ######################################

test_that("ageDimBirthsCompatibleWithPopn works", {
    ageDimBirthsCompatibleWithPopn <- dembase:::ageDimBirthsCompatibleWithPopn
    name <- "age"
    DimScale <- new("Intervals", dimvalues = seq(15, 50, 5))
    namesPopn <- c("time", "age")
    dimtypesPopn <- c("time", "age")
    DimScalesPopn <- list(new("Points", dimvalues = seq(2000, 2020, 5)),
                          new("Intervals", dimvalues = c(seq(0, 100, 5), Inf)))
    nameComponent <- "births"
    expect_true(ageDimBirthsCompatibleWithPopn(name = name,
                                               DimScale = DimScale,
                                               namesPopn = namesPopn,
                                               dimtypesPopn = dimtypesPopn,
                                               DimScalesPopn = DimScalesPopn,
                                               nameComponent = nameComponent))
    ## 'population' has name
    name <- "age"
    DimScale <- new("Intervals", dimvalues = seq(15, 50, 5))
    namesPopn <- c("time", "sex")
    dimtypesPopn <- c("time", "state")
    DimScalesPopn <- list(new("Points", dimvalues = seq(2000, 2020, 5)),
                          new("Categories", dimvalues = c("f", "m")))
    nameComponent <- "births"
    expect_identical(ageDimBirthsCompatibleWithPopn(name = name,
                                                    DimScale = DimScale,
                                                    namesPopn = namesPopn,
                                                    dimtypesPopn = dimtypesPopn,
                                                    DimScalesPopn = DimScalesPopn,
                                                    nameComponent = nameComponent),
                     "'births' has dimension \"age\" but 'population' does not")
    ## dimension in 'population' has dimtype "age"
    name <- "age"
    DimScale <- new("Intervals", dimvalues = seq(15, 50, 5))
    namesPopn <- c("time", "age")
    dimtypesPopn <- c("time", "state")
    DimScalesPopn <- list(new("Points", dimvalues = seq(2000, 2020, 5)),
                          new("Categories", dimvalues = c("f", "m")))
    nameComponent <- "births"
    expect_identical(ageDimBirthsCompatibleWithPopn(name = name,
                                                    DimScale = DimScale,
                                                    namesPopn = namesPopn,
                                                    dimtypesPopn = dimtypesPopn,
                                                    DimScalesPopn = DimScalesPopn,
                                                    nameComponent = nameComponent),
                     "\"age\" dimension of 'births' has dimtype \"age\" but \"age\" dimension of 'population' has dimtype \"state\"")
    ## dimvalues aligned
    name <- "age"
    DimScale <- new("Intervals", dimvalues = seq(15, 50, 5))
    namesPopn <- c("time", "age")
    dimtypesPopn <- c("time", "age")
    DimScalesPopn <- list(new("Points", dimvalues = seq(2000, 2020, 5)),
                          new("Intervals", dimvalues = c(seq(0, 100), Inf)))
    nameComponent <- "births"
    expect_identical(ageDimBirthsCompatibleWithPopn(name = name,
                                                    DimScale = DimScale,
                                                    namesPopn = namesPopn,
                                                    dimtypesPopn = dimtypesPopn,
                                                    DimScalesPopn = DimScalesPopn,
                                                    nameComponent = nameComponent),
                     "\"age\" dimensions have incompatible dimscales")
    name <- "age"
    DimScale <- new("Intervals", dimvalues = seq(15, 50, 5))
    namesPopn <- c("time", "age")
    dimtypesPopn <- c("time", "age")
    DimScalesPopn <- list(new("Points", dimvalues = seq(2000, 2020, 5)),
                          new("Intervals", dimvalues = c(seq(0, 100, 10), Inf)))
    nameComponent <- "births"
    expect_identical(ageDimBirthsCompatibleWithPopn(name = name,
                                                    DimScale = DimScale,
                                                    namesPopn = namesPopn,
                                                    dimtypesPopn = dimtypesPopn,
                                                    DimScalesPopn = DimScalesPopn,
                                                    nameComponent = nameComponent),
                     "\"age\" dimensions have incompatible dimscales")    
})

test_that("default version of agePopnForwardUpperTri works", {
    agePopnForwardUpperTri <- dembase:::agePopnForwardUpperTri
    Population <- dembase:::Population
    population <- Counts(array(1:42,
                               dim = c(2, 7, 3),
                               dimnames = list(reg = c("a", "b"),
                                               age = c("0-4", "5-9", "10-14", "15-19",
                                                       "20-24", "25-29", "30+"),
                                               time = c("2000", "2005", "2010"))))
    population <- Population(population)
    ans.obtained <- agePopnForwardUpperTri(population)
    ans.expected <- Counts(array(c(1:12, 15:26),
                                 dim = c(2, 6, 2),
                                 dimnames = list(reg = c("a", "b"),
                                                 age = c(5, 10, 15, 20, 25, 30),
                                                 time = c("2001-2005", "2006-2010"))))
    expect_identical(ans.obtained, ans.expected)
})

test_that("checkAdjustAndScale works", {
    checkAdjustAndScale <- dembase:::checkAdjustAndScale
    expect_null(checkAdjustAndScale(adjust = FALSE, scale = 0.1))
    expect_null(checkAdjustAndScale(adjust = TRUE, scale = 0.1))
    expect_error(checkAdjustAndScale(adjust = "TRUE", scale = 0.1),
                 "'adjust' does not have type \"logical\"")
    expect_error(checkAdjustAndScale(adjust = c(TRUE, FALSE), scale = 0.1),
                 "'adjust' does not have length 1")
    expect_error(checkAdjustAndScale(adjust = TRUE, scale = "0.1"),
                 "'scale' is non-numeric")
    expect_error(checkAdjustAndScale(adjust = TRUE, scale = c(0.1, 0.1)),
                 "'scale' does not have length 1")
    expect_error(checkAdjustAndScale(adjust = TRUE, scale = 0),
                 "'scale' is non-positive")
})

test_that("checkAndTidyComponent works", {
    checkAndTidyComponent <- dembase:::checkAndTidyComponent
    splitTriangles <- dembase:::splitTriangles
    ## valid object
    x <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(age = c("0-4", "5+"),
                                      time = c("2001-2005", "2006-2010"))))
    set.seed(1)
    ans.obtained <- checkAndTidyComponent(x, name = "immigration")
    set.seed(1)
    ans.expected <- splitTriangles(x)
    expect_identical(ans.obtained, ans.expected)
    ## integer
    x <- Counts(array(as.numeric(1:8),
                      dim = c(2, 2, 2),
                      dimnames = list(age = c("0-4", "5+"),
                                      time = c("2001-2005", "2006-2010"),
                                      triangle = c("TL", "TU"))))
    ans.obtained <- checkAndTidyComponent(x, name = "component")
    ans.expected <- toInteger(x)
    expect_identical(ans.obtained, ans.expected)
    x <- Counts(array(c(1:7, 8.1),
                      dim = c(2, 2, 2),
                      dimnames = list(age = c("0-4", "5+"),
                                      time = c("2001-2005", "2006-2010"),
                                      triangle = c("TL", "TU"))))
    expect_error(checkAndTidyComponent(x, name = "component"),
                 "'component' invalid : non-integer values")
    ## time, age, cohort dimensions
    x <- Counts(array(1:8,
                      dim = c(2, 2, 2),
                      dimnames = list(age = c("0-4", "5+"),
                                      reg = 1:2,
                                      eth = 1:2)))
    expect_error(checkAndTidyComponent(x, name = "component"),
                 "'component' does not have dimension with dimtype \"time\"")
    x <- Counts(array(1:8,
                      dim = c(2, 2, 2),
                      dimnames = list(age = c("0-4", "5+"),
                                      time = c(2000, 2005),
                                      region = 1:2)))
    expect_error(checkAndTidyComponent(x, name = "component"),
                 "dimension of 'component' with dimtype \"time\" has dimscale \"Points\"")
    x <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(age = c(0, 5),
                                      time = c("2001-2005", "2006-2010"))))
    expect_error(checkAndTidyComponent(x, name = "component"),
                 "dimension of 'component' with dimtype \"age\" has dimscale \"Points\"")
    x <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(age = c("<0", "0-4"),
                                      time = c("2001-2005", "2006-2010"))))
    expect_error(checkAndTidyComponent(x, name = "component"),
                 "'component' invalid : first interval of dimension with dimtype \"age\" is open")
    x <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(cohort = c("2001-2005", "2006-20120"),
                                      time = c("2001-2005", "2006-2010"))))
    expect_error(checkAndTidyComponent(x, name = "component"),
                 "'component' has dimension with dimtype \"cohort\"")
    ## origin-destination
    x <- Counts(array(1:16,
                      dim = c(2, 2, 2, 2),
                      dimnames = list(age = c("0-4", "5+"),
                                      time = c("2001-2005", "2006-2010"),
                                      reg_orig = 1:2,
                                      reg_dest = 1:2)))
    set.seed(1)
    ans.obtained <- checkAndTidyComponent(x,
                                          name = "immigration",
                                          allowOrig = TRUE)
    set.seed(1)
    ans.expected <- splitTriangles(x)
    expect_identical(ans.obtained, ans.expected)
    expect_error(checkAndTidyComponent(x, name = "immigration"),
                 "'immigration' has dimensions with dimtypes \"origin\" and \"destination\"")
    ## parent-child
    x <- Counts(array(1:16,
                      dim = c(2, 2, 2, 2),
                      dimnames = list(age = c("0-4", "5+"),
                                      time = c("2001-2005", "2006-2010"),
                                      reg_parent = 1:2,
                                      reg_child = 1:2)))
    set.seed(1)
    ans.obtained <- checkAndTidyComponent(x,
                                          name = "immigration",
                                          allowParent = TRUE)
    set.seed(1)
    ans.expected <- splitTriangles(x)
    expect_identical(ans.obtained, ans.expected)
    expect_error(checkAndTidyComponent(x, name = "immigration"),
                  "'immigration' has dimensions with dimtypes \"parent\" and \"child\"")
    ## regular
    x <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(age = c("0-4", "5+"),
                                      time = c("2001-2005", "2006-2011"))))
    expect_error(checkAndTidyComponent(x, name = "immigration"),
                 "'immigration' does not have regular age-time plan :")
    ## positive length
    x <- Counts(array(0,
                      dim = c(0, 2),
                      dimnames = list(age = character(),
                                      time = c("2001-2005", "2006-2010"))))
    expect_error(checkAndTidyComponent(x, name = "immigration"),
                 "'immigration' has length 0")
    ## negatives
    x <- Counts(array(c(1:3, -1L),
                      dim = c(2, 2),
                      dimnames = list(age = c("0-4", "5+"),
                                      time = c("2001-2005", "2006-2010"))))
    expect_error(checkAndTidyComponent(x, name = "immigration"),
                 "'immigration' has negative values")
    ## allow triangles
    x <- Counts(array(as.numeric(1:8),
                      dim = c(2, 2, 2),
                      dimnames = list(age = c("0-4", "5+"),
                                      time = c("2001-2005", "2006-2010"),
                                      triangle = c("TL", "TU"))))
    expect_error(checkAndTidyComponent(x, name = "component", allowTriangles = FALSE),
                 "'component' has Lexis triangles")
    ## triangle
    x <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(age = c("0-4", "5+"),
                                      time = c("2001-2005", "2006-2010"))))
    x2 <- splitTriangles(x)
    ans.obtained <- checkAndTidyComponent(x2, name = "immigration")
    ans.expected <- x2
    expect_identical(ans.obtained, ans.expected)
    x <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(age = c("0-4", "5+"),
                                      time = c("2001-2005", "2006-2010"))))
    set.seed(1)
    ans.obtained <- checkAndTidyComponent(x, name = "immigration")
    set.seed(1)
    ans.expected <- splitTriangles(x)
    expect_identical(ans.obtained, ans.expected)
    x <- Counts(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(age = c("0-4", "5+"),
                                      time = c("2001-2005", "2006-2010"))))
    set.seed(1)
    ans.obtained <- checkAndTidyComponent(x, name = "immigration", triangles = FALSE)
    ans.expected <- x
    expect_identical(ans.obtained, ans.expected)
})

test_that("checkNamesComponents works", {
    checkNamesComponents <- dembase:::checkNamesComponents
    expect_identical(checkNamesComponents(c("a", "b", "c"),
                                          componentType = "entries"),
                     NULL)
    expect_error(checkNamesComponents(NULL, 
                                      componentType = "entries"),
                 "'entries' does not have names")
    expect_error(checkNamesComponents(c("a", "b", NA),
                                      componentType = "entries"),
                 "names for 'entries' have missing values")
    expect_error(checkNamesComponents(c("a", "b", ""),
                                      componentType = "entries"),
                 "names for 'entries' have blanks")
    expect_error(checkNamesComponents(c("a", "b", "b"),
                                      componentType = "entries"),
                 "names for 'entries' have duplicates")
})    

test_that("derivePopnMoveNoAge works when population is large", {
    set.seed(1)
    derivePopnMoveNoAge <- dembase:::derivePopnMoveNoAge
    population <- Counts(array(1001:1015,
                               dim = c(5, 10),
                               dimnames = list(region = 1:5,
                                               year = seq(2000, 2045, 5))))
    births <- array(1:4,
                    dim = c(5, 9),
                    dimnames = list(region = 1:5,
                                    year = paste(seq(2001, 2041, 5),
                                                 seq(2005, 2045, 5),
                                                 sep = "-")))
    internal <- array(1:10,
                      dim = c(5, 5, 9),
                      dimnames = list(region_orig = 1:5,
                                      region_dest = 1:5,
                                      year = paste(seq(2001, 2041, 5),
                                                   seq(2005, 2045, 5),
                                                   sep = "-")))
    deaths <- array(1:4,
                    dim = c(5, 9),
                    dimnames = list(region = 1:5,
                                    year = paste(seq(2001, 2041, 5),
                                                 seq(2005, 2045, 5),
                                                 sep = "-")))
    births <- Counts(births)
    deaths <- Counts(deaths)
    internal <- Counts(internal)
    object <- Movements(population = population,
                        births = births,
                        internal = internal,
                        exits = list(deaths = deaths))
    ans.obtained <- derivePopnMoveNoAge(object,
                                        adjust = FALSE,
                                        scale = 0.1)
    expect_true(validObject(ans.obtained))
    expect_false(all(ans.obtained@population[,1] == ans.obtained@population[,10]))
    expect_true(all(isConsistent(ans.obtained)))
})

test_that("derivePopnMoveNoAge works when population is small", {
    set.seed(1)
    derivePopnMoveNoAge <- dembase:::derivePopnMoveNoAge
    population <- Counts(array(21:35,
                               dim = c(5, 10),
                               dimnames = list(region = 1:5,
                                               year = seq(2000, 2045, 5))))
    births <- array(1:4,
                    dim = c(5, 9),
                    dimnames = list(region = 1:5,
                                    year = paste(seq(2001, 2041, 5),
                                                 seq(2005, 2045, 5),
                                                 sep = "-")))
    internal <- array(1:10,
                      dim = c(5, 5, 9),
                      dimnames = list(region_orig = 1:5,
                                      region_dest = 1:5,
                                      year = paste(seq(2001, 2041, 5),
                                                   seq(2005, 2045, 5),
                                                   sep = "-")))
    deaths <- array(1:4,
                    dim = c(5, 9),
                    dimnames = list(region = 1:5,
                                    year = paste(seq(2001, 2041, 5),
                                                 seq(2005, 2045, 5),
                                                 sep = "-")))
    net.mig <- array(c(3, -2, 1, -4),
                    dim = c(5, 9),
                    dimnames = list(region = 1:5,
                                    year = paste(seq(2001, 2041, 5),
                                                 seq(2005, 2045, 5),
                                                 sep = "-")))
    births <- Counts(births)
    deaths <- Counts(deaths)
    internal <- Counts(internal)
    net.mig <- Counts(net.mig)
    object <- Movements(population = population,
                        births = births,
                        internal = internal,
                        exits = list(deaths = deaths),
                        net = list(net.mig = net.mig))
    expect_error(derivePopnMoveNoAge(object,
                                     adjust = FALSE,
                                     scale = 0.1),
                 "population has negative values")
    ans.obtained <- derivePopnMoveNoAge(object,
                                        adjust = TRUE,
                                        scale = 0.1)
    expect_true(validObject(ans.obtained))
    expect_false(all(ans.obtained@population[,1] == ans.obtained@population[,10]))
    expect_true(all(isConsistent(ans.obtained)))
})

test_that("derivePopnMoveHasAge works", {
    set.seed(1)
    derivePopnMoveHasAge <- dembase:::derivePopnMoveHasAge
    population <- Counts(array(501:510,
                               dim = c(5, 3, 5),
                               dimnames = list(region = 1:5,
                                               age = c("0-4", "5-9", "10+"),
                                               year = seq(2000, 2020, 5))))
    births <- Counts(array(50:55,
                           dim = c(5, 1, 4),
                           dimnames = list(region = 1:5,
                                           age = "5-9",
                                           year = paste(seq(2001, 2016, 5),
                                                        seq(2005, 2020, 5),
                                                        sep = "-"))))
    internal <- array(1:10,
                      dim = c(5, 5, 3, 4),
                      dimnames = list(region_orig = 1:5,
                                      region_dest = 1:5,
                                      age = c("0-4", "5-9", "10+"),
                                      year = paste(seq(2001, 2016, 5),
                                                   seq(2005, 2020, 5),
                                                   sep = "-")))
    internal[slice.index(internal, 1) == slice.index(internal, 2)] <- 0L
    internal <- Counts(internal)
    deaths <- Counts(array(15:10,
                           dim = c(5, 3, 4),
                           dimnames = list(region = 1:5,
                                           age = c("0-4", "5-9", "10+"),
                                           year = paste(seq(2001, 2016, 5),
                                                        seq(2005, 2020, 5),
                                                        sep = "-"))))
    object <- Movements(population = population,
                        births = births,
                        internal = internal,
                        exits = list(deaths = deaths))
    expect_error(derivePopnMoveHasAge(object,
                                      adjust = FALSE,
                                      scale = 0.1),
                 "population has negative values")
    ans.obtained <- derivePopnMoveHasAge(object,
                                         adjust = TRUE,
                                         scale = 0.1)
    expect_true(validObject(ans.obtained))
    expect_false(all(ans.obtained@population@.Data[,,1] == ans.obtained@population@.Data[,,5]))
    expect_true(all(isConsistent(ans.obtained)))
})




test_that("dimCompCompatibleWithPopn works", {
    dimCompCompatibleWithPopn <- dembase:::dimCompCompatibleWithPopn
    ## births - age dimension
    name <- "age"
    dimtype <- "age"
    DimScale <- new("Intervals", dimvalues = seq(15, 50, 5))
    namesPopn <- c("time", "age", "sex")
    dimtypesPopn <- c("time", "age", "state")
    DimScalesPopn <- list(new("Points", dimvalues = seq(2000, 2020, 5)),
                          new("Intervals", dimvalues = c(seq(0, 100, 5), Inf)),
                          new("Categories", dimvalues = c("f", "m")))
    nameComponent <- "births"
    expect_true(dimCompCompatibleWithPopn(name = name,
                                          dimtype = dimtype,
                                          DimScale = DimScale,
                                          namesPopn = namesPopn,
                                          dimtypesPopn = dimtypesPopn,
                                          DimScalesPopn = DimScalesPopn,
                                          isBirths = TRUE,
                                          nameComponent = nameComponent))
    ## births - non-age dimension
    name <- "sex"
    dimtype <- "state"
    DimScale <- new("Categories", dimvalues = c("f", "m"))
    namesPopn <- c("time", "age", "sex")
    dimtypesPopn <- c("time", "age", "state")
    DimScalesPopn <- list(new("Points", dimvalues = seq(2000, 2020, 5)),
                          new("Intervals", dimvalues = c(seq(0, 100, 5), Inf)),
                          new("Categories", dimvalues = c("f", "m")))
    nameComponent <- "births"
    expect_true(dimCompCompatibleWithPopn(name = name,
                                          dimtype = dimtype,
                                          DimScale = DimScale,
                                          namesPopn = namesPopn,
                                          dimtypesPopn = dimtypesPopn,
                                          DimScalesPopn = DimScalesPopn,
                                          isBirths = TRUE,
                                          nameComponent = nameComponent))
    ## births - parent-child
    name <- "eth_child"
    dimtype <- "child"
    DimScale <- new("Categories", dimvalues = c("a", "b"))
    namesPopn <- c("time", "age", "eth")
    dimtypesPopn <- c("time", "age", "state")
    DimScalesPopn <- list(new("Points", dimvalues = seq(2000, 2020, 5)),
                          new("Intervals", dimvalues = c(seq(0, 100, 5), Inf)),
                          new("Categories", dimvalues = c("a", "b")))
    nameComponent <- "births"
    expect_true(dimCompCompatibleWithPopn(name = name,
                                          dimtype = dimtype,
                                          DimScale = DimScale,
                                          namesPopn = namesPopn,
                                          dimtypesPopn = dimtypesPopn,
                                          DimScalesPopn = DimScalesPopn,
                                          isBirths = TRUE,
                                          nameComponent = nameComponent))
    ## internal - origin-destination
    name <- "eth_orig"
    dimtype <- "origin"
    DimScale <- new("Categories", dimvalues = c("a", "b"))
    namesPopn <- c("time", "age", "eth")
    dimtypesPopn <- c("time", "age", "state")
    DimScalesPopn <- list(new("Points", dimvalues = seq(2000, 2020, 5)),
                          new("Intervals", dimvalues = c(seq(0, 100, 5), Inf)),
                          new("Categories", dimvalues = c("a", "b")))
    nameComponent <- "internal"
    expect_true(dimCompCompatibleWithPopn(name = name,
                                          dimtype = dimtype,
                                          DimScale = DimScale,
                                          namesPopn = namesPopn,
                                          dimtypesPopn = dimtypesPopn,
                                          DimScalesPopn = DimScalesPopn,
                                          isBirths = FALSE,
                                          nameComponent = nameComponent))
    ## exits - time
    name <- "year"
    dimtype <- "time"
    DimScale <- new("Intervals", dimvalues = seq(2000, 2020, 5))
    namesPopn <- c("year", "age", "eth")
    dimtypesPopn <- c("time", "age", "state")
    DimScalesPopn <- list(new("Points", dimvalues = seq(2000, 2020, 5)),
                          new("Intervals", dimvalues = c(seq(0, 100, 5), Inf)),
                          new("Categories", dimvalues = c("a", "b")))
    nameComponent <- "deaths"
    expect_true(dimCompCompatibleWithPopn(name = name,
                                          dimtype = dimtype,
                                          DimScale = DimScale,
                                          namesPopn = namesPopn,
                                          dimtypesPopn = dimtypesPopn,
                                          DimScalesPopn = DimScalesPopn,
                                          isBirths = FALSE,
                                          nameComponent = nameComponent))
    ## exits - time
    name <- "lexis"
    dimtype <- "triangle"
    DimScale <- new("Triangles", dimvalues = c("TL", "TU"))
    namesPopn <- c("year", "age", "eth")
    dimtypesPopn <- c("time", "age", "state")
    DimScalesPopn <- list(new("Points", dimvalues = seq(2000, 2020, 5)),
                          new("Intervals", dimvalues = c(seq(0, 100, 5), Inf)),
                          new("Categories", dimvalues = c("a", "b")))
    nameComponent <- "deaths"
    expect_true(dimCompCompatibleWithPopn(name = name,
                                          dimtype = dimtype,
                                          DimScale = DimScale,
                                          namesPopn = namesPopn,
                                          dimtypesPopn = dimtypesPopn,
                                          DimScalesPopn = DimScalesPopn,
                                          isBirths = FALSE,
                                          nameComponent = nameComponent))
    ## exits - iteration
    name <- "sim"
    dimtype <- "iteration"
    DimScale <- new("Iterations", dimvalues = 1:2)
    namesPopn <- c("year", "age", "sim")
    dimtypesPopn <- c("time", "age", "iteration")
    DimScalesPopn <- list(new("Points", dimvalues = seq(2000, 2020, 5)),
                          new("Intervals", dimvalues = c(seq(0, 100, 5), Inf)),
                          new("Iterations", dimvalues = 1:2))
    nameComponent <- "deaths"
    expect_true(dimCompCompatibleWithPopn(name = name,
                                          dimtype = dimtype,
                                          DimScale = DimScale,
                                          namesPopn = namesPopn,
                                          dimtypesPopn = dimtypesPopn,
                                          DimScalesPopn = DimScalesPopn,
                                          isBirths = FALSE,
                                          nameComponent = nameComponent))
})

test_that("dimCompEqualToPopn works", {
    dimCompEqualToPopn <- dembase:::dimCompEqualToPopn
    name <- "sex"
    dimtype <- "state"
    DimScale <- new("Categories", dimvalues = c("f", "m"))
    namesPopn <- c("time", "sex")
    dimtypesPopn <- c("time", "state")
    DimScalesPopn <- list(new("Points", dimvalues = seq(2000, 2020, 5)),
                          new("Categories", dimvalues = c("f", "m")))
    nameComponent <- "arrivals"
    expect_true(dimCompEqualToPopn(name = name,
                                   dimtype = dimtype,
                                   DimScale = DimScale,
                                   namesPopn = namesPopn,
                                   dimtypesPopn = dimtypesPopn,
                                   DimScalesPopn = DimScalesPopn,
                                   nameComponent = nameComponent))
    ## 'population' has dimension
    name <- "sex"
    dimtype <- "state"
    DimScale <- new("Categories", dimvalues = c("f", "m"))
    namesPopn <- c("time", "region")
    dimtypesPopn <- c("time", "state")
    DimScalesPopn <- list(new("Points", dimvalues = seq(2000, 2020, 5)),
                          new("Categories", dimvalues = c("a", "b")))
    nameComponent <- "arrivals"
    expect_identical(dimCompEqualToPopn(name = name,
                                        dimtype = dimtype,
                                        DimScale = DimScale,
                                        namesPopn = namesPopn,
                                        dimtypesPopn = dimtypesPopn,
                                        DimScalesPopn = DimScalesPopn,
                                        nameComponent = nameComponent),
                     "'arrivals' has dimension \"sex\" but 'population' does not")
    ## identical dimtypes
    name <- "sex"
    dimtype <- "state"
    DimScale <- new("Categories", dimvalues = c("f", "m"))
    namesPopn <- c("time", "sex")
    dimtypesPopn <- c("time", "triangle")
    DimScalesPopn <- list(new("Points", dimvalues = seq(2000, 2020, 5)),
                          new("Triangles", dimvalues = c("TL", "TU")))
    nameComponent <- "arrivals"
    expect_identical(dimCompEqualToPopn(name = name,
                                        dimtype = dimtype,
                                        DimScale = DimScale,
                                        namesPopn = namesPopn,
                                        dimtypesPopn = dimtypesPopn,
                                        DimScalesPopn = DimScalesPopn,
                                        nameComponent = nameComponent),
                     "\"sex\" dimension of 'arrivals' has dimtype \"state\" but \"sex\" dimension of 'population' has dimtype \"triangle\"")
    ## dimvalues aligned
    name <- "sex"
    dimtype <- "state"
    DimScale <- new("Categories", dimvalues = c("f", "m"))
    namesPopn <- c("time", "sex")
    dimtypesPopn <- c("time", "state")
    DimScalesPopn <- list(new("Points", dimvalues = seq(2000, 2020, 5)),
                          new("Categories", dimvalues = c("m", "f")))
    nameComponent <- "arrivals"
    expect_identical(dimCompEqualToPopn(name = name,
                                        dimtype = dimtype,
                                        DimScale = DimScale,
                                        namesPopn = namesPopn,
                                        dimtypesPopn = dimtypesPopn,
                                        DimScalesPopn = DimScalesPopn,
                                        nameComponent = nameComponent),
                     "\"sex\" dimensions have incompatible dimscales")
})

test_that("exposureNoTriangles works", {
    exposureNoTriangles <- dembase:::exposureNoTriangles
    ## time is last dimension; equal time steps
    population <- Counts(array(1:42,
                               dim = c(2, 7, 3),
                               dimnames = list(reg = c("a", "b"),
                                   age = c("0-4", "5-9", "10-14", "15-19",
                                       "20-24", "25-29", "30+"),
                                   time = c("2000", "2005", "2010"))))
    ans.obtained <- exposureNoTriangles(population)
    ans.expected <- 2.5 * (population@.Data[,,1:2] + population@.Data[,,2:3])
    ans.expected <- Counts(array(ans.expected,
                                 dim = c(2, 7, 2),
                                 dimnames = list(reg = c("a", "b"),
                                     age = c("0-4", "5-9", "10-14", "15-19",
                                         "20-24", "25-29", "30+"),
                                     time = c("2001-2005", "2006-2010"))))
    expect_identical(ans.obtained, ans.expected)
    ## time is seceond dimension; equal time steps
    population <- Counts(array(1:42,
                               dim = c(2, 3, 7),
                               dimnames = list(reg = c("a", "b"),
                                   time = c("2000", "2005", "2010"),
                                   age = c("0-4", "5-9", "10-14", "15-19",
                                       "20-24", "25-29", "30+"))))
    ans.obtained <- exposureNoTriangles(population)
    ans.expected <- 2.5 * (population@.Data[,1:2,] + population@.Data[,2:3,])
    ans.expected <- Counts(array(ans.expected,
                                 dim = c(2, 2, 7),
                                 dimnames = list(reg = c("a", "b"),
                                     time = c("2001-2005", "2006-2010"),
                                     age = c("0-4", "5-9", "10-14", "15-19",
                                         "20-24", "25-29", "30+"))))
    expect_identical(ans.obtained, ans.expected)
    ## time is first dimension; unequal time steps
    population <- Counts(array(1:42,
                               dim = c(4, 2, 7),
                               dimnames = list(time = c(2000, 2001, 2005, 2010),
                                   reg = c("a", "b"),
                                   age = c("0-4", "5-9", "10-14", "15-19",
                                       "20-24", "25-29", "30+"))))
    ans.obtained <- exposureNoTriangles(population)
    ans.expected <- c(0.5, 2, 2.5) * (population@.Data[1:3,,] + population@.Data[2:4,,])
    ans.expected <- Counts(array(ans.expected,
                                 dim = c(3, 2, 7),
                                 dimnames = list(time = c("2001", "2002-2005", "2006-2010"),
                                     reg = c("a", "b"),
                                     age = c("0-4", "5-9", "10-14", "15-19",
                                         "20-24", "25-29", "30+"))))
    expect_identical(ans.obtained, ans.expected)
    ## no time dimension, is last dimension; unequal age steps
    population <- Counts(array(1:8,
                               dim = c(2, 4),
                               dimnames = list(reg = c("a", "b"),
                                   age = c(0, 5, 10, 20))))
    ans.obtained <- exposureNoTriangles(population)
    ans.expected <- 0.5 * (population@.Data[,-4] + population@.Data[,-1])
    ans.expected <- ans.expected * rep(c(5, 5, 10), each = 2)
    ans.expected <- Counts(array(ans.expected,
                                 dim = c(2, 3),
                                 dimnames = list(reg = c("a", "b"),
                                     age = c("0-4", "5-9", "10-19"))))
    expect_identical(ans.obtained, ans.expected)
})

test_that("exposureWithTriangles works", {
    exposureWithTriangles <- dembase:::exposureWithTriangles
    exposureNoTriangles <- dembase:::exposureNoTriangles
    ## time is last dimension
    population <- Counts(array(1:42,
                               dim = c(2, 7, 3),
                               dimnames = list(reg = c("a", "b"),
                                   age = c("0-4", "5-9", "10-14", "15-19",
                                       "20-24", "25-29", "30+"),
                                   time = c("2000", "2005", "2010"))))
    ans.obtained <- exposureWithTriangles(population)
    lower <- 2.5 * population@.Data[,,2:3]
    upper <- 2.5 * population@.Data[,,1:2]
    ans.expected <- Counts(array(c(as.numeric(lower), as.numeric(upper)),
                                 dim = c(2, 7, 2, 2),
                                 dimnames = list(reg = c("a", "b"),
                                     age = c("0-4", "5-9", "10-14", "15-19",
                                         "20-24", "25-29", "30+"),
                                     time = c("2001-2005", "2006-2010"),
                                     triangle = c("TL", "TU"))))
    expect_identical(ans.obtained, ans.expected)
    expect_equal(collapseDimension(ans.obtained, dimension = "triangle"),
                     exposureNoTriangles(population))
    ## time is second dimension, last age group is closed
    population <- Counts(array(1:42,
                               dim = c(2, 3, 7),
                               dimnames = list(reg = c("a", "b"),
                                   time = c("2000", "2005", "2010"),
                                   age = c("0-4", "5-9", "10-14", "15-19",
                                       "20-24", "25-29", "30-34"))))
    ans.obtained <- exposureWithTriangles(population)
    lower <- 2.5 * population@.Data[,2:3,]
    upper <- 2.5 * population@.Data[,1:2,]
    ans.expected <- Counts(array(c(as.numeric(lower), as.numeric(upper)),
                                 dim = c(2, 2, 7, 2),
                                 dimnames = list(reg = c("a", "b"),
                                     time = c("2001-2005", "2006-2010"),
                                     age = c("0-4", "5-9", "10-14", "15-19",
                                         "20-24", "25-29", "30-34"),
                                     triangle = c("TL", "TU"))))
    expect_identical(ans.obtained, ans.expected)
    expect_equal(collapseDimension(ans.obtained, dimension = "triangle"),
                     exposureNoTriangles(population))
})

test_that("getDimScaleTimePopn works", {
    getDimScaleTimePopn <- dembase:::getDimScaleTimePopn
    component <- Counts(array(1:12,
                              dim = c(3, 2, 2),
                              dimnames = list(age = c("0-4", "5-9", "10+"),
                                  triangle = c("TL", "TU"),
                                  time = c("2001-2005", "2006-2010"))))
    ans.obtained <- getDimScaleTimePopn(component, name = "component")
    ans.expected <- new("Points", dimvalues = c(2000, 2005, 2010))
    expect_identical(ans.obtained, ans.expected)
    component <- Counts(array(1:6,
                              dim = c(3, 2),
                              dimnames = list(age = c("0-4", "5-9", "10+"),
                                              region = c("a", "b"))))
    expect_error(getDimScaleTimePopn(component, name = "component"),
                 "'component' does not have a dimension with dimtype \"time\"")
    component <- Counts(array(1:6,
                              dim = c(3, 2),
                              dimnames = list(age = c("0-4", "5-9", "10+"),
                                              time = c("2000", "2005"))))
    expect_error(getDimScaleTimePopn(component, name = "component"),
                 "time dimension of 'component' does not have dimscale \"Intervals\"")
})

test_that("iMinAge works", {
    iMinAge <- dembase:::iMinAge
    ## valid arguments
    births <- Counts(array(1L,
                           dim = c(2, 2, 2, 2),
                           dimnames = list(reg = c("a", "b"),
                               age = c("20-24", "25-29"),
                               time = c("2001-2005", "2006-2010"),
                               triangle = c("TL", "TU"))))
    template <- Counts(array(0L,
                             dim = c(2, 7, 2, 2),
                             dimnames = list(reg = c("a", "b"),
                                 age = c("0-4", "5-9", "10-14", "15-19",
                                     "20-24", "25-29", "30+"),
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("TL", "TU"))))
    ans.obtained <- iMinAge(current = births, target = template)
    ans.expected <- 5L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- iMinAge(current = template, target = template)
    ans.expected <- 1L
    expect_identical(ans.obtained, ans.expected)
    wrong <- Counts(array(1L,
                          dim = c(2, 2),
                          dimnames = list(reg = c("a", "b"),
                              time = c("2001-2005", "2006-2010"))))
    expect_error(iMinAge(current = wrong, target = template),
                 "'current' does not have dimension with dimtype \"age\"")
    expect_error(iMinAge(current = births, target = wrong),
                 "'target' does not have dimension with dimtype \"age\"")
    wrong <-  Counts(array(0L,
                           dim = c(2, 4, 2, 2),
                           dimnames = list(reg = c("a", "b"),
                               age = c("0-4", "5-9", "10-14", "15-19"),
                               time = c("2001-2005", "2006-2010"),
                               triangle = c("TL", "TU"))))
    expect_error(iMinAge(current = births, target = wrong),
                 "minimum age of 'current' not found in ages of 'target'")
})

test_that("incrementLowerTriHelper works", {
    incrementLowerTriHelper <- dembase:::incrementLowerTriHelper
    EntriesMovements <- dembase:::EntriesMovements
    component <- Counts(array(1:12,
                              dim = c(3, 2, 2),
                              dimnames = list(age = c("0-4", "5-9", "10+"),
                                  triangle = c("TL", "TU"),
                                  time = c("2001-2005", "2006-2010"))))
    component <- EntriesMovements(component,
                                  template = component,
                                  name = "immigration")
    ans.obtained <- incrementLowerTriHelper(component)
    ans.expected <- Counts(array(c(1:3, 7:9),
                              dim = c(3, 2),
                              dimnames = list(age = c("0-4", "5-9", "10+"),
                                  time = c("2005", "2010"))))
    expect_identical(ans.obtained, ans.expected)
})

test_that("incrementOpenHelper works", {
    incrementOpenHelper <- dembase:::incrementOpenHelper
    EntriesMovements <- dembase:::EntriesMovements
    component <- Counts(array(1:12,
                              dim = c(3, 2, 2),
                              dimnames = list(age = c("0-4", "5-9", "10+"),
                                  triangle = c("TL", "TU"),
                                  time = c("2001-2005", "2006-2010"))))
    component <- EntriesMovements(component,
                                  template = component,
                                  name = "immigration")
    ans.obtained <- incrementOpenHelper(component)
    ans.expected <- Counts(array(c(rep(0L, 2), 6L, rep(0L, 2L), 12L),
                                 dim = c(3, 2),
                                 dimnames = list(age = c("0-4", "5-9", "10+"),
                                                 time = c("2005", "2010"))))
    expect_identical(ans.obtained, ans.expected)
})

test_that("incrementSquareHelper works", {
    incrementSquareHelper <- dembase:::incrementSquareHelper
    EntriesMovements <- dembase:::EntriesMovements
    component <- Counts(array(1:12,
                              dim = c(3, 2, 2),
                              dimnames = list(region = c("a", "b", "c"),
                                              sex = c("F", "M"),
                                              time = c("2001-2005", "2006-2010"))))
    component <- EntriesMovements(component,
                                  template = component,
                                  name = "immigration")
    ans.obtained <- incrementSquareHelper(component)
    ans.expected <- Counts(array(1:12,
                              dim = c(3, 2, 2),
                              dimnames = list(region = c("a", "b", "c"),
                                              sex = c("F", "M"),
                                              time = c("2005", "2010"))))
    expect_identical(ans.obtained, ans.expected)
})

test_that("incrementUpperTriHelper works", {
    incrementUpperTriHelper <- dembase:::incrementUpperTriHelper
    EntriesMovements <- dembase:::EntriesMovements
    component <- Counts(array(1:12,
                              dim = c(3, 2, 2),
                              dimnames = list(age = c("0-4", "5-9", "10+"),
                                  triangle = c("TL", "TU"),
                                  time = c("2001-2005", "2006-2010"))))
    component <- EntriesMovements(component,
                                  template = component,
                                  name = "immigration")
    ans.obtained <- incrementUpperTriHelper(component)
    ans.expected <- Counts(array(c(4:5, 10:11),
                              dim = c(2, 2),
                              dimnames = list(age = c("5", "10"),
                                  time = c("2001-2005", "2006-2010"))))
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeDimtypeIndex works", {
    makeDimtypeIndex <- dembase:::makeDimtypeIndex
    x <- Values(array(0,
                      dim = c(3, 2, 2, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"),
                          region_orig = c("a", "b"),
                          region_dest = c("a", "b"),
                          time = c("2001-2005", "2006-2010"))))
    ans.obtained <- makeDimtypeIndex(x, "age")
    ans.expected <- slice.index(x@.Data, 1)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeMappingBirths works", {
    makeMappingBirths <- dembase:::makeMappingBirths
    births <- Counts(array(1L,
                           dim = c(2, 5, 2),
                           dimnames = list(sex = c("m", "f"),
                                           reg = 1:5,
                                           time = c("2001-2005", "2006-2010"))))
    ans.obtained <- makeMappingBirths(births)
    ans.expected <- 1:10
    expect_identical(ans.obtained, ans.expected)
    births <- Counts(array(1L,
                           dim = c(2, 2, 2, 5, 2),
                           dimnames = list(age = c("5-9", "10-14"),
                                           triangle = c("TL", "TU"),
                                           sex = c("m", "f"),
                                           reg = 1:5,
                                           time = c("2001-2005", "2006-2010"))))
    ans.obtained <- makeMappingBirths(births)
    ans.expected <- rep(1:10, each = 4)
    expect_identical(ans.obtained, ans.expected)
    births <- Counts(array(1L,
                           dim = c(2, 5, 5, 2, 2, 2),
                           dimnames = list(sex = c("m", "f"),
                                           reg_child = 1:5,
                                           reg_parent = 1:5,
                                           time = c("2001-2005", "2006-2010"),
                                           age = c("5-9", "10-14"),
                                           triangle = c("TL", "TU"))))
    ans.obtained <- makeMappingBirths(births)
    ans.expected <- rep(rep(rep(1:10, times = 5), times = 2), times = 2)
    expect_identical(ans.obtained, ans.expected)
    births <- Counts(array(1L,
                           dim = c(2, 2, 5, 5, 2, 2),
                           dimnames = list(time = c("2001-2005", "2006-2010"),
                                           sex = c("m", "f"),
                                           reg_parent = 1:5,
                                           reg_child = 1:5,
                                           age = c("5-9", "10-14"),
                                           triangle = c("TL", "TU"))))
    ans.obtained <- makeMappingBirths(births)
    ans.expected <- rep(rep(c(rep(rep(1:2, each = 2), times = 5),
                          rep(rep(3:4, each = 2), times = 5),
                          rep(rep(5:6, each = 2), times = 5),
                          rep(rep(7:8, each = 2), times = 5),
                          rep(rep(9:10, each = 2), times = 5)),
                          times = 5),
                        times = 2)
    births <- CountsOne(values = 1:10, labels = 2001:2010, name = "year", dimscale = "Intervals")
    ans.obtained <- makeMappingBirths(births)
    ans.expected <- 1L
    expect_identical(ans.obtained, ans.expected)
    births <- Counts(array(1L,
                           dim = c(2, 2, 2),
                           dimnames = list(time = c("2001-2005", "2006-2010"),
                                           age = c("5-9", "10-14"),
                                           triangle = c("TL", "TU"))))
    ans.obtained <- makeMappingBirths(births)
    ans.expected <- rep(1L, 4L)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeMetadataExtendOrigDestParentChild works", {
    makeMetadataExtendOrigDestParentChild <- dembase:::makeMetadataExtendOrigDestParentChild
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
    ans.obtained <- makeMetadataExtendOrigDestParentChild(x = x, y = y)
    ans.expected <- x@metadata
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
    ans.obtained <- makeMetadataExtendOrigDestParentChild(x = x, y = y)
    ans.expected <- new("MetaData",
                        nms = c("age", "region_orig", "region_dest", "time"),
                        dimtypes = c("age", "origin", "destination", "time"),
                        DimScales = DimScales(y, use.names = F)[c(1, 2, 2, 3)])
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
    ans.obtained <- makeMetadataExtendOrigDestParentChild(x = x, y = y)
    ans.expected <- new("MetaData",
                        nms = c("age", "eth_parent", "eth_child", "region_orig", "region_dest", "time"),
                        dimtypes = c("age", "parent", "child", "origin", "destination", "time"),
                        DimScales = DimScales(y, use.names = F)[c(1, 2, 2, 3, 3, 4)])
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeMetadataForExposure works", {
    makeMetadataForExposure <- dembase:::makeMetadataForExposure
    population <- Counts(array(1:120,
                               dim = 4:6,
                               dimnames = list(reg = 1:4,
                                   age = 0:4,
                                   time = 1:6)),
                         dimscales = c(time = "Points"))
    ans.obtained <- makeMetadataForExposure(population = population,
                                            triangles = FALSE)
    ans.expected <- new("MetaData",
                        nms = c("reg", "age", "time"),
                        dimtypes = c("state", "age", "time"),
                        DimScales = list(new("Categories", dimvalues = as.character(1:4)),
                            new("Intervals", dimvalues = as.numeric(0:5)),
                            new("Intervals", dimvalues = as.numeric(1:6))))
    expect_identical(ans.obtained, ans.expected)
    population <- Counts(array(1:120,
                               dim = 4:6,
                               dimnames = list(reg = 1:4,
                                   age = 0:4,
                                   time = 1:6)),
                         dimscales = c(time = "Points"))
    ans.obtained <- makeMetadataForExposure(population = population,
                                            triangles = TRUE)
    ans.expected <- new("MetaData",
                        nms = c("reg", "age", "time", "triangle"),
                        dimtypes = c("state", "age", "time", "triangle"),
                        DimScales = list(new("Categories", dimvalues = as.character(1:4)),
                            new("Intervals", dimvalues = as.numeric(0:5)),
                            new("Intervals", dimvalues = as.numeric(1:6)),
                            new("Triangles", dimvalues = c("TL", "TU"))))
    expect_identical(ans.obtained, ans.expected)
    population <- Counts(array(1:20,
                               dim = 4:5,
                               dimnames = list(reg = 1:4,
                                   age = 0:4)),
                         dimscales = c(age = "Points"))
    ans.obtained <- makeMetadataForExposure(population = population,
                                            triangles = FALSE)
    ans.expected <- new("MetaData",
                        nms = c("reg", "age"),
                        dimtypes = c("state", "age"),
                        DimScales = list(new("Categories", dimvalues = as.character(1:4)),
                            new("Intervals", dimvalues = as.numeric(0:4))))
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeTemplateComponent works", {
    makeTemplateComponent <- dembase:::makeTemplateComponent
    ## has age
    population <- Counts(array(1:120,
                               dim = 4:6,
                               dimnames = list(reg = 1:4,
                                   age = 0:4,
                                   time = 1:6)),
                         dimscales = c(time = "Points"))
    ans.obtained <- makeTemplateComponent(population)
    ans.expected <- Counts(array(0L,
                                 dim = c(4, 5, 5, 2),
                                 dimnames = list(reg = 1:4,
                                     age = 0:4,
                                     time = 1:5,
                                     triangle = c("TL", "TU"))),
                           dimscales = c(time = "Intervals"))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- makeTemplateComponent(population, triangles = FALSE)
    ans.expected <- Counts(array(0L,
                                 dim = c(4, 5, 5),
                                 dimnames = list(reg = 1:4,
                                     age = 0:4,
                                     time = 1:5)),
                           dimscales = c(time = "Intervals"))
    expect_identical(ans.obtained, ans.expected)
    ## no age
    population <- Counts(array(1:24,
                               dim = c(4, 6),
                               dimnames = list(reg = 1:4,
                                   time = 1:6)),
                         dimscales = c(time = "Points"))
    ans.obtained <- makeTemplateComponent(population)
    ans.expected <- Counts(array(0L,
                                 dim = c(4, 5),
                                 dimnames = list(reg = 1:4,
                                     time = 1:5)),
                           dimscales = c(time = "Intervals"))
    expect_identical(ans.obtained, ans.expected)
})

test_that("pairDimCompCompatibleWithPopn works", {
    pairDimCompCompatibleWithPopn <- dembase:::pairDimCompCompatibleWithPopn
    name <- "reg_orig"
    dimtype <- "origin"
    DimScale <- new("Categories", dimvalues = c("a", "b"))
    namesPopn <- c("time", "reg")
    dimtypesPopn <- c("time", "state")
    DimScalesPopn <- list(new("Points", dimvalues = seq(2000, 2020, 5)),
                          new("Categories", dimvalues = c("a", "b")))
    nameComponent <- "internal"
    expect_true(pairDimCompCompatibleWithPopn(name = name,
                                              dimtype = dimtype,
                                              DimScale = DimScale,
                                              namesPopn = namesPopn,
                                              dimtypesPopn = dimtypesPopn,
                                              DimScalesPopn = DimScalesPopn,
                                              nameComponent = nameComponent))
    ## 'population' has dimension
    name <- "reg_dest"
    dimtype <- "destination"
    DimScale <- new("Categories", dimvalues = c("a", "b"))
    namesPopn <- c("time", "wrong")
    dimtypesPopn <- c("time", "state")
    DimScalesPopn <- list(new("Points", dimvalues = seq(2000, 2020, 5)),
                          new("Categories", dimvalues = c("a", "b")))
    nameComponent <- "internal"
    expect_identical(pairDimCompCompatibleWithPopn(name = name,
                                                   dimtype = dimtype,
                                                   DimScale = DimScale,
                                                   namesPopn = namesPopn,
                                                   dimtypesPopn = dimtypesPopn,
                                                   DimScalesPopn = DimScalesPopn,
                                                   nameComponent = nameComponent),
                     "'internal' has dimension \"reg_dest\" but 'population' does not have dimension \"reg\"")
    ## identical dimtypes
    name <- "reg_orig"
    dimtype <- "origin"
    DimScale <- new("Categories", dimvalues = c("a", "b"))
    namesPopn <- c("time", "reg")
    dimtypesPopn <- c("time", "triangle")
    DimScalesPopn <- list(new("Points", dimvalues = seq(2000, 2020, 5)),
                          new("Triangles", dimvalues = c("TL", "TU")))
    nameComponent <- "internal"
    expect_identical(pairDimCompCompatibleWithPopn(name = name,
                                                   dimtype = dimtype,
                                                   DimScale = DimScale,
                                                   namesPopn = namesPopn,
                                                   dimtypesPopn = dimtypesPopn,
                                                   DimScalesPopn = DimScalesPopn,
                                                   nameComponent = nameComponent),
                     "\"reg_orig\" dimension of 'internal' has dimtype \"origin\" but \"reg\" dimension of 'population' has dimtype \"triangle\"")
    ## dimvalues aligned
    name <- "reg_orig"
    dimtype <- "origin"
    DimScale <- new("Categories", dimvalues = c("a", "b"))
    namesPopn <- c("time", "reg")
    dimtypesPopn <- c("time", "state")
    DimScalesPopn <- list(new("Points", dimvalues = seq(2000, 2020, 5)),
                          new("Categories", dimvalues = c("b", "a")))
    nameComponent <- "internal"
    expect_identical(pairDimCompCompatibleWithPopn(name = name,
                                                   dimtype = dimtype,
                                                   DimScale = DimScale,
                                                   namesPopn = namesPopn,
                                                   dimtypesPopn = dimtypesPopn,
                                                   DimScalesPopn = DimScalesPopn,
                                                   nameComponent = nameComponent),
                     "\"reg_orig\" dimension of 'internal' and \"reg\" dimension of 'population' have incompatible dimscales")
})

test_that("popnEndNoAge works", {
    popnEndNoAge <- dembase:::popnEndNoAge
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
    ans.obtained <- popnEndNoAge(x)
    ans.expected <- Counts(x@population)[-1]
    expect_identical(ans.obtained, ans.expected)
})

test_that("popnEndWithAge works", {
    popnEndWithAge <- dembase:::popnEndWithAge
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
    x <- Movements(population = population,
                   births = births,
                   internal = internal)
    ans <- popnEndWithAge(x)
    expect_identical(ans@metadata, population[,,,-1]@metadata)
})

test_that("popnOpen works", {
    popnOpen <- dembase:::popnOpen
    population <- Counts(array(1:90,
                               dim = c(3, 2, 4, 3),
                               dimnames = list(age = c("0-4", "5-9", "10+"),
                                               sex = c("f", "m"),
                                               reg = 1:4,
                                               time = c(2000, 2005, 2010))))
    ans.obtained <- popnOpen(population)
    ans.expected <- Counts(array(0L,
                                 dim = c(3, 2, 4, 2),
                                 dimnames = list(age = c("0-4", "5-9", "10+"),
                                                 sex = c("f", "m"),
                                                 reg = 1:4,
                                                 time = c(2005, 2010))))
    ans.expected[3,,,] <- population[3,,,1:2]
    expect_identical(ans.obtained, ans.expected)
})

test_that("removeDimtypesMetadata works", {
    removeDimtypesFromMetadata <- dembase:::removeDimtypesFromMetadata
    metadata <- new("MetaData",
                    nms = c("age", "reg_dest", "reg_orig"),
                    dimtypes = c("age", "destination", "origin"),
                    DimScales = list(new("Intervals", dimvalues = c(0, 1, Inf)),
                                     new("Categories", dimvalues = c("a", "b")),
                                     new("Categories", dimvalues = c("a", "b"))))
    ans.obtained <- removeDimtypesFromMetadata(metadata, dimtypes = "age")
    ans.expected <- new("MetaData",
                        nms = c("reg_dest", "reg_orig"),
                        dimtypes = c("destination", "origin"),
                        DimScales = list(new("Categories", dimvalues = c("a", "b")),
                                         new("Categories", dimvalues = c("a", "b"))))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- removeDimtypesFromMetadata(metadata, dimtypes = "parent")
    ans.expected <- metadata
    expect_identical(ans.obtained, ans.expected)
    expect_error(removeDimtypesFromMetadata(metadata, dimtypes = c("age", "destination", "origin")),
                 "removing all dimensions")
    metadata <- new("MetaData",
                    nms = c("age", "sex", "reg"),
                    dimtypes = c("age", "sex", "state"),
                    DimScales = list(new("Intervals", dimvalues = c(0, 1, Inf)),
                                     new("Sexes", dimvalues = c("Female", "Male")),
                                     new("Categories", dimvalues = c("a", "b"))))
    ans.obtained <- removeDimtypesFromMetadata(metadata, dimtypes = c("age", "sex"))
    ans.expected <- new("MetaData",
                    nms = "reg",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = c("a", "b"))))
    expect_identical(ans.obtained, ans.expected)
})

test_that("removePairFromMetadata works", {
    removePairFromMetadata <- dembase:::removePairFromMetadata
    metadata <- new("MetaData",
                    nms = c("age", "reg_dest", "reg_orig", "eth_orig", "eth_dest"),
                    dimtypes = c("age", "destination", "origin", "origin", "destination"),
                    DimScales = list(new("Intervals", dimvalues = c(0, 1, Inf)),
                                     new("Categories", dimvalues = c("a", "b")),
                                     new("Categories", dimvalues = c("a", "b")),
                                     new("Categories", dimvalues = c("f", "g")),
                                     new("Categories", dimvalues = c("f", "g"))))
    ans.obtained <- removePairFromMetadata(metadata)
    ans.expected <- new("MetaData",
                        nms = c("age", "reg", "eth"),
                        dimtypes = c("age", "state", "state"),
                        DimScales = list(new("Intervals", dimvalues = c(0, 1, Inf)),
                                         new("Categories", dimvalues = c("a", "b")),
                                         new("Categories", dimvalues = c("f", "g"))))
    expect_identical(ans.obtained, ans.expected)
    metadata <- new("MetaData",
                    nms = c("age", "reg_parent", "reg_child", "eth_orig", "eth_dest"),
                    dimtypes = c("age", "parent", "child", "origin", "destination"),
                    DimScales = list(new("Intervals", dimvalues = c(0, 1, Inf)),
                                     new("Categories", dimvalues = c("a", "b")),
                                     new("Categories", dimvalues = c("a", "b")),
                                     new("Categories", dimvalues = c("f", "g")),
                                     new("Categories", dimvalues = c("f", "g"))))
    ans.obtained <- removePairFromMetadata(metadata, origDest = FALSE)
    ans.expected <- new("MetaData",
                        nms = c("age", "reg", "eth_orig", "eth_dest"),
                        dimtypes = c("age", "state", "origin", "destination"),
                        DimScales = list(new("Intervals", dimvalues = c(0, 1, Inf)),
                                         new("Categories", dimvalues = c("a", "b")),
                                         new("Categories", dimvalues = c("f", "g")),
                                         new("Categories", dimvalues = c("f", "g"))))
    expect_identical(ans.obtained, ans.expected)
    metadata <- new("MetaData",
                    nms = "age", 
                    dimtypes = "age", 
                    DimScales = list(new("Intervals", dimvalues = c(0, 1, Inf))))
    ans.obtained <- removePairFromMetadata(metadata)
    ans.expected <- metadata
    expect_identical(ans.obtained, ans.expected)
})

test_that("safeSample1 works", {
    safeSample1 <- dembase:::safeSample1
    expect_error(safeSample1(numeric()),
                 "'choices' has length 0")
    for (seed in seq_len(10)) {
        set.seed(seed)
        expect_identical(safeSample1(seed), seed)
        expect_true(safeSample1(1:seed) %in% 1:seed)
    }
})

test_that("splitTriangles works", {
    splitTriangles <- dembase:::splitTriangles
    ## all positive
    object <- Counts(array(c(11:13, NA),
                           dim = c(2, 2),
                           dimnames = list(age = c("0-4", "5+"),
                               time = c("2001-2005", "2006-2010"))))
    set.seed(1)
    ans.obtained <- splitTriangles(object)
    set.seed(1)
    lower <- c(rbinom(n = 3, size = 11:13, prob = 0.5), NA)
    upper <- 11:14 - lower
    ans.expected <- Counts(array(c(lower, upper),
                                 dim = c(2, 2, 2),
                                 dimnames = list(age = c("0-4", "5+"),
                                     time = c("2001-2005", "2006-2010"),
                                     triangle = c("TL", "TU"))))
    expect_identical(ans.obtained, ans.expected)
    ## some negative
    object <- Counts(array(c(5:6, -3L, -5L),
                           dim = c(2, 2),
                           dimnames = list(age = c("0-4", "5+"),
                               time = c("2001-2005", "2006-2010"))))
    set.seed(1)
    ans.obtained <- splitTriangles(object)
    set.seed(1)
    lower <- c(rbinom(n = 2, size = 5:6, prob = 0.5),
               -1L * rbinom(n = 2, size = c(3, 5), prob = 0.5))
    upper <- object@.Data - lower
    ans.expected <- Counts(array(c(lower, upper),
                                 dim = c(2, 2, 2),
                                 dimnames = list(age = c("0-4", "5+"),
                                     time = c("2001-2005", "2006-2010"),
                                     triangle = c("TL", "TU"))))
    expect_identical(ans.obtained, ans.expected)
})

test_that("timeDimCompCompatibleWithPopn works", {
    timeDimCompCompatibleWithPopn <- dembase:::timeDimCompCompatibleWithPopn
    name <- "time"
    dimtype <- "time"
    DimScale <- new("Intervals", dimvalues = seq(2000, 2020, 5))
    namesPopn <- c("time", "reg")
    dimtypesPopn <- c("time", "state")
    DimScalesPopn <- list(new("Points", dimvalues = seq(2000, 2020, 5)),
                          new("Categories", dimvalues = c("a", "b")))
    nameComponent <- "internal"
    expect_true(timeDimCompCompatibleWithPopn(name = name,
                                              DimScale = DimScale,
                                              namesPopn = namesPopn,
                                              dimtypesPopn = dimtypesPopn,
                                              DimScalesPopn = DimScalesPopn,
                                              nameComponent = nameComponent))
    ## 'population' has dimension
    name <- "time"
    dimtype <- "time"
    DimScale <- new("Intervals", dimvalues = seq(2000, 2020, 5))
    namesPopn <- c("wrong", "reg")
    dimtypesPopn <- c("time", "state")
    DimScalesPopn <- list(new("Points", dimvalues = seq(2000, 2020, 5)),
                          new("Categories", dimvalues = c("a", "b")))
    nameComponent <- "internal"
    expect_identical(timeDimCompCompatibleWithPopn(name = name,
                                                   DimScale = DimScale,
                                                   namesPopn = namesPopn,
                                                   dimtypesPopn = dimtypesPopn,
                                                   DimScalesPopn = DimScalesPopn,
                                                   nameComponent = nameComponent),
                     "'internal' has dimension \"time\" but 'population' does not")
    ## identical dimtypes
    name <- "time"
    dimtype <- "time"
    DimScale <- new("Intervals", dimvalues = seq(2000, 2020, 5))
    namesPopn <- c("time", "reg")
    dimtypesPopn <- c("state", "reg")
    DimScalesPopn <- list(new("Categories", dimvalues = c("a", "b")),
                          new("Categories", dimvalues = c("a", "b")))
    nameComponent <- "internal"
    expect_identical(timeDimCompCompatibleWithPopn(name = name,
                                                   DimScale = DimScale,
                                                   namesPopn = namesPopn,
                                                   dimtypesPopn = dimtypesPopn,
                                                   DimScalesPopn = DimScalesPopn,
                                                   nameComponent = nameComponent),
                     "\"time\" dimension of 'internal' has dimtype \"time\" but \"time\" dimension of 'population' has dimtype \"state\"")
    ## dimvalues aligned
    name <- "time"
    dimtype <- "time"
    DimScale <- new("Intervals", dimvalues = seq(2000, 2020, 5))
    namesPopn <- c("time", "reg")
    dimtypesPopn <- c("time", "state")
    DimScalesPopn <- list(new("Points", dimvalues = seq(2000, 2025, 5)),
                          new("Categories", dimvalues = c("b", "a")))
    nameComponent <- "internal"
    expect_identical(timeDimCompCompatibleWithPopn(name = name,
                                                   DimScale = DimScale,
                                                   namesPopn = namesPopn,
                                                   dimtypesPopn = dimtypesPopn,
                                                   DimScalesPopn = DimScalesPopn,
                                                   nameComponent = nameComponent),
                     "\"time\" dimensions have incompatible dimscales")
})

test_that("trimAgeIntervalsToMatch works", {
    trimAgeIntervalsToMatch <- dembase:::trimAgeIntervalsToMatch
    x <- Counts(array(1:20,
                      dim = 20,
                      dimnames = list(age = paste(seq(0, 95, 5), seq(4, 99, 5), sep = "-"))))
    y <- Counts(array(1:7,
                      dim = 7,
                      dimnames = list(age = paste(seq(15, 45, 5), seq(19, 49, 5), sep = "-"))))
    ans.obtained <- trimAgeIntervalsToMatch(x = x, y = y)
    ans.expected <- Counts(array(4:10,
                                 dim = 7,
                                 dimnames = list(age = paste(seq(15, 45, 5), seq(19, 49, 5), sep = "-"))))
    expect_identical(ans.obtained, ans.expected)
    x <- Counts(array(1:20,
                      dim = 20,
                      dimnames = list(age = 0:19)))
    y <- Counts(array(1:7,
                      dim = 7,
                      dimnames = list(age = paste(seq(15, 45, 5), seq(19, 49, 5), sep = "-"))))
    expect_error(trimAgeIntervalsToMatch(x = x, y = y),
                 "dimensions with dimtype \"age\" not compatible")
})

test_that("trimAgeIntervalsTo1549 works", {
    trimAgeIntervalsTo1549 <- dembase:::trimAgeIntervalsTo1549
    x <- Counts(array(1:20,
                      dim = 20,
                      dimnames = list(age = paste(seq(0, 95, 5), seq(4, 99, 5), sep = "-"))))
    ans.obtained <- trimAgeIntervalsTo1549(x)
    ans.expected <- Counts(array(4:10,
                                 dim = 7,
                                 dimnames = list(age = paste(seq(15, 45, 5), seq(19, 49, 5), sep = "-"))))
    expect_identical(ans.obtained, ans.expected)
    x <- Counts(array(1:2,
                      dim = 2,
                      dimnames = list(age = c("0-14", "15+"))))
    ans.obtained <- trimAgeIntervalsTo1549(x)
    ans.expected <- Counts(array(2L,
                                 dim = 1,
                                 dimnames = list(age = "15+")))
    expect_identical(ans.obtained, ans.expected)
    x <- Counts(array(1:3,
                      dim = 3,
                      dimnames = list(age = c("0-14", "15-59", "60+"))))
    ans.obtained <- trimAgeIntervalsTo1549(x)
    ans.expected <- Counts(array(2L,
                                 dim = 1,
                                 dimnames = list(age = "15-59")))
    expect_identical(ans.obtained, ans.expected)
    x <- Counts(array(2L,
                      dim = 1,
                      dimnames = list(age = "15-59")))
    expect_error(trimAgeIntervalsTo1549(x),
                 "fewer than 2 age groups")
})





## FUNCTIONS FOR PLOTTING ############################################################

test_that("addOverlayToData works", {
addOverlayToData <- dembase:::addOverlayToData

data <- Counts(array(1:6,
                     dim = c(3, 2),
                     dimnames = list(age = c("0-4", "5-9", "10+"), sex = c("f", "m"))))
data <- as.data.frame(data, direction = "long", midpoints = "age")
overlay <- Counts(array(c(1.5, 2.5, 3.5),
                        dim = 3,
                        dimnames = list(age = c("0-4", "5-9", "10+"))))
overlay <- list(values = overlay)
ans <- addOverlayToData(data, overlay, midpoints = "age")


data <- Counts(array(1:6,
                     dim = c(3, 2),
                     dimnames = list(age = c("0-4", "5-9", "10+"), sex = c("f", "m"))))
data <- as.data.frame(data, direction = "long")
overlay <- Counts(array(c(1.5, 2.5, 3.5),
                        dim = 3,
                        dimnames = list(age = c("0-4", "5-9", "10+"))))
overlay <- list(values = overlay)
ans <- addOverlayToData(data, overlay, midpoints = FALSE)


data <- Counts(array(1:18,
                     dim = c(3, 2, 3),
                     dimnames = list(age = c("0-4", "5-9", "10+"),
                     sex = c("f", "m"),
                     quantile = c("2.5%", "50%", "97.5%"))))
data <- as.data.frame(data, direction = "long", midpoints = "age")
quantile.data <- data[[3]]
data <- data[-3]
attr(data, "quantile") <- quantile.data
overlay <- Counts(array(c(1.5, 2.5, 3.5),
                        dim = 3,
                        dimnames = list(age = c("0-4", "5-9", "10+"))))
overlay <- list(values = overlay)
ans <- addOverlayToData(data, overlay, midpoints = "age")



data <- Counts(array(1:6,
                     dim = c(3, 2),
                     dimnames = list(age = c("0-4", "5-9", "10+"), sex = c("f", "m"))))
data <- as.data.frame(data, direction = "long", midpoints = "age")
overlay <- Counts(array(c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5),
                        dim = c(3, 3),
                        dimnames = list(age = c("0-4", "5-9", "10+"),
                        quantile = c("2.5%", "50%", "97.5%"))))
overlay <- list(values = overlay)
ans <- addOverlayToData(data, overlay, midpoints = "age")


data <- Counts(array(1:18,
                     dim = c(3, 2, 3),
                     dimnames = list(age = c("0-4", "5-9", "10+"),
                     sex = c("f", "m"),
                     quantile = c("2.5%", "50%", "97.5%"))))
data <- as.data.frame(data, direction = "long", midpoints = "age")
quantile.data <- data[[3]]
data <- data[-3]
attr(data, "quantile") <- quantile.data
overlay <- Counts(array(c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 8.5),
                        dim = c(3, 3),
                        dimnames = list(age = c("0-4", "5-9", "10+"),
                        quantile = c("5%", "50%", "95%"))))
overlay <- list(values = overlay)
ans <- addOverlayToData(data, overlay, midpoints = "age")

})

test_that("panel.quantiles, panel.quantile.polygon, and panel.median work", {
    panel.quantiles <- dembase:::panel.quantiles
    panel.quantile.polygon <- dembase:::panel.quantile.polygon
    panel.median <- dembase:::panel.median
    lambda <- array(c(10, 15, 20, 5, 10, 15),
                    dim = c(3, 2),
                    dimnames = list(age = c("0-4", "5-9", "10+"), sex = c("f", "m")))
    x <- Counts(array(replicate(n = 100, rpois(n = 6, lambda = lambda)),
                      dim = c(dim(lambda), 100),
                      dimnames = c(dimnames(lambda), list(iteration = 1:100))))
    x <- collapseIterations(x, probs = c(0.025, 0.5, 0.975))
    x <- as.data.frame(x, direction = "long", midpoints = "age")
    library(lattice)
    p <- xyplot(count ~ age | sex, data = x, quantile = x$quantile,
                col = "red", alpha = 0.5, panel = panel.median)
    expect_is(p, "trellis")
    p <- xyplot(count ~ age, groups = sex, data = x, quantile = x$quantile,
                panel = panel.quantiles, alpha = 0.5)
    expect_is(p, "trellis")
})

## test_that("dapply works", {
##     x <- Counts(array(1:4,
##                       dim = c(2, 2),
##                       dimnames = list(age = c("0-4", "5+"), sex = c("f", "m"))))
##     y <- Counts(array(c(2L, 3L), dim = 2, dimnames = list(age = c("0-4", "5+"))))
##     expect_equal(dapply(x, 1, mean), y)
##     expect_equal(dapply(x, 1:2, mean), x)
##     dapply(x, 1, diff)
##     a <- array(rnorm(1:8),
##                dim = c(2, 2, 2),
##                dimnames = list(age = c("0-4", "5+"),
##                sex = c("f", "m"), region = c("a", "b")))
##     x <- Values(a)
##     expect_identical(dapply(x, c(1, 3), median),
##                      Values(apply(a, c(1, 3), median)))
##     expect_identical(dapply(x, 2:1, median),
##                      Values(apply(a, 2:1, median)))
##     a <- array(0,
##                dim = c(2, 2, 0),
##                dimnames = list(age = c("0-4", "5+"),
##                sex = c("f", "m"), region = NULL))
##     x <- Values(a)
##     expect_identical(dapply(x, 2:3, median),
##                      Values(apply(a, 2:3, median)))
## })


## FUNCTIONS FOR PERTURBING AND REDISTRIBUTING DATA ###################################

test_that("checkAndTidyN works", {
    checkAndTidyN <- dembase:::checkAndTidyN
    expect_identical(checkAndTidyN(NULL), NULL)
    expect_identical(checkAndTidyN(1L), 1L)
    expect_identical(checkAndTidyN(10.0), 10L)
    expect_error(checkAndTidyN(c(1L, 1L)),
                 "'n' does not have length 1")
    expect_error(checkAndTidyN("1"),
                 "'n' is non-numeric")
    expect_error(checkAndTidyN(as.numeric(NA)),
                 "'n' is missing")
    expect_error(checkAndTidyN(1.1),
                 "'n' is not an integer")
    expect_error(checkAndTidyN(0L),
                 "'n' is less than 1")
})    


test_that("checkMeans works", {
    checkMeans <- dembase:::checkMeans
    expect_identical(checkMeans(TRUE), NULL)
    expect_identical(checkMeans(FALSE), NULL)
    expect_error(checkMeans(1L), 
                 "'means' does not have type \"logical\"")
    expect_error(checkMeans(c(TRUE, FALSE)),
                 "'means' does not have length 1")
    expect_error(checkMeans(NA),
                 "'means' is missing")
})    

test_that("perturbUsingIterations works", {
    perturbUsingIterations <- dembase:::perturbUsingIterations
    x <- Values(array(rnorm(n = 18, mean = 10:12),
                      dim = c(3, 2, 3),
                      dimnames = list(age = 0:2, sex = c("f", "m"), iteration = 1:3)))
    x.pert <- perturbUsingIterations(x, n = 3L, i.iter = 3L)
    expect_identical(dimnames(x), dimnames(x.pert))
    expect_identical(sort(x@.Data), sort(x.pert@.Data))
    expect_error(perturbUsingIterations(x, n = 4L, i.iter = 3L),
                 "'n' greater than 'n.iter'")
})

test_that("perturbUsingModel works with all non-negative all non-NA", {
    perturbUsingModel <- dembase:::perturbUsingModel
    set.seed(100)
    x <- Counts(array(rpois(n = 6, lambda = 5),
                      dim = c(3, 2),
                      dimnames = list(age = 0:2, sex = c("f", "m"))))
    x.pert <- perturbUsingModel(x, n = 3L, order = 2L, phi = 2)
    expect_false(identical(x, x.pert))
    expect_identical(dim(x.pert), c(3L, 2L, 3L))
    expect_true(is.integer(x.pert))
})

test_that("perturbUsingModel works with some negative all non-NA", {
    perturbUsingModel <- dembase:::perturbUsingModel
    set.seed(100)
    x <- Counts(array(seq(-1, 4),
                      dim = c(3, 2),
                      dimnames = list(age = 0:2, sex = c("f", "m"))))
    x.pert <- perturbUsingModel(x, n = 4L, order = 2L, phi = 2)
    expect_false(identical(x, x.pert))
    expect_identical(dim(x.pert), c(3L, 2L, 4L))
    expect_false(is.integer(x.pert))
})

test_that("perturbUsingModel works with all non-negative some non-NA", {
    perturbUsingModel <- dembase:::perturbUsingModel
    set.seed(100)
    x <- Counts(array(c(1:5, NA),
                      dim = c(3, 2),
                      dimnames = list(age = 0:2, sex = c("f", "m"))))
    x.pert <- perturbUsingModel(x, n = 1L, order = 2L, phi = 2)
    expect_false(identical(x, x.pert))
    expect_identical(dim(x.pert), c(3L, 2L))
    expect_true(is.integer(x.pert))
})

test_that("perturbUsingModel works with some negative some non-NA", {
    perturbUsingModel <- dembase:::perturbUsingModel
    set.seed(100)
    x <- Counts(array(c(1:4, NA, -0.5),
                      dim = c(3, 2),
                      dimnames = list(age = 0:2, sex = c("f", "m"))))
    x.pert <- perturbUsingModel(x, n = 1L, order = 2L, phi = 2)
    expect_false(identical(x, x.pert))
    expect_identical(dim(x.pert), c(3L, 2L))
    expect_false(is.integer(x.pert))
})

test_that("redistributeInnerDistn gives valid answer", {
    redistributeInnerDistn <- dembase:::redistributeInnerDistn
    makeTransform <- dembase:::makeTransform
    makeCollapseTransformExtra <- dembase:::makeCollapseTransformExtra
    for (seed in seq_len(n.test)) {
        ## extra dim
        set.seed(seed)
        counts <- Counts(array(rpois(n = 4, lambda = 10),
                               dim = 4,
                               dimnames = list(reg = 1:4)))
        weights <- Counts(array(5 * runif(n = 20),
                                dim = 4:5,
                                dimnames = list(reg = 1:4, age = 0:4)))
        transform <- makeTransform(x = weights, y = counts)
        transform <- makeCollapseTransformExtra(transform)
        set.seed(seed + 1)
        ans.obtained <- redistributeInnerDistn(counts = as.integer(counts),
                                          weights = as.double(weights),
                                          transform = transform,
                                          useC = FALSE)
        set.seed(seed + 1)
        f <- function(i) rmultinom(n = 1, size = counts[i], prob = weights[i,])
        ans.expected <- as.integer(t(sapply(1:4, f)))
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
        ## extra categories
        counts <- Counts(array(rpois(n = 6, lambda = 10),
                               dim = 2:3,
                               dimnames = list(age = c("0-9", "10+"),
                                   iteration = 1:3)))
        weights <- Counts(array(runif(n = 12),
                                dim = 4:3,
                                dimnames = list(age = c("0-4", "5-9",
                                                    "10-14", "15+"),
                                    iteration = 1:3)))
        transform <- makeTransform(x = weights, y = counts)
        transform <- makeCollapseTransformExtra(transform)
        set.seed(seed + 1)
        ans.obtained <- redistributeInnerDistn(counts = as.integer(counts),
                                          weights = as.double(weights),
                                          transform = transform,
                                          useC = FALSE)
        set.seed(seed + 1)
        f <- function(i,j) rmultinom(n = 1, size = counts[i,j],
                                     prob = weights[(1:2) + (i-1)*2, j])
        ans.expected <- array(dim = c(4, 3))
        for (j in 1:3)
            for (i in 1:2)
                ans.expected[(1:2)+(i-1)*2, j] <- f(i,j)
        ans.expected <- as.integer(ans.expected) 
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
    ## raises appropriate error
    counts <- Counts(array(rpois(n = 6, lambda = 10),
                           dim = 2:3,
                           dimnames = list(age = c("0-9", "10+"),
                               iteration = 1:3)))
    weights <- Counts(array(runif(n = 12),
                            dim = 4:3,
                            dimnames = list(age = c("0-4", "5-9",
                                                "10-14", "15+"),
                                iteration = 1:3)))
    weights[1:2] <- 0
    transform <- makeTransform(x = weights, y = counts)
    transform <- makeCollapseTransformExtra(transform)
    expect_error(redistributeInnerDistn(counts = as.integer(counts),
                                   weights = weights,
                                   transform = transform,
                                   useC = FALSE),
                 "weights for element 1 of 'counts' sum to 0")
})

test_that("reallocateOvers works", {
    reallocateOvers <- dembase:::reallocateOvers
    x <- 10:1
    max <- 1:10
    expect_identical(reallocateOvers(x = x, max = max), max)
    max <- rmultinom(n = 1, size = 20, prob = rep(1, 10))
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x <- rmultinom(n = 1, size = 15, prob = rep(1, 10))
        ans <- reallocateOvers(x = x, max = max)
        expect_true(all(ans <= max))
    }
})
    

test_that("redistributeInnerMeans gives valid answer", {
    redistributeInnerMeans <- dembase:::redistributeInnerMeans
    makeTransform <- dembase:::makeTransform
    makeCollapseTransformExtra <- dembase:::makeCollapseTransformExtra
    for (seed in seq_len(n.test)) {
        ## extra dim
        set.seed(seed)
        counts <- Counts(array(rpois(n = 4, lambda = 10),
                               dim = 4,
                               dimnames = list(reg = 1:4)))
        weights <- Counts(array(5 * runif(n = 20),
                                dim = 4:5,
                                dimnames = list(reg = 1:4, age = 0:4)))
        transform <- makeTransform(x = weights, y = counts)
        transform <- makeCollapseTransformExtra(transform)
        set.seed(seed + 1)
        ans.obtained <- redistributeInnerMeans(counts = as.integer(counts),
                                          weights = as.double(weights),
                                          transform = transform,
                                          useC = FALSE)
        set.seed(seed + 1)
        f <- function(i) counts[i] * prop.table(weights[i,])
        ans.expected <- as.double(t(sapply(1:4, f)))
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
        ## extra categories
        counts <- Counts(array(rpois(n = 6, lambda = 10),
                               dim = 2:3,
                               dimnames = list(age = c("0-9", "10+"),
                                   iteration = 1:3)))
        weights <- Counts(array(runif(n = 12),
                                dim = 4:3,
                                dimnames = list(age = c("0-4", "5-9",
                                                    "10-14", "15+"),
                                    iteration = 1:3)))
        transform <- makeTransform(x = weights, y = counts)
        transform <- makeCollapseTransformExtra(transform)
        set.seed(seed + 1)
        ans.obtained <- redistributeInnerMeans(counts = as.integer(counts),
                                          weights = as.double(weights),
                                          transform = transform,
                                          useC = FALSE)
        set.seed(seed + 1)
        f <- function(i,j) counts[i,j] * prop.table(weights[(1:2) + (i-1)*2, j])
        ans.expected <- array(dim = c(4, 3))
        for (j in 1:3)
            for (i in 1:2)
                ans.expected[(1:2)+(i-1)*2, j] <- f(i,j)
        ans.expected <- as.double(ans.expected) 
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
    ## raises appropriate error
    counts <- Counts(array(rpois(n = 6, lambda = 10),
                           dim = 2:3,
                           dimnames = list(age = c("0-9", "10+"),
                               iteration = 1:3)))
    weights <- Counts(array(runif(n = 12),
                            dim = 4:3,
                            dimnames = list(age = c("0-4", "5-9",
                                                "10-14", "15+"),
                                iteration = 1:3)))
    weights[1:2] <- 0
    transform <- makeTransform(x = weights, y = counts)
    transform <- makeCollapseTransformExtra(transform)
    expect_error(redistributeInnerMeans(counts = as.integer(counts),
                                   weights = weights,
                                   transform = transform,
                                   useC = FALSE),
                 "weights for element 1 of 'counts' sum to 0")
})


test_that("R and C versions of redistributeInnerDistn give same answer with means is FALSE", {
    redistributeInnerDistn <- dembase:::redistributeInnerDistn
    makeTransform <- dembase:::makeTransform
    makeCollapseTransformExtra <- dembase:::makeCollapseTransformExtra
    for (seed in seq_len(n.test)) {
        ## extra dim
        set.seed(seed)
        counts <- Counts(array(rpois(n = 4, lambda = 10),
                               dim = 4,
                               dimnames = list(reg = 1:4)))
        weights <- Counts(array(5 * runif(n = 20),
                                dim = 4:5,
                                dimnames = list(reg = 1:4, age = 0:4)))
        transform <- makeTransform(x = weights, y = counts)
        transform <- makeCollapseTransformExtra(transform)
        set.seed(seed + 1)
        ans.R <- redistributeInnerDistn(counts = as.integer(counts),
                                   weights = as.double(weights),
                                   transform = transform,
                                   useC = FALSE)
        set.seed(seed + 1)
        ans.C <- redistributeInnerDistn(counts = as.integer(counts),
                                   weights = as.double(weights),
                                   transform = transform,
                                   useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
        ## extra categories
        counts <- Counts(array(rpois(n = 6, lambda = 10),
                               dim = 2:3,
                               dimnames = list(age = c("0-9", "10+"),
                                   iteration = 1:3)))
        weights <- Counts(array(runif(n = 12),
                                dim = 4:3,
                                dimnames = list(age = c("0-4", "5-9",
                                                    "10-14", "15+"),
                                    iteration = 1:3)))
        transform <- makeTransform(x = weights, y = counts)
        transform <- makeCollapseTransformExtra(transform)
        set.seed(seed + 1)
        ans.R <- redistributeInnerDistn(counts = as.integer(counts),
                                   weights = as.double(weights),
                                   transform = transform,
                                   useC = FALSE)
        set.seed(seed + 1)
        ans.C <- redistributeInnerDistn(counts = as.integer(counts),
                                   weights = as.double(weights),
                                   transform = transform,
                                   useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
    ## raises appropriate error
    counts <- Counts(array(rpois(n = 6, lambda = 10),
                           dim = 2:3,
                           dimnames = list(age = c("0-9", "10+"),
                               iteration = 1:3)))
    weights <- Counts(array(runif(n = 12),
                            dim = 4:3,
                            dimnames = list(age = c("0-4", "5-9",
                                                "10-14", "15+"),
                                iteration = 1:3)))
    weights[1:2] <- 0
    transform <- makeTransform(x = weights, y = counts)
    transform <- makeCollapseTransformExtra(transform)
    expect_error(redistributeInnerDistn(counts = as.integer(counts),
                                   weights = weights,
                                   transform = transform,
                                   useC = TRUE),
                 "weights for element 1 of 'counts' sum to 0")
})

test_that("R and C versions of redistributeInnerMeans give same answer", {
    redistributeInnerMeans <- dembase:::redistributeInnerMeans
    makeTransform <- dembase:::makeTransform
    makeCollapseTransformExtra <- dembase:::makeCollapseTransformExtra
    for (seed in seq_len(n.test)) {
        ## extra dim
        set.seed(seed)
        counts <- Counts(array(rpois(n = 4, lambda = 10),
                               dim = 4,
                               dimnames = list(reg = 1:4)))
        weights <- Counts(array(5 * runif(n = 20),
                                dim = 4:5,
                                dimnames = list(reg = 1:4, age = 0:4)))
        transform <- makeTransform(x = weights, y = counts)
        transform <- makeCollapseTransformExtra(transform)
        set.seed(seed + 1)
        ans.R <- redistributeInnerMeans(counts = as.integer(counts),
                                        weights = as.double(weights),
                                        transform = transform,
                                        useC = FALSE)
        set.seed(seed + 1)
        ans.C <- redistributeInnerMeans(counts = as.integer(counts),
                                        weights = as.double(weights),
                                        transform = transform,
                                        useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
        ## extra categories
        counts <- Counts(array(rpois(n = 6, lambda = 10),
                               dim = 2:3,
                               dimnames = list(age = c("0-9", "10+"),
                                   iteration = 1:3)))
        weights <- Counts(array(runif(n = 12),
                                dim = 4:3,
                                dimnames = list(age = c("0-4", "5-9",
                                                    "10-14", "15+"),
                                    iteration = 1:3)))
        transform <- makeTransform(x = weights, y = counts)
        transform <- makeCollapseTransformExtra(transform)
        set.seed(seed + 1)
        ans.R <- redistributeInnerMeans(counts = as.integer(counts),
                                        weights = as.double(weights),
                                        transform = transform,
                                        useC = FALSE)
        set.seed(seed + 1)
        ans.C <- redistributeInnerMeans(counts = as.integer(counts),
                                        weights = as.double(weights),
                                        transform = transform,
                                        useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
    ## raises appropriate error
    counts <- Counts(array(rpois(n = 6, lambda = 10),
                           dim = 2:3,
                           dimnames = list(age = c("0-9", "10+"),
                               iteration = 1:3)))
    weights <- Counts(array(runif(n = 12),
                            dim = 4:3,
                            dimnames = list(age = c("0-4", "5-9",
                                                "10-14", "15+"),
                                iteration = 1:3)))
    weights[1:2] <- 0
    transform <- makeTransform(x = weights, y = counts)
    transform <- makeCollapseTransformExtra(transform)
    expect_error(redistributeInnerMeans(counts = as.integer(counts),
                                        weights = weights,
                                        transform = transform,
                                        useC = TRUE),
                 "weights for element 1 of 'counts' sum to 0")
})

test_that("redistributeInnerDistn works with cases encountered when counts is numeric and means is FALSE", {
    redistributeInnerDistn <- dembase:::redistributeInnerDistn
    makeTransform <- dembase:::makeTransform
    makeCollapseTransformExtra <- dembase:::makeCollapseTransformExtra
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        weights <- Counts(array(rpois(n = 24, lambda = 10),
                                dim = 4:2,
                                dimnames = list(age = 0:3, reg = 1:3, sex = c("f", "m"))))
        counts <- rep(10, 5)
        weights <- rep(rpois(24, lambda = 20), 5)
        transform <- new("CollapseTransform",
                         indices = list(rep(1L, 4),
                             rep(1L, 3),
                             rep(1L, 2),
                             1:5),
                         dims = c(0L, 0L, 0L, 1L),
                         dimBefore = c(4L, 3L, 2L, 5L),
                         dimAfter = 5L)
        transform <- makeCollapseTransformExtra(transform)
        set.seed(seed + 1)
        ans.R <- redistributeInnerDistn(counts = as.integer(counts),
                                        weights = as.double(weights),
                                        transform = transform,
                                        useC = FALSE)
        set.seed(seed + 1)
        ans.C <- redistributeInnerDistn(counts = as.integer(counts),
                                        weights = as.double(weights),
                                        transform = transform,
                                        useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("redistributeInnerDistn works with cases encountered when counts is numeric", {
    redistributeInnerDistn <- dembase:::redistributeInnerDistn
    makeTransform <- dembase:::makeTransform
    makeCollapseTransformExtra <- dembase:::makeCollapseTransformExtra
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        weights <- Counts(array(rpois(n = 24, lambda = 10),
                                dim = 4:2,
                                dimnames = list(age = 0:3, reg = 1:3, sex = c("f", "m"))))
        counts <- rep(10, 5)
        weights <- rep(rpois(24, lambda = 20), 5)
        transform <- new("CollapseTransform",
                         indices = list(rep(1L, 4),
                             rep(1L, 3),
                             rep(1L, 2),
                             1:5),
                         dims = c(0L, 0L, 0L, 1L),
                         dimBefore = c(4L, 3L, 2L, 5L),
                         dimAfter = 5L)
        transform <- makeCollapseTransformExtra(transform)
        set.seed(seed + 1)
        ans.R <- redistributeInnerDistn(counts = as.integer(counts),
                                        weights = as.double(weights),
                                        transform = transform,
                                        useC = FALSE)
        set.seed(seed + 1)
        ans.C <- redistributeInnerDistn(counts = as.integer(counts),
                                        weights = as.double(weights),
                                        transform = transform,
                                        useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})


test_that("resetDiagInner works", {
    resetDiagInner <- dembase:::resetDiagInner
    object <- Counts(array(1:4,
                           dim = c(2, 2),
                           dimnames = list(reg_orig = c("A", "B"),
                                           reg_dest = c("A", "B"))))
    ans.obtained <- resetDiagInner(object,
                                   base = "reg",
                                   reset = 0L)
    ans.expected <- object
    ans.expected[c(1,4)] <- 0L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- resetDiagInner(object,
                                   base = NULL,
                                   reset = 0L)
    ans.expected <- object
    ans.expected[c(1,4)] <- 0L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- resetDiagInner(object,
                                   base = NULL,
                                   reset = NA)
    ans.expected <- object
    ans.expected[c(1,4)] <- NA_integer_
    expect_identical(ans.obtained, ans.expected)
    object <- Counts(array(1:4,
                           dim = c(2, 2),
                           dimnames = list(reg_orig = c("A", "B"),
                                           reg_dest = c("B", "A"))))
    ans.obtained <- resetDiagInner(object,
                                   base = NULL,
                                   reset = 0L)
    ans.expected <- object
    ans.expected[2:3] <- 0L
    expect_identical(ans.obtained, ans.expected)
    object <- Counts(array(1:6,
                           dim = c(2, 3),
                           dimnames = list(reg_orig = c("A", "B"),
                                           reg_dest = c("B", "A", "C"))))
    ans.obtained <- resetDiagInner(object,
                                   base = NULL,
                                   reset = 0L)
    ans.expected <- object
    ans.expected[2:3] <- 0L
    expect_identical(ans.obtained, ans.expected)
    object <- Counts(array(1:8,
                           dim = c(2, 2, 2),
                           dimnames = list(reg_orig = c("A", "B"),
                                           sex = c("Female", "Male"),
                                           reg_dest = c("A", "B"))))
    ans.obtained <- resetDiagInner(object,
                                   base = NULL,
                                   reset = 0L)
    ans.expected <- object
    ans.expected[c(1, 3, 6, 8)] <- 0L
    expect_identical(ans.obtained, ans.expected)
    object <- Counts(array(32,
                           dim = c(2, 2, 2, 2, 2),
                           dimnames = list(reg_orig = c("A", "B"),
                                           eth_orig = c("c", "d"),
                                           sex = c("Female", "Male"),
                                           eth_dest = c("c", "d"),
                                           reg_dest = c("A", "B"))))
    ans.obtained <- resetDiagInner(object,
                                   base = "eth",
                                   reset = 0L)
    ans.expected <- object
    ans.expected[c(1, 2, 5, 6, 11, 12, 15, 16, 17, 18, 21, 22, 27, 28, 31, 32)] <- 0L
    expect_identical(ans.obtained, ans.expected)
})

test_that("resetDiagInner throws appropriate errors", {
    resetDiagInner <- dembase:::resetDiagInner
    object <- Counts(array(1:4,
                           dim = c(2, 2),
                           dimnames = list(reg1 = c("A", "B"),
                                           reg2 = c("A", "B"))))
    expect_error(resetDiagInner(object, base = NULL, reset = 0),
                 "no dimensions with dimtypes \"origin\" or \"destination\"")
    expect_error(resetDiagInner(object, base = "reg1", reset = 0),
                 "'base' outside valid range")
})


## FUNCTIONS RELATED TO LIFE TABLES ##################################################

test_that("expandAx works", {
    expandAx <- dembase:::expandAx
    ## one dimension no adjustment needed
    object <- ValuesOne(c(0.2, 0.5, 0.3),
                        labels = c("0-4", "5-9", "10+"),
                        name = "age")
    ax <- ValuesOne(c(1, 2, 2),
                    labels = c("0-4", "5-9", "10+"),
                    name = "age")
    ans.obtained <- expandAx(ax = ax, object = object)
    ans.expected <- ax
    expect_identical(ans.obtained, ans.expected)
    ## one dimension, add one label below
    object <- ValuesOne(c(0.2, 0.5, 0.3),
                        labels = c("0-4", "5-9", "10+"),
                        name = "age")
    ax <- ValuesOne(c(2, 2),
                    labels = c("5-9", "10+"),
                    name = "age")
    ans.obtained <- expandAx(ax = ax, object = object)
    ans.expected <- ValuesOne(c(2.5, 2, 2),
                              labels = c("0-4", "5-9", "10+"),
                              name = "age")
    expect_identical(ans.obtained, ans.expected)
    ## one dimension and two labels above
    object <- ValuesOne(c(0.2, 0.5, 0.3, 0.2),
                        labels = c("0-4", "5-9", "10-12", "13+"),
                        name = "age")
    ax <- ValuesOne(c(2, 2),
                    labels = c("0-4", "5-9"),
                    name = "age")
    ans.obtained <- expandAx(ax = ax, object = object)
    ans.expected <- ValuesOne(c(2, 2, 1.5, 1.5),
                              labels = c("0-4", "5-9", "10-12", "13+"),
                              name = "age")
    expect_identical(ans.obtained, ans.expected)
    ## two dimensions and two labels above
    object <- Values(array(c(0.2, 0.3, 0.2, 0.3, 0.3, 0.4, 0.2, 0.25),
                           dim = c(4, 2),
                           dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                               sex = c("f", "m"))))
    ax <- Values(array(c(1.2, 2, 1.3, 2.1),
                       dim = c(2, 2),
                       dimnames = list(age = c("0-4", "5-9"),
                           sex = c("f", "m"))))
    ans.obtained <- expandAx(ax = ax, object = object)
    ans.expected <- Values(array(c(1.2, 2, 2.5, 2.5, 1.3, 2.1, 2.5, 2.5),
                                 dim = c(4, 2),
                                 dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                     sex = c("f", "m"))))
    expect_identical(ans.obtained, ans.expected)
    ## two dimensions and two labels above - adding "reg" dimension
    object <- Values(array(c(0.2, 0.3, 0.2, 0.3, 0.3, 0.4, 0.2, 0.25),
                           dim = c(4, 2, 2),
                           dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                               sex = c("f", "m"),
                               reg = c("a", "b"))))
    ax <- Values(array(c(1.2, 2, 1.3, 2.1),
                       dim = c(2, 2),
                       dimnames = list(age = c("0-4", "5-9"),
                           sex = c("f", "m"))))
    ans.obtained <- expandAx(ax = ax, object = object)
    ans.expected <- Values(array(c(1.2, 2, 2.5, 2.5, 1.3, 2.1, 2.5, 2.5),
                                 dim = c(4, 2, 2),
                                 dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                     sex = c("f", "m"),
                                     reg = c("a", "b"))))
    expect_identical(ans.obtained, ans.expected)
    ## single value
    object <- Values(array(c(0.2, 0.3, 0.2, 0.3, 0.3, 0.4, 0.2, 0.25),
                           dim = c(4, 2, 2),
                           dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                               sex = c("f", "m"),
                               reg = c("a", "b"))))
    ax <- ValuesOne(values = 1, labels = "0-4", name = "age")
    ans.obtained <- expandAx(ax = ax, object = object)
    ans.expected <- Values(array(c(1, 2.5, 2.5, 2.5, 1, 2.5, 2.5, 2.5),
                                 dim = c(4, 2, 2),
                                 dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                     sex = c("f", "m"),
                                     reg = c("a", "b"))))
    expect_identical(ans.obtained, ans.expected)
})

test_that("expandAx throws appropriate errors", {
    expandAx <- dembase:::expandAx
    ## "'ax' does not have a dimension with dimtype "age"
    object <- ValuesOne(c(0.2, 0.5, 0.3),
                        labels = c("0-4", "5-9", "10+"),
                        name = "age")
    ax <- ValuesOne(c(1, 2, 2),
                    labels = c("0-4", "5-9", "10+"),
                    name = "wrong")
    expect_error(expandAx(ax = ax, object = object),
                 "'ax' does not have a dimension with dimtype \"age\"")
    ## "'object' does not have a dimension with dimtype "age"
    object <- ValuesOne(c(0.2, 0.5, 0.3),
                        labels = c("0-4", "5-9", "10+"),
                        name = "wrong")
    ax <- ValuesOne(c(1, 2, 2),
                    labels = c("0-4", "5-9", "10+"),
                    name = "age")
    expect_error(expandAx(ax = ax, object = object),
                 "'object' does not have a dimension with dimtype \"age\"")
    ## "age" dimension of "'ax' does not have dimscale "Intervals"
    object <- ValuesOne(c(0.2, 0.5, 0.3),
                        labels = c("0-4", "5-9", "10+"),
                        name = "age")
    ax <- ValuesOne(c(1, 2, 2),
                    labels = c("0", "5", "10"),
                    name = "age")
    expect_error(expandAx(ax = ax, object = object),
                 "dimension of 'ax' with dimtype \"age\" does not have dimscale \"Intervals\"")
    ## "age" dimension of "'ax' does not have dimscale "Intervals"
    object <- ValuesOne(c(0.2, 0.5, 0.3),
                        labels = c("0", "5", "10"),
                        name = "age")
    ax <- ValuesOne(c(1, 2, 2),
                    labels = c("0-4", "5-9", "10+"),
                    name = "age")
    expect_error(expandAx(ax = ax, object = object),
                 "dimension of 'object' with dimtype \"age\" does not have dimscale \"Intervals\"")
    ## first age interval of 'ax' open on left
    object <- ValuesOne(c(0.2, 0.5, 0.3),
                        labels = c("0-4", "5-9", "10+"),
                        name = "age")
    ax <- ValuesOne(c(1, 2, 2),
                    labels = c("<5", "5-9", "10+"),
                    name = "age")
    expect_error(expandAx(ax = ax, object = object),
                 "first age interval of 'ax' is open on left")
    ## first age interval of 'ax' open on left
    object <- ValuesOne(c(0.2, 0.5, 0.3),
                        labels = c("<5", "5-9", "10+"),
                        name = "age")
    ax <- ValuesOne(c(1, 2, 2),
                    labels = c("0-4", "5-9", "10+"),
                    name = "age")
    expect_error(expandAx(ax = ax, object = object),
                 "first age interval of 'object' is open on left")
    ## age dimensions not compatible
    object <- ValuesOne(c(0.2, 0.3, 0.5, 0.3, 0.5),
                        labels = c("0", "1-4", "5-9", "10-14", "15+"),
                        name = "age")
    ax <- ValuesOne(c(1, 2, 2),
                    labels = c("0-4", "5-9", "10+"),
                    name = "age")
    expect_error(expandAx(ax = ax, object = object),
                 "dimensions of 'ax' and 'object' with dimtype \"age\" not compatible")
    ## 'ax' has dimension not in 'object'
    object <- Values(array(c(0.2, 0.3, 0.2, 0.3, 0.3, 0.4, 0.2, 0.25),
                           dim = c(4, 2),
                           dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                               sex = c("f", "m"))))
    ax <- Values(array(c(1.2, 2, 1.3, 2.1),
                       dim = c(2, 2),
                       dimnames = list(age = c("0-4", "5-9"),
                           region = c("a", "b"))))
    expect_error(expandAx(ax = ax, object = object),
                 "'ax' and 'object' not compatible")
})

test_that("imputeA works", {
    imputeA <- dembase:::imputeA
    ans.obtained <- imputeA(m0 = c(0.2, 0.05),
                            A = "1a0",
                            sex = "Female")
    ans.expected <- c(0.35, 0.053 + 2.8 * 0.05)
    expect_equal(ans.obtained, ans.expected)
    ans.obtained <- imputeA(m0 = c(0.2, 0.05),
                            A = "4a1",
                            sex = "Female")
    ans.expected <- c(1.361, 1.522 - 1.518 * 0.05)
    expect_equal(ans.obtained, ans.expected)
    ans.obtained <- imputeA(m0 = c(0.2, 0.05),
                            A = "1a0",
                            sex = "Male")
    ans.expected <- c(0.33, 0.045 + 2.684 * 0.05)
    expect_equal(ans.obtained, ans.expected)
    ans.obtained <- imputeA(m0 = c(0.2, 0.05),
                            A = "4a1",
                            sex = "Male")
    ans.expected <- c(1.352, 1.651 - 2.816 * 0.05)
    expect_equal(ans.obtained, ans.expected)
})

test_that("makeAxStart works", {
    imputeA <- dembase:::imputeA
    ## two sexes, two ages
    mx <- Values(array(c(0.005, 0.006, 0.003, 0.004),
                       dim = c(2, 2),
                       dimnames = list(age = c("0", "1-4"),
                                       sex = c("f", "m"))))
    ans.obtained <- makeAxStart(mx)
    ans.expected <- c(imputeA(mx[1], A = "1a0", sex = "Female"),
                      imputeA(mx[1], A = "4a1", sex = "Female"),
                      imputeA(mx[3], A = "1a0", sex = "Male"),
                      imputeA(mx[3], A = "4a1", sex = "Male"))
    ans.expected <- Values(array(ans.expected,
                                 dim = c(2, 2),
                                 dimnames = list(age = c("0", "1-4"),
                                                 sex = c("f", "m"))))
    expect_identical(ans.obtained, ans.expected)
    ## two sexes, two ages, age dimension second
    mx <- Values(array(c(0.005, 0.006, 0.003, 0.004),
                       dim = c(2, 2),
                       dimnames = list(sex = c("f", "m"),
                                       age = c("0", "1-4"))))
    ans.obtained <- makeAxStart(mx)
    ans.expected <- c(imputeA(mx[1], A = "1a0", sex = "Female"),
                      imputeA(mx[1], A = "4a1", sex = "Female"),
                      imputeA(mx[2], A = "1a0", sex = "Male"),
                      imputeA(mx[2], A = "4a1", sex = "Male"))
    ans.expected <- Values(array(ans.expected,
                                 dim = c(2, 2),
                                 dimnames = list(age = c("0", "1-4"),
                                                 sex = c("f", "m"))))
    expect_identical(ans.obtained, ans.expected)
    ## one sex, two ages
    mx <- Values(array(c(0.005, 0.006, 0.003, 0.004),
                       dim = c(2, 2),
                       dimnames = list(age = c("0", "1-4"),
                                       region = c("a", "b"))))
    ans.obtained <- makeAxStart(mx)
    pr.fem <- 100/205
    ans.expected <- c(pr.fem * imputeA(mx[1], A = "1a0", sex = "Female") +
                      (1-pr.fem) * imputeA(mx[1], A = "1a0", sex = "Male"),
                      pr.fem * imputeA(mx[1], A = "4a1", sex = "Female") +
                      (1-pr.fem) * imputeA(mx[1], A = "4a1", sex = "Male"),
                      pr.fem * imputeA(mx[3], A = "1a0", sex = "Female") +
                      (1-pr.fem) * imputeA(mx[3], A = "1a0", sex = "Male"),
                      pr.fem * imputeA(mx[3], A = "4a1", sex = "Female") +
                      (1-pr.fem) * imputeA(mx[3], A = "4a1", sex = "Male"))
    ans.expected <- Values(array(ans.expected,
                                 dim = c(2, 2),
                                 dimnames = list(age = c("0", "1-4"),
                                                 region = c("a", "b"))))
    expect_identical(ans.obtained, ans.expected)
    ## two sexes, one age
    mx <- Values(array(c(0.005, 0.006),
                       dim = c(2, 1),
                       dimnames = list(sex = c("m", "f"),
                                       age = "0")),
                 dimscales = c(age = "Intervals"))
    ans.obtained <- makeAxStart(mx)
    ans.expected <- c(imputeA(mx[1], A = "1a0", sex = "Male"),
                      imputeA(mx[2], A = "1a0", sex = "Female"))
    ans.expected <- Values(array(ans.expected,
                                 dim = 1:2,
                                 dimnames = list(age = "0",
                                                 sex = c("m", "f"))),
                           dimscales = c(age = "Intervals"))
    expect_identical(ans.obtained, ans.expected)    
    ## one sex, one age
    mx <- Values(array(0.005, 
                       dim = c(1, 1),
                       dimnames = list(sex = "m",
                                       age = "0")),
                 dimscales = c(age = "Intervals"))
    ans.obtained <- makeAxStart(mx)
    ans.expected <- imputeA(mx[1], A = "1a0", sex = "Male")
    ans.expected <- Values(array(ans.expected,
                                 dim = c(1,1),
                                 dimnames = list(age = "0",
                                                 sex = "m")),
                           dimscales = c(age = "Intervals"))
    expect_identical(ans.obtained, ans.expected)    
    ## one sex, one older age
    mx <- Values(array(0.005, 
                       dim = c(1, 1),
                       dimnames = list(sex = "m",
                                       age = "5-9")),
                 dimscales = c(age = "Intervals"))
    ans.obtained <- makeAxStart(mx)
    ans.expected <- 2.5
    ans.expected <- Values(array(ans.expected,
                                 dim = c(1,1),
                                 dimnames = list(age = "5-9",
                                                 sex = "m")),
                           dimscales = c(age = "Intervals"))
    expect_identical(ans.obtained, ans.expected)    
    ## no sexes, one older age
    mx <- Values(array(0.005, 
                       dim = c(1, 1),
                       dimnames = list(region = "m",
                                       age = "5-9")),
                 dimscales = c(age = "Intervals"))
    ans.obtained <- makeAxStart(mx)
    ans.expected <- 2.5
    ans.expected <- Values(array(ans.expected,
                                 dim = c(1,1),
                                 dimnames = list(age = "5-9",
                                                 region = "m")),
                           dimscales = c(age = "Intervals"))
    expect_identical(ans.obtained, ans.expected)    
})

test_that("makeAxStart throws appropriate errors", {
    expect_error(makeAxStart("wrong"),
                 "'mx' has class \"character\"")
    mx <- Values(array(0,
                       dim = c(2, 0),
                       dimnames = list(age = c("0", "1-4"),
                                       sex = character())))
    expect_error(makeAxStart(mx),
                 "'mx' has length 0")
    mx <- Values(array(0,
                       dim = c(2, 2),
                       dimnames = list(region = c("a", "b"),
                                       sex = c("m", "f"))))
    expect_error(makeAxStart(mx),
                 "'mx' does not have dimension with dimtype \"age\"")
    mx <- Values(array(0,
                       dim = c(2, 2),
                       dimnames = list(age = c("0", "5"),
                                       sex = c("m", "f"))))
    expect_error(makeAxStart(mx),
                 "dimension of 'mx' with dimtype \"age\" does not have dimscale \"Intervals\"")
})




## FUNCTIONS FOR CONCORDING #####################################################

test_that("tidyConcordanceList works", {
    tidyConcordanceList <- dembase:::tidyConcordanceList
    Concordance <- classconc::Concordance
    x <- Values(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(age = c(0, "1+"),
                          region = c("a", "b"))))
    concordances <- list()
    ans.obtained <- tidyConcordanceList(concordances = concordances,
                                        object = x)
    ans.expected <- list(age= NULL, region = NULL)
    expect_identical(ans.obtained, ans.expected)
    x <- Values(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(age = c(0, "1+"),
                          region = c("a", "b"))))
    conc <- Concordance(data.frame(from = c("a", "b"), to = "A"))
    concordances <- list(region = conc)
    ans.obtained <- tidyConcordanceList(concordances = concordances,
                                        object = x)
    ans.expected <- list(age = NULL, region = conc)
    expect_identical(ans.obtained, ans.expected)
    concordances <- list(age = conc)
    expect_error(tidyConcordanceList(concordance = concordances,
                                     object = x),
                 "concordance supplied for \"age\" dimension, but \"age\" dimension has dimscale \"Intervals\"")
    x <- Values(array(1:8,
                      dim = c(2, 2, 2),
                      dimnames = list(eth_parent = c("a", "b"),
                          sex = c("m", "f"),
                          eth_child = c("a", "b"))))
    conc <- Concordance(data.frame(from = c("a", "b"), to = "A"))
    concordances.both <- list(eth_child = conc, eth_parent = conc)
    concordances.one <- list(eth = conc)
    ans.both <- tidyConcordanceList(concordances = concordances.both,
                                    object = x)
    ans.one <-  tidyConcordanceList(concordances = concordances.one,
                                    object = x)
    ans.expected <- list(eth_parent = conc, sex = NULL, eth_child = conc)
    expect_identical(ans.both, ans.expected)
    expect_identical(ans.both, ans.one)
    x <- Values(array(1:8,
                      dim = c(2, 2, 2),
                      dimnames = list(eth_parent = c("a", "b"),
                          sex = c("m", "f"),
                          eth_child = c("a", "b"))))
    expect_error(tidyConcordanceList(concordances = NULL, object = x),
                 "'concordances' has class \"NULL\"")
    x <- Values(array(1:8,
                      dim = c(2, 2, 2),
                      dimnames = list(eth_parent = c("a", "b"),
                          sex = c("m", "f"),
                          eth_child = c("a", "b"))))
    conc <- Concordance(data.frame(from = c("a", "b"), to = c("A", "B")))
    expect_error(tidyConcordanceList(concordances = list(eth = conc), object = x),
                 "'concordances' has elements not of class \"ManyToOne\"")
    x <- Values(array(1:8,
                      dim = c(2, 2, 2),
                      dimnames = list(eth_parent = c("a", "b"),
                          sex = c("m", "f"),
                          eth_child = c("a", "b"))))
    conc <- Concordance(data.frame(from = c("a", "b"), to = "A"))
    expect_error(tidyConcordanceList(concordances = list(conc), object = x),
                 "'concordances' does not have names")
    x <- Values(array(1:8,
                      dim = c(2, 2, 2),
                      dimnames = list(eth_parent = c("a", "b"),
                          sex = c("m", "f"),
                          eth_child = c("a", "b"))))
    conc <- Concordance(data.frame(from = c("a", "b"), to = "A"))
    expect_error(tidyConcordanceList(concordances = list(eth = conc, eth = conc),
                                     object = x),
                 "'concordances' has duplicate names")    
})


## HELPER FUNCTIONS FOR 'project' ##############################################

test_that("ageForward works", {
    ageForward <- dembase:::ageForward
    ## last age group open
    population <- Counts(array(1:12,
                               dim = c(4, 3),
                               dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                   reg = 1:3)))
    ans.obtained <- ageForward(population)
    ans.expected <- Counts(array(c(0L, 1:3, 0L, 5:7, 0L, 9:11),
                                 dim = c(4, 3),
                                 dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                     reg = 1:3)))
    ans.expected[4,] <- ans.expected[4,] + population[4,]
    expect_identical(ans.obtained, ans.expected)
    ## last age group closed
    population <- Counts(array(1:12,
                               dim = c(4, 3),
                               dimnames = list(age = c("0-4", "5-9", "10-14", "15-19"),
                                   reg = 1:3)))
    ans.obtained <- ageForward(population)
    ans.expected <- Counts(array(c(0L, 1:3, 0L, 5:7, 0L, 9:11),
                                 dim = c(4, 3),
                                 dimnames = list(age = c("0-4", "5-9", "10-14", "15-19"),
                                     reg = 1:3)))
    expect_identical(ans.obtained, ans.expected)
    ## only one age group - last age group open
    population <- Counts(array(1:3,
                               dim = c(1, 3),
                               dimnames = list(age = "0+", reg = 1:3)))
    ans.obtained <- ageForward(population)
    ans.expected <- population
    expect_identical(ans.obtained, ans.expected)
    ## only one age group - last age group closed
    population <- Counts(array(1:3,
                               dim = c(1, 3),
                               dimnames = list(age = "0-4", reg = 1:3)))
    ans.obtained <- ageForward(population)
    ans.expected <- Counts(array(0L,
                                 dim = c(1, 3),
                                 dimnames = list(age = "0-4", reg = 1:3)))
    expect_identical(ans.obtained, ans.expected)
    ## two age groups - last age group open
    population <- Counts(array(1:6,
                               dim = c(2, 3),
                               dimnames = list(age = c("0-4", "5+"), reg = 1:3)))
    ans.obtained <- ageForward(population)
    ans.expected <- Counts(array(c(0L, 3L, 0L, 7L, 0L, 11L),
                               dim = c(2, 3),
                               dimnames = list(age = c("0-4", "5+"), reg = 1:3)))
    expect_identical(ans.obtained, ans.expected)
    ## only one age group - last age group closed
    population <- Counts(array(1:6,
                               dim = c(2, 3),
                               dimnames = list(age = c("0-4", "5-9"), reg = 1:3)))
    ans.obtained <- ageForward(population)
    ans.expected <- Counts(array(c(0L, 1L, 0L, 3L, 0L, 5L),
                               dim = c(2, 3),
                               dimnames = list(age = c("0-4", "5-9"), reg = 1:3)))
    expect_identical(ans.obtained, ans.expected)
    ## no age dimension    
    population <- Counts(array(1:12,
                               dim = c(4, 3),
                               dimnames = list(eth = 1:4, reg = 1:3)))
    ans.obtained <- ageForward(population)
    ans.expected <- population
    expect_identical(ans.obtained, ans.expected)
    ## does not have dimtype "time" or "cohort"
    population <- Counts(array(1:12,
                               dim = c(4, 3),
                               dimnames = list(eth = 1:4, time = 1:3)),
                         dimscales = c(time = "Intervals"))
    expect_error(ageForward(population),
                 "'population' has dimension with dimtype \"time\"")
    population <- Counts(array(1:12,
                               dim = c(4, 3),
                               dimnames = list(eth = 1:4, cohort = 3:1)))
    expect_error(ageForward(population),
                 "'population' has dimension with dimtype \"cohort\"")
    ## age dimension has length > 0
    population <- Counts(array(0L,
                               dim = c(4, 0),
                               dimnames = list(eth = 1:4, age = character())))
    expect_error(ageForward(population),
                 "\"age\" dimension of 'population' has length 0")
    ## age dimension not open on left
    population <- Counts(array(1:12,
                               dim = c(4, 3),
                               dimnames = list(eth = 1:4, age = c("<0", "0-4", "5+"))))
    expect_error(ageForward(population),
                 "first age group of 'population' is open on left")
})    

test_that("checkAndTidyInitial works", {
    checkAndTidyInitial <- dembase:::checkAndTidyInitial
    initial <- Counts(array(1:6, 
                            dim = 3:2,
                            dimnames = list(age = 0:2, sex = c("f", "m"))))
    expect_identical(checkAndTidyInitial(initial), initial)
    expect_identical(checkAndTidyInitial(toDouble(initial)), initial)
    initial <- Counts(array(c(NA, 0:4), 
                            dim = 3:2,
                            dimnames = list(age = 0:2, sex = c("f", "m"))))
    expect_identical(checkAndTidyInitial(initial), initial)
    initial <- Counts(array(c(NA, 0:4), 
                            dim = 3:1,
                            dimnames = list(age = 0:2, sex = c("f", "m"), time = 2000)),
                      dimscales = c(time = "Points"))
    expect_identical(checkAndTidyInitial(initial), initial)
    expect_error(checkAndTidyInitial("wrong"),
                 "'initial' has class \"character\"")
    expect_error(checkAndTidyInitial(new("Counts")),
                 "'initial' has length 0")
    initial <- Counts(array(c(-1, 0:4), 
                            dim = 3:2,
                            dimnames = list(age = 0:2, sex = c("f", "m"))))
    expect_error(checkAndTidyInitial(initial),
                 "'initial' has negative values")
    initial <- Counts(array(c(NA, 0:10), 
                            dim = c(3,2,2),
                            dimnames = list(age = 0:2, sex = c("f", "m"), time = 2000:2001)),
                      dimscales = c(time = "Points"))
    expect_error(checkAndTidyInitial(initial),
                 "time dimension for 'initial' does not have length 1")
    initial <- Counts(array(c(NA, 0:4), 
                            dim = 3:1,
                            dimnames = list(age = 0:2, sex = c("f", "m"), time = 2000)),
                      dimscales = c(time = "Intervals"))
    expect_error(checkAndTidyInitial(initial),
                 "time dimension for 'initial' has dimscale \"Intervals\"")
    initial <- Counts(array(1, 
                            dim = 1,
                            dimnames = list(time = 2000)),
                      dimscales = c(time = "Points"))
    expect_error(checkAndTidyInitial(initial),
                 "'initial' has only one dimension, which has dimtype \"time\"")
    initial <- Counts(array(c(1.1, 0:4), 
                            dim = 3:2,
                            dimnames = list(age = 0:2, sex = c("f", "m"))))
    expect_error(checkAndTidyInitial(initial),
                 "'initial' invalid : non-integer values")
    initial <- CountsOne(values = 1:5, labels = c("<0", 0:3), name = "age")
    expect_error(checkAndTidyInitial(initial),
                 "first age group of 'initial' is open on left")
})

test_that("checkAndTidyIterations works", {
    checkAndTidyIterationsProject <- dembase:::checkAndTidyIterationsProject
    ## n is NULL, externalIn has iteration
    initial <- Counts(array(1:6, 
                            dim = 3:2,
                            dimnames = list(age = 0:2, sex = c("f", "m"))))
    param <- list(externalIn = Values(array(0.5, 
                      dim = c(3, 2, 5),
                      dimnames = list(age = 0:2, sex = c("f", "m"), iteration = 1:5))),
                  externalOut = Values(array(0.3, 
                      dim = 3:2,
                      dimnames = list(age = 0:2, sex = c("f", "m")))))
    ans.obtained <- checkAndTidyIterationsProject(initial = initial,
                                                  param = param,
                                                  n = NULL)
    ans.expected <- list(initial = Counts(array(1:6, 
                             dim = c(3, 2, 5),
                             dimnames = list(age = 0:2, sex = c("f", "m"),
                                 iteration = 1:5))),
                         param = list(externalIn = Values(array(0.5, 
                                          dim = c(3, 2, 5),
                                          dimnames = list(age = 0:2, sex = c("f", "m"),
                                              iteration = 1:5))),
                             externalOut = Values(array(0.3, 
                                 dim = c(3, 2, 5),
                                 dimnames = list(age = 0:2, sex = c("f", "m"),
                                     iteration = 1:5)))))
    expect_identical(ans.obtained, ans.expected)
    ## n is NULL; initial and externalOut both have iteration
    initial <- Counts(array(1:6, 
                            dim = c(3, 2, 10),
                            dimnames = list(age = 0:2, sex = c("f", "m"), iteration = 1:10)))
    param <- list(externalIn = Values(array(0.5, 
                      dim = c(3, 2),
                      dimnames = list(age = 0:2, sex = c("f", "m")))),
                  externalOut = Values(array(0.3, 
                      dim = c(3, 10, 2),
                      dimnames = list(age = 0:2, iteration = 1:10, sex = c("f", "m")))))
    ans.obtained <- checkAndTidyIterationsProject(initial = initial,
                                                  param = param,
                                                  n = NULL)
    ans.expected <- list(initial = Counts(array(1:6, 
                             dim = c(3, 2, 10),
                             dimnames = list(age = 0:2, sex = c("f", "m"), iteration = 1:10))),
                         param = list(externalIn = Values(array(0.5, 
                                          dim = c(3, 2, 10),
                                          dimnames = list(age = 0:2, sex = c("f", "m"),
                                              iteration = 1:10))),
                             externalOut = Values(array(0.3, 
                                 dim = c(3, 10, 2),
                                 dimnames = list(age = 0:2, iteration = 1:10,
                                     sex = c("f", "m"))))))
    expect_identical(ans.obtained, ans.expected)
    ## n is NULL; no iteration
    initial <- Counts(array(1:6, 
                            dim = c(3, 2),
                            dimnames = list(age = 0:2, sex = c("f", "m"))))
    param <- list(externalIn = Values(array(0.5, 
                      dim = c(3, 2),
                      dimnames = list(age = 0:2, sex = c("f", "m")))),
                  externalOut = Values(array(0.3, 
                      dim = c(3, 2),
                      dimnames = list(age = 0:2, sex = c("f", "m")))))
    ans.obtained <- checkAndTidyIterationsProject(initial = initial,
                                                  param = param,
                                                  n = NULL)
    ans.expected <- list(initial = initial, param = param)
    expect_identical(ans.obtained, ans.expected)
    ## n is 5; no iteration
    initial <- Counts(array(1:6, 
                            dim = c(3, 2),
                            dimnames = list(age = 0:2, sex = c("f", "m"))))
    param <- list(externalIn = Values(array(0.5, 
                      dim = c(3, 2),
                      dimnames = list(age = 0:2, sex = c("f", "m")))),
                  externalOut = Values(array(0.3, 
                      dim = c(3, 2),
                      dimnames = list(age = 0:2, sex = c("f", "m")))))
    ans.obtained <- checkAndTidyIterationsProject(initial = initial,
                                                  param = param,
                                                  n = 5)
    ans.expected <- list(initial = Counts(array(1:6, 
                             dim = c(3, 2, 5),
                             dimnames = list(age = 0:2, sex = c("f", "m"), iteration = 1:5))),
                         param = list(externalIn = Values(array(0.5, 
                                          dim = c(3, 2, 5),
                                          dimnames = list(age = 0:2, sex = c("f", "m"),
                                              iteration = 1:5))),
                             externalOut = Values(array(0.3, 
                                 dim = c(3, 2, 5),
                                 dimnames = list(age = 0:2, sex = c("f", "m"),
                                     iteration = 1:5)))))
    expect_identical(ans.obtained, ans.expected)
    ## n is 5; initial has iteration
    initial <- Counts(array(1:6, 
                            dim = c(3, 2, 10),
                            dimnames = list(age = 0:2, sex = c("f", "m"),
                                iteration = 10:1)))
    param <- list(externalIn = Values(array(0.5, 
                      dim = c(3, 2),
                      dimnames = list(age = 0:2, sex = c("f", "m")))),
                  externalOut = Values(array(0.3, 
                      dim = c(3, 2),
                      dimnames = list(age = 0:2, sex = c("f", "m")))))
    ans.obtained <- checkAndTidyIterationsProject(initial = initial,
                                                  param = param,
                                                  n = 5)
    ans.expected <- list(initial = Counts(array(1:6, 
                             dim = c(3, 2, 5),
                             dimnames = list(age = 0:2, sex = c("f", "m"), iteration = 1:5))),
                         param = list(externalIn = Values(array(0.5, 
                                          dim = c(3, 2, 5),
                                          dimnames = list(age = 0:2, sex = c("f", "m"),
                                              iteration = 1:5))),
                             externalOut = Values(array(0.3, 
                                 dim = c(3, 2, 5),
                                 dimnames = list(age = 0:2, sex = c("f", "m"),
                                     iteration = 1:5)))))
    expect_identical(ans.obtained, ans.expected)
    ## n is 5; initial has 3 iteration
    initial <- Counts(array(1:6, 
                            dim = c(3, 2, 3),
                            dimnames = list(age = 0:2, sex = c("f", "m"),
                                iteration = 1:3)))
    param <- list(externalIn = Values(array(0.5, 
                      dim = c(3, 2),
                      dimnames = list(age = 0:2, sex = c("f", "m")))),
                  externalOut = Values(array(0.3, 
                      dim = c(3, 2),
                      dimnames = list(age = 0:2, sex = c("f", "m")))))
    expect_error(checkAndTidyIterationsProject(initial = initial,
                                               param = param,
                                               n = 5),
                 "'initial' has unexpected number of iterations")
    ## externalIn and externalOut have different number of iterations    
    initial <- Counts(array(1:6, 
                            dim = c(3, 2),
                            dimnames = list(age = 0:2, sex = c("f", "m"))))
    param <- list(externalIn = Values(array(0.5, 
                      dim = c(3, 2, 5),
                      dimnames = list(age = 0:2, sex = c("f", "m"), iteration = 1:5))),
                  externalOut = Values(array(0.3, 
                      dim = c(3, 2, 6),
                      dimnames = list(age = 0:2, sex = c("f", "m"), iteration = 1:6))))
    expect_error(checkAndTidyIterationsProject(initial = initial,
                                               param = param,
                                               n = NULL),
                 "'externalOut' has unexpected number of iterations")
})    

test_that("checkAndTidyNIter works", {
    checkAndTidyNIter <- dembase:::checkAndTidyNIter
    expect_identical(checkAndTidyNIter(NULL), NULL)
    expect_identical(checkAndTidyNIter(1), 1L)
    expect_identical(checkAndTidyNIter(10L), 10L)
    expect_error(checkAndTidyNIter(c(5L, 5L)),
                 "'n' does not have length 1")
    expect_error(checkAndTidyNIter("5"),
                 "'n' is non-numeric")
    expect_error(checkAndTidyNIter(as.integer(NA)),
                 "'n' is missing")
    expect_error(checkAndTidyNIter(5.1),
                 "'n' is not an integer")
    expect_error(checkAndTidyNIter(0L),
                 "'n' is less than 1")
})

test_that("checkAndTidyReset works", {
    checkAndTidyReset <- dembase:::checkAndTidyReset
    expect_identical(checkAndTidyReset(0L), 0L)
    expect_identical(checkAndTidyReset(NA_integer_), NA_integer_)
    expect_identical(checkAndTidyReset(0.1), 0.1)
    expect_identical(checkAndTidyReset(0.0), 0L)
    expect_error(checkAndTidyReset(c(0, 1)),
                 "'reset' does not have length 1")
    expect_error(checkAndTidyReset("0"),
                 "'reset' is non-numeric")
})

test_that("checkAndTidyEpsilon works", {
    checkAndTidyEpsilon <- dembase:::checkAndTidyEpsilon
    expect_identical(checkAndTidyEpsilon(0L), 0)
    expect_identical(checkAndTidyEpsilon(1), 1)
    expect_identical(checkAndTidyEpsilon(0.1), 0.1)
    expect_error(checkAndTidyEpsilon(c(5L, 5L)),
                 "'epsilon' does not have length 1")
    expect_error(checkAndTidyEpsilon("5"),
                 "'epsilon' is non-numeric")
    expect_error(checkAndTidyEpsilon(as.numeric(NA)),
                 "'epsilon' is missing")
    expect_error(checkAndTidyEpsilon(-0.001),
                 "'epsilon' is negative")
})    

test_that("checkAndTidyParam works", {
    checkAndTidyParam <- dembase:::checkAndTidyParam
    x <- Values(array(0.5, 
                      dim = c(3, 2, 5, 4),
                      dimnames = list(age = 0:2, sex = c("f", "m"),
                          iteration = 1:5, time = 2001:2004)),
                dimscales = c(time = "Intervals"))
    y <- Values(array(0.3, 
                      dim = c(3, 4, 2),
                      dimnames = list(age = 0:2, time = 2001:2004,
                          sex = c("f", "m"))),
                dimscales = c(time = "Intervals"))
    ans.obtained <- checkAndTidyParam(birth = NULL,
                                      death = NULL,
                                      externalIn = x,
                                      externalOut = y,
                                      internalIn = NULL,
                                      internalOut = NULL)
    ans.expected <- list(externalIn = toDouble(x),
                         externalOut = toDouble(y))
    expect_identical(ans.obtained, ans.expected)
    ## at least one non-null rate
    expect_error(checkAndTidyParam(birth = NULL,
                                   death = NULL,
                                   externalIn = NULL,
                                   externalOut = NULL,
                                   internalIn = NULL,
                                   internalOut = NULL),
                 "birth, death, and migration rates all NULL")
    ## has externalOut iff has externalIn,
    ## and has internalOut iff has internalIn
    expect_error(checkAndTidyParam(birth = NULL,
                                   death = NULL,
                                   externalIn = x,
                                   externalOut = NULL,
                                   internalIn = x,
                                   internalOut = x),
                 "'externalIn' is non-NULL but 'externalOut' is NULL")
    expect_error(checkAndTidyParam(birth = NULL,
                                   death = NULL,
                                   externalIn = NULL,
                                   externalOut = x,
                                   internalIn = x,
                                   internalOut = x),
                 "'externalIn' is NULL but 'externalOut' is non-NULL")
    expect_error(checkAndTidyParam(birth = NULL,
                                   death = NULL,
                                   externalIn = x,
                                   externalOut = x,
                                   internalIn = x,
                                   internalOut = NULL),
                 "'internalIn' is non-NULL but 'internalOut' is NULL")
    expect_error(checkAndTidyParam(birth = NULL,
                                   death = NULL,
                                   externalIn = x,
                                   externalOut = x,
                                   internalIn = NULL,
                                   internalOut = x),
                 "'internalIn' is NULL but 'internalOut' is non-NULL")
    ## has class "Values"
    expect_error(checkAndTidyParam(birth = as(x, "Counts"),
                                   death = x,
                                   externalIn = x,
                                   externalOut = x,
                                   internalIn = NULL,
                                   internalOut = NULL),
                 "'birth' has class \"Counts\"")
    ## no negative values
    x.wrong <- x
    x.wrong[1] <- -1
    expect_error(checkAndTidyParam(birth = x,
                                   death = x.wrong,
                                   externalIn = x,
                                   externalOut = x,
                                   internalIn = NULL,
                                   internalOut = NULL),
                 "'death' has negative values")
    ## is regular
    x.wrong <- Values(array(0.5, 
                      dim = c(3, 2, 5, 4),
                      dimnames = list(age = c("0-4", "5-9", "10+"), sex = c("f", "m"),
                          iteration = 1:5, time = 2001:2004)),
                      dimscales = c(time = "Intervals"))
          expect_error(checkAndTidyParam(birth = x,
                                   death = x,
                                   externalIn = x.wrong,
                                   externalOut = x,
                                   internalIn = NULL,
                                   internalOut = NULL),
                 paste("'externalIn' does not have regular age-time plan :",
                       "age step \\[5\\] does not equal time step \\[1\\]"))
    ## has time dimension with Intervals dimscale
    x.wrong <- Values(array(0.5, 
                      dim = c(3, 2, 5),
                      dimnames = list(age = c("0-4", "5-9", "10+"), sex = c("f", "m"),
                          iteration = 1:5)))
    expect_error(checkAndTidyParam(birth = x,
                                   death = x,
                                   externalIn = x.wrong,
                                   externalOut = x,
                                   internalIn = NULL,
                                   internalOut = NULL),
                 "'externalIn' does not have dimension with dimtype \"time\"")
    x.wrong <- Values(array(0.5, 
                      dim = c(3, 2, 5),
                      dimnames = list(age = c("0-4", "5-9", "10+"), sex = c("f", "m"),
                          time = c(2000, 2005, 2010, 2015, 2020))))
    expect_error(checkAndTidyParam(birth = x,
                                   death = x,
                                   externalIn = x.wrong,
                                   externalOut = x,
                                   internalIn = NULL,
                                   internalOut = NULL),
                 "time dimension for 'externalIn' does not have \"Intervals\" dimscale")
    ## all param have same time dimscale
    x.wrong <- Values(array(0.5, 
                      dim = c(3, 2, 5),
                      dimnames = list(age = 0:2, sex = c("f", "m"),
                          time = 2001:2005)),
                      dimscales = c(time = "Intervals"))
    expect_error(checkAndTidyParam(birth = x,
                                   death = x,
                                   externalIn = x,
                                   externalOut = x,
                                   internalIn = x,
                                   internalOut = x.wrong),
                 "time dimensions of 'internalOut' and 'birth' differ")
})    

test_that("checkInternalDims works", {
    checkInternalDims <- dembase:::checkInternalDims
    initial <- Counts(array(1:6, 
                            dim = 3:2,
                            dimnames = list(age = 0:2, region = 1:2)))
    internalIn <- Values(array(1:6, 
                            dim = 3:2,
                            dimnames = list(age = 0:2, region = 1:2)))
    expect_identical(checkInternalDims(internalDims = NULL, initial = initial,
                                       internalIn = NULL), 
                     NULL)
    expect_identical(checkInternalDims(internalDims = "region", initial = initial,
                                       internalIn = internalIn),
                     NULL)
    expect_error(checkInternalDims(internalDims = NULL, initial = initial,
                                       internalIn = internalIn),
                 "'internalDims' is NULL but 'internalIn' is non-NULL")
    expect_error(checkInternalDims(internalDims = 1, initial = initial,
                                   internalIn = internalIn),
                 "'internalDims' does not have type \"character\"")
    expect_error(checkInternalDims(internalDims = character(), initial = initial,
                                   internalIn = internalIn),
                 "'internalDims' has length 0")
    expect_error(checkInternalDims(internalDims = as.character(NA), initial = initial,
                                   internalIn = internalIn),
                 "'internalDims' has missing values")
    expect_error(checkInternalDims(internalDims = c("region", "region"),
                                   initial = initial, internalIn = internalIn),
                     "'internalDims' has duplicates")   
    expect_error(checkInternalDims(internalDims = "wrong",
                                   initial = initial, internalIn = internalIn),
                 "'initial' does not have dimension specified by 'internalDims' \\[\"wrong\"\\]")
    expect_error(checkInternalDims(internalDims = "age",
                                   initial = initial, internalIn = internalIn),
                 "dimension \"age\" specified by 'internalDims' has dimtype \"age\"")    
})

test_that("convertToCountsObj works", {
    convertToCountsObj <- dembase:::convertToCountsObj
    x <- Values(array(0.5, 
                      dim = c(3, 2, 5),
                      dimnames = list(age = 0:2, sex = c("f", "m"), year = 2001:2005)),
                dimscales = c(year = "Intervals"))
    ans.obtained <- convertToCountsObj(x)
    ans.expected <- Counts(array(as.integer(NA), 
                                 dim = c(3, 2, 5),
                                 dimnames = list(age = 0:2,
                                     sex = c("f", "m"),
                                     year = 2001:2005)),
                           dimscales = c(year = "Intervals"))
    expect_identical(ans.obtained, ans.expected)
})

test_that("iOverlapBetweenIntervals works", {
    iOverlapBetweenIntervals <- dembase:::iOverlapBetweenIntervals
    x <- new("Intervals", dimvalues = c(seq(0, 100, 5), Inf))
    y <- new("Intervals", dimvalues = seq(15, 50, 5))
    expect_identical(iOverlapBetweenIntervals(x = x, y = y),
                     4:10)
    expect_identical(iOverlapBetweenIntervals(x = x, y = x),
                     seq_len(length(x)))
    x <- new("Intervals", dimvalues = c(seq(0, 100, 5), Inf))
    y <- new("Intervals", dimvalues = 15:50)    
    expect_identical(iOverlapBetweenIntervals(x = x, y = y),
                     4:10) 
    x <- new("Intervals", dimvalues = seq(0, 50, 5))
    y <- new("Intervals", dimvalues = 15:55)
    expect_identical(iOverlapBetweenIntervals(x = x, y = y),
                     integer())  
})

test_that("makeBirths works", {
    makeBirths <- dembase:::makeBirths
    for (seed in seq_len(n.test)) {
        ## has sex dimension, has age dimension, step is 1
        birth <- Values(array(runif(n = 40),
                              dim = c(5, 2, 4),
                              dimnames = list(age = 10:14, sex = c("f", "m"), region = 1:4)))
        population <- Counts(array(rpois(n = 160, lambda = 50),
                                   dim = c(20, 2, 4),
                                   dimnames = list(age = 0:19, sex = c("f", "m"), region = 1:4)))
        step <- 1
        sex <- "sex"
        dominant <- "f"
        set.seed(seed + 1)
        ans.obtained <- makeBirths(birth = birth,
                                   population = population,
                                   step = step,
                                   sex = sex,
                                   dominant = dominant)
        set.seed(seed + 1)
        exposure <- population[11:15,,]
        exposure[,2,] <- population[11:15,1,]
        exposure <- 0.5 * exposure
        ans.expected <- as.integer(rpois(n = 40, lambda = birth * exposure))
        ans.expected <- array(ans.expected, dim = dim(birth), dimnames = dimnames(birth))
        ans.expected <- Counts(ans.expected)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
        expect_true(is.integer(ans.obtained))
        ## no sex dimension, has age dimension, step is 5
        birth <- Values(array(runif(n = 12),
                              dim = c(3, 4),
                              dimnames = list(age = c("15-19", "20-24", "25-29"),
                                              region = 1:4)))
        population <- Counts(array(rpois(n = 32, lambda = 50),
                                   dim = c(8, 4),
                                   dimnames = list(age = c("0-4", "5-9", "10-14",
                                                           "15-19", "20-24", "25-29",
                                                           "30-34", "35-39"),
                                                   region = 1:4)))
        step <- 5
        sex <- NULL
        dominant <- NULL
        set.seed(seed + 1)
        ans.obtained <- makeBirths(birth = birth,
                                   population = population,
                                   step = step,
                                   sex = sex,
                                   dominant = dominant)
        set.seed(seed + 1)
        exposure <- 2.5 * population[4:6,]
        ans.expected <- as.integer(rpois(n = 40, lambda = birth * exposure))
        ans.expected <- array(ans.expected, dim = dim(birth), dimnames = dimnames(birth))
        ans.expected <- Counts(ans.expected)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
        expect_true(is.integer(ans.obtained))
    }
})

test_that("makeDeaths works", {
    makeDeaths <- dembase:::makeDeaths
    for (seed in seq_len(n.test)) {
        death <- Values(array(c(NA, runif(5)),
                              dim = 3:2,
                              dimnames = list(age = 0:2, sex = c("f", "m"))))
        population <- Counts(array(rpois(n = 6, lambda = 20),
                                   dim = 3:2,
                                   dimnames = list(age = 0:2, sex = c("f", "m"))))
        set.seed(seed + 1)
        ans.obtained <- makeDeaths(death = death,
                                   population = population,
                                   upper = TRUE,
                                   step = 1)
        set.seed(seed + 1)
        prob <- 0.5 * death
        suppressWarnings(
            ans.expected <- rbinom(n = 6, size = population, prob = prob))
        ans.expected <- Counts(array(ans.expected,
                                     dim = dim(death),
                                     dimnames = dimnames(death)))
        expect_identical(ans.obtained, ans.expected)
        expect_true(is.integer(ans.obtained))
        death <- Values(array(runif(12),
                              dim = 3:4,
                              dimnames = list(age = 0:2, region = 1:4)))
        population <- Counts(array(rpois(n = 12, lambda = 20),
                                   dim = 3:4,
                                   dimnames = list(age = 0:2, region = 1:4)))
        set.seed(seed + 1)
        ans.obtained <- makeDeaths(death = death,
                                   population = population,
                                   upper = FALSE,
                                   step = 5)
        set.seed(seed + 1)
        prob <- 2.5 * death / (1 + 2.5 * death)
        ans.expected <- rbinom(n = 12, size = population, prob = prob)
        ans.expected <- Counts(array(ans.expected,
                                     dim = dim(death),
                                     dimnames = dimnames(death)))
        expect_identical(ans.obtained, ans.expected)
        expect_true(is.integer(ans.obtained))
    }
})

test_that("makeExternal works", {
    makeExternal <- dembase:::makeExternal
    rpoisDiffConstr <- dembase:::rpoisDiffConstr
    for (seed in seq_len(n.test)) {
        externalIn <- Values(array(runif(n = 40),
                                   dim = c(5, 2, 4),
                                   dimnames = list(age = 10:14,
                                       sex = c("f", "m"), region = 1:4)))
        externalOut <- Values(array(runif(n = 40),
                                    dim = c(5, 2, 4),
                                    dimnames = list(age = 10:14,
                                        sex = c("f", "m"), region = 1:4)))
        population <- Values(array(as.integer(rpois(n = 40, lambda = 10)),
                                   dim = c(5, 2, 4),
                                   dimnames = list(age = 10:14,
                                       sex = c("f", "m"), region = 1:4)))
        step <- 1
        maxAttempt <- 1000L
        set.seed(seed + 1)
        ans.obtained <- makeExternal(externalIn = externalIn,
                                     externalOut = externalOut,
                                     population = population,
                                     step = step,
                                     maxAttempt = maxAttempt)
        set.seed(seed + 1)
        lambda.in <- 0.5 * (population + 1) * externalIn
        lambda.out <- 0.5 * population * externalOut
        l <- rpoisDiffConstr(lambda1 = lambda.in,
                             lambda2 = lambda.out,
                             min = -population,
                             maxAttempt = maxAttempt)
        extIns <- Counts(array(l$y1,
                               dim = dim(externalIn),
                               dimnames = dimnames(externalIn)))
        extOuts <- Counts(array(l$y2,
                                dim = dim(externalIn),
                                dimnames = dimnames(externalIn)))
        extNet <- Counts(array(l$y3,
                                dim = dim(externalIn),
                                dimnames = dimnames(externalIn)))
        ans.expected <- list(externalIns = extIns,
                             externalOuts = extOuts,
                             externalNet = extIns - extOuts)        
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
        expect_true(all(ans.obtained$externalIns -
                        ans.obtained$externalOuts >=
                        -population))      
        expect_true(all(sapply(ans.obtained, is.integer)))
    }
})

test_that("makeInternal works when countsModel is FALSE", {
    makeInternal <- dembase:::makeInternal
    ## one internal dimension
    for (seed in seq_len(n.test)) {
        internalIn <- Values(array(runif(n = 40),
                                   dim = c(5, 2, 4),
                                   dimnames = list(age = 0:4,
                                       sex = c("f", "m"),
                                       region = 1:4)))
        internalOut <- Values(array(runif(n = 40),
                                    dim = c(5, 2, 4),
                                    dimnames = list(age = 0:4,
                                        sex = c("f", "m"),
                                        region = 1:4)))
        population <- Values(array(as.integer(rpois(n = 40, lambda = 10)),
                                   dim = c(5, 2, 4),
                                   dimnames = list(age = 0:4,
                                       sex = c("f", "m"),
                                       region = 1:4)))
        set.seed(seed + 1)
        ans.obtained <- makeInternal(internalIn = internalIn,
                                     internalOut = internalOut,
                                     population = population,
                                     countsModel = FALSE,
                                     internalDims = "region")
        set.seed(seed + 1)
        internalOuts <- rbinom(n = 40, size = population, prob = internalOut)
        internalOuts <- Counts(array(internalOuts,
                                     dim = c(5, 2, 4),
                                     dimnames = list(age = 0:4,
                                         sex = c("f", "m"),
                                         region = 1:4)))
        totals <- collapseDimension(internalOuts, dim = "region")
        lambda <- (population + 1) * internalIn
        ans <- array(dim = c(5, 2, 4))
        for (j in 1:2) {
            for (i in 1:5) {
                ans[i,j,] <- rmultinom(n = 1,
                                       size = totals[i,j],
                                       prob = lambda[i, j, ])
            }
        }
        dimnames(ans) <- dimnames(internalIn)
        internalIns <- Counts(ans)
        ans.expected <- list(internalIns = internalIns,
                             internalOuts = internalOuts,
                             internalNet = internalIns - internalOuts)
        expect_identical(ans.obtained, ans.expected)
        expect_true(all(sapply(ans.obtained, is.integer)))
    }
    ## two internal dimensions
    for (seed in seq_len(n.test)) {
        internalIn <- Values(array(runif(n = 90),
                                   dim = c(3, 3, 5, 2),
                                   dimnames = list(reg = 1:3,
                                       eth = 1:3,
                                       age = 0:4,
                                       sex = c("f", "m"))))
        internalOut <- Values(array(runif(n = 90),
                                    dim = c(3, 3, 5, 2),
                                    dimnames = list(reg = 1:3,
                                        eth = 1:3,
                                        age = 0:4,
                                        sex = c("f", "m"))))
        population <- Values(array(as.integer(rpois(n = 90, lambda = 10)),
                                   dim = c(3, 3, 5, 2),
                                   dimnames = list(reg = 1:3,
                                       eth = 1:3,
                                       age = 0:4,
                                       sex = c("f", "m"))))
        set.seed(seed + 1)
        ans.obtained <- makeInternal(internalIn = internalIn,
                                     internalOut = internalOut,
                                     population = population,
                                     countsModel = FALSE,
                                     internalDims = c("reg", "eth"))
        set.seed(seed + 1)
        internalOuts <- rbinom(n = 90, size = population, prob = internalOut)
        internalOuts <- Counts(array(internalOuts,
                                     dim = c(3, 3, 5, 2),
                                     dimnames = list(
                                         reg = 1:3,
                                         eth = 1:3,
                                         age = 0:4,
                                         sex = c("f", "m"))))
        totals <- collapseDimension(internalOuts, dim = c("reg", "eth"))
        lambda <- (population + 1) * internalIn
        lambda <- as(lambda, "array")
        ans <- array(dim = c(3, 3, 5, 2))
        for (j in 1:2) {
            for (i in 1:5) {
                ans[,,i,j] <- rmultinom(n = 1,
                                        size = totals[i,j],
                                        prob = lambda[,,i,j])
            }
        }
        dimnames(ans) <- dimnames(internalIn)
        internalIns <- Counts(ans)
        ans.expected <- list(internalIns = internalIns,
                             internalOuts = internalOuts,
                             internalNet = internalIns - internalOuts)
        expect_identical(ans.obtained, ans.expected)
        expect_true(all(sapply(ans.obtained, is.integer)))
    }
})


test_that("makeInternal works when countsModel is TRUE", {
    makeInternal <- dembase:::makeInternal
    ## one internal dimension
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        internalIn <- Values(array(rpois(n = 40, lambda = 12),
                                   dim = c(5, 2, 4),
                                   dimnames = list(age = 0:4,
                                       sex = c("f", "m"),
                                       region = 1:4)))
        internalOut <- Values(array(rpois(n = 40, lambda = 10),
                                    dim = c(5, 2, 4),
                                    dimnames = list(age = 0:4,
                                        sex = c("f", "m"),
                                        region = 1:4)))
        population <- Values(array(as.integer(rpois(n = 40, lambda = 10)),
                                   dim = c(5, 2, 4),
                                   dimnames = list(age = 0:4,
                                       sex = c("f", "m"),
                                       region = 1:4)))
        set.seed(seed + 1)
        ans.obtained <- makeInternal(internalIn = internalIn,
                                     internalOut = internalOut,
                                     population = population,
                                     countsModel = TRUE,
                                     internalDims = "region")
        set.seed(seed + 1)
        internalOuts <- rpois(n = 40, lambda = internalOut)
        neg.pop <- internalOuts > population
        internalOuts[neg.pop] <- population[neg.pop]
        internalOuts <- Counts(array(internalOuts,
                                     dim = c(5, 2, 4),
                                     dimnames = list(age = 0:4,
                                         sex = c("f", "m"),
                                         region = 1:4)))
        totals <- collapseDimension(internalOuts, dim = "region")
        lambda <- internalIn
        ans <- array(dim = c(5, 2, 4))
        for (j in 1:2) {
            for (i in 1:5) {
                ans[i,j,] <- rmultinom(n = 1,
                                       size = totals[i,j],
                                       prob = lambda[i, j, ])
            }
        }
        dimnames(ans) <- dimnames(internalIn)
        internalIns <- Counts(ans)
        ans.expected <- list(internalIns = internalIns,
                             internalOuts = internalOuts,
                             internalNet = internalIns - internalOuts)
        expect_identical(ans.obtained, ans.expected)
        expect_true(all(sapply(ans.obtained, is.integer)))
    }
    ## two internal dimensions
    for (seed in seq_len(n.test)) {
        internalIn <- Values(array(rpois(n = 90, lambda = 25),
                                   dim = c(3, 3, 5, 2),
                                   dimnames = list(reg = 1:3,
                                       eth = 1:3,
                                       age = 0:4,
                                       sex = c("f", "m"))))
        internalOut <- Values(array(rpois(n = 90, lambda = 25),
                                    dim = c(3, 3, 5, 2),
                                    dimnames = list(reg = 1:3,
                                        eth = 1:3,
                                        age = 0:4,
                                        sex = c("f", "m"))))
        population <- Values(array(as.integer(rpois(n = 90, lambda = 10)),
                                   dim = c(3, 3, 5, 2),
                                   dimnames = list(reg = 1:3,
                                       eth = 1:3,
                                       age = 0:4,
                                       sex = c("f", "m"))))
        set.seed(seed + 1)
        ans.obtained <- makeInternal(internalIn = internalIn,
                                     internalOut = internalOut,
                                     population = population,
                                     countsModel = TRUE,
                                     internalDims = c("reg", "eth"))
        set.seed(seed + 1)
        internalOuts <- rpois(n = 90, lambda = internalOut)
        neg.pop <- internalOuts > population
        internalOuts[neg.pop] <- population[neg.pop]
        internalOuts <- Counts(array(internalOuts,
                                     dim = c(3, 3, 5, 2),
                                     dimnames = list(
                                         reg = 1:3,
                                         eth = 1:3,
                                         age = 0:4,
                                         sex = c("f", "m"))))
        totals <- collapseDimension(internalOuts, dim = c("reg", "eth"))
        lambda <- internalIn
        lambda <- as(lambda, "array")
        ans <- array(dim = c(3, 3, 5, 2))
        for (j in 1:2) {
            for (i in 1:5) {
                ans[,,i,j] <- rmultinom(n = 1,
                                        size = totals[i,j],
                                        prob = lambda[,,i,j])
            }
        }
        dimnames(ans) <- dimnames(internalIn)
        internalIns <- Counts(ans)
        ans.expected <- list(internalIns = internalIns,
                             internalOuts = internalOuts,
                             internalNet = internalIns - internalOuts)
        expect_identical(ans.obtained, ans.expected)
        expect_true(all(sapply(ans.obtained, is.integer)))
    }
})

test_that("makeParamCompatibleWithInitial works", {
    makeParamCompatibleWithInitial <- dembase:::makeParamCompatibleWithInitial
    ## init does not have time dimension
    initial <- Counts(array(1:6, 
                            dim = 3:2,
                            dimnames = list(age = 0:2, sex = c("f", "m"))))
    x <- Values(array(0.5, 
                      dim = c(3, 2, 5),
                      dimnames = list(age = 0:2, sex = c("f", "m"), year = 2001:2005)),
                dimscales = c(year = "Intervals"))
    param <- list(externalIn = x, externalOut = x)
    ans.obtained <- makeParamCompatibleWithInitial(initial = initial, param = param)
    expect_identical(ans.obtained, param)
    ## init has time dimension
    initial <- Counts(array(1:6, 
                            dim = c(1, 3, 2),
                            dimnames = list(time = 2001, age = 0:2, sex = c("f", "m"))),
                      dimscales = c(time = "Intervals"))
    x <- Values(array(0.5, 
                      dim = c(3, 2, 5),
                      dimnames = list(age = 0:2, sex = c("f", "m"), time = 2001:2005)),
                dimscales = c(time = "Intervals"))
    param <- list(externalIn = x, externalOut = x)
    ans.obtained <- makeParamCompatibleWithInitial(initial = initial, param = param)
    ans.expected <- list(externalIn = aperm(x, perm = c(3, 1, 2)),
                         externalOut = aperm(x, perm = c(3, 1, 2)))
    expect_identical(ans.obtained, ans.expected)
    ## param includes births
    age.labels <- c(paste(seq(0, 85, 5), seq(4, 89, 5), sep = "-"), "90+")
    initial <- Counts(array(1:19, 
                            dim = c(1, 19, 2),
                            dimnames = list(time = 2001, age = age.labels,
                                            sex = c("f", "m"))),
                      dimscales = c(time = "Intervals"))
    birth <- Values(array(0.5, 
                          dim = c(7, 2, 5),
                          dimnames = list(age = age.labels[4:10],
                                          sex = c("f", "m"), time = 2001:2005)),
                    dimscales = c(time = "Intervals"))
    death <- Values(array(0.5, 
                          dim = c(19, 2, 5),
                          dimnames = list(age = age.labels,
                                          sex = c("f", "m"), time = 2001:2005)),
                    dimscales = c(time = "Intervals"))
    param <- list(birth = birth, death = death)
    ans.obtained <- makeParamCompatibleWithInitial(initial = initial, param = param)
    ans.expected <- list(birth = aperm(birth, perm = c(3, 1, 2)),
                         death = aperm(death, perm = c(3, 1, 2)))
    expect_identical(ans.obtained, ans.expected)
    ## year dimension name clash
    initial <- Counts(array(1:6, 
                            dim = 3:2,
                            dimnames = list(year = 0:2, sex = c("f", "m"))),
                      dimtypes = c(year = "age"))
    x <- Values(array(0.5,
                      dim = c(3, 2, 5),
                      dimnames = list(age = 0:2, sex = c("f", "m"), year = 2001:2005)),
                dimscales = c(year = "Intervals"))
    param <- list(externalIn = x, externalOut = x)
    expect_error(makeParamCompatibleWithInitial(initial = initial, param = param),
                 "\"year\" dimensions of 'externalIn' and 'initial' not compatible")
    ## initial has age dimension but births does not
    initial <- Counts(array(1:6, 
                            dim = 3:2,
                            dimnames = list(age = 0:2, sex = c("f", "m"))))
    x <- Values(array(0.5,
                      dim = c(2, 5),
                      dimnames = list(sex = c("f", "m"), year = 2001:2005)),
                dimscales = c(year = "Intervals"))
    param <- list(birth = x)
    expect_error(makeParamCompatibleWithInitial(initial = initial, param = param),
                 "'initial' has age dimension but 'birth' does not")
    ## age dimensions of initial and birth not compatible
    initial <- Counts(array(1:6, 
                            dim = 3:2,
                            dimnames = list(age = 0:2, sex = c("f", "m"))))
    birth <- Values(array(0.5,
                          dim = c(3, 5),
                          dimnames = list(age = 1:3, year = 2001:2005)),
                    dimscales = c(year = "Intervals"))
    param <- list(birth = birth)
    expect_error(makeParamCompatibleWithInitial(initial = initial, param = param),
                 "age dimensions of 'initial' and 'birth' are not compatible")
    ## dimensions incompatible
    initial <- Counts(array(1:6, 
                            dim = 3:2,
                            dimnames = list(age = 0:2, sex = c("f", "m"))))
    death <- Values(array(0.5,
                          dim = c(3, 2, 5),
                          dimnames = list(age = 0:2, region = 1:2, year = 2001:2005)),
                    dimscales = c(year = "Intervals"))
    param <- list(death = death)
    expect_error(makeParamCompatibleWithInitial(initial = initial, param = param),
                 paste0("'death' and 'initial' not compatible : ",
                        "one object has dimension \\[",
                        dQuote("region"),
                        "\\] that other does not"))
})
    
test_that("makePopulationObj works", {
    makePopulationObj <- dembase:::makePopulationObj
    ## initial does not have time dimension
    initial <- Counts(array(1:6, 
                            dim = 3:2,
                            dimnames = list(age = 0:2, sex = c("f", "m"))),
                      dimscales = c(age = "Intervals"))
    x <- Values(array(0.5, 
                      dim = c(3, 2, 5),
                      dimnames = list(age = 0:2, sex = c("f", "m"), year = 2001:2005)),
                dimscales = c(age = "Intervals", year = "Intervals"))
    param <- list(externalIn = x, externalOut = x)
    ans.obtained <- makePopulationObj(initial = initial, param = param)
    ans.expected <- Counts(array(as.integer(NA),
                                 dim = c(3, 2, 6),
                                 dimnames = list(age = 0:2, sex = c("f", "m"),
                                     year = 2000:2005)),
                           dimscales = c(age = "Intervals", year = "Points"))
    expect_identical(ans.obtained, ans.expected)
    ## initial has time dimension
    initial <- Counts(array(1:6, 
                            dim = 3:1,
                            dimnames = list(age = 0:2, sex = c("f", "m"), year = 2000)),
                      dimscales = c(age = "Intervals", year = "Points"))
    x <- Values(array(0.5, 
                      dim = c(3, 2, 5),
                      dimnames = list(age = 0:2, sex = c("f", "m"), year = 2001:2005)),
                dimscales = c(age = "Intervals", year = "Intervals"))
    param <- list(externalIn = x, externalOut = x)
    ans.obtained <- makePopulationObj(initial = initial, param = param)
    ans.expected <- Counts(array(as.integer(NA),
                                 dim = c(3, 2, 6),
                                 dimnames = list(age = 0:2, sex = c("f", "m"),
                                     year = 2000:2005)),
                           dimscales = c(age = "Intervals", year = "Points"))
    expect_identical(ans.obtained, ans.expected)
    ## year dimension name clash
    initial <- Counts(array(1:6, 
                            dim = 3:2,
                            dimnames = list(year = 0:2, sex = c("f", "m"))),
                      dimtypes = c(year = "age"),
                      dimscales = c(year = "Intervals"))
    x <- Values(array(0.5,
                      dim = c(3, 2, 5),
                      dimnames = list(age = 0:2, sex = c("f", "m"), year = 2001:2005)),
                dimscales = c(age = "Intervals", year = "Intervals"))
    param <- list(externalIn = x, externalOut = x)
    expect_error(makePopulationObj(initial = initial, param = param),
                 "\"year\" dimensions of 'externalIn' and 'initial' not compatible")
})
    
test_that("makeProbDeath works", {
    makeProbDeath <- dembase:::makeProbDeath
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        death <- Values(array(c(NA, runif(5)),
                              dim = 3:2,
                              dimnames = list(age = 0:2, sex = c("f", "m"))))
        ans.obtained <- makeProbDeath(death = death,
                                      upper = TRUE,
                                      step = 1)
        ans.expected <- 0.5 * death
        expect_identical(ans.obtained, ans.expected)
        death <- Values(array(runif(6),
                              dim = 3:2,
                              dimnames = list(age = 0:2, sex = c("f", "m"))))
        ans.obtained <- makeProbDeath(death = death,
                                      upper = FALSE,
                                      step = 5)
        ans.expected <-  2.5 * death / (1 + 2.5 * death)
        expect_identical(ans.obtained, ans.expected)
    }
})
    
test_that("makeProjectForward works", {
    makeProjectForward <- dembase:::makeProjectForward
    ## project.forward is TRUE
    initial <- Counts(array(1:6, 
                            dim = 3:2,
                            dimnames = list(age = 0:2, sex = c("f", "m"))),
                      dimscales = c(age = "Intervals"))
    param <- list(externalIn = Values(array(0.5, 
                      dim = c(3, 2, 5),
                      dimnames = list(age = 0:2, sex = c("f", "m"), time = 2001:2005)),
                                      dimscales = c(age = "Intervals", time = "Intervals")),
                  externalOut = Values(array(0.3, 
                      dim = c(3, 2, 5),
                      dimnames = list(age = 0:2, sex = c("f", "m"), time = 2001:2005)),
                  dimscales = c(age = "Intervals", time = "Intervals")))
    ans.obtained <- makeProjectForward(initial = initial, param = param)
    expect_true(ans.obtained)
    initial <- Counts(array(1:6, 
                            dim = 3:1,
                            dimnames = list(age = 0:2, sex = c("f", "m"), time = 2000)),
                      dimscales = c(age = "Intervals", time = "Points"))
    param <- list(externalIn = Values(array(0.5, 
                      dim = c(3, 2, 5),
                      dimnames = list(age = 0:2, sex = c("f", "m"), time = 2001:2005)),
                                      dimscales = c(age = "Intervals", time = "Intervals")),
                  externalOut = Values(array(0.3, 
                      dim = c(3, 2, 5),
                      dimnames = list(age = 0:2, sex = c("f", "m"), time = 2001:2005)),
                                       dimscales = c(age = "Intervals", time = "Intervals")))
    ans.obtained <- makeProjectForward(initial = initial, param = param)
    expect_true(ans.obtained)
    ## project.forward is FALSE
    initial <- Counts(array(1:6, 
                            dim = 3:1,
                            dimnames = list(age = 0:2, sex = c("f", "m"), time = 2005)),
                      dimscales = c(age = "Intervals", time = "Points"))
    param <- list(externalIn = Values(array(0.5, 
                      dim = c(3, 2, 5),
                      dimnames = list(age = 0:2, sex = c("f", "m"), time = 2001:2005)),
                                      dimscales = c(age = "Intervals", time = "Intervals")),
                  externalOut = Values(array(0.3, 
                      dim = c(3, 2, 5),
                      dimnames = list(age = 0:2, sex = c("f", "m"), time = 2001:2005)),
                      dimscales = c(age = "Intervals", time = "Intervals")))
    ans.obtained <- makeProjectForward(initial = initial, param = param)
    expect_false(ans.obtained)
    ## time dimensions incompatible
    initial <- Counts(array(1:6, 
                            dim = 3:1,
                            dimnames = list(age = 0:2, sex = c("f", "m"), time = 1995)),
                      dimscales = c(age = "Intervals", time = "Points"))
    param <- list(externalIn = Values(array(0.5, 
                      dim = c(3, 2, 5),
                      dimnames = list(age = 0:2, sex = c("f", "m"), time = 2001:2005)),
                      dimscales = c(age = "Intervals", time = "Intervals")),
                  externalOut = Values(array(0.3, 
                      dim = c(3, 2, 5),
                      dimnames = list(age = 0:2, sex = c("f", "m"), time = 2001:2005)),
                      dimscales = c(age = "Intervals", time = "Intervals")))
    expect_error(makeProjectForward(initial = initial, param = param),
                 "time dimensions for 'initial' and 'externalIn' incompatible")
})

test_that("rpoisDiffConstr works", {
    rpoisDiffConstr <- dembase:::rpoisDiffConstr
    has.NAs <- FALSE
    for (seed in seq_len(n.test)) {
        lambda1 <- runif(n = 20, min = 5, max = 25)
        lambda2 <- runif(n = 20, min = 0, max = 20)
        min <- as.integer(rpois(n = 20, lambda = 5)) - 5L
        ans <- rpoisDiffConstr(lambda1 = lambda1,
                               lambda2 = lambda2,
                               min = min,
                               maxAttempt = 1000L)
        y1 <- ans$y1
        y2 <- ans$y2
        y3 <- ans$y3
        expect_true(identical(is.na(y1), is.na(y2)))
        expect_true(all((y1 - y2)[!is.na(y1)] >= min[!is.na(y1)]))
        expect_identical(y3[is.na(y1)], min[is.na(y1)])
        expect_true(length(ans$y1) == 20L)
        expect_true(length(ans$y2) == 20L)
        expect_true(all(ans$y1[!is.na(y1)] >= 0))
        expect_true(all(ans$y2[!is.na(y1)] >= 0))
        if (!has.NAs)
            has.NAs <- any(is.na(y1))
    }
    if (!has.NAs)
        warning("no NAs generated")
})

test_that("R and C versions of rpoisDiffConstr give same answer", {
    rpoisDiffConstr <- dembase:::rpoisDiffConstr
    has.NAs <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        lambda1 <- runif(n = 20, min = 5, max = 15)
        lambda2 <- runif(n = 20, min = 0, max = 10)
        min <- as.integer(rpois(n = 20, lambda = 10)) - 5L
        set.seed(seed+1)
        ans.R <- rpoisDiffConstr(lambda1 = lambda1,
                                 lambda2 = lambda2,
                                 min = min,
                                 maxAttempt = 10L,
                                 useC = FALSE)
        set.seed(seed+1)
        ans.C <- rpoisDiffConstr(lambda1 = lambda1,
                                 lambda2 = lambda2,
                                 min = min,
                                 maxAttempt = 10L,
                                 useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
        if (!has.NAs)
            has.NAs <- any(is.na(ans.R$y1))
    }
    if (!has.NAs)
        warning("no NAs generated")
})


test_that("R and C versions of rpoisDiffConstr give same answer, force NAs", {
    rpoisDiffConstr <- dembase:::rpoisDiffConstr
    has.NAs <- FALSE
    seed <- 1
    set.seed(seed)
    n <- 20L
    lambda1 <- runif(n = n, min = 5, max = 25)
    lambda2 <- runif(n = n, min = 0, max = 20)
    min <- as.integer(rpois(n = n, lambda = 5)) - 5L
    lambda1[1] = as.double(NA)
    lambda1[3] = as.double(NA)
    lambda2[10] = as.double(NA)
    lambda2[13] = as.double(NA)
    min[13] = as.integer(NA)
    min[19] = as.integer(NA)
    set.seed(seed+1)
    ans.R <- rpoisDiffConstr(lambda1 = lambda1,
                           lambda2 = lambda2,
                           min = min,
                           maxAttempt = 1000L,
                             useC = FALSE)
    set.seed(seed+1)
    ans.C <- rpoisDiffConstr(lambda1 = lambda1,
                             lambda2 = lambda2,
                             min = min,
                             maxAttempt = 1000L,
                             useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
    if (!has.NAs)
        has.NAs <- any(is.na(ans.R$y1))
    if (!has.NAs)
        warning("no NAs generated")
})


test_that("R and C versions of rpoisDiffConstr give same answer, prop1 - prop2 < m", {
    rpoisDiffConstr <- dembase:::rpoisDiffConstr
    has.NAs <- FALSE
    seed <- 1
    set.seed(seed)
    n <- 20L
    lambda1 <- runif(n = n, min = 0, max = 10)
    lambda2 <- runif(n = n, min = 10, max = 20)
    min <- as.integer(rpois(n = n, lambda = 5)) - 5L
    maxAttemptCode <- 1L
    maxAttemptSeed <- 100L
    foundSeed <- FALSE
    attemptSeed <- 0L
    while (!foundSeed && (attemptSeed < maxAttemptSeed)) {
        seed <- seed + attemptSeed
        set.seed(seed + 1)
        attemptSeed <- attemptSeed + 1
        foundCode <- FALSE
        attemptCode <- 0L
        while (!foundCode && (attemptCode < maxAttemptCode)) {
            attemptCode <- attemptCode + 1
            prop1 <- rpois(n = 1L, lambda = lambda1[1])
            prop2 <- rpois(n = 1L, lambda = lambda2[1])
            foundCode <- (prop1 - prop2) >= min[1]
        }
        foundSeed <- !foundCode
    }
    if (foundSeed) {
        set.seed(seed+1)
        ans.R <- rpoisDiffConstr(lambda1 = lambda1,
                               lambda2 = lambda2,
                               min = min,
                               maxAttempt = maxAttemptCode,
                                 useC = FALSE)
        set.seed(seed+1)
        ans.C <- rpoisDiffConstr(lambda1 = lambda1,
                                 lambda2 = lambda2,
                                 min = min,
                                 maxAttempt = maxAttemptCode,
                                 useC = TRUE)
        expect_identical(ans.R, ans.C)
        has.NAs <- any(is.na(ans.R$y1))
        if (!has.NAs) {
            warning("no NAs generated")
            }
    } else {
        warning("no NAs prop1 - prop1 < m generated")
    }
})

