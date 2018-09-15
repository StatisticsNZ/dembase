

#' Convert age labels based on exact ages to
#' age labels based on intervals
#'
#' Convert a vector of exact ages, or single-year ages, to a vector
#' of age groups. Examples of exact ages are real numbers such
#' as \code{6.238} or \code{77.13}.  Examples of single-year ages
#' are \code{6} or \code{77}.
#'
#'
#' The age groups produced by \code{ageToAgeGroup} are formatted in
#' the way expected by functions such as \code{\link{Counts}} and
#' \code{\link{Values}}.
#'
#' If \code{age} is a factor, then \code{ageToAgeGroup} coerces
#' it to a character vector before trying to coerce it to numeric.
#' See below for an example.
#'
#' By default, \code{ageToAgeGroup} creates 5-year age groups.
#' See below for examples of other intevals.
#'
#' @param age A vector of exact or single-year ages.
#' A numeric vector, or a vector that can be coerced to numeric.
#' @param breaks A vector of breaks, specifying the points that
#' define the age groups.
#' @param firstOpen Logical. Whether the first age group is "open",
#' i.e. has no lower bound. Defaults to \code{FALSE}.
#' @param lastOpen Logical. Whether the last age group is "open",
#' i.e. has no upper bound. Defaults to \code{TRUE}.
#'
#' @return A factor, the same length as \code{age}.
#'
#' @seealso \code{\link{yearToPeriod}}. \code{\link{seq}} (in combination with \code{\link{c}})
#' is useful for creating complicated \code{breaks} arguments.
#'
#' @examples
#' age <- c(22, 18, 4, 0, 89, 103, 7)
#' ## 5-year age groups, 0-4, 5-9, ..., 95-99, 100+
#' ageToAgeGroup(age)
#' ## 1-year age groups, 0, 1, ..., 89, 90+
#' ageToAgeGroup(age, breaks = 0:90)
#' ## age groups 0, 1-4, 5-9, 10-14, ..., 85+
#' ageToAgeGroup(age, breaks = c(0, 1, seq(5, 85, 5)))
#' ## last age group closed
#' ageToAgeGroup(age = c(0, 17, 14, 3, 9),
#'               breaks = seq(0, 20, 5),
#'               lastOpen = FALSE)
#' ## exact ages
#' age <- c(0.356, 1.363, 3.22, 2.109)
#' ageToAgeGroup(age, break = c(0, 1, 2, 3))
#' @export
ageToAgeGroup <- function(age, breaks = seq(0, 100, 5), firstOpen = FALSE,
                          lastOpen = TRUE) {
    singleYearToMultiYear(vec = age,
                          breaks = breaks,
                          labelStart = TRUE,
                          firstOpen = firstOpen,
                          lastOpen = lastOpen,
                          nameVec = "age",
                          isAge = TRUE)
}

test_that("ageToAgeGroup works", {
    ans.obtained <- ageToAgeGroup(c(0, 50, 33, 110))
    ans.expected <- factor(c("0-4", "50-54", "30-34", "100+"),
                           levels = c(paste(seq(0, 95, 5), seq(4, 99, 5), sep = "-"), "100+"))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- ageToAgeGroup(c(0, 50, 33, 99), firstOpen = TRUE, lastOpen = FALSE)
    ans.expected <- factor(c("0-4", "50-54", "30-34", "95-99"),
                           levels = c("<0", paste(seq(0, 95, 5), seq(4, 99, 5), sep = "-")))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- ageToAgeGroup(c(0, 50, 33, 110), breaks = c(0, 1, seq(5, 90, 5)))
    ans.expected <- factor(c("0", "50-54", "30-34", "90+"),
                           levels = c(0, "1-4", paste(seq(5, 85, 5), seq(9, 89, 5), sep = "-"), "90+"))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- ageToAgeGroup(as.character(c(0, 50, 33, 110)))
    ans.expected <- factor(c("0-4", "50-54", "30-34", "100+"),
                           levels = c(paste(seq(0, 95, 5), seq(4, 99, 5), sep = "-"), "100+"))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- ageToAgeGroup(integer())
    ans.expected <- factor(character(),
                           levels = c(paste(seq(0, 95, 5), seq(4, 99, 5), sep = "-"), "100+"))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- ageToAgeGroup(factor(c(0, 50, 33, 110)))
    ans.expected <- factor(c("0-4", "50-54", "30-34", "100+"),
                           levels = c(paste(seq(0, 95, 5), seq(4, 99, 5), sep = "-"), "100+"))
    expect_identical(ans.obtained, ans.expected)
})

test_that("ageToAgeGroup throws appropriate errors", {
    expect_error(ageToAgeGroup(list("a", "b", "c")),
                 "'age' has class \"list\"")
    expect_error(ageToAgeGroup(c("1", NA, "b")),
                 "value \"b\" from 'age' cannot be coerced to numeric")
    expect_error(ageToAgeGroup(c(0, 1, 10), breaks = integer()),
                 "'breaks' has length 0")
    expect_error(ageToAgeGroup(c(0, 1, 10), breaks = "1"),
                 "'breaks' is non-numeric")
    expect_error(ageToAgeGroup(c(0, 1, 10), breaks = c(0, NA)),
                 "'breaks' has missing values")
    expect_error(ageToAgeGroup(c(0, 1, 10), breaks = c(0, 5, 5)),
                 "'breaks' has duplicates")
    expect_error(ageToAgeGroup(c(0, 1, 10), breaks = c(0, 5, 4)),
                 "'breaks' is non-increasing")
    expect_error(ageToAgeGroup(c(0, 1, 10), firstOpen = c(TRUE, FALSE)),
                 "'firstOpen' does not have length 1")
    expect_error(ageToAgeGroup(c(0, 1, 10), lastOpen = "TRUE"),
                 "'lastOpen' has class \"character\"")
    expect_error(ageToAgeGroup(c(0, 1, 10), firstOpen = NA),
                 "'firstOpen' is missing")
    expect_error(ageToAgeGroup(c(0, 1, 10), breaks = c(5, 100), firstOpen = FALSE),
                 "'age' has values less than the lowest value of 'breaks', but 'firstOpen' is FALSE")
    expect_error(ageToAgeGroup(c(0, 1, 10), breaks = c(0, 5), lastOpen = FALSE),
                 "'age' has values greater than or equal to the highest value of 'breaks', but 'lastOpen' is FALSE")
})



## HAS_TESTS
checkAndTidyYearStart <- function(yearStart) {
    if (!is.numeric(yearStart))
        stop(gettextf("'%s' is non-numeric",
                      "yearStart"))
    if (!identical(length(yearStart), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "yearStart", 1L))
    if (is.na(yearStart))
        stop(gettextf("'%s' is missing",
                      "yearStart"))
    if (yearStart != as.integer(yearStart))
        stop(gettextf("'%s' is not an integer",
                      "yearStart"))
    as.integer(yearStart)
}



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





## HAS_TESTS
checkLastOpen <- function(lastOpen) {
    if (!is.logical(lastOpen))
        stop(gettextf("'%s' does not have type \"%s\"",
                      "lastOpen", "logical"))
    if (!identical(length(lastOpen), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "lastOpen", 1L))
    if (is.na(lastOpen))
        stop(gettextf("'%s' is missing",
                      "lastOpen"))
    NULL
}


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


## HAS_TESTS
completedYears <- function(date, dob) {
    year.date <- as.integer(format(date, "%Y"))
    year.dob <- as.integer(format(dob, "%Y"))
    month.date <- as.integer(format(date, "%m"))
    month.dob <- as.integer(format(dob, "%m"))
    day.date <- as.integer(format(date, "%d"))
    day.dob <- as.integer(format(dob, "%d"))
    change.leap.day.date.to.28 <- isLeapYear(year.date) & (month.date == 2L) & (day.date == 29L)
    change.leap.day.dob.to.28 <- isLeapYear(year.dob) & (month.dob == 2L) & (day.dob == 29L)
    day.date[change.leap.day.date.to.28] <- 28L
    day.dob[change.leap.day.dob.to.28] <- 28L
    diff.year <- year.date - year.dob
    diff.month <- month.date - month.dob
    diff.day <- day.date - day.dob
    diff.year + ((diff.month > 0L) | ((diff.month == 0L) & (diff.day >= 0L))) - 1L
}



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


## HAS_TESTS
completedMonths <- function(date, dob) {
    year.date <- as.integer(format(date, "%Y"))
    year.dob <- as.integer(format(dob, "%Y"))
    month.date <- as.integer(format(date, "%m"))
    month.dob <- as.integer(format(dob, "%m"))
    day.date <- as.integer(format(date, "%d"))
    day.dob <- as.integer(format(dob, "%d"))
    change.leap.day.date.to.28 <- isLeapYear(year.date) & (month.date == 2L) & (day.date == 29L)
    change.leap.day.dob.to.28 <- isLeapYear(year.dob) & (month.dob == 2L) & (day.dob == 29L)
    day.date[change.leap.day.date.to.28] <- 28L
    day.dob[change.leap.day.dob.to.28] <- 28L
    diff.year <- year.date - year.dob
    diff.month <- month.date - month.dob
    diff.day <- day.date - day.dob
    12L * diff.year + diff.month + (diff.day >= 0L) - 1L
}



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





#' Calculate ages, periods, cohorts, and Lexis triangles from dates.
#'
#' Calculates ages in completed years, periods, birth cohorts,
#' or Lexis triangles from dates.
#'
#' If \code{date} and \code{dob} are different lengths, the shorter
#' vector is recycled.
#'
#' By default, age-time steps are assumed to be equal to one year. However,
#' alternative lengths can be specified, via the \code{step} argument.
#' The steps are described as multiples of years, quarters or months.
#' If no multiple is provided, it is assumed to equal one.
#' Typical values are \code{"5 years"}, \code{"month"}, 
#' \code{"quarter"}, and \code{"6 months"}.  If a step length is less than 1
#' year,  then it must divide the year without leaving a remainder.
#' Thus, for example, \code{"4 months"} is a valid value for \code{step},
#' but \code{"5 months"} is not.
#'
#' By default, periods of one year are assumed to start on 1 January.
#' Other starting months can be specified via \code{monthStart}.
#' See below for examples.
#'
#' Multi-year periods are assumed to start on years beginning with 0 or 5.
#' Other values can be specified via \code{yearStart}.  See below for examples. 
#' 
#' These functions handle leap years differently from \code{\link{seq.Date}}.
#' Like most people, but unlike \code{\link{seq.Date}}, they treat 29 February
#' as if it was 28 February.  For instance, with \code{datesToAge}, a
#' person who was born on 29 February 2000 reaches age 1 on 28 February 2001.
#' With \code{seq.Date}, they reach age 1 on 1 March 2001.
#' 
#' @param date A vector of class \code{\link[base]{Date}}.  All elements of
#' \code{date} must be equal to or greater than the corresponding elements
#' of \code{dob}.
#' @param dob A vector of class \code{\link[base]{Date}}.
#' @param step Length of age-time step.
#' @param lastOpen Whether last age interval is open on the right.
#' @param monthStart The English name of a month (as defined by
#' \code{\link{month.name}}.  Can be abbreviated.
#' @param yearStart An integer. Controls the start date of multi-year
#' periods.
#'
#' @return A factor.
#'
#' @seealso Vectors of class \code{\link{Date}} can be created with
#' function \code{\link{as.Date}}.
#' @examples
#' date <- as.Date(c("2005-07-05", "2006-06-30", "2008-07-05"))
#' dob <- as.Date(c("2005-06-30", "2005-06-30", "2006-07-01"))
#' dateToAgeGroup(date = date, dob = dob)
#' dateToAgeGroup(date = date, dob = dob, lastOpen = FALSE)
#' dateToAgeGroup(date = date, dob = dob, step = "2 years")
#' dateToAgeGroup(date = date, dob = dob, step = "quarter")
#' dateToAgeGroup(date = date, dob = dob, step = "month")
#' datesToPeriods(date = date)
#' datesToPeriods(date = date, monthStart = "July")
#' datesToPeriods(date = date, step = "2 years", yearStart = 2001)
#' datesToPeriods(date = date, step = "quarter")
#' datesToCohorts(dob)
#' datesToCohorts(dob, step = "2 quarters")
#' datesToTriangles(date = date, dob = dob)
#' datesToTriangles(date = date, dob = dob, step = "2 years")
#' datesToTriangles(date = date, dob = dob, monthStart = "April")
#' 
#' ## 'date' must be later than 'dob'
#' \dontrun{
#' dateToAgeGroup(date = as.Date("2000-01-01"), dob = as.Date("2005-01-01"))
#' }
#' @export
#' @name dateToAgeGroup
NULL


## HAS_TESTS
#' @rdname dateToAgeGroup
#' @export
dateToAgeGroup <- function(date, dob, step = "1 year", lastOpen = TRUE) {
    l <- checkAndTidyDateAndDOB(date = date,
                                dob = dob)
    date <- l$date
    dob <- l$dob
    l <- makeStepUnitsAndStepNum(step)
    step.units <- l$stepUnits
    step.num <- l$stepNum
    checkLastOpen(lastOpen)
    if (step.units == "years")
        age <- completedYears(date = date,
                              dob = dob)
    else if (step.units == "months")
        age <- completedMonths(date = date,
                               dob = dob)
    else
        stop(gettextf("invalid value for '%s' : \"%s\"",
                      "stepUnits", step.units))
    i.age.interval <- age %/% step.num + 1L
    n.age.interval <- max(i.age.interval, na.rm = TRUE)
    age.labels <- makeAgeLabels(stepNum = step.num,
                                stepUnits = step.units,
                                nAgeInterval = n.age.interval,
                                lastOpen = lastOpen)
    ans <- age.labels[i.age.interval]
    factor(ans, levels = age.labels)
}




test_that("dateToAgeGroup works", {
    dob <- as.Date("2000-06-30")
    date <- as.Date(c("2000-06-30", "2000-07-01", "2001-06-29", "2001-06-30",
                      "2001-07-01", "2005-01-01", "2005-12-01"))
    ans.obtained <- dateToAgeGroup(date = date, dob = dob, lastOpen = FALSE)
    ans.expected <- factor(c("0", 0, 0, 1, 1, 4, 5),
                           levels = as.character(0:5))
    expect_identical(ans.obtained, ans.expected)
    dob <- as.Date(c("2000-06-30", "2000-08-30", "2001-01-01", "2001-12-31"))
    date <- as.Date(c("2002-06-30", "2003-07-01"))
    ans.obtained <- dateToAgeGroup(date = date, dob = dob, lastOpen = FALSE)
    ans.expected <- factor(c("2", "2", "1", "1"),
                           levels = as.character(0:2))
    expect_identical(ans.obtained, ans.expected)
    dob <- as.Date(c(NA, "2000-08-30", "2001-01-01", "2001-12-31"))
    date <- as.Date(c("2002-06-30", "2003-07-01"))
    ans.obtained <- dateToAgeGroup(date = date, dob = dob, lastOpen = FALSE)
    ans.expected <- factor(c(NA, "2", "1", "1"),
                           levels = as.character(0:2))
    expect_identical(ans.obtained, ans.expected)
    ## 5-year intervals
    dob <- as.Date("2000-06-30")
    date <- as.Date(c("2000-06-30", "2000-07-01", "2001-06-29", "2001-06-30",
                      "2001-07-01", "2005-01-01", "2005-12-01"))
    ans.obtained <- dateToAgeGroup(date = date, dob = dob, step = "5 years", lastOpen = TRUE)
    ans.expected <- factor(c("0-4", "0-4", "0-4", "0-4", "0-4", "0-4", "5+"),
                           levels = c("0-4", "5+"))
    expect_identical(ans.obtained, ans.expected)
    ## quarter intervals
    dob <- as.Date("2000-01-01")
    date <- as.Date(c("2000-06-30", "2000-07-01", "2001-06-29", "2001-07-01"))
    ans.obtained <- dateToAgeGroup(date = date, dob = dob, step = "quarter", lastOpen = TRUE)
    ans.expected <- factor(c("0.25-0.5", "0.5-0.75", "1.25-1.5", "1.5+"),
                           levels = c("0-0.25", "0.25-0.5", "0.5-0.75", "0.75-1", "1-1.25", "1.25-1.5", "1.5+"))
    expect_identical(ans.obtained, ans.expected)
})

#' @rdname dateToAgeGroup
#' @export
datesToCohorts <- function(dob, step = "years", monthStart = "January",
                           yearStart = 2000) {
    datesToPeriods(date = dob,
                   step = step,
                   monthStart = monthStart,
                   yearStart = yearStart)
}



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

#' @rdname dateToAgeGroup
#' @export
datesToPeriods <- function(date, step = "years", monthStart = "January",
                           yearStart = 2000) {
    checkDate(date)
    l <- makeStepUnitsAndStepNum(step)
    step.units <- l$stepUnits
    step.num <- l$stepNum
    if (step.units == "years") {
        yearStart <- checkAndTidyYearStart(yearStart)
        monthStartNum <- monthStartNum(monthStart)
        date.vec <- makeDateVecYears(dates = date,
                                     stepNum = step.num,
                                     monthStartNum = monthStartNum,
                                     yearStart = yearStart)
        labels <- makePeriodLabelsYears(dateVec = date.vec,
                                        stepNum = step.num)
    }
    else if (step.units == "months") {
        date.vec <- makeDateVecMonths(dates = date,
                                      stepNum = step.num)
        labels <- makePeriodLabelsMonths(dateVec = date.vec,
                                         stepNum = step.num)
    }
    else
        stop(gettextf("invalid value for '%s'",
                      "stepUnits"))
    i.period <- findInterval(x = date,
                             vec = date.vec)
    ans <- labels[i.period]
    s.periods.used <- seq.int(from = min(i.period),
                              to = max(i.period))
    levels <- labels[s.periods.used]
    factor(ans, levels = levels)
}




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




## HAS_TESTS
#' @rdname dateToAgeGroup
#' @export
datesToTriangles <- function(date, dob, step = "years", monthStart = "January",
                             yearStart = 2000) {
    l <- checkAndTidyDateAndDOB(date = date,
                                dob = dob)
    date <- l$date
    dob <- l$dob
    l <- makeStepUnitsAndStepNum(step)
    step.units <- l$stepUnits
    step.num <- l$stepNum
    if (step.units == "years") {
        yearStart <- checkAndTidyYearStart(yearStart)
        month.start.num <- monthStartNum(monthStart)
        age <- completedYears(date = date,
                              dob = dob)
        i.time.interval <- iIntervalSinceBirthYears(date = date,
                                                    dob = dob,
                                                    stepNum = step.num,
                                                    monthStartNum = month.start.num,
                                                    yearStart = yearStart)
    }
    else if (step.units == "months") {
        age <- completedMonths(date = date,
                               dob = dob)
        i.time.interval <- iIntervalSinceBirthMonths(date = date,
                                                     dob = dob,
                                                     stepNum = step.num)
    }
    else
        stop(gettextf("invalid value for '%s' : \"%s\"",
                      "stepUnits", step.units))
    i.age.interval <- age %/% step.num + 1L
    ans <- ifelse(i.time.interval > i.age.interval, "TU", "TL")
    factor(ans, levels = c("TL", "TU"))
}




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

## HAS_TESTS
iIntervalSinceBirthYears <- function(date, dob, stepNum, monthStartNum, yearStart) {
    dates <- c(date, dob)
    vec <- makeDateVecYears(dates = dates,
                            stepNum = stepNum,
                            monthStartNum = monthStartNum,
                            yearStart = yearStart)
    i.date <- findInterval(x = date,
                           vec = vec)
    i.dob <- findInterval(x = dob,
                          vec = vec)
    i.date - i.dob + 1L
}



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

## HAS_TESTS
iIntervalSinceBirthMonths <- function(date, dob, stepNum) {
    dates <- c(date, dob)
    vec <- makeDateVecMonths(dates = dates,
                             stepNum = stepNum)
    i.date <- findInterval(x = date,
                           vec = vec)
    i.dob <- findInterval(x = dob,
                          vec = vec)
    i.date - i.dob + 1L
}





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

## HAS_TESTS
isLeapYear <- function(year) {
    ((year %% 4L == 0L) & (year %% 100L != 0L)) | (year %% 400L == 0L)
}


test_that("isLeapYear works", {
    isLeapYear <- dembase:::isLeapYear
    ans.obtained <- isLeapYear(c(1999, 2000, 2100, 2004, 2003))
    ans.expected <- c(FALSE, TRUE, FALSE, TRUE, FALSE)
    expect_identical(ans.obtained, ans.expected)
})

## HAS_TESTS
makeDateVecYears <- function(dates, stepNum, monthStartNum, yearStart) {
    min.date <- min(dates, na.rm = TRUE)
    max.date <- max(dates, na.rm = TRUE)
    month.min <- as.integer(format(min.date, "%m"))
    month.max <- as.integer(format(max.date, "%m"))
    year.min <- as.integer(format(min.date, "%Y"))
    year.max <- as.integer(format(max.date, "%Y"))
    if (month.min >= monthStartNum)
        year.from <- year.min
    else
        year.from <- year.min - 1L
    year.from <- year.from - ((year.from - yearStart) %% stepNum)
    if (month.max < monthStartNum)
        year.to <- year.max
    else
        year.to <- year.max + 1L
    if ((year.to - yearStart) %% stepNum != 0L)
        year.to <- year.to - ((year.to - yearStart) %% stepNum) + stepNum
    from <- paste(year.from, monthStartNum, 1, sep = "-")
    to <- paste(year.to, monthStartNum, 1, sep = "-")
    from <- as.Date(from)
    to <- as.Date(to)
    by <- paste(stepNum, "years")
    seq.Date(from = from,
             by = by,
             to = to)
}


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

## HAS_TESTS
makeDateVecMonths <- function(dates, stepNum) {
    min.date <- min(dates, na.rm = TRUE)
    max.date <- max(dates, na.rm = TRUE)
    month.min <- as.integer(format(min.date, "%m"))
    month.max <- as.integer(format(max.date, "%m"))
    year.min <- as.integer(format(min.date, "%Y"))
    year.max <- as.integer(format(max.date, "%Y"))
    year.from <- year.min
    month.from <- month.min - ((month.min - 1L) %% stepNum)
    if (month.max < 13L - stepNum) {
        year.to <- year.max
        if (month.max %% stepNum == 0L)
            month.to <- month.max + 1L
        else
            month.to <- month.max - (month.max %% stepNum) + stepNum + 1L
    }
    else {
        year.to <- year.max + 1L
        month.to <- 1L
    }
    from <- paste(year.from, month.from, 1, sep = "-")
    to <- paste(year.to, month.to, 1, sep = "-")
    from <- as.Date(from)
    to <- as.Date(to)
    by <- paste(stepNum, "months")
    seq.Date(from = from,
             by = by,
             to = to)
}



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


## HAS_TESTS
makeStepUnitsAndStepNum <- function(step) {
    kValidUnits <- c("months", "quarters", "years")
    if (!is.character(step))
        stop(gettextf("'%s' does not have type \"%s\"",
                      "step", "character"))
    if (!identical(length(step), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "step", 1L))
    if (is.na(step))
        stop(gettextf("'%s' is missing",
                      "step"))
    step.split <- strsplit(step,
                           split = " ",
                           fixed = TRUE)[[1L]]
    n.split <- length(step.split)
    if (n.split == 1L)
        units <- step.split
    else if (n.split == 2L)
        units <- step.split[2L]
    else
        stop(gettextf("invalid value for '%s'",
                      "step"))
    i.units <- pmatch(units, kValidUnits, nomatch = 0L)
    units.valid <- i.units > 0L
    if (!units.valid)
        stop(gettextf("invalid value for '%s' : invalid units",
                      "step"))
    units <- kValidUnits[i.units]
    if (n.split == 1L)
        num <- 1L
    else {
        num <- step.split[1L]
        num <- tryCatch(as.numeric(num),
                        warning = function(w) w)
        if (methods::is(num, "warning"))
            stop(gettextf("invalid value for '%s' : invalid number of units",
                          "step"))
        if (!isTRUE(all.equal(as.integer(num), num)))
            stop(gettextf("invalid value for '%s' : non-integer number of units",
                          "step"))
        num <- as.integer(num)
        if (num <= 0L)
            stop(gettextf("invalid value for '%s' : non-positive number of units",
                          "step"))
        if ((units == "months") && ((12L %% num) != 0L))
            stop(gettextf("invalid value for '%s' : one year cannot be divided into intervals of length \"%s %s\"",
                          "step", num, units))
        if ((units == "quarters") && ((12L %% (num * 3L)) != 0L))
            stop(gettextf("invalid value for '%s' : one year cannot be divided into intervals of length \"%s %s\"",
                          "step", num, units))
    }
    if (units == "quarters") {
        units <- "months"
        num <- num * 3L
    }
    list(stepUnits = units, stepNum = num)
}


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


## HAS_TESTS
makePeriodLabelsYears <- function(dateVec, stepNum) {
    year.first <- format(dateVec[1L] - 1L, "%Y")
    year.last <- format(dateVec[length(dateVec)] - 1L, "%Y")
    from <- as.integer(year.first)
    to <- as.integer(year.last)
    dimvalues <- seq.int(from = from,
                         by = stepNum,
                         to = to)
    DimScale <- new("Intervals", dimvalues = dimvalues)
    labels(DimScale)
}



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

## HAS_TESTS
makePeriodLabelsMonths <- function(dateVec, stepNum) {
    year.first <- format(dateVec[1L], "%Y")
    year.last <- format(dateVec[length(dateVec)], "%Y")
    month.first <- format(dateVec[1L], "%m")
    month.last <- format(dateVec[length(dateVec)], "%m")
    year.from <- as.integer(year.first)
    year.to <- as.integer(year.last)
    month.from <- as.integer(month.first)
    month.to <- as.integer(month.last)
    from <- year.from + (month.from - 1L) / 12L
    to <- year.to + (month.to - 1L) / 12L
    by <- stepNum / 12L
    dimvalues <- seq.int(from = from,
                         by = by,
                         to = to)
    DimScale <- new("Intervals", dimvalues = dimvalues)
    labels(DimScale)
}






## HAS_TESTS
## assume that all inputs have been sanity checked
makeAgeLabels <- function(stepNum, stepUnits, nAgeInterval, lastOpen) {
    if (stepUnits == "years")
        by <- stepNum
    else if (stepUnits == "months")
        by <- stepNum / 12
    else
        stop(gettextf("invalid value for '%s' : \"%s\"",
                      "stepUnits", stepUnits))
    dimvalues <- seq(from = 0,
                     by = by,
                     length.out = nAgeInterval + 1L)
    if (lastOpen)
        dimvalues[length(dimvalues)] <- Inf
    DimScale <- methods::new("Intervals", dimvalues = dimvalues, isAge = TRUE)
    labels(DimScale)
}

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

## HAS_TESTS
monthStartNum <- function(monthStart) {
    if (!is.character(monthStart))
        stop(gettextf("'%s' does not have type \"%s\"",
                      "monthStart", "character"))
    if (!identical(length(monthStart), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "monthStart", 1L))
    if (is.na(monthStart))
        stop(gettextf("'%s' is missing",
                      "monthStart"))
    i <- pmatch(monthStart, month.name, nomatch = 0L)
    is.valid <- i > 0L
    if (!is.valid)
        stop(gettextf("invalid value for '%s' : \"%s\" is not a valid month",
                      "monthStart", monthStart))
    i
}


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


## HAS_TESTS (via ageToAgeGroup and yearToPeriod)
singleYearToMultiYear <- function(vec, breaks, labelStart, firstOpen,
                                  lastOpen, nameVec, isAge) {
    if (!is.numeric(vec) && !is.character(vec) && !is.factor(vec))
        stop(gettextf("'%s' has class \"%s\"",
                      nameVec, class(vec)))
    is.na.original <- is.na(vec)
    if (is.factor(vec))
        vec <- as.character(vec)
    x <- suppressWarnings(as.numeric(vec))
    is.new.na <- is.na(x) & !is.na.original
    if (any(is.new.na))
        stop(gettextf("value \"%s\" from '%s' cannot be coerced to numeric",
                      vec[is.new.na][[1L]], nameVec))
    if (identical(length(breaks), 0L))
        stop(gettextf("'%s' has length %d",
                      "breaks", 0L))
    if (!is.numeric(breaks))
        stop(gettextf("'%s' is non-numeric",
                      "breaks"))
    if (any(is.na(breaks)))
        stop(gettextf("'%s' has missing values",
                      "breaks"))
    if (any(duplicated(breaks)))
        stop(gettextf("'%s' has duplicates",
                      "breaks"))
    if (any(diff(breaks) < 0))
        stop(gettextf("'%s' is non-increasing",
                      "breaks"))
    for (name in c("labelStart", "firstOpen", "lastOpen")) {
        value <- get(name)
        if (!identical(length(value), 1L))
            stop(gettextf("'%s' does not have length %d",
                          name, 1L))
        if (!is.logical(value))
            stop(gettextf("'%s' has class \"%s\"",
                          name, class(value)))
        if (is.na(value))
            stop(gettextf("'%s' is missing",
                          name))
    }
    if (firstOpen) {
        breaks <- c(-Inf, breaks)
    }
    else {
        if (any(x[!is.na(x)] < breaks[1L]))
            stop(gettextf("'%s' has values less than the lowest value of '%s', but '%s' is %s",
                          nameVec, "breaks", "firstOpen", "FALSE"))
    }
    if (lastOpen) {
        breaks <- c(breaks, Inf)
    }
    else {
        if (any(x[!is.na(x)] >= breaks[length(breaks)]))
            stop(gettextf("'%s' has values greater than or equal to the highest value of '%s', but '%s' is %s",
                          nameVec, "breaks", "lastOpen", "FALSE"))
    }
    labels <- makeLabelsForIntervals(dimvalues = breaks,
                                     labelStart = labelStart,
                                     isAge = isAge)
    cut(x = x,
        breaks = breaks,
        labels = labels,
        right = FALSE)
}




## HAS TESTS
#' Convert labels for single-year time periods to labels for multi-year
#' time periods
#'
#' Convert a vector made of values such as \code{"2004", "2001", "2009"}
#' into a vector of values such as \code{"2000-2005", "2000-2005",
#' "2005-2010"}.  Period labels constructed using \code{yearToPeriod}
#' are  periods are formatted in the way expected by
#' functions such as \code{\link{Counts}} and
#' \code{\link{Values}}.
#'
#' If the \code{year} argument is a factor, \code{yearToPeriod}
#' will coerce it to a character vector before trying to
#' coerce it to numeric. See below for an example.
#'
#' @param year A vector of year labels. A numeric vector, or a vector
#' than can be coerced to numeric.
#' @param breaks Numeric. A vector of breaks, specifying
#' points dividing periods.
#' @param labelStart Logical. Whether the single-year periods
#' described by \code{year} are labelled according to the year
#' at the start of the period or the year at the end.  Defaults
#' to \code{TRUE}.
#' @param firstOpen Logical. Whether the first period created by
#' \code{yearToPeriod} should be "open", i.e. has no lower bound.
#' Defaults to \code{FALSE}.
#' @param lastOpen Logical. Whether the last period created by
#' \code{yearToPeriod} is "open",
#' i.e. has no upper bound. Defaults to \code{FALSE}.
#'
#' @return A factor, the same length as \code{year}.
#'
#' @seealso \code{\link{ageToAgeGroup}}
#'
#' @examples
#' year <- c(2001, 2023, 2000, 2005, 2014, 2013, 2029)
#' ## 5-year periods, 2000-2005, 2005-2010, ..., 2025-2030
#' yearToPeriod(year, breaks = seq(2000, 2030, 5))
#' ## 10-year periods, 2000-2005, 2005-2010, ..., 2025-2030
#' yearToPeriod(year, breaks = seq(2000, 2030, 10))
#' ## single-year periods specified by 'year' are labelled
#' ## according to time at start
#' year <- 2000:2009
#' breaks <- c(1995, 2000, 2005, 2010)
#' yearToPeriod(year = year, breaks = breaks)
#' ## single-year periods specified by 'year' are labelled
#' according to time at end
#' year <- 2000:2009
#' breaks <- c(1995, 2000, 2005, 2010)
#' yearToPeriod(year = year, breaks = breaks, labelStart = FALSE)
#' ## first period open 
#' yearToPeriod(year, breaks = seq(2010, 2030, 5), firstOpen = TRUE)
#' @export
yearToPeriod <- function(year, breaks, labelStart = TRUE,
                         firstOpen = FALSE, lastOpen = FALSE) {
    if (missing(breaks))
        stop(gettextf("argument \"%s\" is missing, with no default",
                      "breaks"))
    singleYearToMultiYear(vec = year,
                          breaks = breaks,
                          labelStart = labelStart,
                          firstOpen = firstOpen,
                          lastOpen = lastOpen,
                          nameVec = "year",
                          isAge = FALSE)
}

test_that("yearToPeriod works", {
    ans.obtained <- yearToPeriod(c(2000, 2049, 2033),
                                 breaks = seq(2000, 2050, 5))
    ans.expected <- factor(c("2000-2005", "2045-2050", "2030-2035"),
                           levels = paste(seq(2000, 2045, 5), seq(2005, 2050, 5), sep = "-"))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- yearToPeriod(c(2000, 2049, 2033),
                                 breaks = seq(1995, 2050, 5),
                                 labelStart = FALSE)
    ans.expected <- factor(c("1995-2000", "2045-2050", "2030-2035"),
                           levels = paste(seq(2000, 2045, 5), seq(2005, 2050, 5), sep = "-"))
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- yearToPeriod(c(1999, 2005, 2033, 2099),
                                 breaks = c(2000, 2010, 2020),
                                 firstOpen = TRUE, lastOpen = TRUE)
    ans.expected <- factor(c("<2000", "2000-2010", "2020+", "2020+"),
                           levels = c("<2000", "2000-2010", "2010-2020", "2020+"))
    expect_identical(ans.obtained, ans.expected)
})

test_that("yearToPeriod throws appropriate errors", {
    expect_error(yearToPeriod(list("a", "b", "c"), breaks = c(2000, 2005)),
                 "'year' has class \"list\"")
    expect_error(yearToPeriod(c("1", NA, "b"), breaks = c(2000, 2005)),
                 "value \"b\" from 'year' cannot be coerced to numeric")
    expect_error(yearToPeriod(c(0, 1, 10), breaks = c(5, 100), firstOpen = FALSE),
                 "'year' has values less than the lowest value of 'breaks', but 'firstOpen' is FALSE")
})
