
## FUNCTIONS TO OBTAIN CONSTANTS ###################################################

getDigitsRoundDimvaluesTimeUnit <- function() 3

getDefaultSexRatio <- function() 105

## HAS_TESTS
## Given dimtypes that have pairs (eg origin, parent),
## return the pairs (eg destination, child).
getDimtypesPairs <- function(dimtypes) {
  if (length(dimtypes) > 0L) {
    f <- function(dimtype)
      switch(dimtype,
             origin = "destination",
             destination = "origin",
             parent = "child",
             child = "parent",
             stop(gettextf("invalid value for \"%s\"", "dimtype")))
    sapply(dimtypes, f, USE.NAMES = FALSE)
  }
  else
    character()
}

## HAS_TESTS
getDimtypesWithPairs <- function(firstElementOnly = FALSE) {
  if (firstElementOnly)
    c("origin", "parent")
  else
    c("origin", "destination", "parent", "child")
}

## HAS_TESTS
## Given dimension names that have pairs (eg "reg_orig", "ethnicity_parent"),
## return the pairs (eg "reg_dest", "ethnicity_child").  If a dimension
## name does not have a pair (eg "age"), then the original name is returned.
getNamesPairs <- function(names) {
    current.dimtypes <- getDimtypesWithPairs()
    target.dimtypes <- getDimtypesPairs(current.dimtypes)
    current.suffixes <- getSuffixes(current.dimtypes)
    current.suffixes <- sprintf("%s$", current.suffixes)
    target.suffixes <- getSuffixes(target.dimtypes)
    matches <- lapply(current.suffixes, grepl, names)
    for (i in seq_along(matches)) {
        current.suffix <- current.suffixes[i]
        contains.current.suffix <- matches[[i]]
        target.suffix <- target.suffixes[i]
        names[contains.current.suffix] <- sub(current.suffix,
                                              target.suffix,
                                              names[contains.current.suffix])
    }
    names
}

## HAS_TESTS
getPossibleDimscales <- function(dimtype)
  switch(dimtype,
         age = c("Intervals", "Points"),
         child = "Categories",
         cohort = "Intervals",
         destination = "Categories",
         iteration = "Iterations",
         origin = "Categories",
         parent = "Categories",
         quantile = "Quantiles",
         sex = "Sexes",
         state = "Categories",
         time = c("Intervals", "Points"),
         triangle = "Triangles",
         stop(gettextf("\"%s\" is not a valid dimtypes", dimtype)))

## HAS_TESTS
getSuffixes <- function(dimtypes) {
  if (length(dimtypes) > 0L) {
    f <- function(dimtype)
      switch(dimtype,
             origin = "_orig",
             destination = "_dest",
             parent = "_parent",
             child = "_child",
             stop("invalid dimtype"))
    sapply(dimtypes, f, USE.NAMES = FALSE)
  }
  else
    character()
}

## HAS_TESTS
## Return pattern to be used for identifying names with that have pairs.
getSuffixPattern <- function(firstElementOnly = FALSE) {
  dimtypes.with.pairs <- getDimtypesWithPairs(firstElementOnly = firstElementOnly)
  all.suffixes <- getSuffixes(dimtypes.with.pairs)
  ans <- sprintf("%s$", all.suffixes)
  paste(ans, collapse = "|")
}

## HAS_TESTS
## dimtypes that where only one dimension is possible per object
getUniqueDimtypes <- function()
  c("age", "cohort", "iteration", "quantile", "time", "triangle")

## HAS_TESTS
## all valid dimtypes
getValidDimtypes <- function()
  c("age", "child", "cohort", "destination", "iteration", "origin",
    "parent", "quantile", "sex", "state", "time", "triangle")




## FUNCTIONS TO PREPARE DATA ########################################################

## HAS_TESTS
#' Clean up age group labels
#'
#' Reformat a vector of age-group labels, so that it matches the format
#' expected by functions such as \code{\link{Counts}} and
#' \code{\link{Values}}.
#'
#' \code{cleanAgeGroup} does the reformatting. \code{cleanAgeGroupConc}
#' constructs a \code{{Concordance}} between the old and
#' new age-group labels.
#'
#' \code{cleanAgeGroup} is suitable for interactive use, or for
#' one-off analyses, where the data will not change in future.
#'
#' Using \code{cleanAgeGroupConc} may be safer in production
#' code where the data may change. The workflow is as follows:
#' \enumerate{
#'  \item Use a vector of age labels to construct a concordance.
#'  \item Store the concordance.
#'  \item When new data arrives, use function
#' \code{\link{translate}}, plus the stored concordance,
#'   to clean the age data.
#' }
#' The advantange of this work flow is that \code{translate}
#' will throw an error if it strikes an age label that it
#' does not recognize.
#'
#' @param age A vector of age labels.
#'
#' @return A character vector of reformatted age labels.
#'
#' @seealso \code{\link{ageToAgeGroup}} converts exact
#' ages into age groups.
#'
#' @examples
#' x1 <- c("0 - 4 years", "90 plus years", "25 - 29 years")
#' x2 <- c("10 to 19 Yr", "80 years and over")
#' x3 <- c("5--9", "10plus", "0--4")
#' x4 <- c("10plus", "10plus", "5--9", "5--9")
#'
#' ## Use cleanAgeGroup directly
#' cleanAgeGroup(x1)
#' cleanAgeGroup(x2)
#'
#' ## Set up concordance and use that
#' conc <- cleanAgeGroupConc(x3)
#' translate(x4, concordance = conc)
#' 
#' @name cleanAgeGroup
NULL

## HAS_TESTS
#' @rdname cleanAgeGroup
#' @export
cleanAgeGroup <- function(age) {
    age <- tolower(age)
    age <- gsub("(?<![0-9])0+(?=[0-9])", "", age, perl = TRUE)
    age <- sub("less than 1 year", "0", age)
    age <- sub("under1|under 1", "0", age)
    age <- sub("year|years|yr|yrs", "", age)
    age <- sub("and over|plus", "+", age)
    age <- sub("to|-+|_+", "-", age)
    age <- gsub(" ", "", age)
    age
}

## HAS_TESTS
#' @rdname cleanAgeGroup
#' @export
cleanAgeGroupConc <- function(age) {
    age.new <- cleanAgeGroup(age)
    ans <- data.frame(old = age, new = age.new)
    ans <- unique(ans)
    Concordance(ans)
}

#' dateOfBirthToAgeCompleted - from date to years - (dateOfBirth, dateObs, completed)
#' dateOfBirthToAgeExact - from date to years - (dateOfBirth, dateObs, completed)
#' ageCompletedToAgeGroup - from years to multiyear age group - (age, breaks)
#' ageExactToMonth - from exact years to months - (age, min, max)
#' ageExactToQuarter - from exact years to quarters - (age, min, max)
#' dateToYear - from date to years - (date, month, day, labelStart) 
#' yearToPeriod - from years to multiyear age group - (year, breaks, labelStart)
#' dateToMonth - from date to month - (date)
#' dateToQuarter - from date to quarters - (date)
#' dateOfBirthToTriangle - (dateOfBirth, dateObs, month, day, width)
#' dateOfBirthToTriangleMonth - (dateOfBirth, dateObs)
#' dateOfBirthToTriangleQuarter - (dateOfBirth, dateObs)


## HAS_TESTS
checkAndTidyDateAndDOB <- function(date, dob) {
    for (name in c("date", "dob")) {
        value <- get(name)
        if (!methods::is(value, "Date"))
            stop(gettextf("'%s' does not have class \"%s\"",
                          name, "Date"))
        if (identical(length(value), 0L))
            stop(gettextf("'%s' has length %d",
                          name, 0L))
        if (identical(length(value[!is.na(value)]), 0L))
            stop(gettextf("all elements of '%s' are missing",
                          name))
    }
    n.date <- length(date)
    n.dob <- length(dob)
    if (n.date > n.dob) {
        if ((n.date %% n.dob) != 0L)
            warning(gettextf("length of '%s' [%d] not a multiple of length of '%s' [%d]",
                             "date", n.date, "dob", n.dob))
        dob <- rep(dob,
                   length.out = n.date)
    }
    if (n.dob > n.date) {
        if ((n.dob %% n.date) != 0L)
            warning(gettextf("length of '%s' [%d] not a multiple of length of '%s' [%d]",
                             "dob", n.dob, "date", n.date))
        date <- rep(date,
                    length.out = n.dob)
    }
    both.obs <- !is.na(date) & !is.na(dob)
    if (any(date[both.obs] < dob[both.obs]))
        stop(gettextf("some elements of '%s' are less than the corresponding elements of '%s'",
                      "date", "dob"))
    list(date = date, dob = dob)
}

## HAS_TESTS
checkDate <- function(date) {
    if (!methods::is(date, "Date"))
        stop(gettextf("'%s' does not have class \"%s\"",
                      "date", "Date"))
    if (identical(length(date), 0L))
        stop(gettextf("'%s' has length %d",
                      "date", 0L))
    if (identical(length(date[!is.na(date)]), 0L))
        stop(gettextf("all elements of '%s' are missing",
                      "date"))
    NULL
}



## NO_TESTS
checkLabelStart <- function(labelStart) {
    if (!is.logical(labelStart))
        stop(gettextf("'%s' does not have type \"%s\"",
                      "labelStart", "logical"))
    if (!identical(length(labelStart), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "labelStart", 1L))
    if (is.na(labelStart))
        stop(gettextf("'%s' is missing",
                      "labelStart"))
    NULL
}

checkAndTidyBreaksDates <- function(breaks) {
    if (!methods::is(breaks, "Date"))
        stop(gettextf("'%s' does not have class \"%s\"",
                      "breaks", "Date"))
    if (any(is.na(breaks)))
        stop(gettextf("'%s' has missing values",
                      "breaks"))
    if (any(diff(breaks) <= 0L))
        stop(gettextf("'%s' not strictly increasing",
                      "breaks"))
    NULL
}
        


dateOfBirthToAgeInner <- function(dateOfBirth, dateObs, completed = TRUE) {
    l <- checkAndTidyDateOfBirthAndDateObs(dateOfBirth = dateOfBirth,
                                           dateObs = dateObs)
    date.bth <- l$dateOfBirth
    date.obs <- l$dateObs
    if (!is.logical(completed))
        stop(gettextf("'%s' does not have type \"%s\"",
                      "completed", "logical"))
    if (!identical(length(completed), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "completed", 1L))
    if (is.na(completed))
        stop(gettextf("'%s' is missing",
                      "completed"))
    year.obs <- as.integer(format(date.obs, "%Y"))
    year.bth <- as.integer(format(date.bth, "%Y"))
    month.obs <- as.integer(format(date.obs, "%m"))
    month.bth <- as.integer(format(date.bth, "%m"))
    day.obs <- as.integer(format(date.obs, "%d"))
    day.bth <- as.integer(format(date.bth, "%d"))
    is.29.feb.obs <- (month.obs == 2L) & (day.obs == 29L)
    is.29.feb.bth <- (month.bth == 2L) & (day.bth == 29L)
    day.obs.no.leap <- day.obs
    day.bth.no.leap <- day.bth
    day.obs.no.leap[is.29.feb.obs] <- 28L
    day.bth.no.leap[is.29.feb.bth] <- 28L
    diff.year <- year.obs - year.bth
    diff.month <- month.obs - month.bth
    diff.day <- day.obs.no.leap - day.bth.no.leap
    had.birthday.this.year <- (diff.month > 0L) | ((diff.month == 0L) & (diff.day >= 0L))
    completed.years <- diff.year + had.birthday.this.year - 1L
    if (completed) {
        as.character(completed.years)
    }
    else {
        date.birthday.last.year <- as.Date("%d-%d-%d", year.obs - 1L, month.birth, day.bth)
        date.birthday.this.year <- as.Date("%d-%d-%d", year.obs, month.birth, day.bth)
        date.birthday.next.year <- as.Date("%d-%d-%d", year.obs + 1L, month.birth, day.bth)
        date.birthday.last.year <- as.integer(date.birthday.last.year)
        date.birthday.this.year <- as.integer(date.birthday.this.year)
        date.birthday.next.year <- as.integer(date.birthday.next.year)
        days.since.last.birthday <- ifelse(had.birthday.this.year,
                                           as.integer(date.obs) - date.birthday.this.year,
                                           as.integer(date.obs) - date.birthday.last.year)
        days.between.birthdays <- ifelse(had.birthday.this.year,
                                         date.birthday.next.year - date.birthday.this.year,
                                         date.birthday.this.year - date.birthday.last.year)
        completed.years + days.since.last.birthday / days.between.birthdays
    }
}

dateOfBirthtoAgeCompleted <- function(dateOfBirth, dateObs) {
    dateOfBirthToAgeInner(dateOfBirth = dateOfBirth,
                          dateObs = dateObs,
                          completed = TRUE)
}

dateOfBirthtoAgeExact <- function(dateOfBirth, dateObs) {
    dateOfBirthToAgeInner(dateOfBirth = dateOfBirth,
                          dateObs = dateObs,
                          completed = FALSE)
}


ageCompletedToAgeGroup <- function(age, breaks) {
    is.already.na <- is.na(age)
    age.num <- as.numeric(age)
    is.new.na <- is.na(age.num) & !is.already.na
    if (any(is.new.na)) {
        num.na.first <- age[is.new.na][1L]
        stop(gettextf("value \"%s\" from '%s' cannot be interpreted as a number",
                      num.na.first, "age"))
    }
    age.int <- as.integer(age.num)
    age.not.int <- age.int[!is.already.na] != age.num[!is.already.na]
    if (any(age.not.int)) {
        age.not.int.first <- age[!is.already.na][age.not.int][1L]
        stop(gettextf("value \"%s\" from '%s' cannot be interpreted as an integer",
                      age.not.int.first, "age"))
    }
    if (!is.numeric(breaks))
        stop(gettextf("'%s' is non-numeric"))
    if (any(is.na(breaks)))
        stop(gettextf("'%s' has missing values",
                      "breaks"))
    if (any(duplicated(breaks)))
        stop(gettextf("'%s' has duplicates",
                      "breaks"))
    breaks.int <- as.integer(breaks)
    breaks.not.int <- breaks.int != breaks
    if (any(breaks.not.int)) {
        breaks.not.int.first <- breaks[breaks.not.int][1L]
        stop(gettextf("value \"%s\" from '%s' is not an integer",
                      breaks.not.int.first, "breaks"))
    }
    if (any(diff(breaks.int) < 0L))
        stop(gettextf("'%s' non-increasing",
                      "breaks"))
    is.less.than.min <- age < min(breaks.int)
    if (any(is.less.than.min))
        stop(gettextf("'%s' has value [%s] that is less than the minimum value for breaks [%s]",
                      "age", age[is.less.than.min], breaks[1L]))
    is.greater.than.max <- age > max(breaks.int)
    if (any(is.greater.than.max))
        stop(gettextf("'%s' has value [%s] that is greater than the maximum value for breaks [%s]",
                      "age", age[is.greater.than.max], breaks[length(breaks)]))
    labels <- makeLabelsForIntervals(dimvalues = breaks.int,
                                     labelStart = FALSE,
                                     isAge = TRUE)
    i <- findInterval(x = age,
                      vec = breaks)
    factor(labels[i], levels = labels)
}

ageExactToMonth <- function(age, min = NULL, max = NULL) {
    if (!is.numeric(age))
        stop(gettextf("'%s' is non-numeric",
                      "age"))
    n.age <- length(age)
    for (name in c("min", "max")) {
        value <- get(name)
        if (!is.null(value)) {
            if (!is.character(value))
                stop(gettextf("'%s' has class \"%s\"",
                              class(value)))
            if (!identical(length(value), 1L))
                stop(gettextf("'%s' does not have length %d",
                              name))
            if (is.na(value))
                stop(gettext("'%s' is missing",
                             name))
        }
    }
    if (all(is.na(age)))
        return(rep(NA_character_, times = n.age))
    pattern <- "^(<?)([0-9]+)y([0-9]+)m(\\+?)$"
    if (is.null(min)) {
        break.min.frac <- min(age, na.rm = TRUE)
        break.min.completed.months <- as.integer(12 * break.min.frac)
        break.min <- break.min.completed.months / 12
        open.left <- FALSE
    }
    else {
        year.min <- sub(pattern, "\\2", min, perl = TRUE)
        month.min <- sub(pattern, "\\3", min, perl = TRUE)
        year.min <- as.numeric(year.min)
        month.min <- as.numeric(month.min)
        if (is.na(year.min) || is.na(month.min))
            stop(gettextf("cannot interpret value for '%s' [%s] as year-month age label",
                          "min", min))
        break.min <- year.min + month.min / 12
        min.open.left <- identical(sub(pattern, "\\1", min, perl = TRUE), "<")
    }
    if (is.null(max)) {
        break.max.frac <- max(age, na.rm = TRUE)
        break.max.completed.months <- as.integer(12 * break.max.frac)
        break.max <- (break.max.completed.months + 1) / 12
        open.right <- FALSE
    }
    else {
        year.max <- sub(pattern, "\\2", max, perl = TRUE)
        month.max <- sub(pattern, "\\3", max, perl = TRUE)
        year.max <- as.numeric(year.max)
        month.max <- as.numeric(month.max)
        if (is.na(year.max) || is.na(month.max))
            stop(gettextf("cannot interpret value for '%s' [%s] as year-month age label",
                          "max", max))
        break.max <- year.max + (month.max + 1) / 12
        open.right <- identical(sub(pattern, "\\4", min, perl = TRUE), "+")
    }
    breaks <- seq(from = break.min,
                  to = break.max,
                  by = 1 / 12)
    if (open.left)
        breaks <- c(-Inf, breaks)
    if (open.right)
        breaks <- c(breaks, Inf)
    labels <- makeLabelsForIntervals(dimvalues = breaks,
                                     labelStart = FALSE,
                                     isAge = TRUE)
    i <- findInterval(x = age,
                      vec = breaks)
    factor(labels[i], levels = labels)
}

ageExactToQuarter <- function(age, min = NULL, max = NULL) {
    stop("not written yet")    
}


checkAndTidyDate <- function(date) {
    if (methods::is(date, "Date")) {
        date
    }
    else if (is.character(date)) {
        is.already.na <- is.na(date)
        date.new <- suppressWarnings(as.Date(date))
        is.na.new <- is.na(date.new) & !is.already.na
        if (any(is.na.new))
            stop(gettextf("value \"%s\" from '%s' cannot be interpreted as a date",
                          date.new[is.na.new][1L], "date"))
        date.new
    }
    else
        stop(gettextf("'%s' has class \"%s\"",
                      "date", class(date)))
}


dateToYear <- function(date, month = 1, day = 1, labelStart = TRUE) {
    date <- checkAndTidyDate(date)
    for (name in c("month", "day")) {
        value <- get(name)
        if (!is.numeric(value))
            stop(gettextf("'%s' is non-numeric",
                          name))
        if (!identical(length(value), 1L))
            stop(gettextf("'%s' does not have length %d",
                          name, 1L))
        if (is.na(value))
            stop(gettextf("'%s' is missing",
                          name))
        if (!isTRUE(all.equal(as.integer(value), value)))
            stop(gettextf("'%s' is not an integer",
                          name))
    }
    is.valid.date <- !is.na(suppressWarnings(as.Date(paste(2000, month, day, sep = "-"))))
    if (!is.valid.date)
        stop(gettextf("'%s' [%s] and '%s' [%s] do not form a valid date",
                      "month", month, "day", day))
    if ((month == 2L) & (day == 29L))
        day <- 28L
    checkLabelStart(labelStart)
    year.date <- as.integer(format(date, "%Y"))
    month.date <- as.integer(format(date, "%m"))
    day.date <- as.integer(format(date, "%d"))
    is.29.feb <- (month.date == 2L) & (day.date == 29L)
    day.date[is.29.feb] <- 28L
    reached.month.day <- (month.date > month) | ((month.date == month) & (day.date >= day))
    as.character(year.date + reached.month.day - labelStart)
}


yearToPeriod <- function(year, breaks, labelStart = TRUE) {
    is.already.na <- is.na(year)
    year.num <- as.numeric(year)
    is.new.na <- is.na(year.num) & !is.already.na
    if (any(is.new.na)) {
        num.na.first <- year[is.new.na][1L]
        stop(gettextf("value \"%s\" from '%s' cannot be interpreted as a number",
                      num.na.first, "year"))
    }
    year.int <- as.integer(year.num)
    year.not.int <- year.int[!is.already.na] != year.num[!is.already.na]
    if (any(year.not.int)) {
        year.not.int.first <- year[!is.already.na][year.not.int][1L]
        stop(gettextf("value \"%s\" from '%s' cannot be interpreted as an integer",
                      year.not.int.first, "year"))
    }
    if (!is.numeric(breaks))
        stop(gettextf("'%s' is non-numeric"))
    if (any(is.na(breaks)))
        stop(gettextf("'%s' has missing values",
                      "breaks"))
    if (any(duplicated(breaks)))
        stop(gettextf("'%s' has duplicates",
                      "breaks"))
    breaks.int <- as.integer(breaks)
    breaks.not.int <- breaks.int != breaks
    if (any(breaks.not.int)) {
        breaks.not.int.first <- breaks[breaks.not.int][1L]
        stop(gettextf("value \"%s\" from '%s' is not an integer",
                      breaks.not.int.first, "breaks"))
    }
    if (any(diff(breaks.int) < 0L))
        stop(gettextf("'%s' non-increasing",
                      "breaks"))
    is.less.than.min <- year < min(breaks.int)
    if (any(is.less.than.min))
        stop(gettextf("'%s' has value [%s] that is less than the minimum value for breaks [%s]",
                      "year", year[is.less.than.min], breaks[1L]))
    is.greater.than.max <- year > max(breaks.int)
    if (any(is.greater.than.max))
        stop(gettextf("'%s' has value [%s] that is greater than the maximum value for breaks [%s]",
                      "year", year[is.greater.than.max], breaks[length(breaks)]))
    checkLabelStart(labelStart)
    labels <- makeLabelsForIntervals(dimvalues = breaks.int,
                                     labelStart = labelStart,
                                     isAge = FALSE)
    i <- findInterval(x = year,
                      vec = breaks)
    factor(labels[i], levels = labels)
}



dateToQuarter <- function(date) {
    date <- checkAndTidyDate(date = date)
    year <- format(date, "%Y")
    month <- as.integer(format(date, "%m"))
    quarter <- ((month - 1L) %/% 3L) + 1L
    sprintf("%s-Q%d", year, quarter)
}

dateToMonth <- function(date) {
    date <- checkAndTidyDate(date = date)
    year <- format(date, "%Y")
    month <- as.integer(format(date, "%m"))
    sprintf("%s-%02.0f", year, month)
}

## NOT WRITTEN YET
dateOfBirthToTriangle <- function(dateOfBirth, dateObs, month = 1, day = 1, width = 1) {
    l <- checkAndTidyDateOfBirthAndDateObs(dateOfBirth = dateOfBirth,
                                           dateObs = dateObs)
    date.bth <- l$dateOfBirth
    date.obs <- l$dateObs
    for (name in c("month", "day", "width")) {
        value <- get(name)
        if (!is.numeric(value))
            stop(gettextf("'%s' is non-numeric",
                          name))
        if (!identical(length(value), 1L))
            stop(gettextf("'%s' does not have length %d",
                          name, 1L))
        if (is.na(value))
            stop(gettextf("'%s' is missing",
                          name))
        if (!isTRUE(all.equal(as.integer(value), value)))
            stop(gettextf("'%s' is not an integer",
                          name))
        if (value < 1L)
            stop(gettextf("'%s' is non-positive",
                          name))
    }
    is.valid.date <- !is.na(suppressWarnings(as.Date(paste(2000, month, day, sep = "-"))))
    if (!is.valid.date)
        stop(gettextf("'%s' [%s] and '%s' [%s] do not form a valid date",
                      "month", month, "day", day))
    if ((month == 2L) & (day == 29L))
        day <- 28L

    year.obs <- as.integer(format(date.obs, "%Y"))
    year.bth <- as.integer(format(date.bth, "%Y"))
    month.obs <- as.integer(format(date.obs, "%m"))
    month.bth <- as.integer(format(date.bth, "%m"))
    day.obs <- as.integer(format(date.obs, "%d"))
    day.bth <- as.integer(format(date.bth, "%d"))
    is.29.feb.obs <- (month.obs == 2L) & (day.obs == 29L)
    is.29.feb.bth <- (month.bth == 2L) & (day.bth == 29L)
    day.obs.no.leap <- day.obs
    day.bth.no.leap <- day.bth
    day.obs.no.leap[is.29.feb.obs] <- 28L
    day.bth.no.leap[is.29.feb.bth] <- 28L

    year.min <- min(year.bth) - 1L
    year.max <- max(year.obs) + 1L
    breaks <- seq(from = as.Date(sprintf("%d-%d-%d", year.min, month, day)),
                  to = as.Date(sprintf("%d-%d-%d", year.max, month, day)),
                  by = paste(width, "year"))
    ## REWRITTEN TO HERE
    diff.year <- year.obs - year.bth
    diff.month <- month.obs - month.bth
    diff.day <- day.obs.no.leap - day.bth.no.leap
    had.birthday.this.year <- (diff.month > 0L) | ((diff.month == 0L) & (diff.day >= 0L))
    completed.years <- diff.year + had.birthday.this.year - 1L
    if (completed) {
        ans <- completed.years
    }
    else {
        date.birthday.last.year <- as.Date("%d-%d-%d", year.obs - 1L, month.birth, day.bth)
        date.birthday.this.year <- as.Date("%d-%d-%d", year.obs, month.birth, day.bth)
        date.birthday.next.year <- as.Date("%d-%d-%d", year.obs + 1L, month.birth, day.bth)
        date.birthday.last.year <- as.integer(date.birthday.last.year)
        date.birthday.this.year <- as.integer(date.birthday.this.year)
        date.birthday.next.year <- as.integer(date.birthday.next.year)
        days.since.last.birthday <- ifelse(had.birthday.this.year,
                                           as.integer(date.obs) - date.birthday.this.year,
                                           as.integer(date.obs) - date.birthday.last.year)
        days.between.birthdays <- ifelse(had.birthday.this.year,
                                         date.birthday.next.year - date.birthday.this.year,
                                         date.birthday.this.year - date.birthday.last.year)
        ans <- completed.years + days.since.last.birthday / days.between.birthdays
    }
    as.character(ans)


    year.date <- as.integer(format(date, "%Y"))
    month.date <- as.integer(format(date, "%m"))
    day.date <- as.integer(format(date, "%d"))
    is.29.feb <- (month.date == 2L) & (day.date == 29L)
    day.date[is.29.feb] <- 28L
    reached.month.day <- (month.date > month) | ((month.date == month) & (day.date >= day))

}

dateOfBirthToTriangleMonths <- function(dateOfBirth, dateObs) {
    stop("not written yet")
}

dateOfBirthToTriangleQuarters <- function(dateOfBirth, dateObs) {
    stop("not written yet")
}



## FUNCTIONS FOR PROCESSING DIMENSIONS NAME AND INDICES #############################

## HAS_TESTS
#' @rdname exported-not-api
#' @export
checkAndTidyAlong <- function(along, metadata, numericDimScales,
                              checkNumericDimscales = TRUE) {
    names <- names(metadata)
    dimtypes <- dimtypes(metadata, use.names = FALSE)
    DimScales <- DimScales(metadata, use.names = FALSE)
    s <- seq_along(names)
    if (is.null(along)) {
        if (length(s) == 1L)
            return(1L)
        for (dimtype in c("time", "age", "cohort")) {
            i.dimtype <- match(dimtype, dimtypes, nomatch = 0L)
            has.dimtype <- i.dimtype > 0
            if (has.dimtype)
                return(i.dimtype)
        }
        stop(gettextf("no '%s' argument supplied but no dimension with dimtype \"%s\", \"%s\", or \"%s\"",
                      "along", "time", "age", "cohort"))
    }
    else {
        if (!identical(length(along), 1L))
            stop(gettextf("'%s' does not have length %s", "along", 1L))
        if (is.na(along))
            stop(gettextf("'%s' is missing", "along"))
        if (!is.numeric(along))
            along <- match(along, names, nomatch = 0L)
        if (!(along %in% s))
            stop(gettextf("'%s' outside valid range", "along"))
        along <- as.integer(along)
        name.along <- names[along]
        dimtype.along <- dimtypes[along]
        if (identical(dimtype.along, "iteration"))
            stop(gettextf("'along' dimension [\"%s\"] has dimtype \"%s\"",
                          name.along, "iteration"))
        if (numericDimScales && checkNumericDimscales) {
            DimScale.along <- DimScales[[along]]
            if (!(methods::is(DimScale.along, "Points") || methods::is(DimScale.along, "Intervals")))
                stop(gettextf("'along' dimension [\"%s\"] has dimscale \"%s\"",
                              name.along, class(DimScale.along)))
        }
        along
    }
}

## HAS_TESTS
## used by collapseCategories and expandCategories
checkAndTidyDimColExtCat <- function(dimension, names, DimScales) {
    if (is.null(dimension))
        which(sapply(DimScales, methods::is,"Categories"))
    else {
        n.dim <- length(names)
        ans <- tidySubscript(subscript = dimension,
                             nDim = n.dim,
                             names = names)
        for (i in ans) {
            DimScale <- DimScales[[i]]
            if (!methods::is(DimScale, "Categories"))
                stop(gettextf("dimension \"%s\" has dimscale \"%s\"",
                              names[i], class(DimScale)))
        }
        ans
    }
}

## HAS_TESTS
## used by collapseCategories and expandCategories
checkAndTidyOldNew <- function(object, name, lengthOne) {
    object <- as.character(object)
    if (any(is.na(object)))
        stop(gettextf("'%s' has missing values",
                      name))
    if (!all(nzchar(object)))
        stop(gettextf("'%s' has blanks",
                      name))
    if (lengthOne) {
        if (!identical(length(object), 1L))
            stop(gettextf("'%s' does not have length %d",
                          name, 1L))
    }
    else {
        if (identical(length(object), 0L))
            stop(gettextf("'%s' has length %d",
                          name, 0L))
        if (any(duplicated(object)))
            stop(gettextf("'%s' has duplicates",
                          name))
    }
    object
}

## HAS_TESTS
## Look through 'namesSupplied' and find any elements that
## refer to pairs of names in 'namesAll' - eg 'namesSupplied'
## contains 'reg' when 'namesAll' contains 'reg_orig' and 'reg_dest'.
## If any such elements are found, replace them with the pairs.
## The function assumes that 'namesAll' come from a valid object, ie that
## origin/destination or parent/child dimensions come in pairs.
expandNamesSupplied <- function(namesSupplied, namesAll) {
  if (length(namesSupplied) > 0L) {
      suffix.pattern <- getSuffixPattern(firstElementOnly = TRUE)
      names.first.elements <- grep(suffix.pattern, namesAll, value = TRUE)
      names.second.elements <- getNamesPairs(names.first.elements)
      pairs <- mapply(c, names.first.elements, names.second.elements,
                      SIMPLIFY = FALSE)
      names.no.suffix <- removeSuffixes(names.first.elements)
      i <- match(namesSupplied, names.no.suffix, nomatch = 0L)
      ans <- as.list(namesSupplied)
      ans[i > 0L] <- pairs[i]
      unlist(ans)
  }
  else
    character()
}

## HAS_TESTS
#' @rdname exported-not-api
#' @export
iFemale <- function(DimScale) {
    kFemale <- c("f", "female", "females")
    if (!is(DimScale, "Sexes"))
        stop(gettextf("'%s' has class \"%s\"",
                      "DimScale", class(DimScale)))
    dimvalues <- DimScale@dimvalues
    dimvalues <- tolower(dimvalues)
    i <- match(kFemale, dimvalues, nomatch = 0L)
    max(i)
}

## HAS_TESTS
#' @rdname exported-not-api
#' @export
iMale <- function(DimScale) {
    kMale <- c("m", "male", "males")
    if (!is(DimScale, "Sexes"))
        stop(gettextf("'%s' has class \"%s\"",
                      "DimScale", class(DimScale)))
    dimvalues <- DimScale@dimvalues
    dimvalues <- tolower(dimvalues)
    i <- match(kMale, dimvalues, nomatch = 0L)
    max(i)
}

## HAS_TESTS
invertSubscript <- function(subscript, nDim) {
    if (!is.integer(subscript))
        stop(gettextf("'%s' does not have type \"%s\"",
                      "subscript", "integer"))
    s <- seq_len(nDim)
    if (!all(subscript %in% s))
        stop(gettextf("'%s' outside valid range",
                      "subscript"))
    setdiff(s, subscript)
}

## HAS_TESTS
makeDimensionSubscript <- function(dimension = NULL, margin = NULL, nDim, names) {
    has.dimension <- !is.null(dimension)
    has.margin <- !is.null(margin)
    if (has.dimension) {
        if (has.margin)
            stop(gettextf("has '%s' and '%s' arguments", "dimension", "margin"))
        else
            tidySubscript(subscript = dimension, nDim = nDim, names = names)
    }
    else {
        if (has.margin) {
            margin <- tidySubscript(subscript = margin, nDim = nDim, names = names)
            invertSubscript(subscript = margin, nDim = nDim)
        }
        else
            stop(gettextf("no '%s' or '%s' arguments", "dimension", "margin"))
    }
}

## HAS_TESTS
## Given a vector of labels, return them in numeric order if they all
## start with a number or '<' then a number; otherwise return them
## in the original order.
orderLabelsNumerically <- function(labels) {
    numbers <- sub("(<? ?)(\\-?\\d+\\.?\\d*)(.*)", "\\2", labels, perl = TRUE)
    numbers <- tryCatch(as.numeric(numbers),
                        warning = function(w) w)
    ## use 'less.than' to deal with ties caused by labels that
    ## cut real line into two, eg "0+", "<0"
    less.than <- 1L - grepl("^<", labels)
    if (is.numeric(numbers))
        labels[order(numbers, less.than)]
    else
        labels
}

## HAS_TESTS
## Permute rows within dimensions of 'a' that have dimtype "Intervals",
## so that these dimensions are consistent with 'metadata'
permuteToMatchIntervalOrPointMetadata <- function(object, metadata) {
    dim.obj <- unname(dim(object))
    dim.meta <- dim(metadata)
    if (!identical(dim.obj, dim.meta))
        stop(gettextf("dimensions of '%s' and '%s' inconsistent",
                      "metadata", "object"))
    dn.obj <- dimnames(object)
    dn.meta <- dimnames(metadata)
    indices <- lapply(dim.obj, seq_len)
    changed <- FALSE
    i.intervals <- grep("Intervals", dimscales(metadata))
    i.points <- grep("Points", dimscales(metadata))
    i.interval.or.point <- c(i.intervals, i.points)
    for (i in i.interval.or.point) {
        unordered <- dn.obj[[i]]
        ordered <- orderLabelsNumerically(unordered)
        if (!identical(unordered, ordered)) {
            changed <- TRUE
            indices[[i]] <- match(unordered, ordered)
        }
    }
    if (changed) {
        dims <- seq_along(dim.obj)
        transform <- methods::new("CollapseTransform",
                         dims = dims,
                         indices = indices,
                         dimBefore = dim.obj,
                         dimAfter = dim.obj)
        object <- collapse(object, transform = transform)
        if (is.null(dimnames(object))) {
            for (i in i.interval.or.point)
                dn.obj[[i]] <- dn.obj[[i]][indices[[i]]]
            dimnames(object) <- dn.obj
        }
    }
    object
}

## HAS_TESTS
removeSuffixes <- function(names) {
  suffix.pattern <- getSuffixPattern()
  sub(suffix.pattern, "", names)
}

## HAS_TESTS
tidySubscript <- function(subscript, nDim, names = NULL) {
    if (any(is.na(subscript)))
        stop(gettextf("'%s' has missing values", "subscript"))
    if (any(duplicated(subscript)))
        stop(gettextf("'%s' contains duplicates", "subscript"))
    if (is.character(subscript)) {
        if (is.null(names))
            stop("'X' must have named dimnames")
        subscript <- expandNamesSupplied(namesSupplied = subscript,
                                         namesAll = names)
        ans <- match(subscript, names, nomatch = 0L)
        outside <- ans == 0L
        n.outside <- sum(outside)
        if (n.outside > 0L)
            stop(sprintf(ngettext(n.outside,
                                  "subscript %s outside valid range",
                                  "subscripts %s outside valid range"),
                         paste(dQuote(subscript[outside]), collapse = ", "))) ## dQuote
    }
    else {
        ans <- suppressWarnings(as.integer(subscript))
        outside <- is.na(ans) | !(ans %in% seq_len(nDim))
        n.outside <- sum(outside)
        if (n.outside > 0L)
            stop(sprintf(ngettext(n.outside,
                                  "subscript %s outside valid range",
                                  "subscripts %s outside valid range"),
                         paste(sQuote(subscript[outside]), collapse = ", "))) ## sQuote
    }
    ans
}

## TRANSFORMS ########################################################################

## Functions for making CollapseTransformExtra

## HAS_TESTS
makeInvIndices <- function(indices) {
    ans <- vector(mode = "list", length = length(indices))
    for (i in seq_along(indices)) {
        index <- indices[[i]]
        margin.after <- seq_along(unique(index[index > 0L]))
        ans[[i]] <- lapply(margin.after, function(i) which(i == index))
    }
    ans
}


## HAS_TESTS
#' @rdname exported-not-api
#' @export
makeCollapseTransformExtra <- function(transform) {
    if (!methods::is(transform, "CollapseTransform"))
        stop(gettextf("'%s' has class \"%s\"",
                      "transform", class(transform)))
    methods::validObject(transform)
    indices <- transform@indices
    dims <- transform@dims
    dim.before <- transform@dimBefore
    dim.after <- transform@dimAfter
    multiplier.before <- c(1L, cumprod(dim.before[-length(dim.before)]))
    multiplier.before <- as.integer(multiplier.before)
    multiplier.after <- c(1L, cumprod(dim.after[-length(dim.after)]))
    multiplier.after <- as.integer(multiplier.after)
    inv.indices <- makeInvIndices(indices)
    methods::new("CollapseTransformExtra",
        transform,
        multiplierBefore = multiplier.before,
        multiplierAfter = multiplier.after,
        invIndices = inv.indices)
}


## Helper functions used by 'getIAfter', 'getIBefore' and 'getIShared'

## A cell within an array can be identified in two ways:
## (i) 'pos', a number between 1 and length(array)
## (ii) 'mar', a vector giving the cell's position along each
##      of the array's dimensions.

## All of the arguments to the helper functions other than
## 'mar' and 'pos' are obtained from an object of class
## "CollapseTransformExtra".

## TRANSLATED
## HAS_TESTS
posToMar <- function(pos, dim, useC = FALSE) {
    ## pos
    stopifnot(is.integer(pos))
    stopifnot(identical(length(pos), 1L))
    stopifnot(!is.na(pos))
    stopifnot(pos >= 1L)
    ## dim
    stopifnot(is.integer(dim))
    stopifnot(length(dim) >= 1L)
    stopifnot(!any(is.na(dim)))
    stopifnot(all(dim >= 0L))
    ## pos and dim
    stopifnot(pos <= prod(dim))
    if (useC) {
        .Call(posToMar_R, pos, dim)
    }
    else {
        ans <- integer(length = length(dim))
        pos <- pos - 1L
        div.by <- 1L
        for (d in seq_along(dim)) {
            pos <- pos %/% div.by
            mod.by <- dim[d]
            ans[d] <- pos %% mod.by
            div.by <- mod.by
        }
        ans + 1L
    }
}

## TRANSLATED
## HAS_TESTS
marToPos <- function(mar, multiplier, useC = FALSE) {
    ## mar
    stopifnot(is.integer(mar))
    stopifnot(length(mar) > 0L)
    stopifnot(!any(is.na(mar)))
    stopifnot(all(mar >= 1L))
    ## multiplier
    stopifnot(is.integer(multiplier))
    stopifnot(length(multiplier) >= 1L)
    stopifnot(!any(is.na(multiplier)))
    stopifnot(all(multiplier >= 0L))
    ## mar and multiplier
    stopifnot(identical(length(mar), length(multiplier)))
    if (useC) {
        .Call(marToPos_R, mar, multiplier)
    }
    else {
        ans <- 1L
        for (i in seq_along(multiplier))
            ans <- ans + as.integer(multiplier[i] * (mar[i] - 1L))
        ans
    }
}

## TRANSLATED
## HAS_TESTS
marBeforeToMarAfter <- function(mar, indices, dims, dimAfter, useC = FALSE) {
    ## mar
    stopifnot(is.integer(mar))
    stopifnot(!any(is.na(mar)))
    stopifnot(all(mar >= 1L))
    stopifnot(identical(length(mar), length(indices)))
    ## indices
    stopifnot(is.list(indices))
    stopifnot(all(sapply(indices, is.integer)))
    stopifnot(all(sapply(indices, function(x) !any(is.na(x)))))
    stopifnot(all(sapply(indices, function(x) all(x >= 0L))))
    ## dims
    stopifnot(is.integer(dims))
    stopifnot(!any(is.na(dims)))
    stopifnot(all(dims >= 0L))
    ## mar and indices
    stopifnot(identical(length(mar), length(indices)))
    ## mar and dims
    stopifnot(identical(length(mar), length(dims)))
    if (useC) {
        .Call(marBeforeToMarAfter_R, mar, indices, dims, dimAfter)
    }
    else {
        n.before <- length(mar)
        n.after <- length(dimAfter)
        ans <- integer(length = n.after)
        for (i.dim.before in seq_len(n.before)) {
            i.margin.before <- mar[i.dim.before]
            index <- indices[[i.dim.before]]
            i.margin.after <- index[i.margin.before]
            if (i.margin.after > 0L) {
                i.dim.after <- dims[i.dim.before]
                if (i.dim.after > 0L)
                    ans[i.dim.after] <- i.margin.after
            }
            else {
                ans <- rep(0L, n.after)
                break
            }
        }
        ans
    }
}

## TRANSLATED
## HAS_TESTS
## Would be more elegant to return margins rather than positions,
## but that would entail creating a potentially large list of vectors
## (as opposed to a single large vector) and in practice we convert
## to positions later anyway.
marAfterToPosBefore <- function(mar, dims, multiplierBefore, invIndices, useC = FALSE) {
    ## mar
    stopifnot(is.integer(mar))
    stopifnot(!any(is.na(mar)))
    stopifnot(all(mar >= 1L))
    ## dims
    stopifnot(is.integer(dims))
    stopifnot(!any(is.na(dims)))
    stopifnot(all(dims >= 0L))
    ## multiplierBefore
    stopifnot(is.integer(multiplierBefore))
    stopifnot(!any(is.na(multiplierBefore)))
    stopifnot(all(multiplierBefore >= 1L))
    ## invIndices
    stopifnot(is.list(invIndices))
    stopifnot(all(sapply(invIndices, is.list)))
    stopifnot(is.integer(unlist(invIndices)))
    stopifnot(!any(is.na(unlist(invIndices))))
    stopifnot(all(unlist(invIndices) >= 1L))
    ## mar and dims
    stopifnot(identical(length(mar), sum(dims > 0L)))
    ## dims and multiplierBefore
    stopifnot(identical(length(dims), length(multiplierBefore)))
    ## dims and invIndices
    stopifnot(identical(length(dims), length(invIndices)))
    if (useC) {
        .Call(marAfterToPosBefore_R, mar, dims, multiplierBefore, invIndices)
    }
    else {
        n.dim.before <- length(dims)
        ## create 'inv.indices.margin' containing indices
        ## back into 'before' specific to 'mar'
        inv.indices.margin <- vector(mode = "list", length = length(invIndices))
        for (i.dim.before in seq_len(n.dim.before)) {
            inv.index <- invIndices[[i.dim.before]]
            i.dim.after <- dims[i.dim.before]
            if (i.dim.after > 0L)
                i.inv.index <- mar[i.dim.after]
            else
                i.inv.index <- 1L
            inv.indices.margin[[i.dim.before]] <- inv.index[[i.inv.index]]
        }
        ## 'mar' for 'before' are formed by taking cartesian
        ## product of 'inv.indices.mar'.  Coordinates of individual
        ## elements of 'inv.indices.mar' can be obtained using
        ## posToMar on 'n.indices'.
        n.indices <- sapply(inv.indices.margin, length)
        n.margin.before <- prod(n.indices)
        ans <- integer(length = n.margin.before)
        margin.before <- integer(length = n.dim.before)
        for (i.ans in seq_len(n.margin.before)) {
            i.indices <- posToMar(pos = i.ans, dim = n.indices)
            for (i.dim.before in seq_len(n.dim.before)) {
                inv.index <- inv.indices.margin[[i.dim.before]]
                i.index <- i.indices[i.dim.before]
                margin.before[i.dim.before] <- inv.index[i.index]
            }
            ans[i.ans] <- marToPos(margin.before, multiplier = multiplierBefore)
        }
        ans
    }
}



## 'getIAfter', 'getIBefore', 'getIShared'

## TRANSLATED
## HAS_TESTS
## returns index of cell in 'after', given index of cell in 'before'
#' @rdname exported-not-api
#' @export
getIAfter <- function(i, transform, check = TRUE, useC = FALSE) {
    if (check) {
        ## i
        stopifnot(is.integer(i))
        stopifnot(identical(length(i), 1L))
        stopifnot(!is.na(i))
        stopifnot(i >= 1L)
        ## transform
        stopifnot(methods::is(transform, "CollapseTransformExtra"))
        stopifnot(methods::validObject(transform))
        ## i and transform
        stopifnot(i <= prod(transform@dimBefore))
    }
    if (useC) {
        .Call(getIAfter_R, i, transform)
    }
    else {
        indices <- transform@indices
        dims <- transform@dims
        dim.before <- transform@dimBefore
        dim.after <- transform@dimAfter
        multiplier.after <- transform@multiplierAfter
        margin.before <- posToMar(pos = i, dim = dim.before)
        margin.after <- marBeforeToMarAfter(mar = margin.before,
                                            indices = indices,
                                            dims = dims,
                                            dimAfter = dim.after)
        if (all(margin.after > 0L))
            marToPos(mar = margin.after, multiplier = multiplier.after)
        else
            0L
    }
}

## TRANSLATED
## HAS_TESTS
## returns indices of cells in 'before', given index of cell in 'after'
#' @rdname exported-not-api
#' @export
getIBefore <- function(i, transform, useC = FALSE) {
    ## i
    stopifnot(is.integer(i))
    stopifnot(identical(length(i), 1L))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## transform
    stopifnot(methods::is(transform, "CollapseTransformExtra"))
    ## i and transform
    stopifnot(i <= prod(transform@dimAfter))
    if (useC) {
        .Call(getIBefore_R, i, transform)
    }
    else {
        dims <- transform@dims
        dim.after <- transform@dimAfter
        multiplier.before <- transform@multiplierBefore
        inv.indices <- transform@invIndices
        margin.after <- posToMar(pos = i, dim = dim.after)
        pos.before <- marAfterToPosBefore(mar = margin.after,
                                          dims = dims,
                                          multiplier.before,
                                          invIndices = inv.indices)
    }
}

## TRANSLATED
## HAS_TESTS
## Returns indices of cells in 'before' that map to cell in 'after',
## given the index of one of the cells in 'before'.  Returns a length-0
## integer vector if the cell in 'before' does not map into 'after'.
#' @rdname exported-not-api
#' @export
getIShared <- function(i, transform, useC = FALSE) {
    ## i
    stopifnot(is.integer(i))
    stopifnot(identical(length(i), 1L))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## transform
    stopifnot(methods::is(transform, "CollapseTransformExtra"))
    ## i and transform
    stopifnot(i <= prod(transform@dimBefore))
    if (useC) {
        .Call(getIShared_R, i, transform)
    }
    else {
        indices <- transform@indices
        dims <- transform@dims
        dim.before <- transform@dimBefore
        dim.after <- transform@dimAfter
        multiplier.before <- transform@multiplierBefore
        inv.indices <- transform@invIndices
        margin.before <- posToMar(pos = i, dim = dim.before)
        margin.after <- marBeforeToMarAfter(mar = margin.before,
                                            indices = indices,
                                            dims = dims,
                                            dimAfter = dim.after)
        if (all(margin.after == 0L))
            integer()
        else
            marAfterToPosBefore(mar = margin.after,
                                dims = dims,
                                multiplierBefore = multiplier.before,
                                invIndices = inv.indices)
    }
}


## ## Functions for ExposeTransform

## ## returns integer vector with length 2
## getIExposeNoAge <- function(i, transform) {
##     indices <- transform@indices
##     dims <- transform@dims
##     dim.before <- transform@dimBe
##     dim.after <- transform@dimAfter
##     multiplier.after <- transform@multiplierAfter
##     i.time <- transform@iTime
##     margin.before <- posToMar(pos = i, dim = dim.before)
##     margin.after <- marBeforeToMarAfter(mar = margin.before,
##                                         indices = indices,
##                                         dims = dims,
##                                         dimAfter = dim.after)
##     pos.after.first <- marToPos(margin.after, multiplier = multiplier)
##     margin.after[i.time] <- margin.after[i.time] + 1L
##     pos.after.second <- marToPos(margin.after, multiplier = multiplier)
##     c(pos.after.first, pos.after.second)
## }

## ## returns integer
## getIExposeHasAge <- function(i, isLower, transform) {
##     indices <- transform@indices
##     dims <- transform@dims
##     dim.before <- transform@dimBe
##     dim.after <- transform@dimAfter
##     multiplier.after <- transform@multiplierAfter
##     i.time <- transform@iTime
##     margin.before <- posToMar(pos = i, dim = dim.before)
##     margin.after <- marBeforeToMarAfter(mar = margin.before,
##                                         indices = indices,
##                                         dims = dims,
##                                         dimAfter = dim.after)
##     pos.after.first <- marToPos(margin.after, multiplier = multiplier)
##     margin.after[i.time] <- margin.after[i.time] + 1L
##     pos.after.second <- marToPos(margin.after, multiplier = multiplier)
##     c(pos.after.first, pos.after.second)
## }
##     indices <- transform@indices
##     dims <- transform@dims
##     dim.before <- transform@dimBefore
##     dim.after <- transform@dimAfter
##     mar <- posToMar(pos = i, dim = dim.before)


##     margin.before <- posToMar(pos = i, dim = dim.before)
##     margin.after <- marBeforeToMarAfter(mar = margin.before,
##                                             indices = indices,
##                                             dims = dims,
##                                             dimAfter = dim.after)
##         if (all(margin.after > 0L))
##             marToPos(mar = margin.after, multiplier = multiplier.after)
##         else
##             0L

#' @rdname exported-not-api
#' @export
makeMetaDataSubarraysBefore <- function(metadata, transform) {
    nms <- names(metadata)
    dimtypes <- dimtypes(metadata, use.names = FALSE)
    DimScales <- DimScales(metadata, use.names = FALSE)
    inv.indices <- transform@invIndices
    inv.indices <- expand.grid(inv.indices)
    m <- nrow(inv.indices)
    n <- ncol(inv.indices)
    ans <- vector(mode = "list", length = m)
    for (i in seq_len(m)) {
        DS <- DimScales
        for (j in seq_len(n)) {
            ind <- inv.indices[[j]][[i]]
            DS[[j]] <- DS[[j]][ind]
        }
        ans[[i]] <- new("MetaData",
                        nms = nms,
                        dimtypes = dimtypes,
                        DimScales = DS)
    }
    ans
}


## FUNCTIONS FOR VALIDITY CHECKING #################################################

## HAS_TESTS
#' @rdname exported-not-api
#' @export
checkAge <- function(object, minAges = 2L, regular = FALSE,
                     openLeftOK = FALSE, openRightOK = FALSE,
                     expectedDimscale = "Intervals") {
    dimtypes <- dimtypes(object, use.names = FALSE)
    DimScales <- DimScales(object, use.names = FALSE)
    i.age <- match("age", dimtypes, nomatch = 0L)
    ## has age dimension
    if (identical(i.age, 0L))
        stop(gettextf("no dimension with dimtype \"%s\"",
                      "age"))
    DimScale <- DimScales[[i.age]]
    n.age <- length(DimScale)
    dimvalues <- dimvalues(DimScale)
    if (minAges > 0L) {
        ## age dimension has length 'minAge' or more
        if (n.age < minAges)
            stop(gettextf("dimension with dimtype \"%s\" has length %d",
                          "age", n.age))
    }
    ## regular
    if (regular) {
        dv <- dimvalues[is.finite(dimvalues)]
        if (length(dv) > 1L) {
            diff.dv <- diff(dv)
            if (any(diff.dv[-1L] != diff.dv[1L]))
                stop(gettextf("dimension with dimtype \"%s\" is not regular",
                              "age"))
        }
    }
    ## age dimension has expected dimscale
    if (!methods::is(DimScale, expectedDimscale))
        stop(gettextf("dimension with dimtype \"%s\" has dimscale \"%s\"",
                      "age", class(DimScale)))
    ## age dimension only open on left or right if permitted
    if (methods::is(DimScale, "Intervals")) {
        if (!openLeftOK) {
            if (is.infinite(dimvalues[1L]))
                stop(gettext("first age group is open"))
        }
        if (!openRightOK) {
            if (is.infinite(dimvalues[length(dimvalues)]))
                stop(gettext("last age group is open"))
        }
    }
    NULL
}

checkAndTidyNIncrement <- function(n) {
    if (!is.numeric(n))
        stop(gettextf("'%s' is non-numeric",
                      "n"))
    if (!identical(length(n), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "n", 1L))
    if (is.na(n))
        stop(gettextf("'%s' is missing",
                      "n"))
    if (n != as.integer(n))
        stop(gettextf("'%s' is not an integer",
                      "n"))
    if (n == 0L)
        stop(gettextf("'%s' equals %d",
                      "n", 0L))
    as.integer(n)
}

## NO_TESTS
checkConcordances <- function(concordances) {
    if (!is.list(concordances))
        stop(gettextf("'%s' has class \"%s\"",
                      "concordances", class(concordances)))
    if (length(concordances) > 1L) {
        names <- names(concordances)
        if (is.null(names))
            stop(gettextf("'%s' does not have names",
                          "concordances"))
        if (any(is.na(names)))
            stop(gettextf("names for '%s' have missing values",
                          "concordances"))
        if (!all(nzchar(names)))
            stop(gettextf("names for '%s' have blanks",
                          "concordances"))
        if (any(duplicated(names)))
            stop(gettextf("names for '%s' have duplicates",
                          "concordances"))
        if (!all(sapply(concordances, methods::is,"Concordance")))
            stop(gettextf("'%s' has elements not of class \"%s\"",
                          "concordances", "Concordance"))
    }
    NULL
}

## HAS_TESTS
checkDimnames <- function(dimnames, includeNames = TRUE) {
  return.value <- validDimnames(dimnames = dimnames, includeNames = includeNames)
  if (isTRUE(return.value))
    NULL
  else
    stop(return.value)
}

## HAS_TESTS
checkDimtypesOrDimscalesArg <- function(arg, nameArg, names) {
    if (is.null(arg))
        return(NULL)
    ## is character vector
    if (!is.character(arg))
        stop(gettextf("'%s' has class \"%s\"",
                      nameArg, class(arg)))
    ## length > 0
    n <- length(arg)
    if (n == 0L)
        stop(gettextf("'%s' has length %d",
                      nameArg, 0L))
    names.arg <- names(arg)
    ## has names
    if (is.null(names.arg))
        stop(gettextf("'%s' does not have names",
                      nameArg))
    ## no blank names
    if (any(!nzchar(names.arg)))
        stop(gettextf("names for '%s' have blanks",
                      nameArg))
    ## no duplicate names
    if (any(duplicated(names.arg)))
        stop(gettextf("names for '%s' have duplicates",
                      nameArg))
    ## names refer to dimensions
    for (name in names.arg) {
        if (!(name %in% names))
            stop(gettextf("'%s' argument invalid : \"%s\" is not a dimension name",
                          nameArg, name))
    }
    NULL
}

## HAS_TESTS
checkIterationDimvalues <- function(x, dots) {
    iter.dv.x <- getIterationDimvalues(x)
    iter.dv.dots <- lapply(dots, getIterationDimvalues)
    iter.dv <- c(list(iter.dv.x), iter.dv.dots)
    has.iter <- !sapply(iter.dv, is.null)
    n.has.iter <- sum(has.iter)
    if (n.has.iter > 1L) {
        iter.dv <- iter.dv[has.iter]
        iterSame <- function(e1, e2) isTRUE(all.equal(e1, e2))
        iter.same <- mapply(iterSame, iter.dv[1L], iter.dv[-1L])
        if (!all(iter.same))
            stop(gettextf("dimensions with dimtype \"%s\" inconsistent",
                          "iteration"))
    }
    NULL
}

## HAS_TESTS
checkNames <- function(names) {
  return.value <- validNames(names)
  if (isTRUE(return.value))
    NULL
  else
    stop(return.value)
}

## HAS_TESTS
## used by Counts methods of expandCategories
checkAndTidyObjExpCatCounts <- function(object, weights, n) {
    .Data <- object@.Data
    metadata <- metadata(object)
    dimtypes.obj <- dimtypes(metadata, use.names = FALSE)
    ## .Data integers
    if (any(.Data != as.integer(.Data)))
        stop(gettextf("'%s' has non-integer values",
                      "object"))
    .Data <- as.integer(.Data)
    ## .Data non-negative
    if (any(.Data[!is.na(.Data)] < 0L))
        stop(gettextf("'%s' has negative values",
                      "object"))
    ## if 'object' does not have iterations,
    ## but (i) weights does or (ii) a value is supplied for 'n',
    ## add iterations to 'object'
    obj.has.iter <- "iteration" %in% dimtypes.obj
    if (!obj.has.iter) {
        wt.has.iter <- (methods::is(weights, "DemographicArray")
                        && ("iteration" %in% dimtypes(weights)))
        if (wt.has.iter) {
            add.iter <- TRUE
            n <- nIteration(weights)
        }
        else {
            n <- checkAndTidyN(n)
            add.iter <- is.integer(n)
        }
        if (add.iter) {
            iterations <- seq_len(n)
            metadata <- addIterationsToMetadata(metadata,
                                                iterations = iterations)
            .Data <- array(.Data,  ## replicated
                           dim = dim(metadata),
                           dimnames = dimnames(metadata))
            object <- methods::new("Counts", .Data = .Data, metadata = metadata)
        }
    }
    object
}

## HAS_TESTS
getIterationDimvalues <- function(object) {
    if (is(object, "DemographicArray")) {
        dimtypes <- dimtypes(object, use.names = FALSE)
        i.iter <- match("iteration", dimtypes, nomatch = 0L)
        has.iter <- i.iter > 0L
        if (has.iter) {
            DimScale <- DimScales(object)[[i.iter]]
            DimScale@dimvalues
        }
        else
            NULL
    }
    else
        NULL
}

## HAS_TESTS
validDimnames <- function(dimnames, includeNames = TRUE) {
  if (is.null(dimnames))
    return(gettextf("'%s' is NULL", "dimnames"))
  for (i in seq_along(dimnames)) {
    dn <- dimnames[[i]]
    if (!is.null(dn)) {
      if (any(is.na(dn)))
        return(gettextf("element %d of '%s' has missing values", i, "dimnames"))
      if (!all(nzchar(dn)))
        return(gettextf("element %d of '%s' has elements with length 0", i, "dimnames"))
      if (any(duplicated(dn)))
        return(gettextf("element %d of '%s' has duplicates", i, "dimnames"))
    }
  }
  if (includeNames) {
    names <- names(dimnames)
    if (is.null(names))
      return(gettext("dimnames do not have names"))
    return.value <- validNames(names)
    if (!isTRUE(return.value))
      return(return.value)
  }
  TRUE
}

## HAS_TESTS
validNames <- function(names) {
  if (is.null(names))
    return(gettextf("'%s' is NULL", "names"))
  if (any(is.na((names))))
    return(gettextf("'%s' has missing values", "names"))
  if (!all(nzchar(names)))
    return(gettextf("'%s' has elements with length %d", "names", 0L))
  if (any(duplicated(names)))
    return(gettextf("'%s' has duplicates", "names"))
  TRUE
}


## FUNCTIONS FOR MAKING INTERVAL LABELS ##############################################

## HAS_TESTS
ageMinMax <- function(object, min = TRUE) {
    dimtypes <- dimtypes(object,
                         use.names = FALSE)
    DimScales <- DimScales(object,
                           use.names = FALSE)
    i.age <- match("age", dimtypes, nomatch = 0L)
    has.age <- i.age > 0L
    if (has.age) {
        DimScale.age <- DimScales[[i.age]]
        dimvalues.age <- dimvalues(DimScale.age)
        n.dimvalues.age <- length(dimvalues.age)
        if (n.dimvalues.age > 0L) {
            if (min)
                dimvalues.age[1L]
            else
                dimvalues.age[n.dimvalues.age]
        }
        else
            integer()
    }
    else
        NULL
}

## HAS_TESTS
ageMinMaxReplace <- function(object, value, min = TRUE) {
    if (!is.numeric(value))
        stop(gettext("replacement value is non-numeric"))
    if (!identical(length(value), 1L))
        stop(gettextf("replacement value does not have length %d",
                      1L))
    if (is.na(value))
        stop(gettext("replacement value is missing"))
    if (is.finite(value) && (value == round(value)))
        value <- as.integer(value)
    names <- names(object)
    dimtypes <- dimtypes(object,
                         use.names = FALSE)
    DimScales <- DimScales(object,
                           use.names = FALSE)
    i.age <- match("age", dimtypes, nomatch = 0L)
    has.age <- i.age > 0L
    if (!has.age)
        stop(gettextf("no dimension with %s \"%s\"",
                      "dimtype", "age"))
    DimScale.age <- DimScales[[i.age]]
    if (!methods::is(DimScale.age, "Intervals"))
        stop(gettextf("dimension with %s \"%s\" has %s \"%s\"",
                      "dimtype", "age", "dimscale", class(DimScale.age)))
    dimvalues.age <- dimvalues(DimScale.age)
    n.dimvalues.age <- length(dimvalues.age)
    if (n.dimvalues.age == 0L)
        stop(gettextf("dimension with %s \"%s\" has length %d",
                      "dimtype", "age", 0L))
    if (min) {
        if (value >= dimvalues.age[2L])
            stop(gettext("replacement value greater than or equal to upper limit of first age group"))
        dimvalues.age[1L] <- value
    }
    else {
        if (value <= dimvalues.age[n.dimvalues.age - 1L])
            stop(gettext("replacement value less than or equal to lower limit of last age group"))
        dimvalues.age[n.dimvalues.age] <- value
    }
    DimScale.age@dimvalues <- dimvalues.age
    DimScales <- replace(DimScales,
                         list = i.age,
                         values = list(DimScale.age))
    methods::new("MetaData",
                 nms = names,
                 dimtypes = dimtypes,
                 DimScales = DimScales)
}

## HAS_TESTS
## convert date to decimal fraction of year (including year)
dateToFracYear <- function(date) {
    digits.round <- getDigitsRoundDimvaluesTimeUnit()
    stopifnot(methods::is(date, "Date"))
    this.year <- format(date, format = "%Y")
    this.year <- as.integer(this.year)
    next.year <- this.year + 1L
    date.start.this.year <- sprintf("%d-01-01", this.year)
    date.start.next.year <- sprintf("%d-01-01", next.year)
    date.start.this.year <- as.Date(date.start.this.year)
    date.start.next.year <- as.Date(date.start.next.year)
    days.start.this.year <- as.integer(date.start.this.year)
    days.start.next.year <- as.integer(date.start.next.year)
    days.date <- as.integer(date)
    frac <- ((days.date - days.start.this.year)
             / (days.start.next.year - days.start.this.year))
    ans <- this.year + frac
    round(ans, digits.round)
}

## HAS_TESTS
intervalDimvaluesFromDates <- function(labels) {
    if (!methods::is(labels, "Date"))
        labels <- as.Date(labels)
    labels <- sort(labels)
    n <- length(labels)
    final <- seq.Date(from = labels[n],
                      length.out = 2L,
                      by = "1 day")[2L]
    labels <- c(labels, final)
    dateToFracYear(labels)
}

## HAS_TESTS
intervalDimvaluesFromMonths <- function(labels) {
    labels <- sort(labels)
    labels <- paste0(labels, "-01")
    labels <- as.Date(labels)
    n <- length(labels)
    final <- seq.Date(from = labels[n],
                      length.out = 2L,
                      by = "1 month")[2L]
    labels <- c(labels, final)
    dateToFracYear(labels)
}

## HAS_TESTS
intervalDimvaluesFromQuarters <- function(labels) {
    labels <- toupper(labels)
    labels <- sort(labels)
    labels <- gsub("Q1", "01", labels)
    labels <- gsub("Q2", "04", labels)
    labels <- gsub("Q3", "07", labels)
    labels <- gsub("Q4", "10", labels)
    labels <- paste0(labels, "-01")
    labels <- as.Date(labels)
    n <- length(labels)
    final <- seq.Date(from = labels[n],
                      length.out = 2L,
                      by = "1 quarter")[2L]
    labels <- c(labels, final)
    dateToFracYear(labels)
}

## HAS_TESTS
isValidIntervalLabelsDate <- function(labels, ordered = FALSE) {
    n <- length(labels)
    if (identical(n, 0L))
        return(TRUE)
    values <- suppressWarnings(as.Date(labels, format = "%Y-%m-%d"))
    has.invalid.dates <- any(is.na(values))
    if (has.invalid.dates)
        return(FALSE)
    if (identical(n, 1L))
        return(TRUE)
    values <- as.integer(values)
    if (!ordered)
        values <- sort(values)
    all(diff(values) == 1L)
}

## HAS_TESTS
isValidIntervalLabelsMonth <- function(labels, ordered = FALSE) {
    n <- length(labels)
    if (identical(n, 0L))
        return(TRUE)
    labels.plus.day <- paste0(labels, "-01")
    values <- suppressWarnings(as.Date(labels.plus.day, format = "%Y-%m-%d"))
    has.invalid.dates <- any(is.na(values))
    if (has.invalid.dates)
        return(FALSE)
    if (identical(n, 1L))
        return(TRUE)
    if (!ordered)
        values <- sort(values)
    from <- values[1L]
    to <- values[n]
    if (to <= from)
        return(FALSE)
    implied.values <- seq.Date(from = from,
                               to = to,
                               by = "1 month")
    isTRUE(all.equal(values, implied.values))
}

## HAS_TESTS
isValidIntervalLabelsQuarter <- function(labels, ordered = FALSE) {
    n <- length(labels)
    if (identical(n, 0L))
        return(TRUE)
    labels <- toupper(labels)
    all.valid.quarters <- all(grepl("^(-?[1-9][0-9]*|0)\\-Q[1-4]$", labels))
    if (!all.valid.quarters)
        return(FALSE)
    if (identical(n, 1L))
        return(TRUE)
    labels.as.dates <- gsub("Q1", "01", labels)
    labels.as.dates <- gsub("Q2", "04", labels.as.dates)
    labels.as.dates <- gsub("Q3", "07", labels.as.dates)
    labels.as.dates <- gsub("Q4", "10", labels.as.dates)
    labels.as.dates <- paste0(labels.as.dates, "-01")
    labels.as.dates <- as.Date(labels.as.dates)
    if (!ordered)
        labels.as.dates <- sort(labels.as.dates)
    from <- labels.as.dates[1L]
    to <- labels.as.dates[n]
    if (to <= from)
        return(FALSE)
    implied.values <- seq.Date(from = from,
                               to = to,
                               by = "1 quarter")
    isTRUE(all.equal(labels.as.dates, implied.values))
}

## HAS_TESTS
isValidPointLabelsDate <- function(labels, ordered = FALSE) {
    n <- length(labels)
    if (identical(n, 0L))
        return(TRUE)
    values <- suppressWarnings(as.Date(labels, format = "%Y-%m-%d"))
    has.invalid.dates <- any(is.na(values))
    if (has.invalid.dates)
        return(FALSE)
    if (identical(n, 1L))
        return(TRUE)
    values <- as.integer(values)
    if (!ordered)
        values <- sort(values)
    all(diff(values) >= 1L)
}    

## HAS_TESTS
makeIntervalLabelsForDateDimvalues <- function(dimvalues) {
    n <- length(dimvalues)
    if (n < 2L)
        return(NULL)
    if (any(is.infinite(dimvalues)))
        return(NULL)
    time.units <- timeUnitsFromDimScales(dimvalues,
                                         unit = "quarter",
                                         successive = TRUE)
    if (!is.null(time.units)) {
        time.units <- time.units[-n]
        time.units <- as.Date(time.units, format = "%Y-%m-%d")
        year <- format(time.units, format = "%Y")
        month <- format(time.units, format = "%m")
        month <- as.integer(month)
        quarter <- month %/% 3L + 1L
        ans <- sprintf("%s-Q%i", year, quarter)
        return(ans)
    }
    time.units <- timeUnitsFromDimScales(dimvalues,
                                         unit = "month",
                                         successive = TRUE)
    if (!is.null(time.units)) {
        time.units <- time.units[-n]
        time.units <- as.Date(time.units, format = "%Y-%m-%d")
        ans <- format(time.units, format = "%Y-%m")
        return(ans)
    }
    time.units <- timeUnitsFromDimScales(dimvalues,
                                         unit = "day",
                                         successive = TRUE)
    if (!is.null(time.units)) {
        time.units <- time.units[-n]
        ans <- format(time.units, format = "%Y-%m-%d")
        return(ans)
    }
    NULL
}

## HAS_TESTS
makeLabelsForClosedIntervals <- function(dimvalues, labelStart, isAge) {
    kDigits <- 3L
    n <- length(dimvalues)
    if (n > 0L) {
        ans <- character(length = n - 1L)
        lower.is.integer <- dimvalues[-n] == round(dimvalues[-n])
        diff.is.integer <- diff(dimvalues) == 1
        use.single.value <- lower.is.integer & diff.is.integer
        if (labelStart)
            ans[use.single.value] <- dimvalues[-n][use.single.value]
        else
            ans[use.single.value] <- dimvalues[-1L][use.single.value]
        lower <- dimvalues[-n][ans == ""]
        upper <- dimvalues[-1L][ans == ""]
        lower.is.integer <- lower == round(lower)
        upper.is.integer <- upper == round(upper)
        if (isAge) {
            reduce.upper <- lower.is.integer & upper.is.integer
            upper[reduce.upper] <- upper[reduce.upper] - 1L
        }
        lower <- round(lower, kDigits)
        upper <- round(upper, kDigits)
        ans[ans == ""] <- paste0(lower, "-", upper)
        ans
    }
    else
        character()
}

## HAS_TESTS
makeMonthLabelsForAgeDimvalues <- function(dimvalues) {
    dimvalues.finite <- dimvalues[is.finite(dimvalues)]
    n.finite <- length(dimvalues.finite)
    if (n.finite < 2L)
        return(NULL)
    dimvalues.finite.implied <- seq(from = dimvalues.finite[1L],
                                    to = dimvalues.finite[n.finite],
                                    by = 1 / 12)
    obs.close.to.implied <- (identical(length(dimvalues.finite.implied), n.finite)
        && all(abs(dimvalues.finite - dimvalues.finite.implied) < 0.001))
    if (!obs.close.to.implied)
        return(NULL)
    year <- as.integer(dimvalues.finite[-n.finite])
    month <- round((dimvalues.finite[-n.finite] - year) * 12)
    ans <- paste0(year, "-", month, "m")
    if (is.infinite(dimvalues[1L])) {
        label.first <- paste0("<", ans[1L])
        ans <- c(label.first, ans)
    }
    if (is.infinite(dimvalues[length(dimvalues)])) {
        dimvalue.last <- dimvalues.finite[n.finite]
        year.last <- as.integer(dimvalue.last)
        month.last <- round((dimvalue.last - year.last) * 12)
        label.last <- paste0(year.last, "-", month.last, "m", "+")
        ans <- c(ans, label.last)
    }
    ans
}

## HAS_TESTS
makeQuarterLabelsForAgeDimvalues <- function(dimvalues) {
    dimvalues.finite <- dimvalues[is.finite(dimvalues)]
    n.finite <- length(dimvalues.finite)
    if (n.finite < 2L)
        return(NULL)
    dimvalues.finite.implied <- seq(from = dimvalues.finite[1L],
                                    to = dimvalues.finite[n.finite],
                                    by = 1 / 4)
    obs.close.to.implied <- (identical(length(dimvalues.finite.implied), n.finite)
        && all(abs(dimvalues.finite - dimvalues.finite.implied) < 0.001))
    if (!obs.close.to.implied)
        return(NULL)
    year <- as.integer(dimvalues.finite[-n.finite])
    quarter <- round((dimvalues.finite[-n.finite] - year) * 4)
    ans <- paste0(year, "-", quarter, "q")
    if (is.infinite(dimvalues[1L])) {
        label.first <- paste0("<", ans[1L])
        ans <- c(label.first, ans)
    }
    if (is.infinite(dimvalues[length(dimvalues)])) {
        dimvalue.last <- dimvalues.finite[n.finite]
        year.last <- as.integer(dimvalue.last)
        quarter.last <- round((dimvalue.last - year.last) * 4)
        label.last <- paste0(year.last, "-", quarter.last, "q", "+")
        ans <- c(ans, label.last)
    }
    ans
}

## HAS_TESTS - needs more
makeLabelsForIntervals <- function(dimvalues, labelStart, isAge) {
    n <- length(dimvalues)
    if (n > 0L) {
        if (isAge) {
            possible.ans <- makeMonthLabelsForAgeDimvalues(dimvalues)
            if (!is.null(possible.ans))
                return(possible.ans)
            possible.ans <- makeQuarterLabelsForAgeDimvalues(dimvalues)
            if (!is.null(possible.ans))
                return(possible.ans)
        }
        else {
            possible.ans <- makeIntervalLabelsForDateDimvalues(dimvalues)
            if (!is.null(possible.ans))
                return(possible.ans)
        }
        first.interval.is.open <- is.infinite(dimvalues[1L])
        last.interval.is.open <- is.infinite(dimvalues[n])
        ans <- character(n - 1L)
        if (first.interval.is.open) {
            upper <- dimvalues[2L]
            ans[1L] <- paste0("<", upper)
        }
        if (last.interval.is.open) {
            lower <- dimvalues[n - 1L]
            ans[n - 1L] <- paste0(lower, "+")
        }
        use.for.closed <- c(!first.interval.is.open,
                            rep(TRUE, n - 2L),
                            !last.interval.is.open)
        dimvalues.closed <- dimvalues[use.for.closed]
        ans[ans == ""] <- makeLabelsForClosedIntervals(dimvalues = dimvalues.closed,
                                                       isAge = isAge,
                                                       labelStart = labelStart)
        ans
    }
    else
        character()
}


## HAS_TESTS
timeUnitsFromDimScales <- function(dimvalues,
                                   unit = c("day", "month", "quarter"),
                                   successive = TRUE) {
    digits.round <- getDigitsRoundDimvaluesTimeUnit()
    unit <- match.arg(unit)
    if (any(is.infinite(dimvalues)))
        return(FALSE)
    n <- length(dimvalues)
    poss.year.first <- floor(dimvalues[1L])
    poss.year.last <- floor(dimvalues[n]) + 1L
    date.from <- as.Date(sprintf("%s-01-01", poss.year.first))
    date.to <- as.Date(sprintf("%s-01-01", poss.year.last))
    poss.dates <- seq(from = date.from,
                      to = date.to,
                      by = unit)
    poss.dimvalues <- dateToFracYear(poss.dates)
    dimvalues <- round(dimvalues, digits.round)
    i <- match(dimvalues, poss.dimvalues, nomatch = 0L)
    all.dimvalues.valid <- all(i > 0L)
    if (all.dimvalues.valid) {
        if (successive) {
            is.successive <- all(diff(i) == 1L)
            if (is.successive)
                poss.dates[i]
            else
                NULL
        }
        else
            poss.dates[i]
    }
    else
        NULL
}    




## FUNCTIONS FOR INFERRING DIMVALUES FOR INTERVALS ###################################

## HAS_TESTS
## Given a label, see if it consists of a number and open interval symbol.
## If it does, return the number; if not, return NULL.  If 'which' is "last"
## the symbol is assumed to be on the right of the number;
## if 'which' is "first", the symbol is assumed to be on the left.
extractNumberFromOpenInterval <- function(name, which = c("first", "last")) {
  if (!identical(length(name), 1L))
    stop(gettextf("'%s' does not have length 1", "name"))
  which <- match.arg(which)
  pattern <- switch(which,
                    first = "^<",
                    last = "\\+$")
  found.symbol <- grepl(pattern, name)
  if (found.symbol) {
      number <- sub(pattern, "", name)
      number <- suppressWarnings(as.numeric(number))
      is.number <- !is.na(number)
      if (is.number)
          number
      else
          NULL
  }
  else
      NULL
}

## HAS_TESTS
extractNumbersFromEndOfStrings <- function(strings) {
    interval.separator <- "-"
    p <- sprintf("^(-?[0-9]*\\.?[0-9]*%s)(-?[0-9]*\\.?[0-9]*)$", interval.separator)
    ans <- mapply(sub, p, "\\2", strings)
    ans <- suppressWarnings(as.numeric(ans))
    ans
}

## HAS_TESTS
extractNumbersFromStartOfStrings <- function(strings) {
  ans <- sub("^(-?[0-9]*\\.?[0-9]*)(.*)$", "\\1", strings)
  ans <- suppressWarnings(as.numeric(ans))
  ans
}

## HAS_TESTS
stringsAreIntegers <- function(strings) {
  strings.as.numeric <- suppressWarnings(as.numeric(strings))
  strings.as.integer <- suppressWarnings(as.integer(strings.as.numeric))
  non.missing <- !is.na(strings.as.numeric)
  finite <- is.finite(strings.as.numeric)
  integer <- strings.as.numeric == strings.as.integer
  non.missing & finite & integer
}

## HAS_TESTS
stringsAreNumbers <- function(strings) {
  strings.as.numeric <- suppressWarnings(as.numeric(strings))
  !is.na(strings.as.numeric)
}



## FUNCTIONS FOR INFERRING DIMTYPES AND DIMSCALES ####################################

## HAS_TESTS
#' @rdname exported-not-api
#' @export
inferDimScale <- function(dimtype, dimscale = NULL, labels, name, labelStart = TRUE) {
    is.age <- dimtype == "age"
    is.time <- dimtype == "time"
    if (is.null(dimscale))
        possible.dimscales <- getPossibleDimscales(dimtype)
    else
        possible.dimscales <- dimscale
    n.possible <- length(possible.dimscales)
    answers <- vector(mode = "list",
                      length = n.possible)
    for (i in seq_len(n.possible)) {
        possible.dimscale <- possible.dimscales[i]
        if (identical(unname(possible.dimscale), "Intervals")) {
            DimScale <- methods::new("Intervals",
                                     isAge = is.age,
                                     labelStart = labelStart)
            dimvalues <- inferDimvalues(DimScale = DimScale,
                                        labels = labels)
            if (is.null(dimvalues))
                answers[[i]] <- NULL
            else
                answers[[i]] <- methods::new("Intervals",
                                             dimvalues = dimvalues,
                                             isAge = is.age,
                                             labelStart = labelStart)
        }
        else {
            DimScale <- methods::new(possible.dimscale)
            dimvalues <- inferDimvalues(DimScale = DimScale,
                                        labels = labels)
            if (is.null(dimvalues))
                answers[[i]] <- NULL
            else
                answers[[i]] <- methods::new(possible.dimscale,
                                             dimvalues = dimvalues)
        }
    }
    is.valid <- !sapply(answers, is.null)
    n.valid <- sum(is.valid)
    if (n.valid == 0L)
        stop(gettextf("unable to infer %s for dimension \"%s\" with %s \"%s\"",
                      "dimscale", name, "dimtype", dimtype))
    else if (n.valid == 1L) {
        i.valid <- which(is.valid)
        answers[[i.valid]]
    }
    else {
        if (is.age) {
            is.intervals <- sapply(answers, methods::is, "Intervals")
            message(gettextf("assuming dimension \"%s\" with %s \"%s\" has %s \"%s\"",
                             name, "dimtype", dimtype, "dimscale", "Intervals"))
            i.intervals <- which(is.intervals)
            answers[[i.intervals]]
        }
        else if (is.time)
            stop(gettextf("dimension \"%s\" with %s \"%s\" could have %s \"%s\" or %s \"%s\" : please supply a '%s' or '%s' argument",
                          name, "dimtype", "time", "dimscale", "Intervals", "dimscale", "Points", "dimscale", "dimscales"))
        else
            stop(gettextf("dimension \"%s\" with %s \"%s\" has multiple possible %s : please supply a '%s' or '%s' argument",
                          name, "dimtype", dimtype, "dimscales", "dimscale", "dimscales"))
    }
}

## HAS_TESTS
inferDimtypes <- function(names) {
  kDefault <- "state"
  kPairs <- c("origin", "destination", "parent", "child")
  kNonStateNonPairSynonyms <- c(age = "age",
                                age5 = "age",
                                age10 = "age",
                                age5yr = "age",
                                age10yr = "age",
                                age5year = "age",
                                age10year = "age",
                                "age group" = "age",
                                agegroup = "age",
                                "birth cohort" = "cohort",
                                cohort = "cohort",
                                duration = "age",
                                gender = "sex",
                                genders = "sex",
                                iterations = "iteration",
                                iter = "iteration",
                                iteration = "iteration",
                                sex = "sex",
                                sexes = "sex",
                                sim = "iteration",
                                simulation = "iteration",
                                quantile = "quantile",
                                quantiles = "quantile",
                                time = "time",
                                period = "time",
                                period5 = "time",
                                quarter = "time",
                                year = "time",
                                yr = "time",
                                lexis = "triangle",
                                "lexis triangle" = "triangle",
                                "lexis triangles" = "triangle",
                                triangle = "triangle",
                                triangles = "triangle")
  ans <- character(length = length(names))
  for (dimtype in kPairs) {
    suffix <- getSuffixes(dimtypes = dimtype)
    p <- paste(suffix, "$", sep = "")
    ans[grep(p, names)] <- dimtype
  }
  names <- tolower(names)
  for (i in seq_along(names))
    if (identical(ans[i], "")) {
      name <- names[i]
      here <- match(name, names(kNonStateNonPairSynonyms))
      if (!is.na(here))
        ans[i] <- kNonStateNonPairSynonyms[here]
      else
        ans[i] <- kDefault
    }
  ans
}



## FUNCTIONS FOR COERCION ###########################################################

## HAS_TESTS
## helper for 'as' methods and 'as.data.frame'
asDataFrame <- function(object, responseName, stringsAsFactors = TRUE) {
    if (length(object) > 0L)
        ans <- as.data.frame.table(object@.Data,
                                   responseName = responseName,
                                   stringsAsFactors = stringsAsFactors)
    else {
        names <- names(object)
        head <- replicate(n = length(names), factor())
        names(head) <- names
        tail <- if (is.integer(object)) integer() else numeric()
        tail <- list(tail)
        names(tail) <- responseName
        ans <- c(head, tail)
        ans <- data.frame(ans)
    }
    i.points <- which(sapply(DimScales(object), methods::is,"Points"))
    i.quantiles <- which(sapply(DimScales(object), methods::is,"Quantiles"))
    coerce.to.numeric <- setdiff(i.points, i.quantiles)
    if (stringsAsFactors) {
        quantileToFactor <- function(x) {
            x <- as.numeric(sub("%$", "", x))
            levels <- sort(unique(x))
            labels <- sprintf("%s%%", levels)
            factor(x, levels = levels, labels = labels)
        }
        ans[i.quantiles] <- lapply(ans[i.quantiles], quantileToFactor)
        factorToNumeric <- function(x) as.numeric(levels(x))[x]
        ans[coerce.to.numeric] <- lapply(ans[coerce.to.numeric], factorToNumeric)
    }
    else
        ans[coerce.to.numeric] <- lapply(ans[coerce.to.numeric], as.numeric)
    ans
}

## HAS_TESTS
intervalsToPoints <- function(object) {
  dimvalues <- dimvalues(object)
  n <- length(dimvalues)
  if (n > 2L) {
    if (is.infinite(dimvalues[n]))
      dimvalues[n] <- 2 * dimvalues[n - 1L] - dimvalues[n - 2L]
    dimvalues <- 0.5 * (dimvalues[-n] + dimvalues[-1L])
  }
  else if (n == 2L) {
    if (is.infinite(dimvalues[2L]))
      dimvalues <- dimvalues[1L]
    else
      dimvalues <- 0.5 * (dimvalues[1L] + dimvalues[2L])
  }
  else
    dimvalues <- numeric()
  methods::new("Points", dimvalues = dimvalues)
}


## FUNCTIONS FOR TESTING COMPATABILITY ###############################################

## HAS_TESTS
## Add an iteration dimension to 'x' to
## make it compatible with 'y'.
addMissingIter <- function(x, y) {
    dim.x <- dim(x)
    dim.y <- dim(y)
    i.iter <- match("iteration", dimtypes(y), nomatch = 0L)
    dim.iter <- dim.y[i.iter]
    x <- array(x, dim = c(dim.x, dim.iter))
    n.dim <- length(dim(x))
    s <- seq_len(n.dim)
    perm <- append(s[-n.dim], values = n.dim, after = i.iter - 1L)
    aperm(x, perm = perm)
}

## HAS_TESTS
## If 'x' has a dimension wih dimtype "iteration",
## check that 'y' also does.  If it does return TRUE;
## otherwise raise an error.
alsoHasIterations <- function(x, y) {
    i.iter.x <- match("iteration", dimtypes(x), nomatch = 0L)
    has.iter.x <- i.iter.x > 0L
    if (has.iter.x) {
        has.iter.y <- "iteration" %in% dimtypes(y)
        if (!has.iter.y)
            stop(gettextf("dimension \"%s\" has dimtype \"%s\" and cannot be collapsed",
                          names(x)[i.iter.x], "iteration"))
    }
    TRUE
}

## HAS_TESTS
## If 'x' has any zero-length dimensions, check that these
## dimensions are also in 'y'.  If all is OK, return TRUE;
## otherwise raise an error.
alsoHasZeroLengthDim <- function(x, y) {
    zero.in.x <- names(x)[dim(x) == 0L]
    zero.in.x.not.in.y <- setdiff(zero.in.x, names(y))
    n.zero.in.x.not.in.y <- length(zero.in.x.not.in.y)
    if (n.zero.in.x.not.in.y > 0L) {
        stop(sprintf(ngettext(n.zero.in.x.not.in.y,
                              "one object has dimension [%s] with length 0 that other does not",
                              "one object has dimensions [%s] with length 0 that other does not"),
                     paste(dQuote(zero.in.x.not.in.y), collapse = ", ")))
    }
    TRUE
}

## HAS_TESTS
## If 'x' has a dimension with dimtype 'iteration', so does 'y'.
## If all OK, return TRUE; otherwise raise an error.
bothHaveIter <- function(x, y) {
    x.has.iter <- "iteration" %in% dimtypes(x)
    if (x.has.iter) {
        y.has.iter <- "iteration" %in% dimtypes(y)
        if (!y.has.iter)
            stop(gettextf("one object has dimension with dimtype \"%s\" but other does not",
                          "iteration"))
    }
    TRUE
}


## Does all the work for 'canMakePairCompatible' methods that
## involve "array".  'x' has class "DemographicArray" and 'y' has class "array".
## Allows minimal perturbation of 'x', since meant for use mainly
## when 'y' is logical.  For more permissive treatment of 'x' and 'y',
## convert 'y' to "DemographicArray".
canMakeDemographicAndArrayCompatible <- function(x, y) {
    dim.x <- dim(x)
    dim.y <- dim(y)
    dn.x <- dimnames(x)
    dn.y <- dimnames(y)
    without.iter <- tryCatch(compatibleDimAndDimnames(dim.x  = dim.x,
                                                      dim.y = dim.y,
                                                      dn.x = dn.x,
                                                      dn.y = dn.y),
                             error = function(e) e)
    if (!isTRUE(without.iter)) {
        i.iter <- match("iteration", dimtypes(x), nomatch = 0L)
        has.iter <- i.iter > 0L
        if (has.iter) {
            dim.iter <- dim.x[i.iter]
            dim.y.with.iter <- append(dim.y, values = dim.iter, after = i.iter - 1L)
            dn.iter <- list(NULL)
            names(dn.iter) <- names(dn.x)[i.iter]
            if (is.null(dn.y))
                dn.y.with.iter <- NULL
            else
                dn.y.with.iter <- append(dn.y, values = dn.iter, after = i.iter - 1L)
            with.iter <- tryCatch(compatibleDimAndDimnames(dim.x = dim.x,
                                                           dim.y = dim.y.with.iter,
                                                           dn.x = dn.x,
                                                           dn.y = dn.y.with.iter),
                                  error = function(e) e)
            if (!isTRUE(with.iter))
                stop(without.iter)
        }
        else
            stop(without.iter)
    }
    TRUE
}

## HAS_TESTS
canMakeSharedDimScalePairsCompatible <- function(e1, e2) {
    shared.names <- intersect(names(e1), names(e2))
    DimScales1 <- DimScales(e1)[shared.names]
    DimScales2 <- DimScales(e2)[shared.names]
    isCounts1 <- methods::is(e1, "Counts")
    isCounts2 <- methods::is(e2, "Counts")
    for (i in seq_along(shared.names)) {
        return.value <- tryCatch(canMakeDimScalePairCompatible(e1 = DimScales1[[i]],
                                                               e2 = DimScales2[[i]],
                                                               isCounts1 = isCounts1,
                                                               isCounts2 = isCounts2),
                                 error = function(e) e)
        if (!isTRUE(return.value))
            stop(gettextf("\"%s\" dimensions have incompatible dimscales : %s",
                          shared.names[i], return.value$message))
    }
    TRUE
}

## HAS_TESTS
## Helper function for 'Ops'.
## 'x' has class "DemographicArray".
checkQuantilesDemographicArray <- function(x, .Generic) {
    if (.Generic %in% c("+", "-", "*", "^", "%%", "%/%", "/")) {
        i.quantile <- match("quantile", dimtypes(x), nomatch = 0L)
        has.quantile <- i.quantile > 0L
        if (has.quantile)
            stop(gettextf("dimension \"%s\" has dimtype \"%s\"",
                          names(x)[i.quantile], "quantile"))
    }
    TRUE
}

## HAS_TESTS
## Helper function for 'Ops'.
## 'e1' has class "DemographicArray"; 'e2' has class "numeric".
checkQuantilesDemographicNumeric <- function(e1, e2, .Generic) {
    i.quantile <- match("quantile", dimtypes(e1), nomatch = 0L)
    has.quantile <- i.quantile > 0L
    if (has.quantile) {
        permitted.operation <- ((.Generic %in% c("+", "-") && all(e2 == 0))  ||
                                (.Generic %in% c("*", "^", "/") && isPositiveScalar(e2)))
        if (!permitted.operation)
            stop(gettextf("dimension \"%s\" has dimtype \"%s\"",
                          names(e1)[i.quantile], "quantile"))
    }
    TRUE
}

## HAS_TESTS
## Helper function for 'Ops'.
## 'e1' has class "numeric"; 'e2' has class "DemographicArray".
checkQuantilesNumericDemographic <- function(e1, e2, .Generic) {
    i.quantile <- match("quantile", dimtypes(e2), nomatch = 0L)
    has.quantile <- i.quantile > 0L
    if (has.quantile) {
        permitted.operation <- ((.Generic == "+" && all(e1 == 0))  ||
                                (.Generic == "*" && isPositiveScalar(e1)))
        if (!permitted.operation)
            stop(gettextf("dimension \"%s\" has dimtype \"%s\"",
                          names(e2)[i.quantile], "quantile"))
    }
    TRUE
}

## HAS_TESTS
## 'x' has class "DemographicArray" and 'y' has class "ANY"
compatibleDimAndDimnames <- function(dim.x, dim.y, dn.x, dn.y) {
    if (!identical(dim.x, dim.y))
        stop(gettext("non-conformable arrays"))
    if (is.null(dn.y))
        return(TRUE)
    nms.y <- names(dn.y)
    if (!is.null(nms.y)) {
        nms.x <- names(dn.x)
        for (i in seq_along(nms.y)) {
            nm.x <- nms.x[i]
            nm.y <- nms.y[i]
            if (nzchar(nm.y) && !identical(nm.x, nm.y))
                stop(gettextf("names of dimensions do not match [\"%s\" versus \"%s\"]",
                                nm.x, nm.y))
        }
    }
    for (i in seq_along(dn.y)) {
        dn.y.i <- dn.y[[i]]
        if (!is.null(dn.y.i)) {
            dn.x.i <- dn.x[[i]]
            if (!identical(dn.x.i, dn.y.i))
                stop(gettextf("dimnames for dimension \"%s\" do not match",
                                nms.x[i]))
        }
    }
    TRUE
}

## HAS_TESTS
## Check that all dimensions shared by 'e1' and 'e2'
## have the same dimtypes.  If they do, return TRUE; otherwise
## raise an error.
consistentDimtypes <- function(e1, e2) {
    shared.names <- intersect(names(e1), names(e2))
    dimtypes.e1 <- dimtypes(e1)[shared.names]
    dimtypes.e2 <- dimtypes(e2)[shared.names]
    different <- dimtypes.e1 != dimtypes.e2
    if (any(different))
        stop(gettextf("%s dimensions have different dimtypes : %s versus %s",
                      paste(dQuote(shared.names[different]), collapse = ", "),
                      paste(dQuote(dimtypes.e1[different]), collapse = ", "),
                      paste(dQuote(dimtypes.e2[different]), collapse = ", ")))
    TRUE
}

## HAS_TESTS
## Check whether all names from 'y' are contained in the names from 'x'.
## If they are, return TRUE; if not, raise an error.
## However, if ignoreIterations is TRUE, allow 'y' to have a
## dimension with dimtype 'iteration' that 'x' does not.
containsNames <- function(x, y, ignoreIterations = TRUE) {
    names.y <- names(y)
    if (ignoreIterations) {
        i.iter <- match("iteration", dimtypes(y), nomatch = 0L)
        if (i.iter > 0L)
            names.y <- names.y[-i.iter]
    }
    names.x <- names(x)
    not.in.x <- setdiff(names.y, names.x)
    n.not.in.x <- length(not.in.x)
    if (n.not.in.x > 0L) {
        stop(sprintf(ngettext(n.not.in.x,
                              "one object has dimension [%s] that other does not",
                              "one object has dimensions [%s] that other does not"),
                     paste(dQuote(not.in.x), collapse = ", ")))
    }
    TRUE
}

## HAS_TESTS
## If 'y' has a dimension with dimtype "Iteration", and 'x' does
## not, add that dimension on to 'x'.
copyIterDim <- function(x, y) {
    x.has.iter <- "iteration" %in% dimtypes(x)
    i.iter.y <- match("iteration", dimtypes(y), nomatch = 0L)
    y.has.iter <- i.iter.y > 0L
    if (y.has.iter && !x.has.iter) {
        names.new <- c(names(x), names(y)[i.iter.y])
        dimtypes.new <- c(dimtypes(x, use.names = FALSE), "iteration")
        DimScales.new <- c(DimScales(x, use.names = FALSE),
                           DimScales(y, use.names = FALSE)[i.iter.y])
        metadata.new <- methods::new("MetaData",
                            nms = names.new,
                            dimtypes = dimtypes.new,
                            DimScales = DimScales.new)
        .Data.new <- rep(x@.Data, times = dim(y)[i.iter.y])
        .Data.new <- array(.Data.new,
                           dim = dim(metadata.new),
                           dimnames = dimnames(metadata.new))
        x <- methods::new(class(x), .Data = .Data.new, metadata = metadata.new)
    }
    x
}

## HAS_TESTS
## If 'y' has a dimension with length 0, and 'x' does
## not, add that dimension on to 'x'.  'x' is "Counts"
copyZeroDim <- function(x, y) {
    if (!methods::is(x, "Counts"))
        stop(gettextf("'%s' has class \"%s\"",
                      "x", class(x)))
    names.x <- names(x)
    names.y <- names(y)
    dim.y <- dim(y)
    names.zero.y <- names.y[dim.y == 0]
    names.zero.not.in.x <- setdiff(names.zero.y, names.x)
    n.zero.not.in.x <- length(names.zero.not.in.x)
    if (n.zero.not.in.x > 0L) {
        dimtypes.x <- dimtypes(x, use.names = FALSE)
        dimtypes.y <- dimtypes(y, use.names = FALSE)
        DimScales.x <- DimScales(x, use.names = FALSE)
        DimScales.y <- DimScales(y, use.names = FALSE)
        i.zero.not.in.x <- match(names.zero.not.in.x, names.y)
        dimtypes.zero.not.in.x <- dimtypes.y[i.zero.not.in.x]
        DimScales.zero.not.in.x <- DimScales.y[i.zero.not.in.x]
        names.x <- c(names.x, names.zero.not.in.x)
        dimtypes.x <- c(dimtypes.x, dimtypes.zero.not.in.x)
        DimScales.x <- c(DimScales.x, DimScales.zero.not.in.x)
        metadata <- methods::new("MetaData",
                        nms = names.x,
                        dimtypes = dimtypes.x,
                        DimScales = DimScales.x)
        .Data <- if (is.integer(x)) integer() else numeric()
        .Data <- array(.Data,
                       dim = dim(metadata),
                       dimnames = dimnames(metadata))
        x <- methods::new("Counts", .Data = .Data, metadata = metadata)
    }
    x
}

## HAS_TESTS
doesNotHaveQuantiles <- function(object) {
    i.quantile <- match("quantile", dimtypes(object), nomatch = 0L)
    has.quantile <- i.quantile > 0L
    if (has.quantile)
        stop(gettextf("dimension \"%s\" has dimtype \"%s\"",
                      names(object)[i.quantile], "quantile"))
    TRUE
}

## HAS_TESTS
haveNamesInCommon <- function(e1, e2, ignoreIterations = TRUE) {
    names.in.common <- intersect(names(e1), names(e2))
    n.names.in.common <- length(names.in.common)
    if (n.names.in.common == 0L)
        stop(gettext("no dimensions in common"))
    if (n.names.in.common == 1L && ignoreIterations) {
        both.one.dim <- (length(names(e1)) == 1L) && (length(names(e2)) == 1L)
        if (!both.one.dim) {
            dimtype <- dimtypes(e1, use.names = TRUE)[[names.in.common]]
            if (identical(dimtype, "iteration"))
                stop(gettextf("no dimensions in common (apart from dimension with dimtype \"%s\")",
                              "iteration"))
        }
    }
    TRUE
}

## HAS_TESTS
## 'x' and 'y' are objects of class "Intervals".  Check that, within the region
## covered by 'x', 'x' has breaks wherever 'y' does, and possibly elsewhere.
## Return TRUE if it does; otherwise raise an error.
internalDetailGreaterOrEqual <- function(x, y) {
    dv.x <- dimvalues(x)
    dv.y <- dimvalues(y)
    if (length(dv.y) > 0L) {
        if (length(dv.x) > 0L) {
            dv.y.internal <- dv.y[(dv.y >= min(dv.x)) & (dv.y <= max(dv.x))]
            not.in.x <- setdiff(dv.y.internal, dv.x)
            n.not.in.x <- length(not.in.x)
            if (n.not.in.x > 0L)
                stop(sprintf(ngettext(n.not.in.x,
                                      "one dimension has break [%s] that other does not",
                                      "one dimension has breaks [%s] that other does not"),
                             paste(sort(not.in.x), collapse = ", ")))
        }
        else
            stop(gettextf("one dimension has %s intervals but other has none",
                          length(y)))
    }
    TRUE
}

## HAS_TESTS
isPositiveScalar <- function(object) {
    identical(length(object), 1L) &&
    is.null(dim(object)) &&
    is.numeric(object) &&
    !is.na(object) &&
    object > 0L
}

## HAS_TESTS
## 'e1' and 'e2' are objects of class "Intervals".  Check that 'e1' starts
## and ends at the same places as 'e2'.  Return TRUE if it does;
## otherwise raise an error.
limitsEqual <- function(e1, e2) {
    dv1 <- dimvalues(e1)
    dv2 <- dimvalues(e2)
    n1 <- length(dv1)
    n2 <- length(dv2)
    ## max and min tests, and error messages, only make sense if there is a
    ## positive number of dimvalues, so handle zero-dimscale cases separately
    if (n1 > 0L && n2 > 0L) {
        if (min(dv1) != min(dv2))
            stop(gettextf("one dimension starts at %s and other starts at %s",
                          min(dv1), min(dv2)))
        if (max(dv1) != max(dv2))
            stop(gettextf("one dimension ends at %s and other ends at %s",
                          max(dv1), max(dv2)))
    }
    else if (n1 == 0L && n2 > 0L) {
        stop(sprintf(ngettext(length(e2),
                              "one dimension has %s interval but other has none",
                              "one dimension has %s intervals but other has none"),
                      length(e2)))
    }
    else if (n1 > 0L && n2 == 0L) {
        stop(sprintf(ngettext(length(e1),
                              "one dimension has %s interval but other has none",
                              "one dimension has %s intervals but other has none"),
                      length(e1)))
    }
    TRUE
}

## HAS_TESTS
## Should only be called when 'x' can be subsetted.
## 'x' and 'y' are objects of class "Intervals".  Check that 'x' starts
## before or at the same point as 'y' and ends at the same point or after 'y'.
## Return TRUE if it does; otherwise raise an error.
limitsGreaterOrEqual <- function(x, y) {
    dv.x <- dimvalues(x)
    dv.y <- dimvalues(y)
    nx <- length(dv.x)
    ny <- length(dv.y)
    ## max and min tests, and error messages, only make sense if there is a
    ## positive number of dimvalues, so handle zero-dimscale cases separately
    if (nx > 0L && ny > 0L) {
        if (min(dv.y) < min(dv.x))
            stop(gettextf("one dimension starts at %s and other starts at %s",
                          min(dv.x), min(dv.y)))
        if (max(dv.y) > max(dv.x))
            stop(gettextf("one dimension ends at %s and other ends at %s",
                          max(dv.x), max(dv.y)))
    }
    else if (nx == 0L && ny > 0L) {
        stop(sprintf(ngettext(length(y),
                              "one dimension has %s interval but other has none",
                              "one dimension has %s intervals but other has none"),
                     length(y)))
    }
    ## Test passes if length(dv.y) is 0.
    TRUE
}


## FUNCTIONS FOR MANIPULATING METADATA #########################################

## HAS_TESTS
#' @rdname exported-not-api
#' @export
addIterationsToMetadata <- function(object, iterations) {
    if (!methods::is(object, "MetaData"))
        stop(gettextf("'%s' has class \"%s\"",
                      "object", class(object)))
    if (any(is.na(iterations)))
        stop(gettextf("'%s' has missing values",
                      "iterations"))
    if (any(round(iterations) != iterations))
        stop(gettextf("'%s' has non-integer values",
                      "iterations"))
    iterations <- as.integer(iterations)
    if (any(iterations <= 0))
        stop(gettextf("'%s' has negative values",
                      "iterations"))
    if (any(duplicated(iterations)))
        stop(gettextf("'%s' has duplicates",
                      "iterations"))
    nms <- names(object)
    dimtypes <- dimtypes(object, use.names = FALSE)
    DimScales <- DimScales(object, use.names = FALSE)
    if ("iteration" %in% dimtypes)
        stop(gettextf("'%s' already has dimension with dimtype \"%s\"",
                      "object", "iteration"))
    if ("quantile" %in% dimtypes)
        stop(gettextf("'%s' has dimension with dimtype \"%s\"",
                      "object", "quantile"))
    nms <- make.unique(c(nms, "iteration"))
    dimtypes <- c(dimtypes, "iteration")
    DimScale <- methods::new("Iterations", dimvalues = iterations)
    DimScales <- c(DimScales, list(DimScale))
    methods::new("MetaData",
        nms = nms,
        dimtypes = dimtypes,
        DimScales = DimScales)
}

## HAS_TESTS
incrementDimvaluesForTimeUnits <- function(dimvalues, forward, n) {
    n.dv <- length(dimvalues)
    if (n.dv >= 2L) {
        for (unit in c("quarter", "month", "day")) {
            time.units <- timeUnitsFromDimScales(dimvalues,
                                                 unit = unit,
                                                 successive = TRUE)
            if (!is.null(time.units)) {
                if (forward) {
                    dates <- seq.Date(from = time.units[n.dv],
                                      by = unit,
                                      length.out = n + 1L)
                    dates <- dates[-1L]
                }
                else {
                    dates <- seq.Date(from = time.units[1L],
                                      by = paste(-1, unit),
                                      length.out = n + 1L)
                    dates <- dates[-1L]
                    dates <- rev(dates)
                }
                ans <- dateToFracYear(dates)
                return(ans)
            }
        }
    }
    NULL
}



## HAS_TESTS
## Assume that metadata and transforms come from object of class "Values",
## and that 'canMakePairCompatible' has returned TRUE
mergeMetadata <- function(metadata1, metadata2, transform1, transform2) {
    names1 <- names(metadata1)
    names2 <- names(metadata2)
    dimtypes1 <- dimtypes(metadata1)
    dimtypes2 <- dimtypes(metadata2)
    DimScales1 <- DimScales(metadata1)
    DimScales2 <- DimScales(metadata2)
    dims1 <- transform1@dims
    dims2 <- transform2@dims
    names <- character(length = length(dims1))
    dimtypes <- character(length = length(dims1))
    DimScales <- vector(mode = "list", length = length(dims1))
    for (i in seq_along(names)) {
        d1 <- dims1[i]
        d2 <- dims2[i]
        if (d1 > 0L) {
            names[i] <- names1[d1]
            dimtypes[i] <- dimtypes1[d1]
            if (d2 > 0L)
                DimScales[[i]] <- mergeDimScales(e1 = DimScales1[[d1]],
                                                 e2 = DimScales2[[d2]])
            else
                DimScales[[i]] <- DimScales1[[d1]]
        }
        else {
            names[i] <- names2[d2]
            dimtypes[i] <- dimtypes2[d2]
            DimScales[[i]] <- DimScales2[[d2]]
        }
    }
    methods::new("MetaData",
        nms = names,
        dimtypes = dimtypes,
        DimScales = DimScales)
}

messageAboutPairSubsetting <- function(pair) {
    if (transformInvolvesSubsetting(pair[[1L]]))
        message(gettextf("omitted some elements of first object"))
    if (transformInvolvesSubsetting(pair[[2L]]))
        message(gettextf("omitted some elements of second object"))
}


## CONCORDANCES ###################################################################





## HAS_TESTS
chain2 <- function(e1, e2) {
    classif1 <- classifications(e1)
    classif2 <- classifications(e2)
    values1 <- getConcValues(e1)
    values2 <- getConcValues(e2)
    i.classif <- match(classif1, classif2, nomatch = 0L)
    match.on.both <- identical(i.classif, 1:2) || identical(i.classif, 2:1)
    if (match.on.both)
        stop(gettextf("both concordances use classifications \"%s\" and \"%s\"",
                      classif1[1L], classif1[2L]))
    match.on.neither <- identical(i.classif, c(0L, 0L))
    if (match.on.neither)
        stop(gettextf("concordances have no classifications in common : \"%s\" and \"%s\" vs \"%s\" and \"%s\"",
                      classif1[1L], classif1[2L], classif2[1L], classif2[2L]))
    match.uses.first.col.e1 <- i.classif[1L] > 0L
    if (match.uses.first.col.e1 && methods::is(e1, "ManyToOne"))
        stop(gettextf("attempt to merge using '%s' classification [\"%s\"] of first concordance",
                      "from", classif1[1L]))
    match.uses.first.col.e2 <- identical(max(i.classif), 1L)
    if (!match.uses.first.col.e2 && methods::is(e2, "ManyToOne"))
        stop(gettextf("attempt to merge using '%s' classification [\"%s\"] of second concordance",
                      "to", classif2[2L]))
    i.match.1 <- 2L - match.uses.first.col.e1
    i.match.2 <- 2L - match.uses.first.col.e2
    values.match.1 <- values1[ , i.match.1]
    values.match.2 <- values2[ , i.match.2]
    if (length(setdiff(values.match.1, values.match.2)) > 0L) {
        classif.match <- classif1[2L - match.uses.first.col.e1]
        stop(gettextf("two versions of classification \"%s\" contain different values",
                      classif.match))
    }
    i.keep.1 <- 1L + match.uses.first.col.e1
    i.keep.2 <- 1L + match.uses.first.col.e2
    values.keep.1 <- values1[ , i.keep.1]
    values.keep.2 <- values2[ , i.keep.2]
    classif.keep.1 <- classif1[i.keep.1]
    classif.keep.2 <- classif2[i.keep.2]
    i <- match(values.match.1, values.match.2)
    values <- cbind(values.keep.1, values.keep.2[i])
    colnames(values) <- c(classif.keep.1, classif.keep.2)
    Concordance(values)
}


## HAS_TESTS
splice2 <- function(e1, e2) {
    classif1 <- classifications(e1)
    classif2 <- classifications(e2)
    values1 <- getConcValues(e1)
    values2 <- getConcValues(e2)
    i.classif <- match(classif1, classif2, nomatch = 0L)
    if (identical(i.classif, 1:2))
        object <- rbind(values1, values2)
    else if (identical(i.classif, 2:1))
        object <- rbind(values1, values2[, 2:1])
    else
        stop(gettextf("classifications do not match : \"%s\" and \"%s\" vs \"%s\" and \"%s\"",
                      classif1[1L], classif1[2L], classif2[1L], classif2[2L]))
    colnames(object) <- classif1
    Concordance(object)
}

## HAS_TESTS
tidyObjectToTranslate <- function(object) {
    if (any(is.na(object)))
        stop(gettextf("'%s' has missing values", "object"))
    ans <- as.character(object)
    if (any(is.na(ans)))
        stop(gettextf("NAs created during coercion to type \"%s\"",
                      "character"))
    ans
}



## HAS_TESTS
#' Chain concordances.
#' 
#' Combine two or more concordances to form a new concordance via a
#' \code{\link[base]{merge}}-like operation.  For instance, if \code{x} is a
#' concordance between classification \code{c1} and classification \code{c2},
#' and \code{y} is a concordance between \code{c2} and \code{c3}, then
#' \code{chain(x, y)} creates a concordance between \code{c1} and \code{c3}.
#' 
#' Two concordances that are to be chained together must have a classification
#' in common.  In addition, the two versions of the common classification must
#' have the same set of values, though not necessarily in the same order.
#' Concordances are merged from left to right.
#' 
#' @param \dots Objects of class \code{\linkS4class{Concordance}}.
#' 
#' @return An object of class \code{\linkS4class{Concordance}}.
#'
#' @seealso \code{splice} combines concordances via a
#' \code{\link[base]{rbind}}-like operation.
#' 
#' @examples
#' x <- cbind(c1 = c("a", "b", "c"), c2 = c("e", "f", "f"))
#' x <- Concordance(x)
#' y <- cbind(c2 = c("e", "f"), c3 = c("i", "j"))
#' y <- Concordance(y)
#' z <- cbind(c3 = c("i", "j"), c4 = c("m", "l"))
#' z <- Concordance(z)
#' x
#' y
#' z
#' chain(x, y)
#' chain(x, y, z)
#' 
#' x <- cbind(c1 = c("a", "b"), c2 = c("e", "f"))
#' x <- Concordance(x)
#' y <- cbind(c3 = c("i", "j"), c2 = c("e", "f"))
#' y <- Concordance(y)
#' z <- cbind(c3 = c("i", "j", "k"), c2 = c("e", "e", "f"))
#' z <- Concordance(z)
#' x
#' y
#' z
#' ## The order of the columns is irrelevant in a "OneToOne" object
#' chain(x, y)
#' ## ...but not a "ManyToOne" object
#' \dontrun{chain(x, z)}
#' @export
chain <- function(...) {
    names.objects <- as.character(substitute(list(...)))[-1L]
    objects <- list(...)
    n <- length(objects)
    if (n == 0L)
        return(NULL)
    for (i in seq_len(n)) {
        object <- objects[[i]]
        if (!methods::is(object, "Concordance")) {
            if (identical(length(dim(object)), 2L)) {
                object <- tryCatch(Concordance(object),
                                   error = function(e) e)
                if (methods::is(object, "error"))
                    stop(gettextf("could not make concordance from '%s' : %s",
                                  names.objects[i], object$message))
                objects[[i]] <- object
            }
            else
                stop(gettextf("'%s' has class \"%s\"",
                              names.objects[i], class(object)))
        }
    }
    ans <- objects[[1L]]
    if (n >= 2L) {
        for (i in 2:n)
            ans <- chain2(e1 = ans, e2 = objects[[i]])
    }
    ans
}


## HAS_TESTS
#' Splice concordances.
#' 
#' Combine two or more concordances to form a new concordance, via
#' a \code{\link[base]{rbind}}-like operation.
#' 
#' Concordances that are to be spliced together must have classifications with
#' the same names.  The result of the splicing must also be a many-to-one or
#' one-to-one relationship between classifications.  Duplicate rows are
#' deleted.
#' 
#' @param \dots Objects of class \code{\linkS4class{Concordance}}.
#'
#' @return An object of class \code{\linkS4class{Concordance}}.
#' 
#' @seealso \code{\link{chain}} combines concordances via a
#' \code{\link[base]{merge}}-like operation.
#'
#' @examples
#' x <- cbind(c1 = c("a", "b", "c"), c2 = c("q", "r", "r"))
#' x <- Concordance(x)
#' y <- cbind(c1 = c("e", "f"), c2 = c("s", "t"))
#' y <- Concordance(y)
#' x
#' y
#' splice(x, y)
#' 
#' x <- cbind(c1 = c("a", "b", "c"), c2 = c("q", "r", "r"))
#' x <- Concordance(x)
#' y <- cbind(c2 = c("r", "s"), c1 = c("c", "d"))
#' y <- Concordance(y)
#' x
#' y  ## columns in different order, and rows overlap with 'x'
#' splice(x, y)
#' 
#' @export
splice <- function(...) {
    objects <- list(...)
    not.concordance <- !sapply(objects, methods::is,"Concordance")
    if (any(not.concordance)) {
        i.first.wrong <- which(not.concordance)[1L]
        class.first.wrong <- class(objects[[i.first.wrong]])
        stop(gettextf("object with class \"%s\"",
                      class.first.wrong))
    }
    n <- length(objects)
    if (n == 0L)
        return(NULL)
    ans <- objects[[1L]]
    if (n >= 2L) {
        for (i in 2:n)
            ans <- splice2(e1 = ans,
                           e2 = objects[[i]])
    }
    ans
}


## HELPER FUNCTIONS FOR DEMOGRAPHIC ACCOUNTS ######################################


## HAS_TESTS
ageDimBirthsCompatibleWithPopn <- function(name, DimScale, namesPopn,
                                           dimtypesPopn, DimScalesPopn,
                                           nameComponent) {
    ## 'population' has name
    i.age.popn <- match(name, namesPopn, nomatch = 0L)
    has.age.popn <- i.age.popn > 0L
    if (!has.age.popn)
        return(gettextf("'%s' has dimension \"%s\" but '%s' does not",
                        nameComponent, name, "population"))
    ## dimension in 'population' has dimtype "age"
    dimtype.popn <- dimtypesPopn[i.age.popn]
    if (!identical(dimtype.popn, "age"))
        return(gettextf("\"%s\" dimension of '%s' has %s \"%s\" but \"%s\" dimension of '%s' has %s \"%s\"",
                        name, nameComponent, "dimtype", "age",
                        name, "population", "dimtype", dimtype.popn))
    ## dimvalues aligned
    DimScale.popn <- DimScalesPopn[[i.age.popn]]
    dv.births <- dimvalues(DimScale)
    dv.popn <- dimvalues(DimScale.popn)
    dv.popn.contains.dv.births <- all(dv.births %in% dv.popn)
    in.overlap <- (dv.popn >= min(dv.births)) & (dv.popn <= max(dv.births))
    dv.births.contains.dv.popn <- all(dv.popn[in.overlap] %in% dv.births)
    if (dv.popn.contains.dv.births && dv.births.contains.dv.popn)
        TRUE
    else
        gettextf("\"%s\" dimensions have incompatible dimscales",
                 name)
}

## HAS_TESTS
agePopnForwardUpperTri <- function(population) {
    .Data <- population@.Data
    dim <- dim(population)
    names <- names(population)
    dimtypes <- dimtypes(population,
                         use.names = FALSE)
    DimScales <- DimScales(population,
                           use.names = FALSE)
    i.time <- match("time", dimtypes)
    i.age <- match("age", dimtypes)
    n.time <- dim[i.time]
    n.age <- dim[i.age]
    DS.time <- DimScales[[i.time]]
    DS.age <- DimScales[[i.age]]
    dv.time <- dimvalues(DS.time)
    dv.age <- dimvalues(DS.age)
    if (any(diff(dv.time) == 1L)) {
        message(gettextf("assuming '%s' is %s\n",
                         "labelStart", new("Intervals", isAge = FALSE)@labelStart))
    }
    DS.time.ans <- methods::new("Intervals",
                                dimvalues = dv.time,
                                isAge = FALSE)
    DimScales.ans <- replace(DimScales,
                             list = i.time,
                             values = list(DS.time.ans))
    dv.age.ans <- dv.age[-c(1L, n.age + 1L)]
    DS.age.ans <- methods::new("Points",
                               dimvalues = dv.age.ans)
    DimScales.ans <- replace(DimScales.ans,
                             list = i.age,
                             values = list(DS.age.ans))
    metadata.ans <- methods::new("MetaData",
                                 nms = names,
                                 dimtypes = dimtypes,
                                 DimScales = DimScales.ans)
    ind <- ((slice.index(.Data, MARGIN = i.time) != n.time)
        & (slice.index(.Data, MARGIN = i.age) != n.age))
    .Data.ans <- .Data[ind]
    .Data.ans <- array(.Data.ans,
                       dim = dim(metadata.ans),
                       dimnames = dimnames(metadata.ans))
    methods::new("Counts",
                 .Data = .Data.ans,
                 metadata = metadata.ans)
}

## HAS_TESTS
checkAdjustAndScale <- function(adjust, scale) {
    if (!is.logical(adjust))
        stop(gettextf("'%s' does not have type \"%s\"",
                      "adjust", "logical"))
    if (!identical(length(adjust), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "adjust", 1L))
    if (adjust) {
        if (!is.numeric(scale))
            stop(gettextf("'%s' is non-numeric",
                          "scale"))
        if (!identical(length(scale), 1L))
            stop(gettextf("'%s' does not have length %d",
                          "scale", 1L))
        if (scale <= 0)
            stop(gettextf("'%s' is non-positive",
                          "scale"))
    }
    NULL
}

## HAS_TESTS
checkAndTidyComponent <- function(object,
                                  name,
                                  requireInteger = TRUE,
                                  allowNegatives = FALSE,
                                  allowOrig = FALSE,
                                  allowParent = FALSE,
                                  allowTriangles = TRUE,
                                  triangles = TRUE) {
    ## extract metadata
    dimtypes <- dimtypes(object, use.names = FALSE)
    DimScales <- DimScales(object, use.names = FALSE)
    ## integer
    if (requireInteger)
        object <- tryCatch(toInteger(object),
                           error = function(e) e)
    if (methods::is(object, "error"))
        stop(gettextf("'%s' invalid : %s",
                      name, object$message))
    ## time, age, cohort dimensions
    i.time <- match("time", dimtypes, nomatch = 0L)
    i.age <- match("age", dimtypes, nomatch = 0L)
    i.cohort <- match("cohort", dimtypes, nomatch = 0L)
    i.triangle <- match("triangle", dimtypes, nomatch = 0L)
    has.time <- i.time > 0L
    has.age <- i.age > 0L
    has.cohort <- i.cohort > 0L
    has.triangle <- i.triangle > 0L
    if (!has.time)
        stop(gettextf("'%s' does not have dimension with dimtype \"%s\"",
                      name, "time"))
    DimScale.time <- DimScales[[i.time]]
    if (!methods::is(DimScale.time, "Intervals"))
        stop(gettextf("dimension of '%s' with dimtype \"%s\" has dimscale \"%s\"",
                      name, "time", class(DimScale.time)))
    if (has.age) {
        DimScale.age <- DimScales[[i.age]]
        if (!methods::is(DimScale.age, "Intervals"))
            stop(gettextf("dimension of '%s' with dimtype \"%s\" has dimscale \"%s\"",
                          name, "age", class(DimScale.age)))
        if (is.infinite(dimvalues(DimScale.age)[1L]))
            stop(gettextf("'%s' invalid : first interval of dimension with dimtype \"%s\" is open",
                          name, "age"))
    }
    if (has.cohort)
        stop(gettextf("'%s' has dimension with dimtype \"%s\"",
                      name, "cohort"))
    ## origin-destination
    if (!allowOrig) {
        is.orig.dest <- any(dimtypes == "origin")
        if (is.orig.dest)
            stop(gettextf("'%s' has dimensions with dimtypes \"%s\" and \"%s\"",
                          name, "origin", "destination"))
    }
    ## parent-child
    if (!allowParent) {
        is.parent.child <- any(dimtypes == "parent")
        if (is.parent.child)
            stop(gettextf("'%s' has dimensions with dimtypes \"%s\" and \"%s\"",
                          name, "parent", "child"))
    }
    ## regular
    is.regular <- tryCatch(hasRegularAgeTime(object),
                           error = function(e) e)
    if (methods::is(is.regular, "error"))
        stop(gettextf("'%s' does not have regular age-time plan : %s",
                      name, is.regular$message))
    ## positive length
    if (length(object) == 0L)
        stop(gettextf("'%s' has length %d",
                      name, 0L))
    ## negatives
    if (!allowNegatives) {
        if (any(object[!is.na(object)] < 0L))
            stop(gettextf("'%s' has negative values",
                          name))
    }
    ## allow triangles
    if (!allowTriangles && has.triangle)
        stop(gettextf("'%s' has Lexis triangles",
                      name))
    ## triangles
    if (has.age) {
        if (!has.triangle && triangles)
            object <- splitTriangles(object)
    }
    object
}

## HAS_TESTS
checkNamesComponents <- function(names, componentType) {
    if (is.null(names))
        stop(gettextf("'%s' does not have names",
                      componentType))
    if (any(is.na((names))))
        stop(gettextf("names for '%s' have missing values",
                      componentType))
    if (!all(nzchar(names)))
        stop(gettextf("names for '%s' have blanks",
                      componentType))
    if (any(duplicated(names)))
        stop(gettextf("names for '%s' have duplicates",
                      componentType))
    NULL
}

## HAS_TESTS
derivePopnMoveNoAge <- function(object, adjust, scale) {
    population <- object@population
    components <- object@components
    names.components <- object@namesComponents
    n.comp <- length(components)
    is.internal <- sapply(components, methods::is, "Internal")
    is.births <- sapply(components, methods::is, "Births")
    has.births <- any(is.births)
    if (has.births) {
        i.births <- which(is.births)
        births <- components[[i.births]]
        mapping.births <- makeMappingBirths(births)
        scale.extra.dims.births <- length(mapping.births) %/% max(mapping.births)
    }
    else
        i.births <- 0L
    has.internal <- any(is.internal)
    if (has.internal) {
        i.internal <- which(is.internal)
        is.orig.dest <- methods::is(components[[i.internal]], "HasOrigDest")
    }
    else
        i.internal <- 0L
    dim.popn <- dim(population)
    i.time.popn <- match("time", dimtypes(population))
    i.time.comps <- sapply(components, function(x) match("time", dimtypes(x)))
    n.time.popn <- dim.popn[i.time.popn]
    index.time.popn <- makeDimtypeIndex(population, dimtype = "time")
    index.time.comps <- lapply(components, makeDimtypeIndex, dimtype = "time")
    slices.comp <- vector(mode = "list", length = n.comp)
    is.pos.increment.comp <- sapply(components, isPositiveIncrement)
    for (it in seq_len(n.time.popn - 1L)) {
        slice.popn.start <- slab(population,
                                 dimension = i.time.popn,
                                 elements = it,
                                 drop = FALSE)
        popn.end <- as.integer(slice.popn.start@.Data)
        for (ic in seq_len(n.comp)) {
            component <- components[[ic]]
            i.time.comp <- i.time.comps[ic]
            slice.comp <- slab(component,
                               dimension = i.time.comp,
                               elements = it,
                               drop = FALSE)
            slices.comp[[ic]] <- slice.comp
            increment <- incrementInteger(slice.comp)
            is.pos.increment <- is.pos.increment.comp[ic]
            if (is.pos.increment)
                popn.end <- popn.end + increment
            else
                popn.end <- popn.end - increment
        }
        if (any(popn.end < 0L) && !adjust)
            stop(gettext("population has negative values"))
        updated.comp <- rep(FALSE, times = n.comp)
        while (any(popn.end < 0L)) {
            ic <- safeSample1(seq_len(n.comp))
            updated.comp[ic] <- TRUE
            if (ic == i.births) {
                multiplier <- ifelse(popn.end < 0L, scale, 0)
                multiplier <- multiplier[mapping.births]
                lambda <- multiplier * slices.comp[[ic]]@.Data / scale.extra.dims.births
                diff <- stats::rpois(n = length(lambda), lambda = lambda)
                diff.ag <- tapply(diff,
                                  INDEX = mapping.births,
                                  FUN = sum)
                diff.ag <- as.integer(diff.ag)
                slices.comp[[ic]]@.Data[] <- slices.comp[[ic]]@.Data + diff
                popn.end <- popn.end + diff.ag
            }
            else if (ic == i.internal) {
                if (is.orig.dest) {
                    increment.old <- incrementInteger(slices.comp[[ic]])
                    multiplier <- stats::runif(n = 1L,
                                               min = max(0.1, 1 - 2 * scale),
                                               max = 1)
                    slices.comp[[ic]]@.Data[] <- as.integer(multiplier * slices.comp[[ic]]@.Data)
                }
                else {
                    if (stats::runif(n = 1L) < scale)
                        slices.comp[[ic]]@.Data[] <- 0L
                }
                increment.new <- incrementInteger(slices.comp[[ic]])
                popn.end <- popn.end + increment.new - increment.old
            }
            else {
                multiplier <- ifelse(popn.end < 0L, scale, 0)
                is.pos.increment <- is.pos.increment.comp[ic]
                lambda <- multiplier * abs(slices.comp[[ic]]@.Data) # abs to deal with NetMovements
                diff <- stats::rpois(n = length(lambda), lambda = lambda)
                if (is.pos.increment) {
                    slices.comp[[ic]]@.Data[] <- slices.comp[[ic]]@.Data + diff
                    popn.end <- popn.end + diff
                }
                else {                    
                    diff <- pmin(slices.comp[[ic]]@.Data, diff)
                    slices.comp[[ic]]@.Data[] <- slices.comp[[ic]]@.Data - diff
                    popn.end <- popn.end + diff
                }
            }
        }
        index <- index.time.popn == it + 1L
        population@.Data[index] <- popn.end
        for (ic in seq_len(n.comp)) {
            if (updated.comp[ic]) {
                index <- index.time.comps[[ic]] == it
                components[[ic]]@.Data[index] <- slices.comp[[ic]]@.Data
            }
        }
    }
    new("Movements",
        population = population,
        components = components,
        namesComponents = names.components)
}

## HAS_TESTS
derivePopnMoveHasAge <- function(object, adjust, scale) {
    population <- object@population
    components <- object@components
    names.components <- object@namesComponents
    n.comp <- length(components)
    is.internal <- sapply(components, methods::is, "Internal")
    is.births <- sapply(components, methods::is, "Births")
    has.births <- any(is.births)
    if (has.births) {
        i.births <- which(is.births)
        births <- components[[i.births]]
        mapping.births <- makeMappingBirths(births)
        scale.extra.dims.births <- length(mapping.births) %/% max(mapping.births)
    }
    else
        i.births <- 0L
    has.internal <- any(is.internal)
    if (has.internal) {
        i.internal <- which(is.internal)
        is.orig.dest <- methods::is(components[[i.internal]], "HasOrigDest")
    }
    else
        i.internal <- 0L
    i.comp <- seq_len(n.comp)
    i.comp.not.births <- setdiff(i.comp, i.births)
    dim.popn <- dim(population)
    i.time.popn <- match("time", dimtypes(population))
    i.time.comps <- sapply(components, function(x) match("time", dimtypes(x)))
    n.time.popn <- dim.popn[i.time.popn]
    i.age.popn <- match("age", dimtypes(population))
    i.age.comps <- sapply(components, function(x) match("age", dimtypes(x)))
    n.age <- dim.popn[i.age.popn]
    i.triangle.comps <- sapply(components, function(x) match("triangle", dimtypes(x)))
    index.time.popn <- makeDimtypeIndex(population, dimtype = "time")
    index.time.comps <- lapply(components, makeDimtypeIndex, dimtype = "time")
    index.age.popn <- makeDimtypeIndex(population, dimtype = "age")
    index.age.comps <- lapply(components, makeDimtypeIndex, dimtype = "age")
    index.triangle.comps <- lapply(components, makeDimtypeIndex, dimtype = "triangle")
    slices.comp <- vector(mode = "list", length = n.comp)
    slices.comp.a <- vector(mode = "list", length = n.comp)
    slices.comp.a.low <- vector(mode = "list", length = n.comp)
    slices.comp.a.up <- vector(mode = "list", length = n.comp)
    is.pos.increment.comp <- sapply(components, isPositiveIncrement)
    length.accession <- length(population) %/% (n.time.popn * n.age)
    for (it in seq_len(n.time.popn - 1L)) {
        slice.popn.start <- slab(population,
                                 dimension = i.time.popn,
                                 elements = it,
                                 drop = FALSE)
        accession <- rep(0L, times = length.accession)
        for (ic in i.comp) {
            component <- components[[ic]]
            i.time.comp <- i.time.comps[ic]
            slices.comp[[ic]] <- slab(component,
                                      dimension = i.time.comp,
                                      elements = it,
                                      drop = FALSE)
        }
        for (ia in seq_len(n.age)) {
            slice.popn.start.a <- slab(slice.popn.start,
                                       dimension = i.age.popn,
                                       elements = ia,
                                       drop = FALSE)
            for (ic in i.comp) {
                if (ic != i.births) {
                    slices.comp.a[[ic]] <- slab(slices.comp[[ic]],
                                                dimension = i.age.comps[ic],
                                                elements = ia,
                                                drop = FALSE)
                    slices.comp.a.low[[ic]] <- slab(slices.comp.a[[ic]],
                                                    dimension = i.triangle.comps[ic],
                                                    elements = 1L,
                                                    drop = FALSE)
                    slices.comp.a.up[[ic]] <- slab(slices.comp.a[[ic]],
                                                   dimension = i.triangle.comps[ic],
                                                   elements = 2L,
                                                   drop = FALSE)
                }
            }
            ## lower triangle
            popn.end <- accession
            for (ic in i.comp) {
                if (ic == i.births) {
                    if (ia == 1L) {
                        increment <- incrementInteger(slices.comp[[ic]])
                        popn.end <- popn.end + increment
                    }
                }
                else {
                    increment <- incrementInteger(slices.comp.a.low[[ic]])
                    is.pos.increment <- is.pos.increment.comp[ic]
                    if (is.pos.increment)
                        popn.end <- popn.end + increment
                    else
                        popn.end <- popn.end - increment
                }
            }
            if (any(popn.end < 0L) && !adjust)
                stop(gettext("population has negative values"))
            updated.comp <- rep(FALSE, times = n.comp)
            while (any(popn.end < 0L)) {
                choices <- if (ia == 1L) i.comp else i.comp.not.births
                ic <- safeSample1(choices)
                updated.comp[ic] <- TRUE
                if (ic == i.births) {
                    multiplier <- ifelse(popn.end < 0L, scale, 0)
                    multiplier <- multiplier[mapping.births]
                    lambda <- multiplier * slices.comp[[ic]]@.Data / scale.extra.dims.births
                    diff <- stats::rpois(n = length(lambda), lambda = lambda)
                    diff.ag <- tapply(diff,
                                      INDEX = mapping.births,
                                      FUN = sum)
                    diff.ag <- as.integer(diff.ag)
                    slices.comp[[ic]]@.Data[] <- slices.comp[[ic]]@.Data + diff
                    popn.end <- popn.end + diff.ag
                }
                else if (ic == i.internal) {
                    increment.old <- incrementInteger(slices.comp.a.low[[ic]])
                    if (is.orig.dest) {
                        multiplier <- stats::runif(n = 1L,
                                                   min = max(0.1, 1 - scale),
                                                   max = 1)
                        slices.comp.a.low[[ic]]@.Data[] <- as.integer(multiplier * slices.comp.a.low[[ic]]@.Data)
                    }
                    else {
                        if (stats::runif(n = 1L) < scale)
                            slices.comp.a.low[[ic]]@.Data[] <- 0L
                    }
                    increment.new <- incrementInteger(slices.comp.a.low[[ic]])
                    popn.end <- popn.end + increment.new - increment.old
                }
                else {
                    multiplier <- ifelse(popn.end < 0L, scale, 0)
                    is.pos.increment <- is.pos.increment.comp[ic]
                    lambda <- multiplier * abs(slices.comp.a.low[[ic]]@.Data) # abs to deal with NetMovements
                    diff <- (accession < 0L) + stats::rpois(n = length(lambda), lambda = lambda)
                    if (is.pos.increment) {
                        slices.comp.a.low[[ic]]@.Data[] <- slices.comp.a.low[[ic]]@.Data + diff
                        popn.end <- popn.end + diff
                    }
                    else {
                        diff <- pmin(slices.comp.a.low[[ic]]@.Data, diff)
                        slices.comp.a.low[[ic]]@.Data[] <- slices.comp.a.low[[ic]]@.Data - diff
                        popn.end <- popn.end + diff
                    }
                }
            }
            index <- (index.time.popn == it + 1L) & (index.age.popn == ia)
            population@.Data[index] <- popn.end
            for (ic in i.comp) {
                if (updated.comp[ic]) {
                    if (ic == i.births) {
                        index <- index.time.comps[[ic]] == it
                        components[[ic]]@.Data[index] <- slices.comp[[ic]]@.Data
                    }
                    else {
                        index <- ((index.time.comps[[ic]] == it)
                            & (index.age.comps[[ic]] == ia)
                            & (index.triangle.comps[[ic]] == 1L))
                        components[[ic]]@.Data[index] <- slices.comp.a.low[[ic]]@.Data
                    }
                }
            }
            ## upper triangle
            accession <- as.integer(slice.popn.start.a@.Data)
            for (ic in i.comp) {
                if (ic != i.births) {
                    increment <- incrementInteger(slices.comp.a.up[[ic]])
                    is.pos.increment <- is.pos.increment.comp[ic]
                    if (is.pos.increment)
                        accession <- accession + increment
                    else
                        accession <- accession - increment
                }
            }
            if (any(accession < 0L) && !adjust)
                stop(gettext("population has negative values"))
            updated.comp <- rep(FALSE, times = n.comp)
            while (any(accession < 0L)) {
                ic <- safeSample1(i.comp.not.births)
                updated.comp[ic] <- TRUE
                if (ic == i.internal) {
                    increment.old <- incrementInteger(slices.comp.a.up[[ic]])
                    if (is.orig.dest) {
                        multiplier <- stats::runif(n = 1L,
                                                   min = max(0.1, 1 - scale),
                                                   max = 1)
                        slices.comp.a.up[[ic]]@.Data[] <- as.integer(multiplier * slices.comp.a.up[[ic]]@.Data)
                    }
                    else {
                        if (stats::runif(n = 1L) < scale)
                            slices.comp.a.up[[ic]]@.Data[] <- 0L
                    }
                    increment.new <- incrementInteger(slices.comp.a.up[[ic]])
                    accession <- accession + increment.new - increment.old
                }
                else {
                    multiplier <- ifelse(accession < 0L, scale, 0)
                    is.pos.increment <- is.pos.increment.comp[ic]
                    lambda <- multiplier * abs(slices.comp.a.up[[ic]]@.Data)
                    diff <- (accession < 0L) + stats::rpois(n = length(lambda), lambda = lambda)
                    if (is.pos.increment) {
                        slices.comp.a.up[[ic]]@.Data[] <- slices.comp.a.up[[ic]]@.Data + diff
                        accession <- accession + diff
                    }
                    else {
                        diff <- pmin(slices.comp.a.up[[ic]]@.Data, diff)
                        slices.comp.a.up[[ic]]@.Data[] <- slices.comp.a.up[[ic]]@.Data - diff
                        accession <- accession + diff
                    }
                }
            }
            for (ic in i.comp) {
                if (updated.comp[ic]) {
                    index <- ((index.time.comps[[ic]] == it)
                        & (index.age.comps[[ic]] == ia)
                        & (index.triangle.comps[[ic]] == 2L))
                    components[[ic]]@.Data[index] <- slices.comp.a.up[[ic]]@.Data
                }
            }
            if (ia == n.age) {
                index <- (index.time.popn == it + 1L) & (index.age.popn == ia)
                population@.Data[index] <- population@.Data[index] + accession            
            }
        }
    }
    new("Movements",
        population = population,
        components = components,
        namesComponents = names.components)
}

## HAS_TESTS
dimCompCompatibleWithPopn <- function(name, dimtype, DimScale,
                                      namesPopn, dimtypesPopn, DimScalesPopn,
                                      isBirths, nameComponent) {
    dimtypes.with.pairs <- getDimtypesWithPairs()
    if (dimtype == "age") {
        if (isBirths) {
            ageDimBirthsCompatibleWithPopn(name = name,
                                           DimScale = DimScale,
                                           namesPopn = namesPopn,
                                           dimtypesPopn = dimtypesPopn,
                                           DimScalesPopn = DimScalesPopn,
                                           nameComponent = nameComponent)
        }
        else
            dimCompEqualToPopn(name = name,
                               dimtype = dimtype,
                               DimScale = DimScale,
                               namesPopn = namesPopn,
                               dimtypesPopn = dimtypesPopn,
                               DimScalesPopn = DimScalesPopn,
                               nameComponent = nameComponent)
    }
    else if (dimtype %in% dimtypes.with.pairs)
        pairDimCompCompatibleWithPopn(name = name,
                                      dimtype = dimtype,
                                      DimScale = DimScale,
                                      namesPopn = namesPopn,
                                      dimtypesPopn = dimtypesPopn,
                                      DimScalesPopn = DimScalesPopn,
                                      nameComponent = nameComponent)
    else if (dimtype == "time")
        timeDimCompCompatibleWithPopn(name = name,
                                      DimScale = DimScale,
                                      namesPopn = namesPopn,
                                      dimtypesPopn = dimtypesPopn,
                                      DimScalesPopn = DimScalesPopn,
                                      nameComponent = nameComponent)
    else if (dimtype == "triangle")
        TRUE
    else
        dimCompEqualToPopn(name = name,
                           dimtype = dimtype,
                           DimScale = DimScale,
                           namesPopn = namesPopn,
                           dimtypesPopn = dimtypesPopn,
                           DimScalesPopn = DimScalesPopn,
                           nameComponent = nameComponent)
}

## HAS_TESTS
dimCompEqualToPopn <- function(name, dimtype, DimScale,
                               namesPopn, dimtypesPopn, DimScalesPopn,
                               nameComponent) {
    ## population has name
    i.dim.popn <- match(name, namesPopn, nomatch = 0L)
    has.dim.popn <- i.dim.popn > 0L
    if (!has.dim.popn)
        return(gettextf("'%s' has dimension \"%s\" but '%s' does not",
                        nameComponent, name, "population"))
    ## identical dimtypes
    dimtype.popn <- dimtypesPopn[i.dim.popn]
    if (!identical(dimtype, dimtype.popn))
        return(gettextf("\"%s\" dimension of '%s' has %s \"%s\" but \"%s\" dimension of '%s' has %s \"%s\"",
                        name, nameComponent, "dimtype", dimtype,
                        name, "population", "dimtype", dimtype.popn))
    ## equal DimScales
    DimScale.popn <- DimScalesPopn[[i.dim.popn]]
    if (!isTRUE(all.equal(DimScale, DimScale.popn)))
        return(gettextf("\"%s\" dimensions have incompatible dimscales",
                        name))
    TRUE
}

## HAS_TESTS
exposureNoTriangles <- function(object) {
    .Data <- object@.Data
    dim <- dim(object)
    dimtypes <- dimtypes(object, use.names = FALSE)
    DimScales <- DimScales(object, use.names = FALSE)
    metadata <- makeMetadataForExposure(population = object,
                                        triangles = FALSE)
    i.time <- match("time", dimtypes, nomatch = 0L)
    has.time <- i.time > 0L
    if (has.time)
        i.along <- i.time
    else
        i.along <- match("age", dimtypes)
    n.along <- dim[i.along]
    DimScale.along <- DimScales[[i.along]]
    dimvalues.along <- dimvalues(DimScale.along)
    index.along <- slice.index(.Data, MARGIN = i.along)
    start <- .Data[index.along < n.along]
    end <- .Data[index.along > 1L]
    step <- diff(dimvalues.along)
    step <- step[index.along[index.along < n.along]]
    .Data <- 0.5 * step * (start + end)
    .Data <- array(.Data,
                   dim = dim(metadata),
                   dimnames = dimnames(metadata))
    methods::new("Counts", .Data = .Data, metadata = metadata)
}

## HAS_TESTS
exposureWithTriangles <- function(object) {
    .Data <- object@.Data
    dim <- dim(object)
    dimtypes <- dimtypes(object, use.names = FALSE)
    DimScales <- DimScales(object, use.names = FALSE)
    i.time <- match("time", dimtypes)
    n.time <- dim[i.time]
    time.step <- ageTimeStep(object)
    i.age <- match("age", dimtypes)
    n.age <- dim[i.age]
    DimScale.age <- DimScales[[i.age]]
    dimvalues.age <- dimvalues(DimScale.age)
    last.age.open <- is.infinite(dimvalues.age[n.age + 1L])
    index.time <- slice.index(.Data, MARGIN = i.time)
    popn.start <- .Data[index.time < n.time]
    popn.end <- .Data[index.time > 1L]
    dim.lower.upper <- replace(dim, list = i.time, values = n.time - 1L)
    lower <- array(0.5 * time.step * popn.end, dim = dim.lower.upper)
    upper <- array(0.5 * time.step * popn.start, dim = dim.lower.upper)
    metadata <- makeMetadataForExposure(population = object, triangles = TRUE)
    .Data <- c(lower, upper)
    .Data <- array(.Data,
                   dim = dim(metadata),
                   dimnames = dimnames(metadata))
    methods::new("Counts", .Data = .Data, metadata = metadata)
}

## HAS_TESTS
getDimScaleTimePopn <- function(object, name) {
    if (!methods::is(object, "Counts"))
        stop(gettextf("'%s' has class \"%s\"",
                      name, class(object)))
    dimtypes <- dimtypes(object,
                         use.names = FALSE)
    DimScales <- DimScales(object,
                           use.names = FALSE)
    i.time <- match("time", dimtypes, nomatch = 0L)
    has.time <- i.time > 0L
    if (!has.time)
        stop(gettextf("'%s' does not have a dimension with %s \"%s\"",
                      name, "dimtype", "time"))
    DimScale.comp <- DimScales[[i.time]]
    if (!methods::is(DimScale.comp, "Intervals"))
        stop(gettextf("time dimension of '%s' does not have %s \"%s\"",
                      name, "dimscale", "Intervals"))
    dimvalues <- dimvalues(DimScale.comp)
    DimScale.popn <- new("Points", dimvalues = dimvalues)
    DimScale.popn
}

## HAS_TESTS
iMinAge <- function(current, target) {
    dimtypes.current <- dimtypes(current, use.names = FALSE)
    dimtypes.target <- dimtypes(target, use.names = FALSE)
    DimScales.current <- DimScales(current, use.names = FALSE)
    DimScales.target <- DimScales(target, use.names = FALSE)
    i.age.current <- match("age", dimtypes.current, nomatch = 0L)
    i.age.target <- match("age", dimtypes.target, nomatch = 0L)
    if (i.age.current == 0L)
        stop(gettextf("'%s' does not have dimension with dimtype \"%s\"",
                      "current", "age"))
    if (i.age.target == 0L)
        stop(gettextf("'%s' does not have dimension with dimtype \"%s\"",
                      "target", "age"))
    DS.age.current <- DimScales.current[[i.age.current]]
    DS.age.target <- DimScales.target[[i.age.target]]
    dv.age.current <- dimvalues(DS.age.current)
    dv.age.target <- dimvalues(DS.age.target)
    dv.age.target <- dv.age.target[-length(dv.age.target)]
    ans <- match(dv.age.current[1L], dv.age.target, nomatch = 0L)
    if (ans == 0L)
        stop(gettextf("minimum age of '%s' not found in ages of '%s'",
                      "current", "target"))
    ans
}

## HAS_TESTS
incrementLowerTriHelper <- function(component) {
    .Data.old <- component@.Data
    dim.old <- dim(component)
    names.old <- names(component)
    dimtypes.old <- dimtypes(component,
                             use.names = FALSE)
    DimScales.old <- DimScales(component,
                               use.names = FALSE)
    i.triangle.old <- match("triangle", dimtypes.old)
    i.time.old <- match("time", dimtypes.old)
    DS.time.old <- DimScales.old[[i.time.old]]
    dv.time.old <- dimvalues(DS.time.old)
    dv.time.new <- dv.time.old[-1L]
    DS.time.new <- methods::new("Points", dimvalues = dv.time.new)
    names.new <- names.old[-i.triangle.old]
    dimtypes.new <- dimtypes.old[-i.triangle.old]
    DimScales.new <- DimScales.old[-i.triangle.old]
    i.time.new <- match("time", dimtypes.new)
    DimScales.new <- replace(DimScales.new,
                             list = i.time.new,
                             values = list(DS.time.new))
    metadata.new <- methods::new("MetaData",
                                 nms = names.new,
                                 dimtypes = dimtypes.new,
                                 DimScales = DimScales.new)
    dim.new <- dim(metadata.new)
    dimnames.new <- dimnames(metadata.new)
    ind.old <- slice.index(.Data.old, MARGIN = i.triangle.old) == 1L
    .Data.new <- .Data.old[ind.old]
    .Data.new <- array(.Data.new,
                       dim = dim.new,
                       dimnames = dimnames.new)
    methods::new("Counts",
                 .Data = .Data.new,
                 metadata = metadata.new)
}

## HAS_TESTS
incrementOpenHelper <- function(component) {
    .Data.old <- component@.Data
    dim.old <- dim(component)
    names.old <- names(component)
    dimtypes.old <- dimtypes(component,
                             use.names = FALSE)
    DimScales.old <- DimScales(component,
                               use.names = FALSE)
    i.age.old <- match("age", dimtypes.old)
    i.triangle <- match("triangle", dimtypes.old)
    i.time.old <- match("time", dimtypes.old)
    n.age <- dim.old[i.age.old]
    n.time.old <- dim.old[i.time.old]
    names.new <- names.old[-i.triangle]
    dimtypes.new <- dimtypes.old[-i.triangle]
    DS.time.old <- DimScales.old[[i.time.old]]
    dv.time.old <- dimvalues(DS.time.old)
    dv.time.new <- dv.time.old[-1L]
    DS.time.new <- methods::new("Points",
                                dimvalues = dv.time.new)
    DimScales.new <- replace(DimScales.old,
                             list = i.time.old,
                             values = list(DS.time.new))
    DimScales.new <- DimScales.new[-i.triangle]
    metadata.new <- methods::new("MetaData",
                                 nms = names.new,
                                 dimtypes = dimtypes.new,
                                 DimScales = DimScales.new)
    dim.new <- dim(metadata.new)
    dimnames.new <- dimnames(metadata.new)
    .Data.new <- array(0L,
                       dim = dim.new,
                       dimnames = dimnames.new)
    ind.old <- ((slice.index(.Data.old, MARGIN = i.triangle) == 2L)
        & (slice.index(.Data.old, MARGIN = i.age.old) == n.age))
    i.age.new <- match("age", dimtypes.new)
    ind.new <- slice.index(.Data.new, MARGIN = i.age.new) == n.age
    .Data.new[ind.new] <- .Data.old[ind.old]
    methods::new("Counts",
                 .Data = .Data.new,
                 metadata = metadata.new)
}

## HAS_TESTS
incrementSquareHelper <- function(component) {
    .Data <- component@.Data
    dim <- dim(component)
    names <- names(component)
    dimtypes <- dimtypes(component,
                         use.names = FALSE)
    DimScales.old <- DimScales(component,
                               use.names = FALSE)
    i.time <- match("time", dimtypes)
    DS.time.old <- DimScales.old[[i.time]]
    dv.time.old <- dimvalues(DS.time.old)
    dv.time.new <- dv.time.old[-1L]
    DS.time.new <- methods::new("Points", dimvalues = dv.time.new)
    DimScales.new <- replace(DimScales.old,
                             list = i.time,
                             values = list(DS.time.new))
    metadata.new <- methods::new("MetaData",
                                 nms = names,
                                 dimtypes = dimtypes,
                                 DimScales = DimScales.new)
    dimnames.new <- dimnames(metadata.new)
    .Data.new <- array(.Data,
                       dim = dim,
                       dimnames = dimnames.new)
    methods::new("Counts",
                 .Data = .Data.new,
                 metadata = metadata.new)
}

## HAS_TESTS
incrementUpperTriHelper <- function(component) {
    .Data.old <- component@.Data
    dim.old <- dim(component)
    names.old <- names(component)
    dimtypes.old <- dimtypes(component,
                             use.names = FALSE)
    DimScales.old <- DimScales(component,
                               use.names = FALSE)
    i.age.old <- match("age", dimtypes.old)
    i.triangle.old <- match("triangle", dimtypes.old)
    DS.age.old <- DimScales.old[[i.age.old]]
    dv.age.old <- dimvalues(DS.age.old)
    n.age.old <- dim.old[i.age.old]
    dv.age.new <- dv.age.old[-c(1L, n.age.old + 1L)]
    DS.age.new <- methods::new("Points", dimvalues = dv.age.new)
    names.new <- names.old[-i.triangle.old]
    dimtypes.new <- dimtypes.old[-i.triangle.old]
    DimScales.new <- DimScales.old[-i.triangle.old]
    i.age.new <- match("age", dimtypes.new)
    DimScales.new <- replace(DimScales.new,
                             list = i.age.new,
                             values = list(DS.age.new))
    metadata.new <- methods::new("MetaData",
                                 nms = names.new,
                                 dimtypes = dimtypes.new,
                                 DimScales = DimScales.new)
    dim.new <- dim(metadata.new)
    dimnames.new <- dimnames(metadata.new)
    ind.old <- ((slice.index(.Data.old, MARGIN = i.triangle.old) == 2L)
        & (slice.index(.Data.old, MARGIN = i.age.old) != n.age.old))
    .Data.new <- .Data.old[ind.old]
    .Data.new <- array(.Data.new,
                       dim = dim.new,
                       dimnames = dimnames.new)
    methods::new("Counts",
                 .Data = .Data.new,
                 metadata = metadata.new)
}

## HAS_TESTS
makeDimtypeIndex <- function(object, dimtype) {
    .Data <- object@.Data
    dimtypes <- dimtypes(object,
                         use.names = FALSE)
    i.dimtype <- match(dimtype, dimtypes)
    slice.index(.Data,
                MARGIN = i.dimtype)
}

## HAS_TESTS
makeMappingBirths <- function(births) {
    dim <- dim(births)
    dimtypes <- dimtypes(births,
                         use.names = FALSE)
    i.time <- match("time", dimtypes)
    i.collapsed <- grep("parent|age|triangle", dimtypes)
    n.collapsed <- length(i.collapsed)
    s <- seq_along(dim)
    s.included <- setdiff(s, c(i.time, i.collapsed))
    n.included <- length(s.included)
    s.no.time <- setdiff(s, i.time)
    dim.collapsed <- dim[i.collapsed]
    if (n.included > 0L) {
        dim.included <- dim[s.included]
        ans <- array(seq_len(prod(dim.included)),
                     dim = c(dim.included, dim.collapsed))
        perm <- match(s.no.time, c(s.included, i.collapsed))
        ans <- aperm(ans, perm = perm)
        as.integer(ans)
    }
    else {
        if (n.collapsed > 0L)
            rep(1L, times = prod(dim[i.collapsed]))
        else
            1L
    }
}

## HAS_TESTS
makeMetadataExtendOrigDestParentChild <- function(x, y) {
    names.x <- names(x)
    names.y <- names(y)
    dimtypes.x <- dimtypes(x, use.names = FALSE)
    dimtypes.y <- dimtypes(y, use.names = FALSE)
    DimScales.y <- DimScales(y, use.names = FALSE)
    base.orig <- removeSuffixes(names.x[dimtypes.x == "origin"])
    base.parent <- removeSuffixes(names.x[dimtypes.x == "parent"])
    suffixes.orig.dest <- getSuffixes(c("origin", "destination"))
    suffixes.parent.child <- getSuffixes(c("parent", "child"))
    names.new <- vector(mode = "list", length = length(names.y))
    dimtypes.new <- vector(mode = "list", length = length(names.y))
    DimScales.new <- vector(mode = "list", length = length(names.y))
    for (i in seq_along(names.new)) {
        name.y <- names.y[i]
        DimScale.y <- DimScales.y[[i]]
        if (name.y %in% base.orig) {
            nms.new <- paste0(name.y, suffixes.orig.dest)
            dt.new <- c("origin", "destination")
            DS.new <- rep(list(DimScale.y), times = 2L)
        }
        else if (name.y %in% base.parent) {
            nms.new <- paste0(name.y, suffixes.parent.child)
            dt.new <- c("parent", "child")
            DS.new <- rep(list(DimScale.y), times = 2L)
        }
        else {
            nms.new <- name.y
            dt.new <- dimtypes.y[i]
            DS.new <- list(DimScale.y)
        }
        names.new[[i]] <- nms.new
        dimtypes.new[[i]] <- dt.new
        DimScales.new[[i]] <- DS.new
    }
    names.new <- unlist(names.new)
    dimtypes.new <- unlist(dimtypes.new)
    DimScales.new <- unlist(DimScales.new, recursive = FALSE)
    methods::new("MetaData",
        nms = names.new,
        dimtypes = dimtypes.new,
        DimScales = DimScales.new)
}

## HAS_TESTS
makeMetadataForExposure <- function(population, triangles) {
    names <- names(population)
    dimtypes <- dimtypes(population, use.names = FALSE)
    DimScales <- DimScales(population, use.names = FALSE)
    i.time <- match("time", dimtypes, nomatch = 0L)
    has.time <- i.time > 0L
    if (has.time)
        i.along <- i.time
    else
        i.along <- match("age", dimtypes)
    DimScale.along <- DimScales[[i.along]]
    dimvalues.along <- dimvalues(DimScale.along)
    DimScale.along <- methods::new("Intervals",
                                   dimvalues = dimvalues.along,
                                   isAge = !has.time)
    DimScales <- replace(DimScales,
                         list = i.along,
                         values = list(DimScale.along))
    if (triangles) {
        names <- make.unique(c(names, "triangle"))
        dimtypes <- c(dimtypes, "triangle")
        DimScale.triangle <- methods::new("Triangles", dimvalues = c("TL", "TU"))
        DimScales <- append(DimScales, values = DimScale.triangle)
    }
    methods::new("MetaData",
                 nms = names,
                 dimtypes = dimtypes,
                 DimScales = DimScales)
}


## HAS_TESTS
makeTemplateComponent <- function(population, triangles = TRUE) {
    names <- names(population)
    dimtypes <- dimtypes(population, use.names = FALSE)
    DimScales <- DimScales(population, use.names = FALSE)
    i.time <- match("time", dimtypes)
    i.age <- match("age", dimtypes, nomatch = 0L)
    DimScale.time <- DimScales[[i.time]]
    dimvalues.time <- dimvalues(DimScale.time)
    DimScale.time.new <- methods::new("Intervals",
                                      dimvalues = dimvalues.time,
                                      isAge = FALSE)
    DimScales <- replace(DimScales,
                         list = i.time,
                         values = list(DimScale.time.new))
    metadata <- methods::new("MetaData",
                             nms = names,
                             dimtypes = dimtypes,
                             DimScales = DimScales)
    .Data <- array(0L,
                   dim = dim(metadata),
                   dimnames = dimnames(metadata))
    ans <- methods::new("Counts", .Data = .Data, metadata = metadata)
    has.age <- i.age > 0L
    if (has.age && triangles)
        ans <- splitTriangles(ans)
    ans
}

## HAS_TESTS
pairDimCompCompatibleWithPopn <- function(name, dimtype, DimScale,
                                          namesPopn, dimtypesPopn, DimScalesPopn,
                                          nameComponent) {
    name.popn <- removeSuffixes(name)
    ## population has name
    i.dim.popn <- match(name.popn, namesPopn, nomatch = 0L)
    has.dim.popn <- i.dim.popn > 0L
    if (!has.dim.popn)
        return(gettextf("'%s' has dimension \"%s\" but '%s' does not have dimension \"%s\"",
                        nameComponent, name, "population", name.popn))
    ## population dimension has dimtype "state"
    dimtype.popn <- dimtypesPopn[i.dim.popn]
    if (!identical(dimtype.popn, "state"))
        return(gettextf("\"%s\" dimension of '%s' has %s \"%s\" but \"%s\" dimension of '%s' has %s \"%s\"",
                        name, nameComponent, "dimtype", dimtype,
                        name.popn, "population", "dimtype", dimtype.popn))
    ## identical DimScales
    DimScale.popn <- DimScalesPopn[[i.dim.popn]]
    if (!isTRUE(all.equal(DimScale, DimScale.popn)))
        return(gettextf("\"%s\" dimension of '%s' and \"%s\" dimension of '%s' have incompatible dimscales",
                        name, nameComponent, name.popn, "population"))
    TRUE
}

## HAS_TESTS
popnEndNoAge <- function(object) {
    stopifnot(methods::is(object, "Movements"))
    population <- object@population
    components <- object@components
    .Data <- population@.Data
    dim <- dim(population)
    names <- names(population)
    dimtypes <- dimtypes(population,
                         use.names = FALSE)
    DimScales <- DimScales(population,
                           use.names = FALSE)
    i.time <- match("time", dimtypes)
    n.time <- dim[i.time]
    DS.time <- DimScales[[i.time]]
    DS.time.ans <- DS.time[-1L]
    DimScales.ans <- replace(DimScales,
                             list = i.time,
                             values = list(DS.time.ans))
    metadata.ans <- new("MetaData",
                        nms = names,
                        dimtypes = dimtypes,
                        DimScales = DimScales.ans)
    .Data.ans <- .Data[slice.index(.Data, MARGIN = i.time) != n.time]
    .Data.ans <- array(.Data.ans,
                       dim = dim(metadata.ans),
                       dimnames = dimnames(metadata.ans))
    ans <- new("Counts",
               .Data = .Data.ans,
               metadata = metadata.ans)
    for (component in components)
        ans <- ans + incrementSquare(component = component,
                                     population = population)
    ans
}

## NO_TESTS
popnEndWithAge <- function(object) {
    stopifnot(methods::is(object, "Movements"))
    population <- object@population
    components <- object@components
    .Data <- population@.Data
    dim <- dim(population)
    names <- names(population)
    dimtypes <- dimtypes(population,
                         use.names = FALSE)
    DimScales <- DimScales(population,
                           use.names = FALSE)
    i.time <- match("time", dimtypes)
    n.time <- dim[i.time]
    DS.time <- DimScales[[i.time]]
    DS.time.ans <- DS.time[-1L]
    DimScales.ans <- replace(DimScales,
                             list = i.time,
                             values = list(DS.time.ans))
    metadata.ans <- new("MetaData",
                        nms = names,
                        dimtypes = dimtypes,
                        DimScales = DimScales.ans)
    accession <- accession(object,
                           births = TRUE)
    .Data.ans <- array(accession@.Data,
                       dim = dim(metadata.ans),
                       dimnames = dimnames(metadata.ans))
    ans <- new("Counts",
               .Data = .Data.ans,
               metadata = metadata.ans)
    ans <- ans + popnOpen(population)
    for (component in components) {
        increment <- incrementLowerTri(component = component,
                                       population = population)
        ans <- ans + increment
        increment <- incrementOpen(component = component,
                                   population = population)
        ans <- ans + increment
    }
    ans
}

## HAS_TESTS
popnOpen <- function(population) {
    .Data <- population@.Data
    dim <- dim(population)
    names <- names(population)
    dimtypes <- dimtypes(population,
                         use.names = FALSE)
    DimScales <- DimScales(population,
                           use.names = FALSE)
    i.age <- match("age", dimtypes)
    i.time <- match("time", dimtypes)
    n.age <- dim[i.age]
    n.time <- dim[i.time]
    DS.time <- DimScales[[i.time]]
    DS.time.ans <- DS.time[-1L]
    DimScales.ans <- replace(DimScales,
                             list = i.time,
                             values = list(DS.time.ans))
    metadata.ans <- new("MetaData",
                        nms = names,
                        dimtypes = dimtypes,
                        DimScales = DimScales.ans)
    .Data.ans <- array(0L,
                       dim = dim(metadata.ans),
                       dimnames = dimnames(metadata.ans))
    ind <- ((slice.index(.Data, MARGIN = i.age) == n.age)
        & slice.index(.Data, MARGIN = i.time) != n.time)
    ind.ans <- slice.index(.Data.ans, MARGIN = i.age) == n.age
    .Data.ans[ind.ans] <- .Data[ind]
    new("Counts",
        .Data = .Data.ans,
        metadata = metadata.ans)
}

## HAS_TESTS
removeDimtypesFromMetadata <- function(metadata, dimtypes) {
    dimtypes.old <- dimtypes(metadata,
                             use.names = FALSE)
    i.dimtype <- match(dimtypes, dimtypes.old, nomatch = 0L)
    s <- seq_along(dimtypes.old)
    s <- setdiff(s, i.dimtype)
    if (length(s) == 0L)
        stop(gettext("removing all dimensions"))
    metadata[s]
}

## HAS_TESTS
removePairFromMetadata <- function(metadata, origDest = TRUE) {
    names <- names(metadata)
    dimtypes <- dimtypes(metadata,
                         use.names = FALSE)
    DimScales <- DimScales(metadata,
                           use.names = FALSE)
    suffix.keep <- if (origDest) "orig" else "child"
    suffix.remove <- if (origDest) "dest" else "parent"
    p.keep <- sprintf("_%s$", suffix.keep)
    names.keep <- grep(p.keep, names, value = TRUE)
    names.base <- sub(p.keep, "", names.keep)
    names.remove <- sprintf("%s_%s", names.base, suffix.remove)
    i.keep <- match(names.keep, names)
    i.remove <- match(names.remove, names)
    names.new <- replace(names,
                         list = i.keep,
                         values = names.base)
    dimtypes.new <- replace(dimtypes,
                            list = i.keep,
                            values = "state")
    s <- seq_along(names)
    s <- setdiff(s, i.remove)
    names.new <- names.new[s]
    dimtypes.new <- dimtypes.new[s]
    DimScales.new <- DimScales[s]
    metadata.new <- new("MetaData",
                        nms = names.new,
                        dimtypes = dimtypes.new,
                        DimScales = DimScales.new)
}

## HAS_TESTS
safeSample1 <- function(choices) {
    n <- length(choices)
    if (n == 0L)
        stop(gettextf("'%s' has length %d",
                      "choices", 0L))
    else if (n == 1L)
        choices
    else
        sample(choices, size = 1L)
}

## HAS_TESTS
splitTriangles <- function(object) {
    .Data <- object@.Data
    names <- names(object)
    dimtypes <- dimtypes(object, use.names = FALSE)
    DimScales <- DimScales(object, use.names = FALSE)
    names <- make.unique(c(names, "triangle"))
    dimtypes <- c(dimtypes, "triangle")
    DimScale.triangle <- methods::new("Triangles", dimvalues = c("TL", "TU"))
    DimScales <- append(DimScales, DimScale.triangle)
    metadata <- methods::new("MetaData",
                    nms = names,
                    dimtypes = dimtypes,
                    DimScales = DimScales)
    is.obs <- !is.na(.Data)
    .Data.lower <- rep(NA_integer_, times = length(.Data))
    .Data.lower[is.obs] <- (as.integer(sign(.Data[is.obs]))
                            * stats::rbinom(n = sum(is.obs),
                                     size = abs(.Data[is.obs]),
                                     prob = 0.5))
    .Data.upper <- .Data - .Data.lower
    .Data <- array(c(.Data.lower, .Data.upper),
                   dim = dim(metadata),
                   dimnames = dimnames(metadata))
    methods::new("Counts", .Data = .Data, metadata = metadata)
}

## HAS_TESTS
timeDimCompCompatibleWithPopn <- function(name, DimScale,
                                          namesPopn, dimtypesPopn, DimScalesPopn,
                                          nameComponent) {
    ## population has name
    i.dim.popn <- match(name, namesPopn, nomatch = 0L)
    has.dim.popn <- i.dim.popn > 0L
    if (!has.dim.popn)
        return(gettextf("'%s' has dimension \"%s\" but '%s' does not",
                        nameComponent, name, "population"))
    ## population dimension has dimtype "time"
    dimtype.popn <- dimtypesPopn[i.dim.popn]
    if (!identical(dimtype.popn, "time"))
        return(gettextf("\"%s\" dimension of '%s' has %s \"%s\" but \"%s\" dimension of '%s' has %s \"%s\"",
                        name, nameComponent, "dimtype", "time",
                        name, "population", "dimtype", dimtype.popn))
    ## compatible DimScales
    DimScale.popn <- DimScalesPopn[[i.dim.popn]]
    dv <- dimvalues(DimScale)
    dv.popn <- dimvalues(DimScale.popn)
    if (!isTRUE(all.equal(dv, dv.popn)))
        return(gettextf("\"%s\" dimensions have incompatible dimscales",
                        name))
    TRUE
}

## HAS_TESTS
## 'x' and 'y' both have dimensions with dimtype "age"
## and dimscale "Intervals".  Extract slab of 'x'
## so that the minimum and maximum ages of 'x' and 'y'
## match.  Raise error if this is not possible.
trimAgeIntervalsToMatch <- function(x, y) {
    dimtypes.x <- dimtypes(x, use.names = FALSE)
    dimtypes.y <- dimtypes(y, use.names = FALSE)
    DimScales.x <- DimScales(x, use.names = FALSE)
    DimScales.y <- DimScales(y, use.names = FALSE)
    i.x <- match("age", dimtypes.x)
    i.y <- match("age", dimtypes.y)
    DS.x <- DimScales.x[[i.x]]
    DS.y <- DimScales.y[[i.y]]
    dv.x <- dimvalues(DS.x)
    dv.y <- dimvalues(DS.y)
    min.y <- min(dv.y)
    max.y <- max(dv.y)
    i.min <- match(min.y, dv.x, nomatch = 0L)
    i.max <- match(max.y, dv.x, nomatch = 0L)
    if ((i.min == 0L) || (i.max == 0L))
        stop(gettextf("dimensions with dimtype \"%s\" not compatible",
                      "age"))
    elements <- seq.int(from = i.min, to = i.max - 1L)
    slab(x, dimension = i.x, elements = elements, drop = FALSE)
}

## assume that (i) 'x' has dimension with dimtype "age",
## (ii) the age dimension has dimscale "Intervals",
## (iii) there are at least 2 age groups
trimAgeIntervalsTo1549 <- function(x) {
    dimtypes <- dimtypes(x, use.names = FALSE)
    DimScales <- DimScales(x, use.names = FALSE)
    i.age <- match("age", dimtypes)
    DimScale.age <- DimScales[[i.age]]
    dimvalues.age <- dimvalues(DimScale.age)
    n.age <- length(dimvalues.age) - 1L
    if (n.age < 2L)
        stop(gettextf("fewer than %d age groups",
                      2L))
    i.first <- findInterval(x = 15, vec = dimvalues.age)
    i.last <- findInterval(x = 49, vec = dimvalues.age)
    if (i.first < 2L)
        i.first <- 2L
    if (i.last > n.age)
        i.last <- n.age
    elements <- seq.int(from = i.first, to = i.last)
    slab(x,
         dimension = i.age,
         elements = elements,
         drop = FALSE)
}


## DBIND AND HELPER FUNCTIONS #####################################################

## HAS_TESTS
checkCanCombineAlong <- function(e1, e2, along) {
    e1 <- DimScales(e1)[[along]]
    e2 <- DimScales(e2)[[along]]
    dbindDimScales(e1 = e1, e2 = e2, along = along)
    NULL
}

## HAS_TESTS
combineDbindData <- function(e1, e2, metadata) {
    both.integer <- is.integer(e1) && is.integer(e2)
    if (both.integer) {
        e1 <- as.integer(e1)
        e2 <- as.integer(e2)
    }
    else {
        e1 <- as.double(e1)
        e2 <- as.double(e2)
    }
    array(c(e1, e2),
          dim = dim(metadata),
          dimnames = dimnames(metadata))
}

## HAS_TESTS
combineDbindMetadataCounts <- function(e1, e2, along) {
    names <- names(e1)
    dimtypes <- dimtypes(e1, use.names = FALSE)
    DimScales1 <- DimScales(e1, use.names = FALSE)
    DimScales2 <- DimScales(e2, use.names = FALSE)
    i.along <- match(along, names)
    DS1 <- DimScales1[[i.along]]
    DS2 <- DimScales2[[i.along]]
    DS <- dbindDimScales(e1 = DS1, e2 = DS2, along = along)
    DimScales <- replace(DimScales1, list = i.along, values = list(DS))
    methods::new("MetaData",
        nms = names,
        dimtypes = dimtypes,
        DimScales = DimScales)
}

## HAS_TESTS
combineDbindMetadataValues <- function(metadata1, metadata2, transform1, transform2, along) {
    names1 <- names(metadata1)
    names2 <- names(metadata2)
    dimtypes1 <- dimtypes(metadata1)
    dimtypes2 <- dimtypes(metadata2)
    DimScales1 <- DimScales(metadata1)
    DimScales2 <- DimScales(metadata2)
    dims1 <- transform1@dims
    dims2 <- transform2@dims
    names <- character(length = length(dims1))
    dimtypes <- character(length = length(dims1))
    DimScales <- vector(mode = "list", length = length(dims1))
    for (i in seq_along(names)) {
        d1 <- dims1[i]
        d2 <- dims2[i]
        if (d1 > 0L) {
            names[i] <- names1[d1]
            dimtypes[i] <- dimtypes1[d1]
            if (d2 > 0L) {
                is.along <- names[i] == along
                if (is.along)
                    DimScales[[i]] <- dbindDimScales(e1 = DimScales1[[d1]],
                                                     e2 = DimScales2[[d2]])
                else
                    DimScales[[i]] <- mergeDimScales(e1 = DimScales1[[d1]],
                                                     e2 = DimScales2[[d2]])
            }
            else
                DimScales[[i]] <- DimScales1[[d1]]
        }
        else {
            names[i] <- names2[d2]
            dimtypes[i] <- dimtypes2[d2]
            DimScales[[i]] <- DimScales2[[d2]]
        }
    }
    methods::new("MetaData",
        nms = names,
        dimtypes = dimtypes,
        DimScales = DimScales)
}

## HAS_TESTS
combineDimvaluesForIntervals <- function(e1, e2, along) {
    dimvalues1 <- dimvalues(e1, use.names = FALSE)
    dimvalues2 <- dimvalues(e2, use.names = FALSE)
    n1 <- length(dimvalues1)
    n2 <- length(dimvalues2)
    if ((n1 > 0L) && (n2 > 0L)) {
        min1 <- min(dimvalues1)
        min2 <- min(dimvalues2)
        max1 <- max(dimvalues1)
        max2 <- max(dimvalues2)
        if (max1 < min2)
            stop(gettextf("gap between \"%s\" dimensions", along))
        else if (max1 == min2)
            c(dimvalues1[-length(dimvalues1)], dimvalues2)
        else if ((max1 > min2) && (min1 < max2))
            stop(gettextf("\"%s\" dimensions overlap", along))
        else if (min1 == max2)
            c(dimvalues2[-length(dimvalues2)], dimvalues1)
        else
            stop(gettextf("gap between \"%s\" dimensions", along))
    }
    else if ((n1 == 0L) && (n2 > 0L))
        dimvalues2
    else if ((n1 > 0L) && (n2 == 0L))
        dimvalues1
    else
        dimvalues1
}

## HAS_TESTS
combineDimvaluesForPoints <- function(e1, e2, along) {
    dimvalues1 <- dimvalues(e1, use.names = FALSE)
    dimvalues2 <- dimvalues(e2, use.names = FALSE)
    n1 <- length(dimvalues1)
    n2 <- length(dimvalues2)
    if ((n1 > 0L) && (n2 > 0L)) {
        min1 <- min(dimvalues1)
        min2 <- min(dimvalues2)
        max1 <- max(dimvalues1)
        max2 <- max(dimvalues2)
        if (max1 < min2)
            c(dimvalues1, dimvalues2)
        else if ((max1 >= min1) && (min1 <= max2))
            stop(gettextf("\"%s\" dimensions overlap", along))
        else
            c(dimvalues2, dimvalues1)
    }
    else if ((n1 == 0L) && (n2 > 0L))
        dimvalues2
    else if ((n1 > 0L) && (n2 == 0L))
        dimvalues1
    else
        dimvalues1
}

## HAS_TESTS
#' Combine DemographicArray objects.
#'
#' Combine two or more \code{"\linkS4class{DemographicArray}"} objects to form a new
#' object of the same class.  Objects are automatically reshaped to make them
#' compatible before they are bound together.
#'
#' During interactive use, objects to be \code{dbind}ed are typically specified
#' using \dots{}.  The \code{args} argument more likely to be used in
#' programming.
#'
#' An object in \dots{} or \code{args} may or may not have the dimension
#' specified by \code{along}.  Different objects' \code{along} dimensions must
#' not overlap.  If an object does not have an \code{along} dimension, then one
#' is added to it.  This new dimension consists of a single value, taken from
#' the name of the object.  See below for examples.
#'
#' Before objects are combined, they are automatically manipulated to make all
#' dimensions other than \code{along} dimension compatible.  The automatic
#' manipulation includes collapsing, adding, and permuting dimensions,
#' collapsing or splitting intervals, and permuting categories.  However,
#' objects are \emph{not} subsetted.  If it is not possible to make objects
#' compatible using the manipulations available, then an error is raised.
#'
#' If \code{along} has \code{\link{dimtype}} \code{"iteration"}, the
#' iterations for all objects are reset; if an object does not have a dimension
#' with dimtype \code{"iteration"}, it is treated as a single iteration.  See
#' below for examples.
#'
#' The categories in \code{along} are taken from existing \code{along}
#' dimensions, if any, from names passed as part of \code{dots}, or from the
#' names of the objects.  See below for an example.
#'
#' @param \dots Objects of class \code{\linkS4class{DemographicArray}}.
#' @param args List of objects of class \code{\linkS4class{DemographicArray}}.
#' @param along A dimension name.
#' @return Object of the same class as the objects specified in \dots{} or
#' \code{args}.
#' @seealso \code{dbind} is based on function \code{abind} in package
#' \pkg{abind}.
#' @examples
#' library(demdata)
#' popn <- Counts(VAPopn)
#'
#' ## Split 'popn' into two
#' under60 <- subarray(popn, age < 60)
#' over60 <- subarray(popn, age > 60)
#'
#' ## then recombine using dbind
#' dbind(under60, over60, along = "age")
#'
#' ## or with via 'args'
#' dbind(args = list(under60, over60), along = "age")
#'
#' ## or even both.
#' dbind(under60, args = list(over60), along = "age")
#'
#' ## Now an example of automatic permuting and extending:
#' under60new <- collapseDimension(under60, dimension = "color")
#' under60new <- aperm(under60new, perm = c("sex", "residence", "age"))
#' ## over60 loses "color" dimension and is permuted,
#' ## to match under60new
#' dbind(under60new, over60, along = "age")
#'
#' ## An example of some categories being taken from existing dimension
#' ## and some being taken from the name of an object
#' NZ <- Counts(nz.mig)
#' NZ <- collapseDimension(NZ, dimension = "island_orig")
#' Australia <- array(c(2704276, 8376751, 2192675, 2644374),
#'                    dim = 4,
#'                    dimnames = list(age = c("15-24",
#'                          "25-54", "55-64", "65+")))
#' Australia <- Counts(Australia)
#' ## categories for 'along' taken from 'along' dimension of
#' ## 'NZ' object, plus name of 'Australia' object
#' dbind(NZ, Australia, along = "island")
#' ## now supply a more descriptive name
#' dbind(NZ, "West Island" = Australia, along = "island")
#'
#' ## example of iterations being reset
#' x <- Counts(array(1:4,
#'                   dim = c(2, 2),
#'                   dimnames = list(sex = c("Female", "Male"),
#'                       iteration = 10:11)))
#' y <- Counts(array(5:10,
#'                   dim = c(2, 3),
#'                   dimnames = list(sex = c("Female", "Male"),
#'                       iteration = 5:7)))
#' x
#' y
#' dbind(x, y, along = "iteration")
#'
#' ## iterations reset, and 'y' treated as a single iteration
#' x <- Counts(array(1:4,
#'                   dim = c(2, 2),
#'                   dimnames = list(sex = c("Female", "Male"),
#'                       iteration = 10:11)))
#' y <- Counts(array(5:6,
#'                   dim = 2,
#'                   dimnames = list(sex = c("Female", "Male"))))
#' x
#' y
#' dbind(x, y, along = "iteration")
#' @export
dbind <- function(..., args = list(), along) {
    ## Construction of names is based on idea from
    ## Venables & Ripley. 2000. S Programming. p46
    dots.subs <- as.list(substitute(list(...)))[-1L]
    args.subs <- as.list(substitute(args))[-1L]
    dots.args.subs <- c(dots.subs, args.subs)
    names <- names(dots.args.subs)
    if (is.null(names))
        i.missing <- seq_along(dots.args.subs)
    else
        i.missing <- which(names == "")
    if (length(i.missing) > 0L) {
        derived.names <- sapply(dots.args.subs[i.missing],
                                function(x) deparse(x)[[1L]])
        names[i.missing] <- derived.names
    }
    objects <- c(list(...), args)
    not.demographic <- !sapply(objects, methods::is,"DemographicArray")
    if (any(not.demographic)) {
        i.first.wrong <- which(not.demographic)[1L]
        class.first.wrong <- class(objects[[i.first.wrong]])
        stop(gettextf("object with class \"%s\"",
                      class.first.wrong))
    }
    along <- as.character(along)
    if (!identical(length(along), 1L))
        stop(gettextf("'%s' does not have length %d", "along", 1L))
    if (is.na(along))
        stop(gettextf("'%s' is missing", "along"))
    if (!nzchar(along))
        stop(gettextf("'%s' is blank", "along"))
    n <- length(objects)
    if (n == 0L)
        NULL
    else {
        ans <- objects[[1L]]
        if (n == 1L)
            ans
        else {
            dimtype.along <- dimtypeAlongDbind(objects = objects, along = along)
            for (i in 2:n)
                ans <- dbind2(e1 = ans,
                              e2 = objects[[i]],
                              name1 = names[1L],
                              name2 = names[i],
                              along = along,
                              dimtypeAlong = dimtype.along)
        }
        names.ans <- names(ans)
        names1 <- names(objects[[1L]])
        perm <- c(intersect(names1, names.ans), setdiff(names.ans, names1))
        aperm(ans, perm = perm)
    }
}

## HAS_TESTS
dimtypeAlongDbind <- function(objects, along) {
    getDimtypeAlong <- function(x) {
        if (along %in% names(x))
            dimtypes(x)[[along]]
        else
            NA
    }
    dimtype <- sapply(objects, getDimtypeAlong)
    dimtype <- dimtype[!is.na(dimtype)]
    dimtype <- unique(dimtype)
    n <- length(dimtype)
    if (n == 0L)
        "state"
    else if (n == 1L)
        dimtype
    else
        stop(gettextf("\"%s\" dimensions have different dimtypes : \"%s\" versus \"%s\"",
                      along, dimtype[1L], dimtype[2L]))
}

## HAS_TESTS
e1IsFirst <- function(e1, e2, along) {
    DimScale1 <- DimScales(e1)[[along]]
    DimScale2 <- DimScales(e2)[[along]]
    e1IsFirstDimScale(e1 = DimScale1, e2 = DimScale2)
}

## HAS_TESTS
fixAlongForDbind <- function(object, name, along, dimtypeAlong) {
    has.along <- along %in% names(object)
    if (!has.along) {
        along.is.iter <- identical(dimtypeAlong, "iteration")
        labels <- if (along.is.iter) "1" else name
        object <- addDimension(object,
                               name = along,
                               labels = labels,
                               dimtype = dimtypeAlong)
    }
    object
}


## FUNCTIONS FOR PRINTING  #######################################################

## HAS_TESTS
showMetaData <- function(object) {
    n <- length(dim(object))
    if (n > 0) {
        limits <- limits(object)
        limits[] <- lapply(limits, as.character)
        first <- limits["first", ]
        last <- limits["last", ]
        first <- as.character(first)
        last <- as.character(last)
        m <- rbind("name:" = names(object),
                   "length:" = dim(object),
                   "dimtype:" = dimtypes(object),
                   "dimscale:" = dimscales(object),
                   "first:" = first,
                   "last:" = last)
        colnames(m) <- rep("", ncol(m))
        print(m, quote = FALSE)
    }
}


## FUNCTIONS FOR PLOTTING  #########################################################


## parsOverlay <- function(overlay) {
##     getValue <- function(name, default) {
##         ans <- overlay[[name]]
##         if (is.null(ans)) default else ans
##     }
##     col <- getValue("col", trellis.par.get("plot.line"))
##     alpha <- getValue("alpha", 1)
##     lwd <- getValue("lwd", 1)
##     top <- getValue("top", TRUE)
##     type <- getValue("type", NULL)
##     list(col = col,
##          alpha = alpha,
##          lwd = lwd,
##          top = top,
##          type = type)
## }

## NO_TESTS
addOverlayToData <- function(data, overlay, weights, probs, midpoints) {
    values <- overlay$values
    if (is.null(values))
        stop(gettextf("'%s' does not have a component named \"%s\"",
                      "overlay", "values"))
    values.has.quantile <- "quantile" %in% dimtypes(values)
    if (!values.has.quantile) {
        is.not.iter <- dimtypes(values) != "iteration"
        length1 <- dim(values) == 1L
        dim.to.collapse <- setdiff(names(values)[is.not.iter & !length1], names(data))
        if (length(dim.to.collapse) > 0L) {
            if (methods::is(values, "Counts"))
                values <- collapseDimension(values, dimension = dim.to.collapse)
            else if (methods::is(values, "Values")) {
                not.in.weights <- setdiff(dim.to.collapse, names(weights))
                n.not.in.weights <- length(not.in.weights)
                if (n.not.in.weights > 0L)
                    stop(sprintf(ngettext(n.not.in.weights,
                                          "need to collapse %s dimension in '%s' but '%s' argument not supplied",
                                          "need to collapse %s dimensions in '%s' but '%s' argument not supplied"),
                                 paste(dQuote(not.in.weights), collapse = ", "),
                                 "overlay", "weights"))
                values <- collapseDimension(values, dimension = dim.to.collapse, weights = weights)
            }
            else
                stop(gettextf("'%s' has class \"%s\"", values, class(values)))
        }
        if ("iteration" %in% dimtypes(values)) {
            values <- collapseIterations(values, probs = probs)
            values.has.quantile <- TRUE
        }
    }
    i.quantile.values <- match("quantile", dimtypes(values), nomatch = 0L)
    values <- as.data.frame(values, direction = "long", midpoints = midpoints)
    x <- unique(data[-length(data)])  ## 'unique' has effect if data has quantiles
    by <- names(values)[setdiff(seq_along(values), c(length(values), i.quantile.values))]
    values <- merge(x = x, y = values, by = by, sort = FALSE)
    if (values.has.quantile)
        i.quantile.values <- length(values) - 1L
    quantile.data <- attr(data, "quantile")
    data.has.quantile <- !is.null(quantile.data)
    if (values.has.quantile) {
        quantile.values <- values[[i.quantile.values]]
        values <- values[-i.quantile.values]
        ## create factors that preserve level order
        if (data.has.quantile) {
            x <- c(as.character(quantile.data), as.character(quantile.values))
            levels <- union(levels(quantile.data), levels(quantile.values))
            levels <- levels[order(as.numeric(sub("%$", "", levels)))]
            quantile <- factor(x = x, levels = levels)
        }
        else
            quantile <- factor(c(rep(NA, nrow(data)), as.character(quantile.values)),
                               levels = levels(quantile.values))
    }
    else {
        if (data.has.quantile)
            quantile <- factor(c(as.character(quantile.data), rep(NA, nrow(values))),
                               levels = levels(quantile.data))
        else
            quantile <- NULL
    }
    is.data <- c(rep(TRUE, nrow(data)), rep(FALSE, nrow(values)))
    names(values)[length(values)] <- names(data)[length(data)]
    ans <- rbind(data, values)
    attr(ans, "quantile") <- quantile
    attr(ans, "is.data") <- is.data
    ans
}

panel.dplot <- function(x, y, groups = NULL, subscripts, type = NULL,
                        col = NULL, lwd = NULL, pch = NULL, alpha = NULL,
                        quantile, horizontal, is.data, overlay, ...) {
    has.overlay <- !is.null(is.data)
    if (has.overlay) {
        is.data.panel <- is.data[subscripts]
        x.data <- x[is.data.panel]
        x.overlay <- x[!is.data.panel]
        y.data <- y[is.data.panel]
        y.overlay <- y[!is.data.panel]
        if (is.null(quantile)) {
            quantile.data <- NULL
            quantile.overlay <- NULL
        }
        else {
            quantile.data <- quantile[is.data]
            if (all(is.na(quantile.data)))
                quantile.data <- NULL
            quantile.overlay <- quantile[!is.data]
            if (all(is.na(quantile.overlay)))
                quantile.overlay <- NULL
        }
        subscripts.data <- subscripts[is.data.panel]
        subscripts.overlay <- subscripts[!is.data.panel]
        overlay.last <- overlay$last
        if (is.null(overlay.last))
            overlay.last <- TRUE
        if (overlay.last) {
            panel.data.or.overlay(x = x.data, y = y.data, groups = groups,
                                  subscripts = subscripts.data,
                                  type = type, col = col, lwd = lwd,
                                  pch = pch, alpha = alpha,
                                  horizontal = horizontal,
                                  quantile = quantile.data, ...)
            panel.data.or.overlay(x = x.overlay, y = y.overlay, groups = groups,
                                  subscripts = subscripts.overlay,
                                  type = overlay$type, col = overlay$col,
                                  lwd = overlay$lwd, pch = overlay$pch,
                                  alpha = overlay$alpha,
                                  horizontal = horizontal,
                                  quantile = quantile.overlay, ...)
        }
        else {
            panel.data.or.overlay(x = x.overlay, y = y.overlay, groups = groups,
                                  subscripts = subscripts.overlay,
                                  type = overlay$type, col = overlay$col,
                                  lwd = overlay$lwd, pch = overlay$pch,
                                  alpha = overlay$alpha,
                                  horizontal = horizontal,
                                  quantile = quantile.overlay, ...)
            panel.data.or.overlay(x = x.data, y = y.data, groups = groups,
                                  subscripts = subscripts.data, type = type,
                                  col = col, lwd = lwd, pch = pch,
                                  alpha = alpha,
                                  horizontal = horizontal,
                                  quantile = quantile.data, ...)
        }
    }
    else
        panel.data.or.overlay(x = x, y = y, groups = groups,
                              subscripts = subscripts, type = type,
                              col = col, lwd = lwd, pch = pch,
                              alpha = alpha, horizontal = horizontal,
                              quantile = quantile, ...)
}

panel.data.or.overlay <- function(x, y, groups = NULL, subscripts,
                                  type = NULL, col = NULL, lwd = NULL,
                                  pch = NULL, alpha = NULL,
                                  horizontal = FALSE,
                                  quantile = NULL, ...) {
    kShowSymbols <- 10L
    has.groups <- !is.null(groups)
    has.quantile <- !is.null(quantile)
    if (horizontal)
        predictor.numeric <- is.numeric(y)
    else
        predictor.numeric <- is.numeric(x)
    if (has.groups)
        line.pars <- lattice::trellis.par.get("superpose.line")
    else
        line.pars <- lattice::trellis.par.get("plot.line")
    if (is.null(col))
        col <- line.pars$col
    if (is.null(alpha))
        alpha <- line.pars$alpha
    if (is.null(lwd))
        lwd <- 1
    if (is.null(pch))
        pch <- 16
    if (is.null(type))
        type <- if (length(subscripts) <= kShowSymbols) "o" else "l"
    if (has.quantile)
        panel.quantiles(x = x, y = y, groups = groups, subscripts = subscripts,
                        type = type, col = col, lwd = lwd, alpha = alpha,
                        predictor.numeric = predictor.numeric, horizontal = horizontal,
                        quantile = quantile, ...)
    else
        panel.point.estimate(x = x, y = y, groups = groups, subscripts = subscripts,
                             type = type, col = col, lwd = lwd, pch = pch,
                             alpha = alpha, horizontal = horizontal,
                             predictor.numeric = predictor.numeric, ...)
}

panel.point.estimate <- function(x, y, groups = NULL, subscripts, type, col,
                                 lwd, pch, alpha, horizontal, predictor.numeric, ...) {
    has.groups <- !is.null(groups)
    if (has.groups)
        lattice::panel.superpose(x = x, y = y, groups = groups, subscripts = subscripts,
                                 type = type, col = col, lwd = lwd, pch = pch, alpha = alpha,
                                 predictor.numeric = predictor.numeric, horizontal = horizontal,
                                 panel.groups = panel.point.estimate, ...)
    else {
        if (predictor.numeric) {
            lattice::panel.xyplot(x = x, y = y,
                                  groups = groups, subscripts = subscripts,
                                  type = type, col = col, lwd = lwd, pch = pch,
                                  alpha = alpha, ...)
        }
        else {
            if (horizontal) {
                y <- as.integer(y)
                x0 <- x; x1 <- x; y0 <- y - 0.4; y1 <- y + 0.4
            }
            else {
                x <- as.integer(x)
                x0 <- x - 0.4; x1 <- x + 0.4; y0 <- y; y1 <- y
            }
            lattice::panel.segments(x0 = x0, x1 = x1, y0 = y0, y1 = y1,
                                    groups = groups, subscripts = subscripts,
                                    col = col, lwd = lwd, pch = pch, alpha = alpha,
                                    identifier = "dplot.point.estimate", ...)
        }
    }
}

panel.quantiles <- function(x, y, groups = NULL, subscripts, type, col, lwd, alpha,
                            horizontal, predictor.numeric, quantile, ...) {
    if (is.null(groups)) {
        panel.quantile.polygon(x = x, y = y, groups = groups, subscripts = subscripts, col = col,
                               alpha = alpha, predictor.numeric = predictor.numeric,
                               horizontal = horizontal, quantile = quantile, ...)
        panel.median(x = x, y = y, groups = groups, subscripts = subscripts, type = type,
                     col = col, lwd = lwd, predictor.numeric = predictor.numeric,
                     horizontal = horizontal, quantile = quantile, ...)
    }
    else {
        lattice::panel.superpose(x = x, y = y, groups = groups, subscripts = subscripts, col = col,
                                 alpha = alpha, predictor.numeric = predictor.numeric,
                                 horizontal = horizontal, quantile = quantile,
                                 panel.groups = panel.quantile.polygon, ...)
        lattice::panel.superpose(x = x, y = y, groups = groups, subscripts = subscripts, type = type,
                                 col = col, lwd = lwd, predictor.numeric = predictor.numeric,
                                 horizontal = horizontal, quantile = quantile,
                                 panel.groups = panel.median, ...)
    }
}

panel.quantile.polygon <- function(x, y, groups = NULL, subscripts, col, alpha,
                                   predictor.numeric, horizontal, quantile, ...) {
    levels <- intersect(levels(quantile), quantile)
    n.levels <- length(levels)
    n.polygons <- floor(n.levels / 2)
    colfun <- grDevices::colorRampPalette(colors = c("white", col, "black"))
    col.polygons <- colfun(n.polygons + 2L)[seq(from = 2L, to = n.polygons + 1L)]
    if (!predictor.numeric) {
        if (horizontal)
            y <- as.integer(y)
        else
            x <- as.integer(x)
    }
    for (i in seq_len(n.polygons)) {
        if (horizontal) {
            x0 <- x[quantile[subscripts] == levels[i]]
            x1 <- x[quantile[subscripts] == levels[n.levels - i + 1L]]
            y0 <- y[quantile[subscripts] == levels[1L]]
            y1 <- y[quantile[subscripts] == levels[1L]]
            if (predictor.numeric) {
                x <- c(x0, rev(x1))
                y <- c(y0, rev(y1))
            }
            else {
                xleft <- x0
                xright <- x1
                ybottom <- y0 - 0.4
                ytop <- y0 + 0.4
            }
        }
        else {
            x0 <- x[quantile[subscripts] == levels[1L]]
            x1 <- x[quantile[subscripts] == levels[1L]]
            y0 <- y[quantile[subscripts] == levels[i]]
            y1 <- y[quantile[subscripts] == levels[n.levels - i + 1L]]
            if (predictor.numeric) {
                x.poly <- c(x0, rev(x1))
                y.poly <- c(y0, rev(y1))
            }
            else {
                xleft <- x0 - 0.4
                xright <- x0 + 0.4
                ybottom <- y0
                ytop <- y1
            }
        }
        if (predictor.numeric)
            lattice::panel.polygon(x = x.poly, y = y.poly,
                                   col = col.polygons[i], border = FALSE,
                                   alpha = alpha, identifier = "dplot.quantile.polygon", ...)
        else
            lattice::panel.rect(xleft = xleft, xright = xright,
                                ybottom = ybottom, ytop = ytop,
                                col = col.polygons[i], border = FALSE, alpha = alpha,
                                identifier = "dplot.quantile.polygon")
    }
}

panel.median <- function(x, y, groups = NULL, subscripts, type, col, lwd,
                         predictor.numeric, horizontal, quantile,
                         col.line = col, col.symbol = col,
                         ...) {
    n.levels <- nlevels(quantile)
    colfun <- grDevices::colorRampPalette(colors = c("white", col, "black"))
    col <- colfun(n.levels + 4L)[2L]
    col.line <- col
    is.median <- quantile[subscripts] == "50%"
    if (any(is.median)) {
        x <- x[is.median]
        y <- y[is.median]
        if (predictor.numeric)
            lattice::panel.xyplot(x = x, y = y, col = col, type = type, lwd = lwd,
                                  identifier = "dplot.median", ...)
        else {
            if (horizontal) {
                y <- as.integer(y)
                x0 <- x
                x1 <- x
                y0 <- y - 0.4
                y1 <- y + 0.4
            }
            else {
                x <- as.integer(x)
                x0 <- x - 0.4
                x1 <- x + 0.4
                y0 <- y
                y1 <- y
            }                
            lattice::panel.segments(x0 = x0, x1 = x1, y0 = y0, y1 = y1, col = col,
                                    lwd = lwd, identifier = "dplot.median", ...)
        }
    }
}


## NO_TESTS
dapply <- function(X, MARGIN, FUN, ...) {
    n.dim <- length(dim(X))
    MARGIN <- tidySubscript(MARGIN, nDim = n.dim, names = names(X))
    dimension <- invertSubscript(MARGIN, nDim = n.dim)
    .Data <- X@.Data
    metadata <- metadata(X)
    subarrays <- apply(.Data, MARGIN = MARGIN, FUN = list)
    subarrays <- unlist(subarrays, recursive = FALSE)
    makeArray <- function(x) array(x, dim = dim(X)[dimension], dimnames = dimnames(X)[dimension])
    subarrays <- lapply(subarrays, makeArray)
    tmp <- methods::new(class(X), subarrays[[1L]], metadata = metadata[dimension])
    for (i in seq_along(subarrays)) {
        tmp@.Data <- subarrays[[i]]  ## avoid repeated validity tests
        subarrays[[i]] <- FUN(tmp, ...)
    }
    lengths <- sapply(subarrays, length)
    lengths.equal <- all(lengths == lengths[1L])
    if (!lengths.equal)
        return(subarrays)
    first.subarray <- subarrays[[1L]]
    has.dims <- !is.null(dim(first.subarray))
    if (has.dims) {
        if (validDimnames(dimnames(first.subarray))) {
            if (methods::is(first.subarray, "DemographicArray")) {
                metadata.new <- metadata(first.subarray)
                class.new <- class(first.subarray)
            }
            else {
                metadata.new <- MetaData(first.subarray)
                class.new <- class(X)
            }
            nms <- c(names(metadata.new), names(metadata)[MARGIN])
            dimtypes <- c(dimtypes(metadata.new, use.names = FALSE),
                          dimtypes(metadata, use.names = FALSE)[MARGIN])
            DimScales <- c(DimScales(metadata.new, use.names = FALSE),
                           DimScales(metadata, use.names = FALSE)[MARGIN])
            metadata <- methods::new("MetaData", nms = nms, dimtypes = dimtypes, DimScales = DimScales)
            .Data <- array(unlist(subarrays), dim = dim(metadata), dimnames = dimnames(metadata))
            ans <- methods::new(class.new, .Data = .Data, metadata = metadata)
            perm <- match(names(ans), names(X), nomatch = 0L)
            perm <- c(which(perm == 0L), perm[perm != 0L])
            aperm(ans, perm = perm)
        }
        else
            simplify2array(subarrays)
    }
    else {
        if (identical(lengths[1L], 1L)) {
            metadata <- metadata[MARGIN]
            .Data <- array(.Data, dim = dim(X)[MARGIN], dimnames = dimnames(X)[MARGIN])
            methods::new(class(X), .Data = .Data, metadata = metadata)
        }
        else {
            if (is.null(names(first.subarray)))
                use.dimension <- identical(lengths[1L], prod(dim(X)[dimension]))
            else
                use.dimension <- (identical(length(dimension), 1L) &&
                                  identical(names(first.subarray), dimnames(X)[[dimension]]))
            if (use.dimension) {
                s <- c(dimension, MARGIN)
                .Data <- array(unlist(subarrays), dim = dim(X)[s], dimnames = dimnames(X)[s])
                .Data <- aperm(.Data, perm = match(seq_len(n.dim), s))
                methods::new(class(X), .Data = .Data, metadata = metadata)
            }
            else
                simplify2array(subarrays)
        }
    }
}


## FUNCTIONS FOR MANIPULATING, PERTURBING AND REDISTRIBUTING DATA ######################



checkAndTidyReset <- function(reset) {
    if (!identical(length(reset), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "reset", 1L))
    if (is.na(reset))
        NA_integer_
    else {
        if (!is.numeric(reset))
            stop(gettextf("'%s' is non-numeric",
                          "reset"))
        if (isTRUE(all.equal(reset, round(reset))))
            as.integer(reset)
        else
            as.numeric(reset)
    }
}



## HAS_TESTS
checkAndTidyEpsilon <- function(epsilon) {
    if (!identical(length(epsilon), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "epsilon", 1L))
    if (!is.numeric(epsilon))
        stop(gettextf("'%s' is non-numeric",
                      "epsilon"))
    epsilon <- as.numeric(epsilon)
    if (is.na(epsilon))
        stop(gettextf("'%s' is missing",
                      "epsilon"))
    if (epsilon < 0)
        stop(gettextf("'%s' is negative",
                      "epsilon"))
    epsilon
}

## HAS_TESTS
checkAndTidyN <- function(n) {
    if (is.null(n))
        NULL
    else {
        if (!identical(length(n), 1L))
            stop(gettextf("'%s' does not have length %d",
                          "n", 1L))
        if (!is.numeric(n))
            stop(gettextf("'%s' is non-numeric",
                          "n"))
        if (is.na(n))
            stop(gettextf("'%s' is missing",
                          "n"))
        if (!isTRUE(all.equal(n, as.integer(n))))
            stop(gettextf("'%s' is not an integer",
                          "n"))
        n <- as.integer(n)
        if (n < 1L)
            stop(gettextf("'%s' is less than %d",
                          "n", 1L))
        n
    }
}

## HAS_TESTS
checkMeans <- function(means) {
    if (!is.logical(means))
        stop(gettextf("'%s' does not have type \"%s\"",
                      "means", "logical"))
    if (!identical(length(means), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "means", 1L))
    if (is.na(means))
        stop(gettextf("'%s' is missing",
                      "means"))
    NULL
}

## HAS_TESTS
perturbUsingIterations <- function(object, n, i.iter) {
    n.iter <- dim(object)[i.iter]
    if (n <= n.iter) {
        i.keep <- sample.int(n.iter, size = n, replace = FALSE)
        ans <- slab(object, dimension = i.iter, elements = i.keep, drop = TRUE)
    }
    else
        stop(gettextf("'%s' greater than '%s'", "n", "n.iter"))
    if (n > 1L) {
        ## TODO - shorten this when dimnames<- written
        nms <- names(ans)
        dimtypes <- dimtypes(ans, use.names = FALSE)
        DimScales <- replace(DimScales(ans, use.names = FALSE),
                             list = i.iter,
                             values = list(methods::new("Iterations", dimvalues = seq_len(n))))
        metadata <- methods::new("MetaData", nms = nms, dimtypes = dimtypes, DimScales = DimScales)
        .Data <- array(ans@.Data, dim = dim(metadata), dimnames = dimnames(metadata))
        ans <- methods::new(class(ans), .Data = .Data, metadata = metadata)
    }
    ans
}

## HAS_TESTS
perturbUsingModel <- function(object, n, order, phi) {
    no.negative <- !any(object < 0L, na.rm = TRUE)
    no.missing <- !any(is.na(object))
    data.as.array <- no.negative && no.missing
    use.loglm <- no.negative
    if (data.as.array) {
        data <- object@.Data
        predictors <- paste(names(object), collapse = "+")
        if (order > 1L)
            formula <- sprintf("~ (%s)^%d", predictors, order)
        else
            formula <- sprintf("~ %s", predictors)
    }
    else {
        data <- as.data.frame(object, direction = "long")
        data <- data[stats::complete.cases(data), ]
        ncol <- ncol(data)
        response <- names(data)[ncol]
        predictors <- paste(names(data)[-ncol], collapse = "+")
        if (order > 1L)
            formula <- sprintf("%s ~ (%s)^%d", response, predictors, order)
        else
            formula <- sprintf("%s ~ %s", response, predictors)
    }
    formula <- stats::as.formula(formula)
    if (use.loglm)
        fitted <- fitted(MASS::loglm(formula, data = data, fit = TRUE))
    else
        fitted <- fitted(stats::lm(formula, data = data))
    if (any(is.na(fitted)))
        stop(gettextf("fitted values from %s model of order %d include NAs",
                      if (no.negative) "loglinear" else "linear", order))
    if (use.loglm) {
        if (phi == 1)
            .Data <- stats::rpois(n = n * length(object), lambda = fitted)
        else {
            size <- 1 / (phi - 1)
            .Data <- stats::rnbinom(n = n * length(object), size = size, mu = fitted)
        }
        .Data <- as.integer(.Data)
    }
    else {
        sd.errors <- phi * sqrt(mean(abs(fitted))) # analagous to Poisson
        .Data <- stats::rnorm(n = n * length(object), mean = fitted, sd = sd.errors)
    }
    if (n > 1L) {
        nms <- make.unique(c(names(object), "iteration"))
        dimtypes <- c(dimtypes(object, use.names = FALSE), "iteration")
        DimScales <- c(DimScales(object, use.names = FALSE),
                       list(methods::new("Iterations", dimvalues = seq_len(n))))
        metadata <- methods::new("MetaData", nms = nms, dimtypes = dimtypes, DimScales = DimScales)
    }
    else
        metadata <- metadata(object)
    .Data <- array(.Data, dim = dim(metadata), dimnames = dimnames(metadata))
    methods::new(class(object), .Data = .Data, metadata = metadata)
}

## HAS_TESTS
reallocateOvers <- function(x, max) {
    ## 'x'
    stopifnot(is.integer(x))
    stopifnot(!any(is.na(x)))
    stopifnot(all(x >= 0))
    ## 'max'
    stopifnot(is.integer(max))
    stopifnot(!any(is.na(max)))
    stopifnot(all(max >= 0))
    ## 'x' and 'max'
    stopifnot(identical(length(x), length(max)))
    stopifnot(sum(x) <= sum(max))
    n <- length(x)
    diff <- x - max
    total.overs <- sum(diff[diff > 0L])
    ans <- pmin(x, max)
    unders <- max - ans
    while (total.overs > 0L) {
        i <- sample.int(n = n, size = 1L, prob = unders)
        ans[i] <- ans[i] + 1L
        unders[i] <- unders[i] - 1L
        total.overs <- total.overs - 1L
    }
    ans
}

## TRANSLATED
## HAS_TESTS
redistributeInnerMeans <- function(counts, weights, transform, useC) {
    ## counts
    stopifnot(length(counts) > 0L)
    stopifnot(is.integer(counts))
    stopifnot(!any(is.na(counts)))
    stopifnot(all(counts >= 0L))
    ## weights
    stopifnot(length(weights) > 0L)
    stopifnot(is.double(weights))
    stopifnot(!any(is.na(weights)))
    stopifnot(all(weights >= 0))
    stopifnot(sum(weights) > 0)
    ## transform
    stopifnot(methods::is(transform, "CollapseTransformExtra"))
    ## counts and transform
    stopifnot(identical(length(counts), as.integer(prod(transform@dimAfter))))
    ## weights and transform
    stopifnot(identical(length(weights), as.integer(prod(transform@dimBefore))))
    if (useC) {
        .Call(redistributeInnerMeans_R, counts, weights, transform)
    }
    else {
        ans <- rep(0L, times = length(weights))
        for (i.counts in seq_along(counts)) {
            size <- counts[i.counts]
            if (size > 0L) {
                i.weights <- getIBefore(i.counts, transform = transform)
                prob <- weights[i.weights]
                if (sum(prob) > 0) {
                    prob <- prob / sum(prob)
                    ans[i.weights] <- size * prob
                }
                else
                    stop(sprintf("weights for element %d of 'counts' sum to 0",
                                 i.counts))
            }
        }
        ans
    }
}

## TRANSLATED
## HAS_TESTS
redistributeInnerDistn <- function(counts, weights, transform, useC) {
    ## counts
    stopifnot(length(counts) > 0L)
    stopifnot(is.integer(counts))
    stopifnot(!any(is.na(counts)))
    stopifnot(all(counts >= 0L))
    ## weights
    stopifnot(length(weights) > 0L)
    stopifnot(is.double(weights))
    stopifnot(!any(is.na(weights)))
    stopifnot(all(weights >= 0))
    stopifnot(sum(weights) > 0)
    ## transform
    stopifnot(methods::is(transform, "CollapseTransformExtra"))
    ## counts and transform
    stopifnot(identical(length(counts), as.integer(prod(transform@dimAfter))))
    ## weights and transform
    stopifnot(identical(length(weights), as.integer(prod(transform@dimBefore))))
    if (useC) {
        .Call(redistributeInnerDistn_R, counts, weights, transform)
    }
    else {
        ans <- rep(0L, times = length(weights))
        for (i.counts in seq_along(counts)) {
            size <- counts[i.counts]
            if (size > 0L) {
                i.weights <- getIBefore(i.counts, transform = transform)
                prob <- weights[i.weights]
                if (sum(prob) > 0)
                    ans[i.weights] <- stats::rmultinom(n = 1L, size = size, prob = prob)
                else
                    stop(sprintf("weights for element %d of 'counts' sum to 0",
                                 i.counts))
            }
        }
        ans
    }
}

## HAS_TESTS
resetDiagInner <- function(object, base, reset) {
    reset <- checkAndTidyReset(reset)
    .Data <- object@.Data
    names <- names(object)
    dimtypes <- dimtypes(object, use.names = FALSE)
    DimScales <- DimScales(object, use.names = FALSE)
    if (is.null(base)) {
        is.orig <- dimtypes == "origin"
        if (!any(is.orig))
            stop(gettextf("no dimensions with dimtypes \"%s\" or \"%s\"",
                          "origin", "destination"))
        base <- removeSuffixes(names[is.orig])
    }
    i.orig <- match(sprintf("%s_orig", base), names, nomatch = 0L)
    if (any(i.orig == 0L))
        stop(gettextf("'%s' outside valid range", "base"))
    i.dest <- match(sprintf("%s_dest", base), names)
    for (i in seq_along(i.orig)) {
        i.dim.orig <- i.orig[i]
        i.dim.dest <- i.dest[i]
        DS.orig <- DimScales[[i.dim.orig]]
        DS.dest <- DimScales[[i.dim.dest]]
        dv.orig <- dimvalues(DS.orig)
        dv.dest <- dimvalues(DS.dest)
        index.orig <- slice.index(.Data, MARGIN = i.dim.orig)
        index.dest <- slice.index(.Data, MARGIN = i.dim.dest)
        for (i.dv.orig in seq_along(dv.orig)) {
            dv <- dv.orig[i.dv.orig]
            i.dv.dest <- match(dv, dv.dest, nomatch = 0L)
            has.dv <- i.dv.dest > 0L
            if (has.dv) {
                index <- (index.orig == i.dv.orig) & (index.dest == i.dv.dest)
                .Data[index] <- reset
            }
        }
    }
    class <- if (methods::is(object, "Counts")) "Counts" else "Values"
    metadata <- object@metadata
    methods::new(class,
                 .Data = .Data,
                 metadata = metadata)
}

uniformWeightsForExpandIntervals <- function(breaks, dimension, metadata) {
    dim <- dim(metadata)
    DimScales <- DimScales(metadata, use.names = FALSE)
    DimScale <- DimScales[[dimension]]
    s <- seq_along(dim)
    s.perm <- c(dimension, s[-dimension])
    dim.permuted <- dim[s.perm]
    widths <- diff(breaks)
    widths[is.infinite(widths)] <- 1
    dim.permuted[1L] <- length(widths)
    ans <- array(widths, dim = dim.permuted)
    perm <- match(s, s.perm)
    ans <- aperm(ans, perm = perm)
    ans
}


## FUNCTIONS RELATED TO LIFE TABLES ##################################################

## These functions belong more naturally in 'demlife', but they are included here
## so that 'demest' has access to them without needing to install 'demlife'.


## HAS_TESTS
#' @rdname exported-not-api
#' @export
expandAx <- function(ax, object) {
    names.ax <- names(ax)
    names.obj <- names(object)
    dimtypes.ax <- dimtypes(ax)
    dimtypes.obj <- dimtypes(object)
    DimScales.ax <- DimScales(ax, use.names = FALSE)
    DimScales.obj <- DimScales(object, use.names = FALSE)
    dimnames.ax <- dimnames(ax)
    dimnames.obj <- dimnames(object)
    i.age.ax <- match("age", dimtypes.ax, nomatch = 0L)
    i.age.obj <- match("age", dimtypes.obj, nomatch = 0L)
    has.age.ax <- i.age.ax > 0L
    has.age.obj <- i.age.obj > 0L
    if (!has.age.ax)
        stop(gettextf("'%s' does not have a dimension with %s \"%s\"",
                      "ax", "dimtype", "age"))
    if (!has.age.obj)
        stop(gettextf("'%s' does not have a dimension with %s \"%s\"",
                      "object", "dimtype", "age"))
    DimScale.age.ax <- DimScales.ax[[i.age.ax]]
    DimScale.age.obj <- DimScales.obj[[i.age.obj]]
    if (!methods::is(DimScale.age.ax, "Intervals"))
        stop(gettextf("dimension of '%s' with %s \"%s\" does not have %s \"%s\"",
                      "ax", "dimtype", "age", "dimscale", "Intervals"))
    if (!methods::is(DimScale.age.obj, "Intervals"))
        stop(gettextf("dimension of '%s' with %s \"%s\" does not have %s \"%s\"",
                      "object", "dimtype", "age", "dimscale", "Intervals"))
    dv.ax.old <- DimScale.age.ax@dimvalues
    dv.obj <- DimScale.age.obj@dimvalues
    n.dv.ax.old <- length(dv.ax.old)
    n.dv.obj <- length(dv.obj)
    dn.ax <- dimnames.ax[[i.age.ax]]
    dn.obj <- dimnames.obj[[i.age.obj]]
    if (is.infinite(dv.ax.old[1L]))
        stop(gettextf("first age interval of '%s' is open on left",
                      "ax"))
    if (is.infinite(dv.obj[1L]))
        stop(gettextf("first age interval of '%s' is open on left",
                      "object"))
    min.dv.ax.old <- min(dv.ax.old)
    i.min <- match(min.dv.ax.old, dv.obj, nomatch = 0L)
    if (i.min > 1L) {
        s <- seq_len(i.min)
        labels.lower <- dn.obj[s[-length(s)]]
        ax <- extrapolate(ax,
                          along = i.age.ax,
                          labels = labels.lower,
                          type = "missing")
        nx <- diff(dv.obj[s])
        index <- slice.index(ax@.Data, MARGIN = i.age.ax)
        for (i in seq_along(nx))
            ax[index == i] <- 0.5 * nx[i]
        ## in future add better approximation for age group 0 and 1-4
    }
    max.dv.ax.old <- max(dv.ax.old)
    i.max <- match(max.dv.ax.old, dv.obj, nomatch = 0L)
    if ((0L < i.max ) && (i.max < n.dv.obj)) {
        s <- seq.int(from = i.max, to = n.dv.obj)
        labels.higher <- dn.obj[s[-length(s)]]
        ax <- extrapolate(ax,
                          along = i.age.ax,
                          labels = labels.higher,
                          type = "missing")
        nx <- diff(dv.obj[s])
        index <- slice.index(ax@.Data, MARGIN = i.age.ax)
        for (i in seq_along(nx)) {
            if (is.finite(nx[i]))
                ax[index == s[i]] <- 0.5 * nx[i]
            else
                ax[index == s[i]] <- 0.5 * nx[i - 1L]
        }
    }
    DimScale.ax.new <- DimScales(ax)[[i.age.ax]]
    dv.ax.new <- DimScale.ax.new@dimvalues
    ## use stricter test for compatibility than standard one, since do not want
    ## ax values shared across multiple age intervals in object
    if (!isTRUE(all(dv.obj %in% dv.ax.new)))
        stop(gettextf("dimensions of '%s' and '%s' with dimtype \"%s\" not compatible",
                      "ax", "object", "age"))
    ans <- tryCatch(makeCompatible(x = ax,
                                   y = object,
                                   subset = TRUE,
                                   check = TRUE),
                    error = function(e) e)
    if (methods::is(ans, "error"))
        stop(gettextf("'%s' and '%s' not compatible : %s",
                      "ax", "object", ans$message))
    ans
}

## HAS_TESTS
## Based on Coale-Demeny formulas given in Preston et al. 2001.
## Demography. p48
imputeA <- function(m0, A = c("1a0", "4a1"), sex = c("Female", "Male")) {
    if (!is.numeric(m0))
        stop(gettextf("'%s' is non-numeric",
                      "m0"))
    if (any(m0[!is.na(m0)] < 0))
        stop(gettextf("'%s' has negative values",
                      "m0"))
    A <- match.arg(A)
    sex <- match.arg(sex)
    ans <- rep(as.numeric(NA), times = length(m0))
    is.high <- !is.na(m0) & m0 >= 0.107
    is.low <- !is.na(m0) & m0 < 0.107
    if (identical(sex, "Female")) {
        if (identical(A, "1a0")) {
            ans[is.high] <- 0.35
            ans[is.low] <- 0.053 + 2.8 * m0[is.low]
        }
        else {
            ans[is.high] <- 1.361
            ans[is.low] <- 1.522 - 1.518 * m0[is.low]
        }
    }
    else {
        if (identical(A, "1a0")) {
            ans[is.high] <- 0.33
            ans[is.low] <- 0.045 + 2.684 * m0[is.low]
        }
        else {
            ans[is.high] <- 1.352
            ans[is.low] <- 1.651 - 2.816 * m0[is.low]
        }
    }
    ans
}

## HAS_TESTS
#' @rdname exported-not-api
#' @export
makeAxStart <- function(mx) {
    if (!is(mx, "Values"))
        stop(gettextf("'%s' has class \"%s\"",
                      "mx", class(mx)))
    if (length(mx) == 0L)
        stop(gettextf("'%s' has length %d",
                      "mx", 0L))
    dim <- dim(mx)
    names <- names(mx)
    dimtypes <- dimtypes(mx, use.names = FALSE)
    DimScales <- DimScales(mx, use.names = FALSE)
    i.age <- match("age", dimtypes, nomatch = 0L)
    i.sex <- match("sex", dimtypes, nomatch = 0L)
    has.age <- i.age > 0L
    has.sex <- i.sex > 0L
    if (!has.age)
        stop(gettextf("'%s' does not have dimension with %s \"%s\"",
                      "mx", "dimtype", "age"))
    DimScale.age <- DimScales[[i.age]]
    if (!methods::is(DimScale.age, "Intervals"))
        stop(gettextf("dimension of '%s' with %s \"%s\" does not have %s \"%s\"",
                      "mx", "dimtype", "age", "dimscale", "Intervals"))
    dv.age <- dimvalues(DimScale.age)
    n.age <- length(DimScale.age)
    if (has.sex) {
        DimScale.sex <- DimScales[[i.sex]]
        n.sex <- length(DimScale.sex)
    }
    has.1m0 <- (isTRUE(all.equal(dv.age[1L], 0))
        && isTRUE(all.equal(dv.age[2L], 1)))
    has.4m1 <- ((n.age > 1L)
        && isTRUE(all.equal(dv.age[2L], 1))
        && isTRUE(all.equal(dv.age[3L], 5)))
    if (has.1m0) {
        nrow.ans <- if (has.4m1) 2L else 1L
        ncol.ans <- prod(dim[-c(i.age, i.sex)])
        .Data.ans.female <- matrix(nrow = nrow.ans,
                                   ncol = ncol.ans)
        .Data.ans.male <- .Data.ans.female
        m0 <- slab(mx,
                   dimension = i.age,
                   elements = 1L,
                   drop = FALSE)
        if (has.sex) {
            i.female <- iFemale(DimScale.sex)
            i.male <- iMale(DimScale.sex)
            has.female <- i.female > 0L
            has.male <- i.male > 0L
            if (has.female) {
                m0.female <- slab(m0,
                                  dimension = i.sex,
                                  elements = i.female)
                m0.female <- as.numeric(m0.female)
                .Data.ans.female[1L, ] <- imputeA(m0 = m0.female,
                                                  A = "1a0",
                                                  sex = "Female")
            }
            if (has.male) {
                m0.male <- slab(m0,
                                dimension = i.sex,
                                elements = i.male)
                m0.male <- as.numeric(m0.male)
                .Data.ans.male[1L, ] <- imputeA(m0 = m0.male,
                                                A = "1a0",
                                                sex = "Male")
            }
        }
        else {
            m0 <- as.numeric(m0)
            .Data.ans.female[1L, ] <- imputeA(m0 = m0,
                                              A = "1a0",
                                              sex = "Female")
            .Data.ans.male[1L, ] <- imputeA(m0 = m0,
                                            A = "1a0",
                                            sex = "Male")
        }
        if (has.4m1) {
            if (has.sex) {
                if (has.female)
                    .Data.ans.female[2L, ] <- imputeA(m0 = m0.female,
                                                      A = "4a1",
                                                      sex = "Female")
                if (has.male)
                    .Data.ans.male[2L, ] <- imputeA(m0 = m0.male,
                                                    A = "4a1",
                                                    sex = "Male")
            }
            else {
                .Data.ans.female[2L, ] <- imputeA(m0 = m0,
                                                  A = "4a1",
                                                  sex = "Female")
                .Data.ans.male[2L, ] <- imputeA(m0 = m0,
                                                A = "4a1",
                                                sex = "Male")
            }
        }
        if (has.sex) {
            if (has.female && has.male) {
                if (i.female == 1L)
                    .Data.ans <- c(.Data.ans.female, .Data.ans.male)
                else
                    .Data.ans <- c(.Data.ans.male, .Data.ans.female)
            }
            else {
                if (has.female)
                    .Data.ans <- .Data.ans.female
                else
                    .Data.ans <- .Data.ans.male
            }
        }
        else {
            sex.ratio <- getDefaultSexRatio()
            pr.female <- 100 / (100 + sex.ratio)
            .Data.ans <- (pr.female * .Data.ans.female
                + (1 - pr.female) * .Data.ans.male)
        }
    }
    else {
        nx <- dv.age[2L] - dv.age[1L]
        .Data.ans <- rep(nx / 2, times = length(mx))
    }
    dv.age.ans <- if (has.4m1) dv.age[1:3] else dv.age[1:2]
    DimScale.age.ans <- new("Intervals",
                            dimvalues = dv.age.ans,
                            isAge = TRUE,
                            labelStart = TRUE)
    DimScales.ans <- replace(DimScales,
                             list = i.age,
                             values = list(DimScale.age.ans))
    s <- seq_along(dim)
    perm <- c(i.age, s[-c(i.age, i.sex)], i.sex)
    names.ans <- names[perm]
    dimtypes.ans <- dimtypes[perm]
    DimScales.ans <- DimScales.ans[perm]
    metadata.ans <- new("MetaData",
                        nms = names.ans,
                        dimtypes = dimtypes.ans,
                        DimScales = DimScales.ans)
    .Data.ans <- array(.Data.ans,
                       dim = dim(metadata.ans),
                       dimnames = dimnames(metadata.ans))
    new("Values",
        .Data = .Data.ans,
        metadata = metadata.ans)
}    


## FUNCTIONS RELATED TO CONCORDANCES ##################################################

## HAS_TESTS
tidyConcordanceList <- function(concordances, object) {
    metadata <- metadata(object)
    names.obj <- names(metadata)
    n <- length(names.obj)
    ans <- rep(list(NULL), times = n)
    names(ans) <- names.obj
    if (identical(concordances, list()))
        return(ans)
    if (!is.list(concordances))
        stop(gettextf("'%s' has class \"%s\"",
                      "concordances", class(concordances)))
    if (!all(sapply(concordances, methods::is,"ManyToOne")))
        stop(gettextf("'%s' has elements not of class \"%s\"",
                      "concordances", "ManyToOne"))
    names.conc <- names(concordances)
    if (is.null(names.conc))
        stop(gettextf("'%s' does not have names",
                      "concordances"))
    if (any(duplicated(names.conc)))
        stop(gettextf("'%s' has duplicate names",
                      "concordances"))
    dimtypes <- dimtypes(metadata, use.names = FALSE)
    DimScales <- DimScales(metadata, use.names = FALSE)
    dimtypes.with.pairs <- getDimtypesWithPairs()
    for (i.obj in seq_along(ans)) {
        name <- names.obj[i.obj]
        i.conc <- match(name, names.conc, nomatch = 0L)
        if (i.conc > 0L) {
            DimScale <- DimScales[[i.obj]]
            if (!methods::is(DimScale, "Categories"))
                stop(gettextf("concordance supplied for \"%s\" dimension, but \"%s\" dimension has dimscale \"%s\"",
                              name, name, class(DimScale)))
        }
        else {
            dimtype <- dimtypes[i.obj]
            if (dimtype %in% dimtypes.with.pairs) {
                name <- removeSuffixes(name)
                i.conc <- match(name, names.conc, nomatch = 0L)
            }
        }
        found.match <- i.conc > 0L
        if (found.match)
            ans[[i.obj]] <- concordances[[i.conc]]
    }
    ans
}





## HELPER FUNCTIONS FOR 'project' ##############################################

## make a future version of this visible to users?
## HAS_TESTS
ageForward <- function(population) {
    dimtypes <- dimtypes(population, use.names = FALSE)
    ## does not have dimtypes "time" or "cohort"
    for (dimtype in c("time", "cohort")) {
        if (dimtype %in% dimtypes)
            stop(gettextf("'%s' has dimension with %s \"%s\"",
                          "population", "dimtype", dimtype))
    }
    i.age <- match("age", dimtypes(population), nomatch = 0L)
    has.age <- i.age > 0L
    if (!has.age)
        return(population)
    DS.age <- DimScales(population)[[i.age]]
    n.age <- length(DS.age)
    ## "age" dimension has length > 0
    if (n.age == 0L)
        stop(gettextf("\"%s\" dimension of '%s' has length %d",
                      "age", "population", 0L))
    if (methods::is(DS.age, "Intervals")) {
        dv.age <- dimvalues(DS.age)
        ## "age" dimension not open on left
        first.age.open <- is.infinite(dv.age[1L])
        if (first.age.open)
            stop(gettextf("first age group of '%s' is open on left",
                          "population"))
        last.age.open <- is.infinite(dv.age[n.age + 1L])
    }
    else
        last.age.open <- FALSE
    .Data <- array(0L,
                   dim = dim(population),
                   dimnames = dimnames(population))
    metadata <- metadata(population)
    ans <- methods::new("Counts", .Data = .Data, metadata = metadata)
    if (n.age > 1L) {
        elements.to <- seq(from = 2L, to = n.age)
        elements.from <- seq(from = 1L, to = n.age - 1L)
        slab(ans, dimension = i.age, elements = elements.to) <-
            slab(population, dimension = i.age, elements = elements.from)
    }
    if (last.age.open) {
        slab(ans, dimension = i.age, elements = n.age) <-
            (slab(ans, dimension = i.age, elements = n.age)
             + slab(population, dimension = i.age, elements = n.age))
    }
    ans
}

## HAS_TESTS
checkAndTidyInitial <- function(initial) {
    ## 'initial' has class "Counts"
    if (!methods::is(initial, "Counts"))
        stop(gettextf("'%s' has class \"%s\"",
                      "initial", class(initial)))
    ## 'initial' has positive length
    if (identical(length(initial), 0L))
        stop(gettextf("'%s' has length %d",
                      "initial", 0L))
    ## 'initial' has no negative values
    if (any(initial[!is.na(initial)] < 0L))
        stop(gettextf("'%s' has negative values",
                      "initial"))
    ## Either (i) 'initial' has no time dimension, or (ii) the time dimension
    ## has length 1 and a "Points" dimscale and is not the only dimension.
    i.time <- match("time", dimtypes(initial), nomatch = 0L)
    has.time <- i.time > 0L
    if (has.time) {
        dim <- dim(initial)
        n.time <- dim[i.time]
        if (!identical(n.time, 1L))
            stop(gettextf("time dimension for '%s' does not have length 1",
                          "initial"))
        DimScale.time <- DimScales(initial)[[i.time]]
        if (!methods::is(DimScale.time, "Points"))
            stop(gettextf("time dimension for '%s' has %s \"%s\"",
                          "initial", "dimscale", class(DimScale.time)))
        if (identical(length(dim), 1L))
            stop(gettextf("'%s' has only one dimension, which has %s \"%s\"",
                          "initial", "dimtype", "time"))
    }
    ## if has "age" dimension, first age group is not open on left
    i.age <- match("age", dimtypes(initial), nomatch = 0L)
    has.age <- i.age > 0L
    if (has.age) {
        DS.age <- DimScales(initial)[[i.age]]
        dv.age <- dimvalues(DS.age)
        open.left <- is.infinite(dv.age[1L])
        if (open.left)
            stop(gettextf("'first age group of '%s' is open on left",
                          "initial"))
    }
    ## coerce to integer, raising error if non-integer values
    return.value <- tryCatch(toInteger(initial),
                             error = function(e) e)
    if (methods::is(return.value, "error"))
        stop(gettextf("'%s' invalid : %s",
                      "initial", return.value$message))
    return.value
}

## HAS_TESTS
checkAndTidyIterationsProject <- function(initial, param, n) {
    n <- checkAndTidyNIter(n)
    values <- c(list(initial = initial), param)
    names <- names(values)
    iIter <- function(x) match("iteration", dimtypes(x), nomatch = 0L)
    i.iter <- sapply(values, iIter)
    has.iter <- i.iter > 0L
    if (!is.null(n) || any(has.iter)) {
        if (is.null(n)) {
            i.first <- which(has.iter)[1L]
            val <- values[[i.first]]
            dim.first <- dim(val)
            i.iter.first <- i.iter[i.first]
            n.iter.first <- dim.first[i.iter.first]
            iter.standard <- seq_len(n.iter.first)
        }
        else
            iter.standard <- seq_len(n)
        n.iter.standard <- length(iter.standard)
        for (i in seq_along(values)) {
            val <- values[[i]]
            if (has.iter[i]) {
                i.iter.val <- i.iter[i]
                n.iter.val <- dim(val)[i.iter.val]
                if (n.iter.val > n.iter.standard) {
                    if (is.null(n))
                        stop(gettextf("'%s' has unexpected number of iterations",
                                      names[i]))
                    else {
                        keep <- sample(x = n.iter.val,
                                       size = n.iter.standard,
                                       replace = FALSE)
                        val <- slab(val,
                                     dimension = i.iter.val,
                                     elements = keep,
                                     drop = FALSE)
                        val <- resetIterations(val)
                    }
                }
                else if (n.iter.val == n.iter.standard) {
                    val <- resetIterations(val)
                }
                else
                    stop(gettextf("'%s' has unexpected number of iterations",
                                  names[i]))
            }
            else {
                metadata.old <- metadata(val)
                metadata.new <- addIterationsToMetadata(object = metadata.old,
                                                        iterations = iter.standard)
                .Data.old <- val@.Data
                .Data.new <- array(.Data.old,  ## replicated
                                   dim = dim(metadata.new),
                                   dimnames = dimnames(metadata.new))
                val <- methods::new(class(val), .Data = .Data.new, metadata = metadata.new)
            }
            values[[i]] <- val
        }
    }
    list(initial = values[[1L]], param = values[-1L])
}

## HAS_TESTS
checkAndTidyNIter <- function(n) {
    if (!is.null(n)) {
        if (!identical(length(n), 1L))
            stop(gettextf("'%s' does not have length %d",
                          "n", 1L))
        if (!is.numeric(n))
            stop(gettextf("'%s' is non-numeric",
                          "n"))
        if (is.na(n))
            stop(gettextf("'%s' is missing",
                          "n"))
        if (n != round(n))
            stop(gettextf("'%s' is not an integer",
                          "n"))
        n <- as.integer(n)
        if (n < 1L)
            stop(gettextf("'%s' is less than %d",
                          "n", 1L))
    }
    n
}

## HAS_TESTS
checkAndTidyParam <- function(birth, death,
                              externalIn, externalOut,
                              internalIn, internalOut) {
    ans <- list(birth = birth,
                death = death,
                externalIn = externalIn,
                externalOut = externalOut,
                internalIn = internalIn,
                internalOut = internalOut)
    ## at least one non-null rate
    is.null <- sapply(ans, is.null)
    if (all(is.null))
        stop(gettextf("birth, death, and migration rates all %s",
                      "NULL"))
    ans <- ans[!is.null]
    names <- names(ans)
    ## has externalOut iff has externalIn,
    ## and has internalOut iff has internalIn
    has.EI <- "externalIn" %in% names
    has.EO <- "externalOut" %in% names
    has.II <- "internalIn" %in% names
    has.IO <- "internalOut" %in% names
    if (has.EI && !has.EO)
        stop(gettextf("'%s' is non-%s but '%s' is %s",
                      "externalIn", "NULL", "externalOut", "NULL"))
    if (!has.EI && has.EO)
        stop(gettextf("'%s' is %s but '%s' is non-%s",
                      "externalIn", "NULL", "externalOut", "NULL"))
    if (has.II && !has.IO)
        stop(gettextf("'%s' is non-%s but '%s' is %s",
                      "internalIn", "NULL", "internalOut", "NULL"))
    if (!has.II && has.IO)
        stop(gettextf("'%s' is %s but '%s' is non-%s",
                      "internalIn", "NULL", "internalOut", "NULL"))
    ## checks that apply to all param
    for (i in seq_along(ans)) {
        value <- ans[[i]]
        name <- names[i]
        ## has class "Values"
        if (!methods::is(value, "Values"))
            stop(gettextf("'%s' has class \"%s\"",
                          name, class(value)))
        ## no negative values
        if (any(value[!is.na(value)] < 0L))
            stop(gettextf("'%s' has negative values",
                          name))
        ## is regular
        return.value <- tryCatch(hasRegularAgeTime(value),
                                 error = function(e) e)
        if (methods::is(return.value, "error"))
            stop(gettextf("'%s' does not have regular age-time plan : %s",
                          name, return.value$message))
        ## has time dimension with Intervals dimscale
        i.time <- match("time", dimtypes(value), nomatch = 0L)
        has.time <- i.time > 0L
        if (!has.time)
            stop(gettextf("'%s' does not have dimension with dimtype \"%s\"",
                          name, "time"))
        DS.time <- DimScales(value)[[i.time]]
        if (!methods::is(DS.time, "Intervals")) {
            stop(gettextf("time dimension for '%s' does not have \"%s\" dimscale",
                          name, "Intervals"))
        }
    }
    ## all param have same time dimscale
    n <- length(ans)
    if (n > 1L) {
        value <- ans[[1L]]
        i.time.1 <- match("time", dimtypes(value))
        DS.time.1 <- DimScales(value)[[i.time.1]]
        for (i in seq.int(from = 2L, to = n)) {
            value <- ans[[i]]
            i.time <- match("time", dimtypes(value))
            DS.time <- DimScales(value)[[i.time]]
            if (!identical(DS.time, DS.time.1))
                stop(gettextf("time dimensions of '%s' and '%s' differ",
                              names[i], names[1L]))
        }
    }
    ## coerce data slots to type "double"
    ans <- lapply(ans, toDouble)
    ans
}


## HAS_TESTS
## assume 'initial' valid
checkInternalDims <- function(internalDims, initial, internalIn) {
    if (is.null(internalDims)) {
        if (is.null(internalIn))
            NULL
        else
            stop(gettextf("'%s' is %s but '%s' is non-%s",
                          "internalDims", "NULL", "internalIn", "NULL"))
    }
    else {
        if (!is.character(internalDims))
            stop(gettextf("'%s' does not have type \"%s\"",
                          "internalDims", "character"))
        if (identical(length(internalDims), 0L))
            stop(gettextf("'%s' has length %d",
                          "internalDims", 0L))
        if (any(is.na(internalDims)))
            stop(gettextf("'%s' has missing values",
                          "internalDims"))
        if (any(duplicated(internalDims)))
            stop(gettextf("'%s' has duplicates",
                          "internalDims"))
        names <- names(initial)
        dimtypes <- dimtypes(initial)
        for (internalDim in internalDims) {
            i <- match(internalDim, names, nomatch = 0L)
            has.dim <- i > 0L
            if (!has.dim)
                stop(gettextf("'%s' does not have dimension specified by '%s' [\"%s\"]",
                              "initial", "internalDims", internalDim))
            dimtype <- dimtypes[[i]]
            if (!identical(dimtype, "state"))
                stop(gettextf("dimension \"%s\" specified by '%s' has %s \"%s\"",
                              internalDim, "internalDims", "dimtype", dimtype))
        }
        NULL
    }
}

## HAS_TESTS
convertToCountsObj <- function(object) {
    .Data <- array(as.integer(NA),
                   dim = dim(object),
                   dimnames = dimnames(object))
    metadata <- metadata(object)
    methods::new("Counts", .Data = .Data, metadata = metadata)
}

## HAS_TESTS
## 'y' should be contained within 'x'
## looks only at endpoints - leave 'makeCompatible' to check rest
iOverlapBetweenIntervals <- function(x, y) {
    dv.x <- dimvalues(x)
    dv.y <- dimvalues(y)
    min.y <- dv.y[1L]
    max.y <- dv.y[length(dv.y)]
    i.min <- match(min.y, dv.x, nomatch = 0L)
    i.max <- match(max.y, dv.x, nomatch = 0L)
    if ((i.min == 0L) || (i.max == 0L))
        integer()
    else
        seq.int(from = i.min, to = i.max - 1L)
}

## HAS_TESTS
makeBirths <- function(birth, population, step, sex, dominant) {
    has.age <- "age" %in% dimtypes(birth)
    if (has.age)
        population <- makeCompatible(x = population,
                                     y = birth,
                                     subset = TRUE)
    if (is.null(sex))
        exposure <- 0.5 * step * population
    else {
        i.sex <- match(sex, names(population))
        n.sex <- dim(population)[i.sex]
        if (n.sex > 1L) {
            popn.dominant <- slab(population,
                                   dimension = i.sex,
                                   elements = dominant)
            popn.dominant <- as.numeric(popn.dominant)
            s <- seq_along(dim(population))
            perm <- c(s[-i.sex], i.sex)
            ## popn.dominant recycled n.sex times
            exposure <- array(popn.dominant, dim = dim(population)[perm])
            perm <- match(s, perm)
            exposure <- aperm(exposure, perm = perm)
            exposure <- 0.5 * step * exposure
        }
    }
    lambda <- exposure * birth
    not.na <- !is.na(lambda)
    .Data <- rep(NA, times = length(lambda))
    .Data[not.na] <- stats::rpois(n = sum(not.na), lambda = lambda[not.na])
    .Data <- as.integer(.Data)
    .Data <- array(.Data,
                   dim = dim(birth),
                   dimnames = dimnames(birth))
    metadata <- metadata(birth)
    methods::new("Counts", .Data = .Data, metadata = metadata)
}

## HAS_TESTS
makeDeaths <- function(death, population, upper, step) {
    prob <- makeProbDeath(death = death, upper = upper, step = step)
    metadata <- metadata(death)
    not.na <- !is.na(prob) & !is.na(population)
    .Data <- rep(NA, times = length(death))
    .Data[not.na] <- stats::rbinom(n = sum(not.na),
                            size = population[not.na],
                            prob = prob[not.na])
    .Data <- as.integer(.Data)
    .Data <- array(.Data,
                   dim = dim(metadata),
                   dimnames = dimnames(metadata))
    methods::new("Counts", .Data = .Data, metadata = metadata)
}

## HAS_TESTS
makeExternal <- function(externalIn, externalOut, population, step,
                         maxAttempt) {
    exposure.in <- 0.5 * step * (population + 1)
    exposure.out <- 0.5 * step * population
    lambda.ins <- exposure.in * externalIn
    lambda.outs <- exposure.out * externalOut
    l <- rpoisDiffConstr(lambda1 = lambda.ins,
                         lambda2 = lambda.outs,
                         min = -population,
                         maxAttempt = maxAttempt,
                         useC = TRUE)
    metadata <- metadata(externalIn)
    .Data.ins <- l$y1
    .Data.outs <- l$y2
    .Data.net <- l$y3
    .Data.ins <- array(.Data.ins,
                       dim = dim(metadata),
                       dimnames = dimnames(metadata))
    .Data.outs <- array(.Data.outs,
                        dim = dim(metadata),
                        dimnames = dimnames(metadata))
    .Data.net <- array(.Data.net,
                       dim = dim(metadata),
                       dimnames = dimnames(metadata))
    externalIns <- methods::new("Counts", .Data = .Data.ins, metadata = metadata)
    externalOuts <- methods::new("Counts", .Data = .Data.outs, metadata = metadata)
    externalNet <- methods::new("Counts", .Data = .Data.net, metadata = metadata)
    list(externalIns = externalIns,
         externalOuts = externalOuts,
         externalNet = externalNet)
}

## HAS_TESTS
makeInternal <- function(internalIn, internalOut, population,
                         countsModel, internalDims) {
    not.na <- !is.na(internalOut) & !is.na(population)
    metadata <- metadata(internalIn)
    .Data.outs <- rep(NA, times = length(internalOut))
    if (countsModel) {
        .Data.outs[not.na] <- stats::rpois(n = sum(not.na),
                                    lambda = internalOut[not.na])
        neg.popn <- .Data.outs > population
        neg.popn[is.na(neg.popn)] <- FALSE
        .Data.outs[neg.popn] <- population[neg.popn]
    }
    else
        .Data.outs[not.na] <- stats::rbinom(n = sum(not.na),
                                     size = population[not.na],
                                     prob = internalOut[not.na])
    .Data.outs <- as.integer(.Data.outs)
    .Data.outs <- array(.Data.outs,
                        dim = dim(metadata),
                        dimnames = dimnames(metadata))
    internalOuts <- methods::new("Counts", .Data = .Data.outs, metadata = metadata)
    if (countsModel)
        lambda <- internalIn
    else
        lambda <- (population + 1) * internalIn
    names <- names(lambda)
    dim <- dim(lambda)
    dimnames <- dimnames(lambda)
    i.int.dims <- match(internalDims, names)
    s <- seq_along(dim)
    perm <- c(i.int.dims, s[-i.int.dims])
    lambda <- aperm(lambda, perm = perm)
    nrow <- prod(dim[i.int.dims])
    ncol <- prod(dim[-i.int.dims])
    lambda <- matrix(lambda, nrow = nrow, ncol = ncol)
    totals <- collapseDimension(internalOuts, dimension = internalDims)
    generateIns <- function(j) {
        size <- totals[j]
        prob <- lambda[ , j]
        if (is.na(size) || any(is.na(prob)))
            rep(as.integer(NA), times = nrow)
        else
            as.integer(stats::rmultinom(n = 1L, size = size, prob = prob))
    }
    .Data.ins <- sapply(seq_len(ncol), generateIns)
    .Data.ins <- array(.Data.ins, dim = dim[perm], dimnames = dimnames[perm])
    perm <- match(s, perm)
    .Data.ins <- aperm(.Data.ins, perm = perm)
    internalIns <- methods::new("Counts", .Data = .Data.ins, metadata = metadata)
    internalNet <- internalIns - internalOuts
    list(internalIns = internalIns,
         internalOuts = internalOuts,
         internalNet = internalNet)
}

## HAS_TESTS
makeParamCompatibleWithInitial <- function(param, initial) {
    names.param <- names(param)
    param1 <- param[[1L]]
    i.time.param <- match("time", dimtypes(param1))
    name.time.param <- names(param1)[i.time.param]
    DS.time.param <- DimScales(param1)[[i.time.param]]
    names.init <- names(initial)
    dimtypes.init <- dimtypes(initial, use.names = FALSE)
    DimScales.init <- DimScales(initial, use.names = FALSE)
    i.time.init <- match("time", dimtypes.init, nomatch = 0L)
    has.time.init <- i.time.init > 0L
    if (has.time.init) {
        names.target <- names.init
        dimtypes.target <- dimtypes.init
        DimScales.target <- replace(DimScales.init,
                                    list = i.time.init,
                                    values = list(DS.time.param))
    }
    else {
        if (name.time.param %in% names.init)
            stop(gettextf("\"%s\" dimensions of '%s' and '%s' not compatible",
                          name.time.param, names.param[1L], "initial"))
        names.target <- c(names.init, name.time.param)
        dimtypes.target <- c(dimtypes.init, "time")
        DimScales.target <- c(DimScales.init, list(DS.time.param))
    }
    metadata.target <- methods::new("MetaData",
                           nms = names.target,
                           dimtypes = dimtypes.target,
                           DimScales = DimScales.target)
    .Data.target <- array(0L,
                          dim = dim(metadata.target),
                          dimnames = dimnames(metadata.target))
    target <- methods::new("Counts", .Data = .Data.target, metadata = metadata.target)
    for (i in seq_along(param)) {
        val <- param[[i]]
        name <- names.param[i]
        if (identical(name, "birth")) {
            i.age.target <- match("age", dimtypes.init, nomatch = 0L)
            has.age.target <- i.age.target > 0L
            if (has.age.target) {
                DS.age.target <- DimScales(target)[[i.age.target]]
                i.age.birth <- match("age", dimtypes(val), nomatch = 0L)
                has.age.birth <- i.age.birth > 0L
                if (!has.age.birth)
                    stop(gettextf("'%s' has age dimension but '%s' does not",
                                  "initial", "birth"))
                DS.age.birth <- DimScales(val)[[i.age.birth]]
                i.overlap <- iOverlapBetweenIntervals(x = DS.age.target, y = DS.age.birth)
                if (identical(i.overlap, integer()))
                    stop(gettextf("age dimensions of '%s' and '%s' are not compatible",
                                  "initial", "birth"))
                target.i <- slab(target, dimension = i.age.target, elements = i.overlap)
            }
            else
                target.i <- target
        }
        else
            target.i <- target
        val <- tryCatch(makeCompatible(x = val, y = target.i, subset = TRUE),
                        error = function(e) e)
        if (methods::is(val, "error"))
            stop(gettextf("'%s' and '%s' not compatible : %s",
                          name, "initial", val$message))
        param[[i]] <- val
    }
    param
}

## HAS_TESTS
makePopulationObj <- function(initial, param) {
    names.init <- names(initial)
    dimtypes.init <- dimtypes(initial, use.names = FALSE)
    DimScales.init <- DimScales(initial, use.names = FALSE)
    i.time.init <- match("time", dimtypes.init, nomatch = 0L)
    has.time.init <- i.time.init > 0L
    param1 <- param[[1L]]
    i.time.param <- match("time", dimtypes(param1))
    name.time.param <- names(param1)[i.time.param]
    DS.time.param <- DimScales(param1)[[i.time.param]]
    dv.time.param <- dimvalues(DS.time.param)
    DS.time.ans <- methods::new("Points", dimvalues = dv.time.param)
    if (has.time.init) {
        names.ans <- names.init
        dimtypes.ans <- dimtypes.init
        DimScales.ans <- replace(DimScales.init,
                                 list = i.time.init,
                                 values = list(DS.time.ans))
    }
    else {
        if (name.time.param %in% names.init)
            stop(gettextf("\"%s\" dimensions of '%s' and '%s' not compatible",
                          name.time.param, names(param)[1L], "initial"))
        names.ans <- c(names.init, name.time.param)
        dimtypes.ans <- c(dimtypes.init, "time")
        DimScales.ans <- c(DimScales.init, list(DS.time.ans))
    }
    metadata.ans <- methods::new("MetaData",
                        nms = names.ans,
                        dimtypes = dimtypes.ans,
                        DimScales = DimScales.ans)
    .Data.ans <- array(as.integer(NA),
                       dim = dim(metadata.ans),
                       dimnames = dimnames(metadata.ans))
    methods::new("Counts", .Data = .Data.ans, metadata = metadata.ans)
}

## HAS_TESTS
makeProbDeath <- function(death, upper, step) {
    ans <- 0.5 * step * death
    if (upper) {
        ans[!is.na(ans) & (ans > 1)] <- 1
        ans
    }
    else {
        ans / (1 + ans)
    }
}

## HAS_TESTS
makeProjectForward <- function(initial, param) {
    i.time.init <- match("time", dimtypes(initial), nomatch = 0L)
    has.time.init <- i.time.init > 0L
    if (has.time.init) {
        DS.time.init <- DimScales(initial)[[i.time.init]]
        dv.init <- dimvalues(DS.time.init)
        param1 <- param[[1L]]
        i.time.param <- match("time", dimtypes(param1))
        DS.time.param <- DimScales(param1)[[i.time.param]]
        dv.param <- dimvalues(DS.time.param)
        if (isTRUE(all.equal(dv.init, dv.param[1L])))
            TRUE
        else if (isTRUE(all.equal(dv.init, dv.param[length(dv.param)])))
            FALSE
        else
            stop(gettextf("time dimensions for '%s' and '%s' incompatible",
                          "initial", names(param)[1L]))
    }
    else
        TRUE
}

## TRANSLATED
## HAS_TESTS
## The function does not recycle its arguments.
## It processes NAs without complaining.
rpoisDiffConstr <- function(lambda1, lambda2, min, maxAttempt = 1000L, useC = FALSE) {
    ## lambda1
    stopifnot(is.numeric(lambda1))
    stopifnot(length(lambda1) > 0L)
    stopifnot(all(lambda1[!is.na(lambda1)] >= 0))
    ## lambda2
    stopifnot(is.numeric(lambda2))
    stopifnot(all(lambda2[!is.na(lambda2)] >= 0))
    ## min
    stopifnot(is.integer(min))
    ## maxAttempt
    stopifnot(is.integer(maxAttempt))
    stopifnot(identical(length(maxAttempt), 1L))
    stopifnot(!is.na(maxAttempt))
    stopifnot(maxAttempt > 0L)
    ## lambda1 and lambda2
    stopifnot(identical(length(lambda2), length(lambda1)))
    ## lambda1 and min
    stopifnot(identical(length(min), length(lambda1)))
    ##
    if (useC) {
        .Call(rpoisDiffConstr_R, lambda1, lambda2, min, maxAttempt)
    }
    else {
        ## Non-double lambda should be picked up in preamble,
        ## but this makes the R function equivalent to the
        ## C function, which will fail if passed non-double.
        if (!is.double(lambda1))
            stop(gettextf("''%s' does not have type \"%s\"",
                          "lambda1", "double"))
        if (!is.double(lambda2))
            stop(gettextf("''%s' does not have type \"%s\"",
                          "lambda2", "double"))
        n <- length(lambda1)
        y1 <- integer(length = n)
        y2 <- integer(length = n)
        y3 <- integer(length = n)
        for (i in seq_len(n)) {
            l1 <- lambda1[i]
            l2 <- lambda2[i]
            m <- min[i]
            if (is.na(l1) || is.na(l2) || is.na(m)) {
                y1[i] <- as.integer(NA)
                y2[i] <- as.integer(NA)
                y3[i] <- as.integer(NA)
            }
            else {
                found <- FALSE
                attempt <- 0L
                while (!found && (attempt < maxAttempt)) {
                    attempt <- attempt + 1L
                    prop1 <- stats::rpois(n = 1L, lambda = l1)
                    prop1 <- as.integer(prop1)
                    prop2 <- stats::rpois(n = 1L, lambda = l2)
                    prop2 <- as.integer(prop2)
                    found <- (prop1 - prop2) >= m
                }
                if (found) {
                    y1[i] <- prop1
                    y2[i] <- prop2
                    y3[i] <- prop1 - prop2
                }
                else {
                    y1[i] <- as.integer(NA)
                    y2[i] <- as.integer(NA)
                    y3[i] <- m
                }
            }
        }
        list(y1 = y1, y2 = y2, y3 = y3)
    }
}

