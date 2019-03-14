
## General-purpose checking functions

## HAS_TESTS
checkAndTidyPercentage <- function(value, name) {
    if (!is.numeric(value))
        stop(gettextf("'%s' has class \"%s\"",
                      name, class(value)))
    if (!identical(length(value), 1L))
        stop(gettextf("'%s' does not have length %d",
                      name, 1L))
    if (is.na(value))
        stop(gettextf("'%s' is missing",
                      name))
    if ((value < 0) || (value > 100))
        stop(gettextf("'%s' is not between %d and %d",
                      name, 0L, 100L))
    as.numeric(value)
}

checkLogicalFlag <- function(value, name) {
    if (!is.logical(value))
        stop(gettextf("'%s' has class \"%s\"",
                      name, class(value)))
    if (!identical(length(value), 1L))
        stop(gettextf("'%s' does not have length %d",
                      name, 1L))
    if (is.na(value))
        stop(gettextf("'%s' is missing",
                      name))
    NULL             
}

checkPositiveVector <- function(value, name) {
    if (!is.numeric(value))
        stop(gettextf("'%s' has class \"%s\"",
                      name, class(value)))
    if (any(is.na(value)))
        stop(gettextf("'%s' has missing values",
                      name))
    if (!all(value > 0))
        stop(gettextf("'%s' has non-positive values",
                      name))
    NULL             
}
